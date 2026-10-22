###############################################################################
##  DIAGNOSTICS OF THE ANALYSIS OF VARIANCE
###############################################################################
## The checks of the Assumptions & Diagnostics tab beyond the tests of
## normality and equal variances (R/assumptions.R): Tukey's test of
## additivity in designs with blocks, Cook's distance for plots with unusual
## influence, and the summary that sets each check beside what it means and
## what to do. Some assumptions can be tested from the data; the independence
## of the errors cannot, and rests on the randomisation and the layout.

## Tukey's one-degree-of-freedom test for non-additivity: does a treatment's
## effect stay the same from block to block, or does it grow with the
## block's level, as multiplicative effects do? The squared fitted values of
## the additive model are added to it as one more term; its sum of squares
## is Tukey's (1949) non-additivity sum of squares, exact for a complete
## block-by-treatment table and the usual extension for a missing plot or a
## Latin square. Only designs with blocks (or rows and columns) and one error
## term have such a question to ask.
tukey_additivity <- function(res) {
  out <- list(F = NA_real_, df1 = 1, df2 = NA_real_, p = NA_real_, applies = FALSE, why = NULL)
  if (!res$design %in% c("RCBD", "LSD", "FRCBD")) {
    out$why <- if (res$design %in% c("CRD", "FCRD"))
      "the design has no blocks, so there is no block-by-treatment additivity to test"
    else "it is not assessed for designs with more than one error term"
    return(out)
  }
  out$applies <- TRUE
  if (exact_fit(res)) { out$why <- "the model fits every value exactly"; return(out) }
  fit <- res$lm
  d <- res$data
  f <- stats::fitted(fit)
  q <- (f - mean(f))^2
  d$.tukey <- q / max(stats::sd(q), .Machine$double.eps)
  f2 <- tryCatch(stats::lm(stats::update(stats::formula(fit), . ~ . + .tukey), data = d),
                 error = function(e) NULL)
  if (is.null(f2) || f2$rank <= fit$rank || stats::df.residual(f2) < 1) {
    out$why <- "there are too few error degrees of freedom once the term is added"
    return(out)
  }
  a <- stats::anova(fit, f2)
  out$F <- a$F[2]; out$df2 <- a$Res.Df[2]; out$p <- a$`Pr(>F)`[2]
  if (!is.finite(out$p)) { out$why <- "the test could not be computed"; out$p <- NA_real_ }
  out
}

## Cook's distance of every analysed plot, and Cook's guide for a large one:
## the median of the F distribution on p and n - p degrees of freedom, p the
## number of parameters of the model. A plot that alone determines a mean
## (leverage 1) has a residual of zero whatever its value, so its distance is
## not defined and it is named instead.
cooks_info <- function(res) {
  if (exact_fit(res)) return(list(d = NULL, why = "the model fits every value exactly, so no plot can change the fit"))
  fit <- res$lm
  h <- stats::hatvalues(fit)
  D <- suppressWarnings(stats::cooks.distance(fit))
  lev1 <- which(h > 1 - 1e-8)
  D[lev1] <- NA
  p <- fit$rank; n <- length(D)
  guide <- if (n > p) stats::qf(0.5, p, n - p) else NA_real_
  hh <- h[-lev1]; if (!length(lev1)) hh <- h
  list(d = unname(D), rows = row_ids(res$data), guide = guide, p = p, n = n,
       flag = which(!is.na(D) & is.finite(guide) & D > guide), lev1 = lev1,
       equal_h = length(hh) > 0 && diff(range(hh)) < 1e-8, why = NULL)
}

plot_cooks <- function(res) {
  ci <- cooks_info(res)
  if (is.null(ci$d)) return(NULL)
  df <- data.frame(i = seq_along(ci$d), D = ci$d, row = ci$rows)
  df <- df[!is.na(df$D), , drop = FALSE]
  if (!nrow(df)) return(NULL)
  df$big <- df$i %in% ci$flag
  far <- df[df$big, , drop = FALSE]
  ggplot(df, aes(x = .data$i, y = .data$D)) +
    geom_segment(aes(xend = .data$i, yend = 0), colour = "#C9D6EA", linewidth = 0.8) +
    geom_point(aes(colour = .data$big), size = 2.3, show.legend = FALSE) +
    scale_colour_manual(values = c(`FALSE` = SLX_BLUE, `TRUE` = SLX_RED)) +
    (if (is.finite(ci$guide)) geom_hline(yintercept = ci$guide, linetype = 2, colour = "#5B6B86") else NULL) +
    (if (nrow(far)) geom_text(data = far, aes(label = paste("row", .data$row)), vjust = -0.9, size = 3.4, colour = SLX_RED) else NULL) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(title = "Cook's distance of each plot", x = "Plot (in the order of the data)", y = "Cook's distance",
         caption = if (is.finite(ci$guide)) sprintf("Dashed line: Cook's guide, the median of F on %d and %d d.f. (%s).",
                                                    ci$p, ci$n - ci$p, fmt(ci$guide, 3)) else NULL) +
    theme_doe()
}

## "What it shows" and the reading of the plot of Cook's distances
cooks_text <- function(res) {
  ci <- cooks_info(res)
  if (is.null(ci$d)) return(NULL)
  what <- paste("Cook's distance measures how far the fitted values would move if one plot were left out: a plot with a",
                "large distance pulls the analysis towards itself. The dashed line is Cook's guide, the median of the F",
                "distribution on the model's parameters and its error degrees of freedom; a plot above it has unusual influence.")
  big <- ci$flag
  mx <- which.max(replace(ci$d, is.na(ci$d), -Inf))
  out <- c(
    if (length(big)) sprintf("%s %s above the guide (%s); check %s for a recording error and, if %s correct, see whether the conclusions change without %s.",
                             cap1(rows_text(ci$rows[big])), pl(length(big), "lies", "lie"),
                             paste(sprintf("D = %s", fmt(ci$d[big], 3)), collapse = ", "),
                             pl(length(big), "it", "them"), pl(length(big), "it is", "they are"), pl(length(big), "it", "them"))
    else if (length(mx) && is.finite(ci$d[mx]))
      sprintf("No plot lies above the guide (%s); the largest distance is %s, at row %s.", fmt(ci$guide, 3), fmt(ci$d[mx], 3), ci$rows[mx]),
    if (ci$equal_h) paste("Every plot has the same leverage here, as in any balanced layout, so Cook's distance follows the size",
                          "of the residual and picks out the same plots as the residual plot."),
    if (length(ci$lev1)) sprintf(paste("%s alone %s %s mean, so %s residual is zero whatever %s value and %s distance is not",
                                       "defined; such a plot cannot be checked against the others."),
                                 cap1(rows_text(ci$rows[ci$lev1])), pl(length(ci$lev1), "determines", "determine"),
                                 pl(length(ci$lev1), "its", "their"), pl(length(ci$lev1), "its", "their"),
                                 pl(length(ci$lev1), "its", "their"), pl(length(ci$lev1), "its", "their")))
  list(what = what, reading = paste(out, collapse = " "))
}

## --------------------------------------------------- summary of the checks --

## What each nature of response means for the analysis of variance
NATURE <- c(auto = "Detect automatically", continuous = "Continuous measurement",
            count = "Counts (insects, grains, spores)", percent = "Percentage / proportion")

nature_text <- function(nature, sug) {
  switch(nature,
    count = c(paste("Counts are not normal: their variance usually rises with the mean. The analysis of variance treats",
                    "them as roughly normal, which the square-root transformation often makes reasonable."),
              paste("A transformation does not guarantee the assumptions: check the plots again after applying one.",
                    "Counts with many zeros or very small means may need a generalised linear model (Poisson or",
                    "negative binomial), which this app does not fit.")),
    percent = c(paste("Percentages and proportions are bounded by 0 and 100 (or 0 and 1), and near the bounds their variance",
                      "shrinks. The angular (arcsine square-root) transformation is conventional for proportions out of a",
                      "fixed number of units, such as germination out of 50 seeds."),
                paste("A transformation does not guarantee the assumptions: check the plots again after applying one.",
                      "Proportions out of a known number of units may be better analysed with a binomial model, which",
                      "this app does not fit.")),
    continuous = c("A continuous measurement, analysed as it is entered unless a check below finds a problem.",
                   "Use the checks below to decide whether a transformation is needed."),
    c(if (identical(sug$method, "none")) "No particular nature was declared; the adviser judged the response from its values and name."
      else sprintf("No particular nature was declared; the adviser suggests %s.", TRANS[[sug$method]]$lab),
      "Declare the nature of the response if it is a count or a percentage: the advice then follows it."))
}

## One row for each check of one response's analysis: what was checked and
## whether it can be tested from the data, the result, what it means, and
## what to do. `fit` is one response's fit (fit_final()); its checks are on
## the scale analysed. `nature` is the declared nature of the response.
assum_summary <- function(fit, nature = "auto") {
  r <- fit$final; a <- fit$asm; al <- r$alpha; lvl <- rd_lvl(al)
  exact <- isTRUE(a$exact)
  rows <- list()
  add <- function(check, kind, result, meaning, action)
    rows[[length(rows) + 1L]] <<- data.frame(Check = check, Kind = kind, Result = result, Meaning = meaning,
                                             Action = action, stringsAsFactors = FALSE)
  na_result <- function(why) paste("Not available:", why)
  ## 1. normality of the residuals
  if (exact) add("Normality of the residuals", "Tested", na_result("the model fits every value exactly"),
                 "There is no error variation to check.", "Check that each value is one plot's observation.")
  else if (is.na(a$p_norm))
    add("Normality of the residuals", "Tested", na_result(a$norm_why %||% "too few residuals"),
        "The test cannot say whether the residuals are normal.", "Judge from the Q-Q plot and the histogram below.")
  else {
    dep <- a$p_norm < al
    add("Normality of the residuals", "Tested",
        sprintf("Shapiro-Wilk W = %s, %s", fmt(a$shapiro$statistic, 3), rd_p(a$p_norm, al)),
        if (dep) sprintf("The residuals depart from a normal distribution (significant at the %s level).", lvl)
        else sprintf("Insufficient evidence at the %s level that the residuals depart from normal.", lvl),
        if (dep) paste("Look at the Q-Q plot: a stray value is checked first; a curved pattern suggests a transformation",
                       "(see the suggestion below). Moderate departures matter little with equal replication.")
        else "None needed; a small departure may still have gone undetected.")
  }
  ## 2. equal variances
  grp <- if (isTRUE(r$pooled)) "the treatment cells within each environment"
         else if (length(r$facs) > 1) "the treatment combinations" else "the treatments"
  if (exact) add("Equal variances", "Tested", na_result("the model fits every value exactly"), "There is no error variation to compare.", "-")
  else if (is.na(a$p_hov))
    add("Equal variances", "Tested", na_result(a$hov_why %||% "too few values"),
        sprintf("The variances of %s cannot be compared by a test.", grp),
        "Look at the spread in the residual plot; with two values per cell only a large difference shows.")
  else {
    dep <- a$p_hov < al
    add("Equal variances", "Tested",
        sprintf("%s across %s: %s", a$hov_short, grp, rd_p(a$p_hov, al)),
        if (dep) sprintf("The variances differ between %s (significant at the %s level).", grp, lvl)
        else sprintf("Insufficient evidence at the %s level that the variances of %s differ.", lvl, grp),
        if (dep) "A variance-stabilising transformation may help; the mean-variance slope and Box-Cox below say which."
        else "None needed.")
  }
  ## 3. the variance against the mean
  if (!exact && !is.na(a$slope))
    add("Variance against the mean (Taylor's power law)", "Tested",
        if (is.na(a$slope_p)) sprintf("b = %s (cannot be tested with three cells)", fmt(a$slope, 2))
        else sprintf("b = %s, %s", fmt(a$slope, 2), rd_p(a$slope_p, al)),
        if (is.na(a$slope_p) || a$slope_p >= al) sprintf("Insufficient evidence at the %s level that the variance changes with the mean.", lvl)
        else sprintf("The variance %s with the mean (b about 1 is typical of counts, about 2 of a multiplicative error).",
                     if (a$slope > 0) "rises" else "falls"),
        if (!is.na(a$slope_p) && a$slope_p < al && a$slope >= 0.5)
          if (a$slope < 1.5) "The square root usually suits a slope near 1." else if (a$slope < 2.5) "The logarithm usually suits a slope near 2."
          else "The reciprocal suits a slope above 2.5."
        else "None needed.")
  ## 4. Box-Cox
  if (!exact && !is.null(a$bc)) {
    inside1 <- a$bc$ci[1] <= 1 && a$bc$ci[2] >= 1
    edge <- abs(a$lambda) >= 2 - 1e-9
    add("Box-Cox power", "Tested",
        sprintf("lambda = %s%s, %s%% interval %s to %s", if (edge) "beyond " else "", fmt(a$lambda, 2), pct(a$bc$level),
                fmt(a$bc$ci[1], 2), fmt(a$bc$ci[2], 2)),
        if (inside1) sprintf("The interval includes 1: insufficient evidence at the %s level that a power transformation fits better.", lvl)
        else sprintf("The interval leaves out 1: at the %s level the values as they are fit worse than the best power.", lvl),
        if (inside1) "None needed." else "Consider the transformation suggested below, then check the plots again.")
  }
  ## 5. additivity of blocks and treatments
  ta <- tukey_additivity(r)
  if (ta$applies) {
    what <- if (r$design == "LSD") "rows, columns and treatments" else "blocks and treatments"
    if (is.na(ta$p))
      add(sprintf("Additivity of %s (Tukey's one-degree-of-freedom test)", what), "Tested", na_result(ta$why),
          "Additivity cannot be tested here.", "-")
    else {
      dep <- ta$p < al
      add(sprintf("Additivity of %s (Tukey's one-degree-of-freedom test)", what), "Tested",
          sprintf("F = %s on 1 and %s d.f., %s", fmt(ta$F, 2), df_text(ta$df2), rd_p(ta$p, al)),
          if (dep) sprintf(paste("The treatment differences change with the level of the %s (significant at the %s level), as",
                                 "when effects multiply rather than add."), if (r$design == "LSD") "rows and columns" else "blocks", lvl)
          else sprintf("Insufficient evidence at the %s level that the effects are not additive.", lvl),
          if (dep) "A logarithmic transformation often makes multiplicative effects additive; check again after it."
          else "None needed.")
    }
  }
  ## 6. outliers
  if (!exact) {
    far <- rd_far_rows(r)
    add("Outlying residuals", "Visual check",
        if (length(far)) sprintf("%s beyond -3 or 3", cap1(rows_text(far)))
        else if (!rd_can_flag(r)) sprintf("Cannot be flagged with %s error d.f.", df_text(r$dfe))
        else "None beyond -3 or 3",
        if (length(far)) "Normal residuals seldom lie this far out." else if (!rd_can_flag(r))
          "With so few error degrees of freedom no standardised residual can reach 3."
        else "No plot stands far apart from the fit.",
        if (length(far)) "Check those rows for recording or entry errors; do not delete a correct value." else "None needed.")
  }
  ## 7. influence
  ci <- cooks_info(r)
  if (!is.null(ci$d)) {
    mx <- which.max(replace(ci$d, is.na(ci$d), -Inf))
    add("Influential plots (Cook's distance)", "Visual check",
        if (length(ci$flag)) sprintf("%s above Cook's guide (%s)", cap1(rows_text(ci$rows[ci$flag])), fmt(ci$guide, 3))
        else if (length(mx) && is.finite(ci$d[mx])) sprintf("Largest D = %s (row %s); guide %s", fmt(ci$d[mx], 3), ci$rows[mx], fmt(ci$guide, 3))
        else "Not defined",
        if (length(ci$flag)) "These plots pull the fit towards themselves more than the others do."
        else "No plot has unusual influence on the fit.",
        if (length(ci$flag)) "Check them; if correct, see whether the conclusions change without them, and report both." else "None needed.")
  }
  ## 8. error variances across environments (pooled analyses)
  if (isTRUE(r$pooled) && !is.null(r$homogeneity)) {
    h <- r$homogeneity
    dep <- isTRUE(h$p < al)
    add("Equal error variances across environments (Bartlett)", "Tested",
        sprintf("chi-squared = %s on %d d.f., %s", fmt(h$chisq, 3), h$df, rd_p(h$p, al)),
        if (dep) sprintf("The environments' error variances differ (significant at the %s level), so pooling them is questionable.", lvl)
        else sprintf("Insufficient evidence at the %s level that the environments' error variances differ.", lvl),
        if (dep) "Read the pooled F-tests with caution; consider a transformation or a separate analysis of each environment."
        else "Pooling the errors is reasonable.")
  }
  ## 9. independence
  add("Independence of the errors", "Design requirement", "Not testable from these data",
      paste("Rests on the randomisation of the treatments", if (length(r$blks)) "within each block" else "to the plots",
            "and on plots not influencing one another (no spread of treatment, drift or shared watering between neighbours)."),
      "Make sure the treatments were randomised as the design requires and the plots kept apart; say so when reporting.")
  ## 10. the nature of the response
  nt <- nature_text(nature, fit$sug)
  add("Nature of the response", "Declared", if (identical(nature %||% "auto", "auto")) "Not declared" else NATURE[[nature]],
      nt[1], nt[2])
  do.call(rbind, rows)
}

assum_summary_html <- function(tab, header = NULL) {
  if (is.null(tab) || !nrow(tab)) return("")
  tag <- function(k) sprintf("<span class='slx-kind slx-kind-%s'>%s</span>", gsub("[^a-z]", "", tolower(k)), esc(k))
  body <- lapply(seq_len(nrow(tab)), function(i) c(
    paste0("<b>", esc(tab$Check[i]), "</b><br>", tag(tab$Kind[i])), esc(tab$Result[i]), esc(tab$Meaning[i]), esc(tab$Action[i])))
  raw_table(c("Check", "Result", "What it means", "Recommended action"), body,
            caption = if (is.null(header)) "Summary of the checks" else sprintf("Summary of the checks: %s", esc(header)),
            cls = "doe doe-summary")
}
