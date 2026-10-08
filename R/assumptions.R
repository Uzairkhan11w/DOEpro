###############################################################################
##  ASSUMPTIONS  &  TRANSFORMATIONS
###############################################################################
## Levene's test, median-centred (Brown-Forsythe): a one-way ANOVA on the
## absolute deviations from the cell medians. In a cell with an odd number of
## values the median is one of them, so one deviation is always zero; when
## every cell has three values that caps F at 4, and the test could never find
## unequal variances among four or fewer treatments. When every cell used is
## of odd size, each therefore loses that one zero (Hines and O'Hara Hines,
## 2000, Biometrics 56: 451-454), which by simulation holds the 5% level for
## normal, skewed and count data with three or five values per cell. When
## sizes are mixed (a missing plot among cells of four) the zero stays:
## removing it from the odd cells alone makes them look more spread out than
## the even ones, and the test then calls equal variances unequal too often,
## while the plain test holds its level there. `zeros_removed` counts the
## cells that lost a zero.
## A cell needs three values to take part. With two, both values lie equally far from their median, so
## nothing varies within the cell: a design with two replications gave an
## enormous F and a false verdict of unequal variances, and keeping two-value
## cells beside larger ones (as car::leveneTest does) adds error degrees of
## freedom with no variation in them, which by simulation calls equal
## variances unequal 13-45% of the time at the 5% level. Such cells are left
## out and named in `left_out`; when fewer than two cells remain, or the
## deviations do not vary within any cell, the test is not run and `why`
## gives the reason as a plain clause.
levene_test <- function(y, g) {
  ok <- !is.na(y) & !is.na(g)
  y <- y[ok]; g <- droplevels(factor(g[ok]))
  n <- table(g)
  out <- list(F = NA_real_, p = NA_real_, df1 = NA_real_, df2 = NA_real_,
              cells = sum(n >= 3), dropped = sum(n < 3), left_out = names(n)[n < 3],
              zeros_removed = 0L, why = NULL)
  if (out$cells < 2) {
    out$why <- if (out$cells == 0) "no cell has three or more values" else "only one cell has three or more values"
    return(out)
  }
  keep <- g %in% names(n)[n >= 3]
  constant <- all(tapply(y, g, function(v) length(unique(v)) == 1))
  y <- y[keep]; g <- droplevels(g[keep])
  z <- abs(y - stats::ave(y, g, FUN = stats::median))
  if (all(table(g) %% 2 == 1)) {
    sz <- unlist(lapply(split(seq_along(z), g), function(i) i[which(z[i] == 0)[1]]), use.names = FALSE)
    z <- z[-sz]; g <- droplevels(g[-sz]); out$zeros_removed <- length(sz)
  }
  tss <- sum((z - mean(z))^2)
  rss <- sum((z - stats::ave(z, g))^2)
  if (tss == 0 || rss <= 1e-10 * tss) {
    out$why <- if (constant) "every cell's values are identical"
               else if (all(z == 0)) "the values are identical within every cell of three or more"
               else if (out$zeros_removed > 0)
                 "with the median's own zero set aside, the other deviations from the median are equal within every cell"
               else "the deviations from the cell medians do not vary within any cell"
    return(out)
  }
  df1 <- nlevels(g) - 1; df2 <- length(z) - nlevels(g)
  Fv <- ((tss - rss) / df1) / (rss / df2)
  out[c("F", "p", "df1", "df2")] <- list(Fv, stats::pf(Fv, df1, df2, lower.tail = FALSE), df1, df2)
  out
}

## Bartlett's test of equal variances across the cells. A cell needs two
## values for a variance, so cells of one value are left out; a variance of
## zero (every value in a cell the same) has no logarithm, so the test is not
## run and `why` says so.
bartlett_cells <- function(y, g) {
  ok <- !is.na(y) & !is.na(g)
  y <- y[ok]; g <- droplevels(factor(g[ok]))
  n <- table(g)
  out <- list(statistic = NA_real_, p.value = NA_real_, cells = sum(n >= 2),
              dropped = sum(n < 2), why = NULL)
  if (out$cells < 2) {
    out$why <- "fewer than two cells have two or more values"
    return(out)
  }
  keep <- g %in% names(n)[n >= 2]
  y <- y[keep]; g <- droplevels(g[keep])
  s2 <- tapply(y, g, stats::var)
  if (any(s2 == 0)) {
    out$why <- if (all(s2 == 0)) "every cell's values are identical"
               else "the values in some cells are all the same, so those cells have no variance"
    return(out)
  }
  b <- stats::bartlett.test(y, g)
  out$statistic <- unname(b$statistic); out$p.value <- b$p.value
  out
}

## An exact fit: every observation equals its fitted value, so the residuals
## are only the rounding left by the arithmetic (about 1e-16 of the data).
## There is then no error variation to test, transform or plot, and treating
## the rounding as residuals would give verdicts about nothing.
## The rounding grows with the size of the values (10000.001 carries more
## than 0.001), so the tolerance is set against their magnitude; real
## residuals are never within a billionth of the values themselves.
exact_fit <- function(res) {
  y <- res$data[[res$resp]]
  size <- max(abs(y), na.rm = TRUE)
  spread <- max(abs(y - mean(y, na.rm = TRUE)), na.rm = TRUE)
  r <- res$resid
  !length(r) || !is.finite(size) || spread == 0 || all(abs(r) <= 1e-9 * size, na.rm = TRUE)
}

## Box-Cox log-likelihood profile.  For a positive response y and design matrix X,
##   z(lambda) = (y^lambda - 1) / (lambda * gm^(lambda-1)),   z(0) = gm * log(y)
## with gm the geometric mean of y, and  l(lambda) = -n/2 * log(RSS(z)/n).
## Computed directly, so it never depends on re-evaluating a stored model call.
## The confidence interval is at `level`, which the analysis sets to 1 - alpha.
boxcox_profile <- function(y, X, level, lambda = seq(-2, 2, 0.02)) {
  if (any(!is.finite(y)) || any(y <= 0) || is.null(X)) return(NULL)
  n <- length(y); gm <- exp(mean(log(y))); qrx <- qr(X)
  ll <- vapply(lambda, function(l) {
    z <- if (abs(l) < 1e-9) gm * log(y) else (y^l - 1) / (l * gm^(l - 1))
    rss <- sum(qr.resid(qrx, z)^2)
    if (!is.finite(rss) || rss <= 0) return(NA_real_)
    -n / 2 * log(rss / n)
  }, numeric(1))
  if (all(is.na(ll))) return(NULL)
  best <- lambda[which.max(ll)]
  ## confidence interval: lambda values within qchisq(level, 1) / 2 of the
  ## maximum log-likelihood
  inside <- lambda[!is.na(ll) & ll > max(ll, na.rm = TRUE) - 0.5 * stats::qchisq(level, 1)]
  list(x = lambda, y = ll, lambda = best, ci = range(inside), level = level)
}

## The assumption tests are judged at the analysis's own significance level, so
## a user who chose 1% is not told about departures at 5%.
check_assumptions <- function(res) {
  alpha <- res$alpha
  r <- res$resid
  d <- res$data; resp <- res$resp
  cells <- interaction(d[res$facs], drop = TRUE)
  exact <- exact_fit(res)

  sw <- if (!exact && length(r) >= 3 && length(r) <= 5000) stats::shapiro.test(r) else NULL
  norm_why <- if (exact) "the model fits every value exactly, so the residuals are all zero"
              else if (is.null(sw)) "Shapiro-Wilk needs between 3 and 5000 residuals" else NULL
  lev  <- levene_test(d[[resp]], cells)
  bart <- bartlett_cells(d[[resp]], cells)
  if (exact) {
    why <- "the model fits every value exactly"
    lev[c("F", "p")] <- NA_real_; lev$why <- why
    bart[c("statistic", "p.value")] <- NA_real_; bart$why <- why
  }
  ## The equal-variance verdict comes from the test that covers every
  ## treatment: Levene's, which does not assume normality, when every cell has
  ## three or more values; otherwise Bartlett's, which needs only two but
  ## assumes normality; and Levene's on the cells it can use only when
  ## Bartlett's cannot be run. `hov_label` names the test as the text uses it.
  lev_full <- is.finite(lev$p) && lev$dropped == 0
  hov_test <- if (lev_full) "Levene" else if (is.finite(bart$p.value)) "Bartlett"
              else if (is.finite(lev$p)) "Levene" else NA_character_
  p_hov <- if (identical(hov_test, "Levene")) lev$p else if (identical(hov_test, "Bartlett")) bart$p.value else NA_real_
  left <- join_and(head_more(lev$left_out, 6))
  hov_label <- if (identical(hov_test, "Levene") && lev_full) "Levene's test"
    else if (identical(hov_test, "Levene")) sprintf("Levene's test on the cells with three or more values (%s left out)", left)
    else if (identical(hov_test, "Bartlett")) sprintf("Bartlett's test (%s; Bartlett's assumes the values are normal)",
      if (is.finite(lev$p)) sprintf("Levene's test leaves out %s, which %s fewer than three values", left,
                                    if (lev$dropped == 1) "has" else "have")
      else sprintf("Levene's test could not be run: %s", lev$why))
    else NA_character_
  hov_short <- if (identical(hov_test, "Levene") && lev_full) "Levene's"
               else if (identical(hov_test, "Levene")) "Levene's, on some cells" else if (identical(hov_test, "Bartlett")) "Bartlett's"
               else NA_character_
  ## why equal variances could not be tested at all
  hov_why <- if (!is.na(hov_test)) NULL
    else if (identical(lev$why, bart$why)) lev$why
    else sprintf("for Levene's test, %s; for Bartlett's test, %s", lev$why, bart$why)

  ## mean-variance relationship -> Taylor's power law slope
  mv <- data.frame(m = tapply(d[[resp]], cells, mean),
                   v = tapply(d[[resp]], cells, stats::var))
  mv <- mv[stats::complete.cases(mv) & mv$m > 0 & mv$v > 0, ]
  ## with an exact fit the cell variances are block (or row and column)
  ## effects, not error, so they say nothing about the error variance
  if (exact) mv <- mv[0, ]
  slope <- if (nrow(mv) >= 3)
    unname(stats::coef(stats::lm(log(v) ~ log(m), data = mv))[2]) else NA_real_
  ## the slope's own p-value: with two values per cell each variance rests on
  ## one degree of freedom, and a slope from such variances is mostly noise
  slope_p <- if (nrow(mv) >= 4)
    tryCatch(suppressWarnings(summary(stats::lm(log(v) ~ log(m), data = mv))$coefficients[2, 4]),
             error = function(e) NA_real_)
    else NA_real_

  bc <- if (exact) NULL else tryCatch(boxcox_profile(d[[resp]], res$X, level = 1 - alpha),
                                      error = function(e) NULL)

  outliers <- if (exact) integer(0) else which(abs(r / stats::sd(r)) > 3)

  list(alpha = alpha, exact = exact, strata = isTRUE(res$design %in% c("SPLIT", "STRIP")),
       shapiro = sw, norm_why = norm_why,
       levene = lev, bartlett = bart, hov_test = hov_test, hov_label = hov_label, hov_short = hov_short,
       hov_why = hov_why, slope = slope, slope_p = slope_p,
       bc = bc, lambda = if (is.null(bc)) NA_real_ else bc$lambda,
       mv = mv, outliers = outliers,
       p_norm = if (is.null(sw)) NA_real_ else sw$p.value,
       p_hov  = p_hov)
}

## What the checks of normality and equal variances found, for the sentence
## that opens a recommendation: only the tests that could be run are named.
checks_text <- function(asm, alpha) {
  lvl <- paste0(pct(alpha), "%")
  norm <- !is.na(asm$p_norm); hov <- !is.na(asm$p_hov)
  hov_name <- sprintf("the test of equal variances (%s)", asm$hov_short)
  if (norm && hov) sprintf("Neither the normality test nor %s finds a significant departure at the %s level", hov_name, lvl)
  else if (norm) sprintf("The normality test finds no significant departure at the %s level, and equal variances could not be tested (%s)",
                         lvl, asm$hov_why %||% "too few values")
  else if (hov) sprintf("%s finds no significant departure at the %s level, and normality could not be tested (%s)",
                        paste0(toupper(substr(hov_name, 1, 1)), substring(hov_name, 2)), lvl,
                        asm$norm_why %||% "too few residuals")
  else ""
}

suggest_transform <- function(res, asm, dtype = "auto") {
  y <- res$data[[res$resp]]
  y <- y[!is.na(y)]
  pn <- asm$p_norm; ph <- asm$p_hov; b <- asm$slope; lam <- asm$lambda

  nm <- if (!is.null(res$orig_resp)) res$orig_resp else res$resp
  has_frac    <- any(abs(y - round(y)) > 1e-8)
  looks_prop  <- min(y) >= 0 && max(y) <= 1 && has_frac
  looks_count <- min(y) >= 0 && !has_frac
  in_pct_range <- min(y) >= 0 && max(y) <= 100 && !looks_prop
  ## a 0-100 range alone does NOT make a variable a percentage (most yields and
  ## heights live there too), so auto-detection also needs the column name to say so
  pct_name <- grepl("perc|pct|%|incid|sever|infest|germinat|surviv|mortal|infect|damage",
                    tolower(nm))
  looks_pct   <- in_pct_range && pct_name
  count_slope <- !is.na(b) && b >= 0.5 && b < 1.5

  ## an exact fit leaves nothing unexplained for a transformation to correct
  if (isTRUE(asm$exact))
    return(list(method = "none", optional = FALSE,
      why = paste("The model fits every value exactly, so there is no unexplained variation:",
                  "no transformation can help, and none is needed.")))

  ok <- (is.na(pn) || pn > res$alpha) && (is.na(ph) || ph > res$alpha)
  ## Equal variances are tested in full only when the verdict covers every
  ## cell; Levene's on some cells alone leaves the others untested.
  hov_full <- !is.na(ph) && !(identical(asm$hov_test, "Levene") && isTRUE(asm$levene$dropped > 0))
  ## An assumption that could not be tested has not been passed: then a rise
  ## of the variance with the mean decides instead, but only when the slope
  ## is itself significant at the chosen level, since a slope from variances
  ## of two values each is mostly noise.
  slope_evidence <- !is.na(b) && b >= 0.5 && isTRUE(asm$slope_p <= res$alpha)
  if (ok && !hov_full && slope_evidence) ok <- FALSE
  all_tested <- !is.na(pn) && hov_full

  ## When the diagnostics are satisfactory we still name the conventional
  ## transformation for data that are plainly counts or percentages, flagged as
  ## optional - agronomic convention transforms them, the diagnostics do not
  ## demand it, and the analyst should decide knowingly. The opening sentence
  ## names only the checks that could be run.
  if (ok && dtype == "auto") {
    fine <- checks_text(asm, res$alpha)
    so <- if (all_tested) paste0(fine, ", so no transformation is strictly required.")
          else if (nzchar(fine)) paste0(fine, "; the checks that could be run give no reason for a transformation.")
          else "Neither normality nor equal variances could be tested, so the checks give no reason for a transformation."
    if (looks_prop)
      return(list(method = "arcsine01", optional = TRUE,
        why = paste(so, "The response is a proportion, however, and convention is to analyse proportions on the angular (arcsine square-root) scale.")))
    ## only a slope that is itself significant shows the variance rising
    ## with the mean; otherwise the claim would rest on noise
    if (looks_count && count_slope && slope_evidence)
      return(list(method = if (min(y) < 1) "sqrt0.5" else "sqrt", optional = TRUE,
        why = sprintf("%s The response is nevertheless integer-valued with variance proportional to the mean (Taylor slope b = %.2f) - i.e. count data, for which the square root is conventional.", so, b)))
    if (looks_pct)
      return(list(method = "arcsine", optional = TRUE,
        why = sprintf("%s '%s' is bounded by 0 and 100 and named as a percentage, for which the angular (arcsine square-root) transformation is conventional.", so, nm)))
    return(list(method = "none", optional = FALSE,
      why = if (all_tested) paste0(fine, " - no transformation is needed.")
            else if (nzchar(fine)) paste0(fine, "; the checks that could be run give no reason for a transformation.")
            else "Neither normality nor equal variances could be tested, so there is no evidence for or against a transformation."))
  }

  ## user-declared data type wins over any guessing
  if (dtype == "percent")
    return(list(method = if (max(y) <= 1) "arcsine01" else "arcsine",
      why = "You declared the response to be a percentage/proportion, so the angular (arcsine square-root) transformation applies."))
  if (dtype == "count")
    return(list(method = if (min(y) < 1) "sqrt0.5" else "sqrt",
      why = "You declared the response to be a count, so the square-root transformation applies (sqrt(y+0.5) when zeros are present)."))

  if (looks_prop)
    return(list(method = "arcsine01",
      why = "The response lies between 0 and 1 and is not integer - it behaves like a proportion, so the angular (arcsine square-root) transformation applies."))

  if (looks_count && count_slope)
    return(list(method = if (min(y) < 1) "sqrt0.5" else "sqrt",
      why = sprintf("The response is integer-valued and the variance rises in proportion to the mean (Taylor slope b = %.2f) - the classic signature of count data, so the square root is indicated.", b)))

  if (looks_pct && !(looks_count && count_slope))
    return(list(method = "arcsine",
      why = sprintf("'%s' is bounded between 0 and 100 and its name suggests a percentage, so the angular (arcsine square-root) transformation applies.", nm)))

  if (!is.na(b)) {
    if (b >= 0.5 && b < 1.5)
      return(list(method = if (min(y) < 1) "sqrt0.5" else "sqrt",
        why = sprintf("Variance rises roughly in proportion to the mean (Taylor slope b = %.2f) - square root is indicated.", b)))
    if (b >= 1.5 && b < 2.5)
      return(list(method = if (min(y) <= 0) "log1" else "log",
        why = sprintf("Variance rises with the square of the mean (b = %.2f) - the logarithmic transformation is indicated.", b)))
    if (b >= 2.5)
      return(list(method = "reciprocal",
        why = sprintf("Variance rises faster than the square of the mean (b = %.2f) - the reciprocal transformation is indicated.", b)))
  }
  if (!is.na(lam)) {
    m <- if (abs(lam) < 0.15) "log" else if (abs(lam - 0.5) < 0.2) "sqrt" else
         if (abs(lam + 1) < 0.25) "reciprocal" else "boxcox"
    return(list(method = m, why = sprintf("The Box-Cox profile peaks at lambda = %.2f.", lam)))
  }
  hint <- if (in_pct_range && !pct_name)
    " Note: your response lies between 0 and 100. If it really is a percentage, set 'Nature of the response' to 'Percentage / proportion' and the angular transformation will be applied." else ""
  list(method = "none",
       why = paste0("No clear transformation is indicated. If normality is badly violated, consider a non-parametric test (Kruskal-Wallis / Friedman).", hint))
}

#' The transformations DOEpro offers
#'
#' A named list of the variance-stabilising transformations. Each entry holds a
#' label (\code{lab}), the transformation (\code{f}) and its inverse
#' (\code{inv}), which is what lets DOEpro report means on the original scale of
#' measurement alongside the transformed ones.
#'
#' @format A named list of length 9. The names are the keys used in the
#'   \code{trans} argument of \code{\link{run_all}}: \code{"none"},
#'   \code{"log"}, \code{"log1"}, \code{"sqrt"}, \code{"sqrt0.5"},
#'   \code{"arcsine"}, \code{"arcsine01"}, \code{"reciprocal"} and
#'   \code{"boxcox"}.
#'
#' @return A named list of length 9. Each element is itself a list with
#'   \code{lab}, the label shown in the application; \code{f}, a function
#'   applying the transformation; and \code{b}, a function applying its
#'   inverse, which is what allows means to be reported on the original scale
#'   of measurement.
#'
#' @examples
#' names(TRANS)
#' vapply(TRANS, `[[`, character(1), "lab")
#'
#' @export
TRANS <- list(
  none       = list(lab = "None",                       f = function(y, l) y,
                    b = function(z, l) z),
  log        = list(lab = "log(y)",                     f = function(y, l) log(y),
                    b = function(z, l) exp(z)),
  log1       = list(lab = "log(y + 1)",                 f = function(y, l) log(y + 1),
                    b = function(z, l) exp(z) - 1),
  sqrt       = list(lab = "sqrt(y)",                    f = function(y, l) sqrt(y),
                    b = function(z, l) z^2),
  sqrt0.5    = list(lab = "sqrt(y + 0.5)",              f = function(y, l) sqrt(y + 0.5),
                    b = function(z, l) z^2 - 0.5),
  arcsine    = list(lab = "arcsine sqrt(y/100), degrees",
                    f = function(y, l) asin(sqrt(y / 100)) * 180 / pi,
                    b = function(z, l) (sin(z * pi / 180))^2 * 100),
  arcsine01  = list(lab = "arcsine sqrt(y), degrees",
                    f = function(y, l) asin(sqrt(y)) * 180 / pi,
                    b = function(z, l) (sin(z * pi / 180))^2),
  reciprocal = list(lab = "1 / y",                      f = function(y, l) 1 / y,
                    b = function(z, l) 1 / z),
  boxcox     = list(lab = "Box-Cox (optimal lambda)",
                    f = function(y, l) if (abs(l) < 1e-6) log(y) else (y^l - 1) / l,
                    b = function(z, l) if (abs(l) < 1e-6) exp(z) else (z * l + 1)^(1 / l))
)
