###############################################################################
##  MEANS AND COMPARISONS
###############################################################################
## What a table of means shows follows the comparison chosen, and only it:
##   - the means alone, with SE(m) and SE(d) at the foot;
##   - the critical difference (C.D., Fisher's protected LSD) at the foot,
##     and no letters;
##   - superscript letters from a multiple-comparison procedure (Tukey's
##     HSD, Duncan's, Student-Newman-Keuls, Scheffe's, the Bonferroni-
##     adjusted LSD), and no C.D.;
##   - Dunnett's test against a control, as a table of comparisons.
## A C.D. and letters are never shown together: they come from different
## procedures, and printing both invites reading one by the other's rule.
## The calculations stay in the engine (analyze(), posthoc()); this file
## decides what is shown. Letters and verdicts appear only when the effect's
## F-test is significant at the chosen level, as everywhere in DOEpro.

## the choices, as the Means & Comparisons tab offers them
CMP_CHOICES <- c(
  "Means only (no comparisons)" = "none",
  "Critical difference, C.D. (Fisher's protected LSD)" = "LSD (Fisher's protected)",
  "Tukey's HSD (letters)" = "Tukey HSD",
  "Duncan's multiple range test, DMRT (letters)" = "Duncan's DMRT",
  "Student-Newman-Keuls test, SNK (letters)" = "Student-Newman-Keuls",
  "Scheffe's test (letters)" = "Scheffe",
  "Bonferroni-adjusted LSD (letters)" = "LSD (Bonferroni-adjusted)",
  "Dunnett's test (each treatment against a control)" = "Dunnett")

## each procedure as running text names it
CMP_NAME <- c(
  "none" = "no comparison",
  "LSD (Fisher's protected)" = "Fisher's protected LSD",
  "Tukey HSD" = "Tukey's HSD",
  "Duncan's DMRT" = "Duncan's multiple range test (DMRT)",
  "Student-Newman-Keuls" = "the Student-Newman-Keuls (SNK) test",
  "Scheffe" = "Scheffe's test",
  "LSD (Bonferroni-adjusted)" = "the Bonferroni-adjusted LSD",
  "Dunnett" = "Dunnett's test")

## what the tables show for a procedure: "none", "cd", "letters" or "dunnett"
cmp_kind <- function(method) {
  if (is.null(method) || !nzchar(method) || identical(method, "none")) "none"
  else if (identical(method, "LSD (Fisher's protected)")) "cd"
  else if (identical(method, "Dunnett")) "dunnett"
  else "letters"
}

#' The comparison shown with the tables of means
#'
#' Describes how the tables of means of \code{\link{build_report}} (and of the
#' application) compare the means: not at all, by the critical difference
#' (Fisher's protected LSD), by the letters of a multiple-comparison
#' procedure, or by Dunnett's test against a control. Letters and a critical
#' difference are never shown together.
#'
#' @param method \code{"none"} for the means alone, or one of the
#'   multiple-comparison procedures: \code{"LSD (Fisher's protected)"} (shown as the
#'   C.D.), \code{"LSD (Bonferroni-adjusted)"}, \code{"Tukey HSD"},
#'   \code{"Duncan's DMRT"}, \code{"Student-Newman-Keuls"}, \code{"Scheffe"}
#'   (shown as letters) or \code{"Dunnett"}.
#' @param control For Dunnett's test, the control level; the first level when
#'   \code{NULL}.
#' @param alternative For Dunnett's test: \code{"two.sided"}, \code{"greater"}
#'   or \code{"less"}.
#' @param effect For Dunnett's test, the effect whose levels include the
#'   control; the first main effect when \code{NULL}.
#'
#' @return A list with the method, what the tables show (\code{kind}) and the
#'   settings of Dunnett's test.
#'
#' @examples
#' rr <- run_all(demo_data("CRD"), "CRD", list(treat = "Treatment"), "Yield")
#' html <- build_report(rr, comparison = comparison("Tukey HSD"))
#'
#' @export
comparison <- function(method = "none", control = NULL, alternative = "two.sided", effect = NULL) {
  method <- if (is.null(method) || !nzchar(method)) "none" else method
  if (!method %in% c("none", PH_METHODS))
    stop(sprintf("Unknown comparison '%s'. Use \"none\" or one of: %s.", method,
                 paste(sprintf("\"%s\"", PH_METHODS), collapse = ", ")), call. = FALSE)
  list(method = method, kind = cmp_kind(method), control = control,
       alternative = alternative %||% "two.sided", effect = effect)
}

## The letters of `effect` by the chosen procedure, in the order of its table
## of means: empty when the procedure gives no letters or the F-test is not
## significant. `x` is the procedure's result, for the readings; `err` says
## why letters could not be worked out (Tukey's test with one error degree
## of freedom, for instance).
cmp_letters <- function(res, effect, cmp, by = NULL) {
  e <- res$effects[[effect]]
  out <- list(letters = rep("", nrow(e$means)), x = NULL, err = NULL)
  if (!identical(cmp$kind, "letters")) return(out)
  x <- tryCatch(posthoc(res, effect, cmp$method, e$alpha, by = by), error = function(err) err)
  if (inherits(x, "error")) { out$err <- conditionMessage(x); return(out) }
  x <- gate_posthoc(x)
  out$letters <- x$letters
  out$x <- x
  out
}

## a mean as a table prints it: back-transformed with the transformed value
## in parentheses when there is a transformation, and its letter, if any
cmp_cell <- function(mu, letter = "", digits = 2, mu_bt = NULL) {
  letter[is.na(letter)] <- ""
  if (is.null(mu_bt)) paste0(fmt(mu, digits), sup(letter))
  else paste0(fmt(mu_bt, digits), " (", fmt(mu, digits), ")", sup(letter))
}

## what a table's caption says it shows
cmp_caption <- function(cmp, alpha, any_tr = FALSE) {
  what <- switch(cmp$kind,
    none = "means",
    cd = "means, with the critical difference (C.D.) at the foot",
    letters = sprintf("means, with letters from %s at the %s%% level", CMP_NAME[[cmp$method]], pct(alpha)),
    dunnett = "means (the comparisons with the control follow)")
  paste0(" &mdash; ", what,
         if (any_tr) "; for a transformed response, the back-transformed mean with the transformed value in parentheses" else "")
}

## The rows at the foot of a table of means: SE(m) and SE(d) always, the
## C.D. only when the comparison is the C.D., and the CV. `within` qualifies
## the single figures of an interaction compared within one factor's levels.
cmp_foot_rows <- function(e, cmp, digits, cv_text = NULL, within = "") {
  c(list(c(paste0("SE(m) &plusmn;", within), err_text(e, "sem", digits)),
         c(paste0("SE(d) &plusmn;", within), err_text(e, "sed", digits))),
    if (identical(cmp$kind, "cd")) list(c(paste0(cd_name(e$alpha), within), cd_or_ns(e, digits))),
    if (!is.null(cv_text)) list(c("C.V. (%)", cv_text)))
}

## An effect's further comparisons (split, strip and pooled interactions):
## each SE(d) always, each C.D. only when the comparison is the C.D.
cmp_extra <- function(e, cmp, digits = 3) {
  xt <- extra_text(e, digits)
  if (!length(xt) || identical(cmp$kind, "cd")) return(xt)
  xt[startsWith(names(xt), "SE(d)")]
}

## The note under a table of means: what its letters compare, and why a
## response has none
cmp_letters_note <- function(cmp, e, L, header = NULL) {
  if (!identical(cmp$kind, "letters")) return(character(0))
  who <- if (is.null(header)) "" else sprintf(" for %s", header)
  if (!is.null(L$err))
    return(sprintf("%s cannot be applied%s: %s", cap1(CMP_NAME[[cmp$method]]), who, L$err))
  if (!effect_sig(e))
    return(sprintf("No letters%s: the F-test for %s is not significant at the %s%% level.", who, e$label, pct(e$alpha)))
  character(0)
}

## the letters' rule, at the foot of the section
cmp_rule <- function(cmp, alpha) {
  switch(cmp$kind,
    none = paste("No comparison is made: the F-test says whether the means differ at all, and a comparison",
                 "method, chosen above, says which differ."),
    cd = sprintf(paste("Two means differ significantly at the %s%% level when their difference is larger than the C.D.",
                       "(Fisher's protected LSD); NS means the F-test is not significant at that level, so no C.D. is given."),
                 pct(alpha)),
    letters = sprintf(paste("Means sharing a letter do not differ significantly by %s at the %s%% level. Letters are given",
                            "only where the F-test is significant at that level."), CMP_NAME[[cmp$method]], pct(alpha)),
    dunnett = sprintf("Each treatment is compared with the control by Dunnett's test at the %s%% level; the comparisons follow the tables.",
                      pct(alpha)))
}

## ------------------------------------------------------------ readings ----

## The reading of a table of means without comparisons: the highest and
## lowest mean as printed and the F-test's verdict
means_only_text <- function(e, digits = 2, tr = "none", cmp = comparison()) {
  a <- e$alpha; lvl <- rd_lvl(a); v <- paste(e$vars, collapse = " x ")
  lab <- apply(e$means[e$vars], 1, function(z) paste(trimws(z), collapse = " x "))
  k <- length(lab); shown <- rd_shown(e, tr); pr <- fmt(shown, digits)
  top <- at_extreme(pr, max); bot <- at_extreme(pr, min)
  ends <- if (length(top) == k) sprintf("All %d %s means print as %s.", k, v, pr[1])
    else if (k == 2) sprintf("%s has the higher mean (%s) and %s the lower (%s).", lab[top], pr[top], lab[bot], pr[bot])
    else sprintf("%s %s the highest mean (%s) and %s %s the lowest (%s).", join_and(lab[top]),
                 pl(length(top), "has", "share"), pr[top[1]], join_and(lab[bot]), pl(length(bot), "has", "share"), pr[bot[1]])
  if (is.na(e$p)) return(ends)
  verdict <- if (effect_sig(e))
    sprintf("The F-test for %s is %s: there is evidence that the %s means are not all equal.%s", e$label, rd_sig(e$p, a), v,
            switch(cmp$kind, none = " Choose a comparison method to see which of them differ.",
                   dunnett = " Dunnett's comparisons with the control follow.", ""))
  else sprintf("The F-test for %s is %s: there is insufficient evidence that the %s means differ%s.", e$label, rd_sig(e$p, a), v,
               if (identical(cmp$kind, "letters")) ", so no letters are given" else "")
  paste(ends, verdict)
}

## The reading of a one-factor table of means, as the comparison chosen
## judges it: the C.D. reading, the procedure's own verdicts, or the means
## and the F-test alone. A significant interaction involving the factor is
## pointed out.
cmp_means_text <- function(fit, effect, cmp, digits = 2, L = NULL) {
  r <- fit$final; e <- r$effects[[effect]]; tr <- fit$trans
  if (exact_fit(r)) return(RD_EXACT)
  note <- rd_avg_note(r, e$vars)
  body <- switch(cmp$kind,
    cd = means_body(e, digits, tr),
    letters = {
      L <- L %||% cmp_letters(r, effect, cmp)
      if (!is.null(L$err)) paste(means_only_text(e, digits, tr, comparison()),
                                 sprintf("%s cannot be applied here: %s", cap1(CMP_NAME[[cmp$method]]), L$err))
      else if (!effect_sig(e)) means_only_text(e, digits, tr, cmp)
      else posthoc_groups_text(L$x, e, tr, digits = digits)
    },
    means_only_text(e, digits, tr, cmp))
  if (nzchar(note)) paste(body, note) else body
}

## The reading of a two-way table of means: the interaction's pattern, then
## how the cells are compared under the procedure chosen
cmp_twoway_text <- function(fit, f1, f2, cmp, digits = 2, L = NULL, cd_digits = digits) {
  r <- fit$final; key <- paste0(f1, ":", f2); e <- r$effects[[key]]; tr <- fit$trans
  if (is.null(e) || is.na(e$p)) return("")
  if (cmp$kind == "cd") return(twoway_text(r, e, f1, f2, digits, tr, cd_digits))
  base <- twoway_text(r, e, f1, f2, digits, tr, cd_digits, rule = "")
  if (exact_fit(r) || !effect_sig(e) || cmp$kind != "letters") return(base)
  L <- L %||% cmp_letters(r, key, cmp)
  if (!is.null(L$err)) return(paste(base, sprintf("%s cannot be applied here: %s", cap1(CMP_NAME[[cmp$method]]), L$err)))
  paste(base, posthoc_groups_text(L$x, e, tr, digits = digits))
}

## ------------------------------------------------- comparisons of an effect --

## The comparisons of one effect of one response by the procedure chosen,
## as HTML: the error term, the means with their verdicts, the procedure's
## parameters and every comparison, each followed by its reading. `by` is
## the factor within whose levels an interaction is compared (simple effects).
cmp_effect_html <- function(fit, effect, cmp, by = NULL) {
  r <- fit$final; e <- r$effects[[effect]]
  if (is.null(e)) return("")
  head <- sprintf("<h4 class='slx-resp'>%s</h4>", esc(fit$header))
  if (identical(cmp$kind, "none"))
    return(paste0(head, "<div class='note'>Choose a comparison method above to compare these means.</div>"))
  x <- tryCatch(posthoc(r, effect, cmp$method, e$alpha, control = cmp$control,
                        alternative = cmp$alternative %||% "two.sided", by = by),
                error = function(err) list(err = conditionMessage(err)))
  if (!is.null(x$err)) return(paste0(head, "<div class='err'>", esc(x$err), "</div>"))
  x <- gate_posthoc(x)
  eb <- effect_by(e, by)
  exact <- exact_fit(r)
  err_box <- if (is.null(e$mix)) sprintf(
      "Comparisons use the error term of <b>%s</b>: error mean square %s on %s degrees of freedom, at the %s%% significance level.",
      esc(e$label), fmt(e$mse, 4), df_text(e$df), pct(e$alpha))
    else sprintf(paste("Comparisons of <b>%s</b> means use, for each pair, the environment interactions of the factors",
                       "in which the two differ, with Satterthwaite's degrees of freedom for each pair, at the %s%% significance level."),
                 esc(e$label), pct(e$alpha))
  g <- x$groups
  if (cmp$kind == "cd") g$Group <- NULL
  names(g)[names(g) == "Group"] <- "Letters"
  lsd <- if (isTRUE(x$f_sig) && cmp$method != "LSD (Fisher's protected)")
    tryCatch(posthoc(r, effect, "LSD (Fisher's protected)", e$alpha, by = by), error = function(err) NULL)
  ranges <- if (is.null(x$ranges)) "" else paste0(
    df_html(x$ranges, caption = "Critical ranges"),
    "<div class='note'>p is the number of means spanned by the comparison once the means are ranked in order.</div>")
  paste0(head,
    sprintf("<div class='box'><b>What the test does.</b> %s<br>%s</div>", esc(PH_WHAT[[cmp$method]]), err_box),
    if (length(x$note)) paste0("<div class='warn'>", paste(x$note, collapse = "<br><br>"), "</div>") else "",
    df_html(g, caption = if (cmp$kind == "dunnett") "Each treatment against the control" else "The means, highest first"),
    rd_html(posthoc_groups_text(x, eb, fit$trans, exact), "What the comparisons show."),
    "<details class='slx-fold'><summary>Every comparison, and the test's parameters</summary>",
    df_html(x$stats, caption = "Test parameters"), ranges,
    df_html(x$pairs, caption = if (cmp$kind == "dunnett") "Comparisons with the control" else "Every pair of means"),
    rd_html(posthoc_pairs_text(x, eb, cmp$method, lsd, exact)),
    "</details>")
}

## --------------------------------------------------------------- exports ----

## One row per mean of every effect, for the CSV of the Means tab. The
## columns follow the comparison chosen: the C.D. only with the C.D., the
## letters only with a letter procedure. SEm is that mean's own standard
## error (for a split, strip or pooled interaction, the SE(m) for comparing
## means within one level of its slicing factor); SEd and the C.D. are given
## only when one value applies to every pair they describe.
means_csv <- function(rr, cmp = comparison()) {
  out <- do.call(rbind, lapply(names(rr$fits), function(nm) {
    fit <- rr$fits[[nm]]; r <- fit$final
    do.call(rbind, lapply(names(r$effects), function(en) {
      e <- r$effects[[en]]; m <- e$means
      L <- cmp_letters(r, en, cmp)
      xt <- cmp_extra(e, cmp)
      note <- c(
        if (is.na(e$sed)) "SEd differs between pairs of means; the comparisons on the Means & Comparisons tab list each pair."
        else if (!isTRUE(e$equal_rep)) "SEm differs between means.",
        if (!is.null(e$slice))
          sprintf("SEd%s apply to means at the same level of %s. %s", if (cmp$kind == "cd") " and CD" else "", e$slice,
                  paste(sprintf("%s = %s", names(xt), xt), collapse = "; ")),
        cmp_letters_note(cmp, e, L))
      out <- data.frame(
        Response = nm, Transformation = TRANS[[fit$trans]]$lab, Effect = e$label,
        Level = apply(m[e$vars], 1, paste, collapse = " x "),
        Mean = m$Mean,
        Unadjusted_mean = if ("Raw_mean" %in% names(m)) m$Raw_mean else NA_real_,
        Back_transformed = if ("Mean_bt" %in% names(m)) m$Mean_bt else NA_real_,
        N = m$N, SD = m$SD, SEm = m$SE, SEd = e$sed,
        alpha = e$alpha, p_value = e$p, Comparison = if (cmp$kind == "none") "None" else CMP_NAME[[cmp$method]],
        row.names = NULL, check.names = FALSE, stringsAsFactors = FALSE)
      if (cmp$kind == "cd") out$CD <- if (effect_sig(e)) e$cd else NA_real_
      if (cmp$kind == "letters") out$Letters <- L$letters
      out$Note <- paste(note, collapse = " ")
      out
    }))
  }))
  rownames(out) <- NULL
  out
}
