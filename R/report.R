###############################################################################
##  REPORT  (HTML on screen, PDF via a headless browser / weasyprint)
###############################################################################

authors_html <- function(with_contact = FALSE) paste0(
  "<div class='authors'><b>Developed by</b><br>",
  paste(vapply(seq_along(AUTHORS), function(i) {
    a <- AUTHORS[[i]]
    orc <- if (!is.null(a$orcid) && !is.na(a$orcid))
      sprintf(" &nbsp;<a href='https://orcid.org/%s'>ORCID</a>", a$orcid) else ""
    eml <- if (with_contact && !is.null(a$email) && !is.na(a$email))
      sprintf(" &nbsp;&middot;&nbsp; <a href='mailto:%s'>%s</a>", a$email, a$email) else ""
    sprintf("%d. %s &mdash; %s, %s%s%s", i, a$name, a$role, a$aff, orc, eml)
  }, character(1)), collapse = "<br>"),
  "</div>")

credit_block <- function() paste0(
  "<div class='creditblock'>", CREDIT_LONG, "<br>",
  "Please cite: Shah, I. A., Khan, U. J. &amp; Jeelani, M. I. (",
  format(Sys.Date(), "%Y"), "). ", APP_NAME,
  ": analysis of designed agricultural experiments. Version ", APP_VERSION,
  ". Zenodo. doi:<a href='https://doi.org/", APP_DOI, "'>", APP_DOI, "</a><br>",
  format(Sys.time(), "%d %B %Y, %H:%M"), "</div>")

#' Render an analysis as an HTML report
#'
#' Turns the result of \code{\link{run_all}} into a complete, self-contained HTML
#' report: the analysis of variance, the tables of means with their standard
#' errors and critical differences, the checks of the assumptions, and a
#' plain-English interpretation. The report carries the DOEpro citation in its
#' footer.
#'
#' @param rr The object returned by \code{\link{run_all}}.
#' @param letters_on Show the grouping letters. Letters are suppressed
#'   automatically for any effect whose F-test is not significant, whatever this
#'   is set to.
#' @param detailed If \code{TRUE}, include the full per-effect tables of means.
#'   If \code{FALSE}, give the compact summary tables only.
#' @param screen If \code{TRUE}, return a fragment styled for display inside the
#'   application. If \code{FALSE}, return a complete standalone HTML document.
#'
#' @return A character string of HTML, of length one.
#'
#' @examples
#' rr <- run_all(demo_data("CRD"), "CRD", list(treat = "Treatment"), "Yield")
#' html <- build_report(rr)
#' substr(html, 1, 60)
#' \donttest{
#' # writeLines(html, "report.html")
#' }
#'
#' @export
build_report <- function(rr, letters_on = TRUE, detailed = TRUE, screen = FALSE) {
  fits <- rr$fits
  hdr <- sprintf(
    "<div class='rpt-head'><img class='rpt-logo' src='%s' alt='DOEpro'><h1>Analysis of Variance Report</h1></div><p class='meta'>Design: <b>%s</b> &nbsp;|&nbsp; Response variable(s): <b>%s</b> &nbsp;|&nbsp; Significance level: <b>%s</b> &nbsp;|&nbsp; %s</p>%s",
    LOGO_URI,
    names(DESIGNS)[match(rr$design, DESIGNS)],
    paste(vapply(fits, function(f) f$header, character(1)), collapse = ", "),
    rr$alpha, format(Sys.Date(), "%d %B %Y"), authors_html())

  anv <- combined_anova_html(rr)
  parts <- c(hdr,
    "<h2>1. Analysis of variance</h2>",
    if (!is.null(anv)) anv else "",
    paste(vapply(names(fits), function(nm) paste0(
      "<h4>", fits[[nm]]$header, "</h4>",
      df_html(anova_display(fits[[nm]]$final$anova, rr$alpha)),
      anova_note(fits[[nm]]$final)), character(1)), collapse = ""),
    "<h2>2. Tables of means</h2>",
    means_section_html(rr, letters_on = letters_on, detailed = detailed),
    "<h2>3. Assumptions and transformation</h2>",
    paste(vapply(names(fits), function(nm) {
      f <- fits[[nm]]
      sprintf("<h4>%s</h4>%s<div class='note'>Suggested transformation: <b>%s</b>. %s</div>",
              f$header, assum_table_html(f$asm), TRANS[[f$sug$method]]$lab, f$sug$why)
    }, character(1)), collapse = ""),
    "<h2>4. Interpretation</h2>",
    paste(vapply(names(fits), function(nm) {
      f <- fits[[nm]]
      paste0("<h4>", f$header, "</h4>",
             as.character(interpret(f$final, f$asm, f$sug, TRANS[[f$trans]]$lab)))
    }, character(1)), collapse = ""),
    credit_block(),
    if (screen) sprintf("<div class='screencredit'>%s</div>", CREDIT_SHORT) else "")

  paste0("<!DOCTYPE html><html><head><meta charset='utf-8'>",
         "<title>DOEpro report</title><style>", REPORT_CSS, MEANS_CSS, "</style></head><body>",
         paste(parts, collapse = "\n"), "</body></html>")
}

## Compile the HTML report to PDF.  Tries, in order: pagedown (headless Chrome),
## weasyprint, wkhtmltopdf.  Returns TRUE on success.
save_pdf <- function(html, outfile) {
  tmp <- tempfile(fileext = ".html")
  writeLines(html, tmp, useBytes = TRUE)

  ftr <- sprintf(paste0("<div style=\"font-size:8px;color:#666;width:100%%;",
                        "padding:0 12mm 0 0;text-align:right\">%s &nbsp;|&nbsp; ",
                        "page <span class='pageNumber'></span> of ",
                        "<span class='totalPages'></span></div>"), CREDIT_SHORT)

  if (requireNamespace("pagedown", quietly = TRUE)) {
    ok <- tryCatch({
      pagedown::chrome_print(tmp, output = outfile, verbose = 0, timeout = 90,
        options = list(displayHeaderFooter = TRUE, printBackground = TRUE,
                       headerTemplate = "<span></span>", footerTemplate = ftr,
                       marginTop = 0.6, marginBottom = 0.75))
      file.exists(outfile)
    }, error = function(e) FALSE)
    if (isTRUE(ok)) return(TRUE)
  }
  for (bin in c("weasyprint", "wkhtmltopdf")) {
    p <- Sys.which(bin)
    if (!nzchar(p)) next
    args <- if (bin == "weasyprint") c(shQuote(tmp), shQuote(outfile))
            else c("--enable-local-file-access", "--footer-right", shQuote(CREDIT_SHORT),
                   "--footer-font-size", "7", shQuote(tmp), shQuote(outfile))
    ok <- tryCatch(system2(p, args, stdout = FALSE, stderr = FALSE) == 0L,
                   error = function(e) FALSE)
    if (isTRUE(ok) && file.exists(outfile)) return(TRUE)
  }
  FALSE
}

## What sits under an ANOVA table: the key to the stars at the chosen level, and
## a warning when the sums of squares are adjusted ones that need not add up.
anova_note <- function(res) {
  paste0("<div class='note'>", star_key(res$alpha),
         if (isTRUE(res$adjusted_ss))
           paste0(" The data are unbalanced, so each term is tested after allowing for ",
                  "every other term (Type III sums of squares); these need not add up to ",
                  "the total.") else "",
         "</div>")
}

## The assumption checks, each judged at the analysis's significance level.
## A test that could not be computed says so and why; it is never given a
## verdict, least of all "significant departure".
assum_table_html <- function(a) {
  rows <- character(0)
  ok <- function(p, why = NULL) {
    if (!is.finite(p)) return(paste0("not available", if (length(why)) paste0(": ", esc(why)) else ""))
    if (p > a$alpha) "no significant departure" else "<b>significant departure</b>"
  }
  stat <- function(lab, v) if (is.finite(v)) paste(lab, "=", fmt(v)) else "-"
  pv <- function(p) if (is.finite(p)) p_eq(p) else "-"
  rows <- c(rows, sprintf(
    "<tr><td>Shapiro-Wilk (normality of residuals)</td><td>%s</td><td>%s</td><td>%s</td></tr>",
    stat("W", if (is.null(a$shapiro)) NA else a$shapiro$statistic), pv(a$p_norm), ok(a$p_norm, a$norm_why)))
  rows <- c(rows, sprintf(
    "<tr><td>Levene, median-centred (homogeneity)</td><td>%s</td><td>%s</td><td>%s</td></tr>",
    stat("F", a$levene$F), pv(a$levene$p), ok(a$levene$p, a$levene$why)))
  rows <- c(rows, sprintf(
    "<tr><td>Bartlett (homogeneity)</td><td>%s</td><td>%s</td><td>%s</td></tr>",
    stat("K2", a$bartlett$statistic), pv(a$bartlett$p.value), ok(a$bartlett$p.value, a$bartlett$why)))
  rows <- c(rows, sprintf(
    "<tr><td>Taylor's power-law slope b</td><td colspan='2'>%s</td><td>%s</td></tr>",
    dfmt(a$slope, 2), if (isTRUE(a$exact)) "not estimable (the model fits every value exactly)"
      else if (is.na(a$slope)) "-" else if (abs(a$slope) < 0.5)
      "little sign that the variance changes with the mean"
      else if (a$slope > 0) "the variance appears to rise with the mean"
      else "the variance appears to fall as the mean rises"))
  rows <- c(rows, sprintf(
    "<tr><td>Optimal Box-Cox lambda</td><td colspan='2'>%s</td><td>%s</td></tr>",
    fmt(a$lambda, 2),
    if (isTRUE(a$exact)) "not estimable (the model fits every value exactly)"
    else if (is.null(a$bc)) "not estimable (response must be &gt; 0)"
    else sprintf("%s%% CI %.2f to %.2f", pct(a$bc$level), a$bc$ci[1], a$bc$ci[2])))
  rows <- c(rows, sprintf(
    "<tr><td>Possible outliers (|std resid| &gt; 3)</td><td colspan='2'>%s</td><td></td></tr>",
    if (length(a$outliers)) paste(a$outliers, collapse = ", ") else "none"))
  lev <- a$levene; bart <- a$bartlett
  left <- join_and(head_more(lev$left_out, 6))
  sig <- function(p) is.finite(p) && p <= a$alpha
  notes <- c(
    if (isTRUE(a$exact)) paste("The model fits every value exactly: each observation equals its fitted value,",
                               "so there is no", if (isTRUE(a$strata)) "sub-plot" else "residual",
                               "error variation to test or to plot."),
    if (isTRUE(lev$zeros_removed > 0))
      paste("In cells with an odd number of values the median is one of the values, so its deviation is always",
            "zero; Levene's test leaves that one zero out (Hines and O'Hara Hines, 2000), without which it could",
            "hardly ever find unequal variances with three values per cell."),
    if (!isTRUE(a$exact) && !is.finite(lev$p) && lev$cells < 2) "Levene's test needs at least three values in a cell.",
    if (is.finite(lev$p) && lev$dropped > 0)
      sprintf("Levene's test leaves out %s, which %s fewer than three values, so it compares only the other cells.",
              left, if (lev$dropped == 1) "has" else "have"),
    if (identical(a$hov_test, "Bartlett"))
      sprintf("The verdict on equal variances uses Bartlett's test, which covers every cell%s but assumes the values are normal.",
              if (bart$dropped > 0) " with two or more values" else ""),
    if (identical(a$hov_test, "Levene") && lev$dropped > 0)
      "Bartlett's test cannot be run here, so the verdict on equal variances rests on Levene's test of the cells it could use.",
    ## the two tests disagree: say which verdict is used, and why
    if (identical(a$hov_test, "Levene") && lev$dropped == 0 && is.finite(bart$p.value) && sig(bart$p.value) != sig(lev$p))
      if (sig(bart$p.value)) paste("Bartlett's test finds a departure that Levene's does not. Bartlett's is the more",
                                   "sensitive of the two when the values are normal but is easily misled when they are not,",
                                   "so the verdict follows Levene's; treat the variances with some caution.")
      else "Levene's test finds a departure that Bartlett's does not; the verdict follows Levene's, which does not assume normality.",
    if (is.finite(bart$p.value) && bart$dropped > 0)
      sprintf("Bartlett's test leaves out %d %s with a single value.", bart$dropped,
              if (bart$dropped == 1) "cell" else "cells"))
  paste0("<table class='doe'><thead><tr><th>Test</th><th>Statistic</th><th>p</th>",
         "<th>Verdict</th></tr></thead><tbody>", paste(rows, collapse = ""), "</tbody></table>",
         sprintf("<div class='note'>Verdicts are at the %s%% significance level.%s</div>", pct(a$alpha),
                 paste0(" ", notes, collapse = "")))
}
