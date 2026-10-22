###############################################################################
##  REPORT  (HTML on screen and to print; PDF where a renderer is installed)
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
  "Please cite: ", CITE_AUTHORS, " (", format(Sys.Date(), "%Y"), "). ", APP_NAME,
  ": analysis of designed agricultural experiments. Version ", APP_VERSION,
  ". Zenodo. doi:<a href='https://doi.org/", APP_DOI, "'>", APP_DOI, "</a><br>",
  format(Sys.time(), "%d %B %Y, %H:%M"), "</div>")

## the heading over one response's analysis of variance
anova_heading <- function(header) sprintf("Analysis of Variance (%s)", header)

## An ANOVA table as the app and the report show it, its significance column
## headed with the level it is judged at
anova_show <- function(an, alpha) {
  d <- anova_display(an, alpha)
  names(d)[names(d) == "Signif"] <- sprintf("Significance (%s%%)", pct(alpha))
  d
}

#' Render an analysis as an HTML report
#'
#' Turns the result of \code{\link{run_all}} into a complete, self-contained HTML
#' report: the analysis of variance of each response, the tables of means with
#' their standard errors and the comparison chosen, the checks of the
#' assumptions, and a plain-English interpretation. The report carries the
#' StatLabX credit and the DOEpro citation, and prints cleanly on A4 (the
#' browser's \emph{Print} then \emph{Save as PDF} gives a PDF).
#'
#' @param rr The object returned by \code{\link{run_all}}.
#' @param comparison How the means are compared, from
#'   \code{\link{comparison}}: the means alone (the default), the critical
#'   difference, the letters of a multiple-comparison procedure, or Dunnett's
#'   test. Letters and a critical difference are never shown together, and
#'   letters are suppressed for any effect whose F-test is not significant.
#' @param detailed If \code{TRUE}, include the full per-effect tables of means.
#'   If \code{FALSE}, give the compact summary tables only.
#' @param screen If \code{TRUE}, add a small credit line fixed to the corner of
#'   the screen.
#' @param digits Decimal places for the means in the tables of means. A
#'   standard error or critical difference that would print as zero is given
#'   more.
#' @param letters_on No longer used: the comparison decides whether letters are
#'   shown. Kept so that older calls still run.
#' @param nature The declared nature of each response, for the summary of the
#'   checks: \code{"auto"}, \code{"continuous"}, \code{"count"} or
#'   \code{"percent"}, one value or one per response (named).
#'
#' @return A character string of HTML, of length one.
#'
#' @examples
#' rr <- run_all(demo_data("CRD"), "CRD", list(treat = "Treatment"), "Yield")
#' html <- build_report(rr, comparison = comparison("Tukey HSD"))
#' substr(html, 1, 60)
#' \donttest{
#' # writeLines(html, "report.html")
#' }
#'
#' @export
build_report <- function(rr, comparison = NULL, detailed = TRUE, screen = FALSE, digits = 2,
                         letters_on = NULL, nature = "auto") {
  ## an older call passing letters_on (TRUE or FALSE) second
  if (is.logical(comparison)) comparison <- NULL
  cmp <- if (is.null(comparison)) comparison_spec("none") else comparison
  fits <- rr$fits
  heads <- vapply(fits, function(f) esc(f$header), character(1))
  cmp_txt <- if (cmp$kind == "none") "means only" else CMP_NAME[[cmp$method]]
  hdr <- sprintf(paste0(
    "<header class='rpt-head'><img class='rpt-logo' src='%s' alt='StatLabX'>",
    "<div><h1>Analysis report</h1><div class='rpt-sub'>Generated with %s &middot; %s v%s</div></div></header>",
    "<p class='meta'>Design: <b>%s</b> &nbsp;|&nbsp; Response variable(s): <b>%s</b> &nbsp;|&nbsp; ",
    "Significance level: <b>%s</b> &nbsp;|&nbsp; Comparison of means: <b>%s</b> &nbsp;|&nbsp; %s</p>%s"),
    LOGO_URI, APP_BRAND, APP_NAME, APP_VERSION,
    names(DESIGNS)[match(rr$design, DESIGNS)], paste(heads, collapse = ", "),
    p_lab(rr$alpha), cmp_txt, format(Sys.Date(), "%d %B %Y"), authors_html())

  ## Dunnett's comparisons are made on one effect, against one control
  dunnett <- if (cmp$kind == "dunnett") {
    eff <- cmp$effect %||% rr$facs[1]
    paste(vapply(names(fits), function(nm) cmp_effect_html(fits[[nm]], eff, cmp), character(1)), collapse = "")
  } else ""

  parts <- c(hdr,
    "<section class='rpt-sec'><h2>1. Analysis of variance</h2>",
    paste(vapply(names(fits), function(nm) paste0(
      "<div class='rpt-block'><h3>", esc(anova_heading(fits[[nm]]$header)), "</h3>",
      df_html(anova_show(fits[[nm]]$final$anova, rr$alpha)),
      anova_note(fits[[nm]]$final), "</div>"), character(1)), collapse = ""),
    "</section>",
    "<section class='rpt-sec'><h2>2. Means and comparisons</h2>",
    means_section_html(rr, digits = digits, cmp = cmp, detailed = detailed),
    if (nzchar(dunnett)) paste0("<h3>Dunnett's comparisons with the control</h3>",
                                gsub("<details class='slx-fold'>", "<details class='slx-fold' open>", dunnett, fixed = TRUE)) else "",
    "</section>",
    "<section class='rpt-sec'><h2>3. Assumptions and diagnostics</h2>",
    paste(vapply(names(fits), function(nm) {
      f <- fits[[nm]]
      paste0("<div class='rpt-block'><h3>", esc(f$header), "</h3>",
             assum_summary_html(assum_summary(f, dtype_for(nature, nm))),
             assum_table_html(f$asm),
             sprintf("<div class='note'>Suggested transformation%s: <b>%s</b>. %s</div>",
                     if (identical(f$trans, "none")) ""
                     else sprintf(", judged on the values as entered (the checks above are on the %s scale)", TRANS[[f$trans]]$lab),
                     TRANS[[f$sug$method]]$lab, esc(f$sug$why)), "</div>")
    }, character(1)), collapse = ""),
    "</section>",
    "<section class='rpt-sec'><h2>4. Interpretation</h2>",
    paste(vapply(names(fits), function(nm) {
      f <- fits[[nm]]
      paste0("<div class='rpt-block'><h3>", esc(f$header), "</h3>",
             as.character(interpret(f$final, f$asm, f$sug, TRANS[[f$trans]]$lab, cmp)), "</div>")
    }, character(1)), collapse = ""),
    "</section>",
    credit_block(),
    sprintf("<div class='print-foot'>%s &middot; %s</div>", esc(CREDIT_LONG), format(Sys.Date(), "%d %B %Y")),
    if (screen) sprintf("<div class='screencredit'>%s</div>", CREDIT_SHORT) else "")

  paste0("<!DOCTYPE html><html lang='en'><head><meta charset='utf-8'>",
         "<meta name='viewport' content='width=device-width, initial-scale=1'>",
         "<title>StatLabX report</title><style>", REPORT_CSS, MEANS_CSS, "</style></head><body>",
         "<main class='rpt'>", paste(parts, collapse = "\n"), "</main></body></html>")
}

## comparison() under another name, for build_report(), whose argument of
## that name would otherwise hide it
comparison_spec <- function(...) comparison(...)

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
    if (p >= a$alpha) "no significant departure" else "<b>significant departure</b>"
  }
  stat <- function(lab, v) if (is.finite(v)) paste(lab, "=", fmt(v)) else "-"
  pv <- function(p) if (is.finite(p)) p_eq(p, a$alpha) else "-"
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
      else if (is.na(a$slope)) "-"
      ## judged by the slope's own test, as the mean-variance plot's note is
      else if (is.na(a$slope_p)) "cannot be tested with three points"
      else if (a$slope_p >= a$alpha)
        sprintf("insufficient evidence that the variance changes with the mean (%s)", p_eq(a$slope_p, a$alpha))
      else sprintf("the variance %s with the mean (%s)", if (a$slope > 0) "rises" else "falls",
                   p_eq(a$slope_p, a$alpha))))
  rows <- c(rows, sprintf(
    "<tr><td>Optimal Box-Cox lambda</td><td colspan='2'>%s</td><td>%s</td></tr>",
    ## at the end of the range searched the profile is still rising: the
    ## optimum lies beyond it, and the figure is only where the search stopped
    if (!is.na(a$lambda) && abs(a$lambda) >= 2 - 1e-9) sprintf("beyond %s", fmt(a$lambda, 2)) else fmt(a$lambda, 2),
    if (isTRUE(a$exact)) "not estimable (the model fits every value exactly)"
    else if (is.null(a$bc)) "not estimable (response must be &gt; 0)"
    else sprintf("%s%% CI %.2f to %.2f", pct(a$bc$level), a$bc$ci[1], a$bc$ci[2])))
  rows <- c(rows, sprintf(
    "<tr><td>Possible outliers (|std resid| &gt; 3)</td><td colspan='2'>%s</td><td></td></tr>",
    if (length(a$outliers)) paste(a$outliers, collapse = ", ") else "none"))
  lev <- a$levene; bart <- a$bartlett
  left <- join_and(head_more(lev$left_out, 6))
  sig <- function(p) is.finite(p) && p < a$alpha
  notes <- c(
    if (isTRUE(a$on_residuals) && !isTRUE(a$exact))
      paste("The design has blocks, so the tests of equal variances use each plot's residual, recentred on its",
            "treatment's mean: the block differences, which the error variance does not include, would otherwise",
            "hide unequal variances. The Taylor slope uses the values themselves, as residuals would flatten it."),
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
  paste0("<table class='doe'><caption>The tests in detail</caption><thead><tr><th>Test</th><th>Statistic</th><th>p</th>",
         "<th>Verdict</th></tr></thead><tbody>", paste(rows, collapse = ""), "</tbody></table>",
         sprintf("<div class='note'>Verdicts are at the %s%% significance level.%s</div>", pct(a$alpha),
                 paste0(" ", notes, collapse = "")))
}

## The report's own look, on screen and on paper. On paper: A4, the
## navigation and buttons gone, no table or heading split from what follows
## it, each response's block kept together where it fits, wide tables on a
## landscape page (where the browser supports named pages), and the credit
## line at the foot of every page.
REPORT_CSS <- "
@page{size:A4 portrait;margin:15mm 13mm 18mm 13mm}
@page wide{size:A4 landscape;margin:13mm 13mm 16mm 13mm}
*{box-sizing:border-box}
body{font-family:Segoe UI,Inter,Helvetica,Arial,sans-serif;margin:0;background:#F5F7FB;color:#1C2B4A;line-height:1.5}
main.rpt{max-width:1060px;margin:24px auto;background:#fff;border:1px solid #E2E8F2;border-radius:16px;padding:28px 34px 40px}
.rpt-head{display:flex;align-items:center;gap:18px;border-bottom:3px solid #3B7DD8;padding-bottom:10px;margin-bottom:6px}
.rpt-head h1{margin:0;font-size:26px;color:#14284B;letter-spacing:-.01em}
.rpt-sub{font-size:12px;color:#5B6B86;letter-spacing:.04em}
.rpt-logo{height:46px;width:auto;flex:0 0 auto}
h2{color:#14284B;border-bottom:1px solid #C8D8EE;margin:30px 0 10px;padding-bottom:4px;font-size:20px;break-after:avoid;page-break-after:avoid}
h3{color:#1B4F9C;margin:18px 0 6px;font-size:16px;break-after:avoid;page-break-after:avoid}
h4{margin:12px 0 4px;color:#1B4F9C;break-after:avoid;page-break-after:avoid}
p.meta{color:#5B6B86;font-size:12px;margin:8px 0 0}
table.doe{border-collapse:collapse;margin:8px 0 4px 0;font-size:13px;font-variant-numeric:tabular-nums;break-inside:avoid;page-break-inside:avoid}
table.doe th,table.doe td{border:1px solid #C3D0E2;padding:5px 10px;text-align:right}
table.doe th{background:#EEF3FA;text-align:center;color:#14284B}
table.doe td:first-child,table.doe th:first-child{text-align:left}
table.doe caption{caption-side:top;text-align:left;font-weight:600;padding:6px 0;color:#1B4F9C}
table.doe tfoot td{background:#FAFCFF;font-size:12px}
table.doe-summary td{text-align:left;vertical-align:top;font-size:12.5px}
td.cdrow{text-align:left !important;background:#FAFCFF;font-size:12px}
table.foot{font-size:12px;margin-top:0;background:#FAFCFF}
span.sig{color:#C0392B;font-weight:600}
span.ns{color:#7A889E}
sup{color:#1B4F9C;font-weight:600}
.slx-kind{display:inline-block;margin-top:3px;padding:1px 7px;border-radius:999px;font-size:10.5px;font-weight:600;
  background:#EEF3FA;color:#1B4F9C}
.slx-kind-designrequirement{background:#FFF4E0;color:#8A5A00}
.slx-kind-visualcheck{background:#EEF8F1;color:#1E7A44}
.block,.rpt-block{margin-bottom:22px}
.note{font-size:12px;color:#5B6B86;margin:2px 0 14px 0}
.box{border:1px solid #E2E8F2;border-left:4px solid #3B7DD8;border-radius:10px;padding:10px 14px;margin:10px 0;font-size:13px}
.warn{background:#FFF6E5;border-left:4px solid #E8A33D;padding:8px 12px;margin:10px 0;border-radius:8px}
.err{background:#FDEEEC;border-left:4px solid #C0392B;padding:8px 12px;margin:10px 0;border-radius:8px}
details.slx-fold>summary{cursor:pointer;color:#1B4F9C;font-weight:600;margin:6px 0}
.authors{font-size:12px;color:#444;margin:12px 0 0 0}
.creditblock{margin-top:30px;padding-top:8px;border-top:1px solid #C8D8EE;
  font-size:11px;color:#5B6B86;text-align:right;break-inside:avoid;page-break-inside:avoid}
.print-foot{display:none}
.screencredit{position:fixed;right:12px;bottom:8px;font-size:10px;color:#8a8a8a;
  background:rgba(255,255,255,.85);padding:2px 6px;border-radius:3px}
@media print{
  body{background:#fff;font-size:11pt}
  main.rpt{max-width:none;margin:0;border:0;border-radius:0;padding:0 0 10mm}
  .screencredit{display:none}
  details.slx-fold>summary{display:none}
  details.slx-fold{display:block}
  details.slx-fold>*{display:block}
  table.doe{font-size:10pt}
  table.doe.wide{page:wide}
  table.doe tbody tr:hover td{background:inherit}
  .rpt-sec h2{break-before:auto}
  .print-foot{display:block;position:fixed;bottom:0;left:0;right:0;text-align:right;font-size:8pt;color:#666}
  a{color:inherit;text-decoration:none}
}
"
