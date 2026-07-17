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
      df_html(anova_display(fits[[nm]]$final$anova))), character(1)), collapse = ""),
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

assum_table_html <- function(a) {
  rows <- character(0)
  if (!is.null(a$shapiro)) rows <- c(rows, sprintf(
    "<tr><td>Shapiro-Wilk (normality of residuals)</td><td>W = %s</td><td>p = %s</td><td>%s</td></tr>",
    fmt(a$shapiro$statistic), pval(a$p_norm),
    if (isTRUE(a$p_norm > 0.05)) "OK" else "violated"))
  if (!is.null(a$levene)) rows <- c(rows, sprintf(
    "<tr><td>Levene, median-centred (homogeneity)</td><td>F = %s</td><td>p = %s</td><td>%s</td></tr>",
    fmt(a$levene$F), pval(a$p_hov), if (isTRUE(a$p_hov > 0.05)) "OK" else "violated"))
  if (!is.null(a$bartlett)) rows <- c(rows, sprintf(
    "<tr><td>Bartlett (homogeneity)</td><td>K2 = %s</td><td>p = %s</td><td>%s</td></tr>",
    fmt(a$bartlett$statistic), pval(a$bartlett$p.value),
    if (isTRUE(a$bartlett$p.value > 0.05)) "OK" else "violated"))
  rows <- c(rows, sprintf(
    "<tr><td>Taylor's power-law slope b</td><td colspan='2'>%s</td><td>%s</td></tr>",
    fmt(a$slope, 2), if (is.na(a$slope)) "-" else if (abs(a$slope) < 0.5)
      "variance independent of mean" else "variance depends on mean"))
  rows <- c(rows, sprintf(
    "<tr><td>Optimal Box-Cox lambda</td><td colspan='2'>%s</td><td>%s</td></tr>",
    fmt(a$lambda, 2),
    if (is.null(a$bc)) "not estimable (response must be &gt; 0)"
    else sprintf("95%% CI %.2f to %.2f", a$bc$ci[1], a$bc$ci[2])))
  rows <- c(rows, sprintf(
    "<tr><td>Possible outliers (|std resid| &gt; 3)</td><td colspan='2'>%s</td><td></td></tr>",
    if (length(a$outliers)) paste(a$outliers, collapse = ", ") else "none"))
  paste0("<table class='doe'><thead><tr><th>Test</th><th>Statistic</th><th>p</th>",
         "<th>Verdict</th></tr></thead><tbody>", paste(rows, collapse = ""), "</tbody></table>")
}
