###############################################################################
##  MULTI-RESPONSE DRIVER
###############################################################################

## Analyse every selected response variable with the same design and mapping.
## `trans` is a named character vector: response -> transformation key.
#' Analyse several responses from a designed experiment
#'
#' Runs \code{\link{analyze}} for every response variable in turn, using the same
#' design and mapping, and optionally applying a variance-stabilising
#' transformation to each. This is what the application calls when more than one
#' response is selected.
#'
#' @param d A data frame in long format.
#' @param design The design code; see \code{\link{DESIGNS}}.
#' @param map A named list mapping roles to columns; see \code{\link{analyze}}.
#'   The \code{response} element is set for each response in turn and need not be
#'   supplied.
#' @param responses A character vector of response column names.
#' @param alpha The significance level. Defaults to 0.05.
#' @param trans Either \code{NULL} (analyse every response untransformed) or a
#'   named list or character vector giving a transformation for each response.
#'   The names are the response columns and the values are keys of
#'   \code{\link{TRANS}}, for example \code{list(Incidence = "arcsine")}.
#' @param dtype Passed to the transformation adviser; \code{"auto"} lets DOEpro
#'   judge the type of each response from its name and its mean-variance
#'   behaviour.
#'
#' @return A list with \code{fits} (one entry per response, each containing the
#'   fitted analysis in \code{final}), together with the design, the mapping and
#'   the significance level.
#'
#' @examples
#' d <- demo_data("FRCBD")
#' rr <- run_all(d, "FRCBD",
#'               list(factors = c("Nitrogen", "Variety"), block = "Block"),
#'               responses = "Yield")
#' rr$fits[["Yield"]]$final$anova
#'
#' @export
run_all <- function(d, design, map, responses, alpha = 0.05,
                    trans = NULL, dtype = "auto") {
  fits <- list()
  for (v in responses) {
    mv   <- modifyList(map, list(response = v))
    base <- analyze(d, design, mv, alpha)
    asm0 <- check_assumptions(base)
    sug  <- suggest_transform(base, asm0, dtype)

    tr  <- if (is.null(trans)) "none" else {
      x <- trans[[v]]; if (is.null(x) || !nzchar(x)) "none" else x
    }
    lam <- if (!is.na(asm0$lambda)) asm0$lambda else 1

    if (identical(tr, "none")) {
      fin <- base; asm <- asm0
    } else {
      z <- suppressWarnings(TRANS[[tr]]$f(d[[v]], lam))
      if (any(!is.finite(z[!is.na(d[[v]])])))
        stop(sprintf("The %s transformation is not defined for '%s' - the column has values outside its permitted range (e.g. zero or negative).",
                     TRANS[[tr]]$lab, v))
      d2 <- d; d2[[v]] <- z
      fin <- analyze(d2, design, mv, alpha)
      fin$trans <- tr; fin$lambda <- lam
      fin$effects <- lapply(fin$effects, function(e) {
        e$means$Mean_bt <- TRANS[[tr]]$b(e$means$Mean, lam); e
      })
      asm <- check_assumptions(fin)
    }
    fits[[v]] <- list(resp = v, base = base, final = fin, asm = asm, sug = sug,
                      trans = tr, lambda = lam,
                      header = if (identical(tr, "none")) v
                               else sprintf("%s (%s)", v, TRANS[[tr]]$lab))
  }
  list(design = design, alpha = alpha, map = map, responses = responses,
       facs = fits[[1]]$final$facs, fits = fits)
}

## quick pre-flight scan: what transformation does each numeric column want?
auto_scan <- function(d, design, map, candidates, alpha = 0.05, dtype = "auto") {
  do.call(rbind, lapply(candidates, function(v) {
    tryCatch({
      r <- analyze(d, design, modifyList(map, list(response = v)), alpha)
      a <- check_assumptions(r); s <- suggest_transform(r, a, dtype)
      data.frame(Variable = v,
                 Key = s$method,
                 Optional = isTRUE(s$optional),
                 `Shapiro-Wilk p` = fmt(a$p_norm, 3),
                 `Levene p` = fmt(a$p_hov, 3),
                 `Taylor b` = fmt(a$slope, 2),
                 `Box-Cox lambda` = fmt(a$lambda, 2),
                 `CV (%)` = fmt(r$cv[length(r$cv)], 2),
                 `Suggested transformation` = TRANS[[s$method]]$lab,
                 Why = s$why, check.names = FALSE, stringsAsFactors = FALSE)
    }, error = function(e) NULL)
  }))
}

CREDIT_ASCII <- "DOEpro | Shah, Khan & Jeelani"

## Plain monospaced PDF, drawn on the base graphics device.  Used when no
## HTML-to-PDF renderer is installed, so the PDF button always works.
pdf_plain <- function(rr, file, letters_on = TRUE) {
  L <- c(sprintf("ANALYSIS OF VARIANCE REPORT   %s", format(Sys.Date(), "%d %B %Y")),
         strrep("=", 92), "",
         sprintf("Design                : %s", names(DESIGNS)[match(rr$design, DESIGNS)]),
         sprintf("Response variable(s)  : %s",
                 paste(vapply(rr$fits, function(f) f$header, character(1)), collapse = ", ")),
         sprintf("Significance level    : %s", rr$alpha), "",
         "Developed by",
         vapply(seq_along(AUTHORS), function(i) sprintf("  %d. %s - %s, %s", i,
                AUTHORS[[i]]$name, AUTHORS[[i]]$role, AUTHORS[[i]]$aff), character(1)), "")

  txt <- function(x) utils::capture.output(print(x, row.names = FALSE))
  for (nm in names(rr$fits)) {
    f <- rr$fits[[nm]]
    L <- c(L, strrep("-", 92), paste("RESPONSE:", f$header), strrep("-", 92), "",
           "ANALYSIS OF VARIANCE", txt(anova_display(f$final$anova)), "")
    for (en in names(f$final$effects)) {
      e <- f$final$effects[[en]]
      m <- e$means
      keep <- intersect(c(e$vars, "Mean", "Mean_bt", "N", "Letter"), names(m))
      L <- c(L, sprintf("MEANS: %s", e$label), txt(m[, keep, drop = FALSE]),
             sprintf("  SE(m) = %s   SE(d) = %s   C.D.(5%%) = %s   C.D.(1%%) = %s",
                     fmt(e$sem), fmt(e$sed),
                     if (!is.na(e$p) && e$p < 0.05) fmt(e$cd5) else "NS",
                     if (!is.na(e$p) && e$p < 0.01) fmt(e$cd1) else "NS"), "")
    }
    L <- c(L, sprintf("C.V. : %s",
                      paste(sprintf("%s = %s", names(f$final$cv), fmt(f$final$cv, 2)),
                            collapse = "   ")), "",
           "INTERPRETATION",
           strwrap(gsub("<[^>]*>", " ", interpret(f$final, f$asm, f$sug, TRANS[[f$trans]]$lab)),
                   width = 90), "")
  }

  per <- 62L
  pages <- split(L, ceiling(seq_along(L) / per))
  grDevices::pdf(file, width = 8.27, height = 11.69)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mar = c(2, 1, 1, 1), family = "mono")
  for (pg in pages) {
    graphics::plot.new()
    graphics::text(0, 1, paste(pg, collapse = "\n"), adj = c(0, 1), cex = 0.55)
    graphics::mtext(CREDIT_ASCII, side = 1, adj = 1, cex = 0.5, col = "grey40", line = 0.5)
  }
  invisible(TRUE)
}


HELP_HTML <- "
<h3>Quick start</h3>
<ol>
<li>Copy your data from Excel in <b>long format</b> (one row per plot) and paste it into tab 1, or load an example.</li>
<li>Go to tab 2, choose the design, map each column to its role, and press <b>Run analysis</b>.</li>
<li>Tab 3 gives every table of means with SEm&plusmn;, SEd, CD (5% and 1%) and CV(%).</li>
<li>Tab 4 tests the ANOVA assumptions and suggests a transformation.</li>
<li>Tabs 5-6 give post-hoc groupings and publication-ready plots; tab 7 writes the interpretation and exports a report.</li>
</ol>

<h3>Layout expected for each design</h3>
<table class='doe'>
<tr><th>Design</th><th>Columns you must supply</th><th>Error term used for CD</th></tr>
<tr><td>CRD</td><td>Response, Treatment</td><td>Error</td></tr>
<tr><td>RCBD</td><td>Response, Treatment, Block</td><td>Error</td></tr>
<tr><td>Latin square</td><td>Response, Treatment, Row, Column</td><td>Error</td></tr>
<tr><td>Factorial CRD / RCBD</td><td>Response, 2-4 factors (+ Block for RCBD)</td><td>Error (pooled)</td></tr>
<tr><td>Split plot</td><td>Response, Replication, Main-plot factor, Sub-plot factor</td><td>Error(a) for main plots, Error(b) for sub plots</td></tr>
<tr><td>Strip plot</td><td>Response, Replication, Horizontal factor, Vertical factor</td><td>Error(a), Error(b), Error(c)</td></tr>
</table>

<h3>Standard errors and critical differences</h3>
<p>For a mean based on <i>n</i> observations, SEm&plusmn; = &radic;(MSE/n), SEd = &radic;(2&middot;MSE/n) and CD = t<sub>&alpha;/2, df</sub> &times; SEd. Two means differ significantly when their difference exceeds the CD. CD is quoted only when the corresponding F-test is significant.</p>
<p>In a <b>split plot</b> the two factors are tested against different errors, so four different comparisons exist:</p>
<ul>
<li>two main-plot means: SEd = &radic;(2&middot;Ea/(r&middot;b))</li>
<li>two sub-plot means: SEd = &radic;(2&middot;Eb/(r&middot;a))</li>
<li>two sub-plot means at the same main plot: SEd = &radic;(2&middot;Eb/r)</li>
<li>two main-plot means at the same sub-plot level: SEd = &radic;(2[(b-1)Eb + Ea]/(r&middot;b)), tested with a Satterthwaite-weighted <i>t</i></li>
</ul>
<p>The <b>strip plot</b> uses the analogous formulae with the three error terms Ea, Eb and Ec. The app applies the correct one automatically for whichever effect you select.</p>

<h3>Choosing a transformation</h3>
<ul>
<li><b>Square root</b> - counts, variance proportional to the mean (Taylor slope b &asymp; 1). Use &radic;(y+0.5) when zeros are present.</li>
<li><b>Logarithm</b> - variance proportional to the square of the mean (b &asymp; 2), multiplicative effects. Use log(y+1) when zeros are present.</li>
<li><b>Angular (arcsine &radic;p)</b> - percentages or proportions bounded at 0-100% or 0-1.</li>
<li><b>Reciprocal</b> - variance rising faster than the square of the mean; rates and times.</li>
<li><b>Box-Cox</b> - lets the data choose the exponent; the profile plot shows the optimal &lambda;.</li>
</ul>
<p>Always analyse on the transformed scale but present <b>back-transformed means</b> (given in tab 3) with the SEd/CD from the transformed scale.</p>

<h3>Which post-hoc test?</h3>
<ul>
<li><b>LSD</b> - only after a significant F (Fisher's protected LSD); most powerful, highest false-positive risk with many treatments.</li>
<li><b>Duncan's DMRT</b> - widely used in agronomy; intermediate.</li>
<li><b>SNK</b> - intermediate, controls better than DMRT.</li>
<li><b>Tukey HSD</b> - controls the family-wise error rate; the safe default for all pairwise comparisons.</li>
<li><b>Scheffe</b> - the most conservative; suitable for complex contrasts.</li>
<li><b>Bonferroni</b> - simple and conservative; fine for a small pre-planned set of comparisons.</li>
</ul>
<p><b>Interpreting an interaction:</b> when A&times;B is significant, do not read the main-effect means; compare cell means using the appropriate CD and describe how the response to one factor changes across levels of the other.</p>

<h3>Several response variables at once</h3>
<p>Select as many response columns as you like in <i>Response variable(s)</i>. Every one of them is analysed with the same design and mapping, and the tables of means place them side by side, one column per character, exactly as in a results table for publication. Each response keeps its own transformation, its own assumption checks and its own interpretation.</p>

<h3>The automatic scan</h3>
<p>As soon as data are loaded and the design columns are mapped, tab 1 reports Shapiro-Wilk, Levene, Taylor's slope, the optimal Box-Cox lambda and the CV for every numeric column, and names the transformation that column wants. A suggestion marked <i>optional</i> means the diagnostics are satisfactory but convention (counts, percentages) would still transform. Nothing is applied until you press <i>Apply all suggested</i> or choose a transformation yourself.</p>

<h3>Reading the mean tables</h3>
<p>A one-factor table shows <b>mean &plusmn; SE</b> for every treatment and every character, with SE(m), SE(d), C.D. (P&le;0.05) and C.V. (%) beneath it. A two-factor table is a grid of the two factors with marginal means, and the C.D. line quotes the critical difference for factor 1, for factor 2 and for their interaction. Where a response was transformed the back-transformed mean is printed first and the transformed value, on which every statistic was computed, follows in parentheses.</p>

<h3>Reports</h3>
<p>Tab 7 exports the whole analysis as a self-contained HTML file or as a PDF. The PDF is typeset by a headless browser when the <code>pagedown</code> package (or a <code>weasyprint</code> / <code>wkhtmltopdf</code> binary) is available; otherwise a plain typeset PDF is written. Both carry the credit line in the bottom-right corner of every page.</p>
"
