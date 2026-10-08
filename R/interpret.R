###############################################################################
##  PLAIN-ENGLISH INTERPRETATION
###############################################################################
cv_verdict <- function(cv) {
  if (is.na(cv)) return("")
  if (cv < 10) "low - the experiment was precisely conducted"
  else if (cv < 20) "moderate - acceptable for most field experiments"
  else if (cv < 30) "high - precision is poor; treatment differences must be large to be detected"
  else "very high - the experiment has low reliability; check for outliers, plot heterogeneity or a wrong error term"
}

## Every statement is made at the significance level the user chose, and a
## non-significant result is reported as insufficient evidence of a difference,
## never as evidence that there is none.
interpret <- function(res, asm, sug, trans_lab = "None") {
  d <- res$data; p <- character(0)
  dn <- names(DESIGNS)[match(res$design, DESIGNS)]
  a <- res$alpha; lvl <- sprintf("%s%%", pct(a))

  ## why the data are unbalanced, in the terms of the design
  why_unbal <- switch(res$design,
    CRD = "the treatments have unequal numbers of replications",
    FCRD = "the treatment combinations have unequal numbers of replications",
    LSD = "a plot is missing from the Latin square",
    "a plot is missing from a block, or the treatments are unequally replicated")
  adjusted <- isTRUE(res$adjusted_ss) &&
    any(vapply(res$effects, function(e) !is.null(e$means$Raw_mean), logical(1)))

  p <- c(p, sprintf("<h4>1. What was analysed</h4><p>A <b>%s</b> was analysed with <b>%s</b> as the response (%d observations). Every test is at the %s significance level.%s%s%s%s</p>",
    dn, res$resp, nrow(d), lvl,
    if (is.null(res$excluded)) "" else paste0(" ", excluded_text(res$excluded, html = TRUE)),
    if (res$balanced) " The data are balanced."
    else sprintf(" The data are <b>unbalanced</b>: %s. Each mean therefore has its own standard error, and two means are compared using the standard error of that particular pair.", why_unbal),
    if (adjusted) " The means are adjusted (least-squares) means, and each term in the ANOVA is tested after allowing for every other term (Type III sums of squares, which need not add up to the total)."
    else if (isTRUE(res$adjusted_ss)) " Each term in the ANOVA is tested after allowing for every other term (Type III sums of squares, which need not add up to the total)."
    else "",
    if (trans_lab != "None") sprintf(" The response was transformed using <b>%s</b>; all means, SEd and C.D. values below are on the transformed scale (back-transformed means are shown alongside).", trans_lab) else ""))

  p <- c(p, sprintf("<h4>2. Precision of the experiment</h4><p>%s. Grand mean = %s. A CV of this magnitude is %s.</p>",
    paste(sprintf("%s = %s", names(res$cv), fmt(res$cv, 2)), collapse = "; "),
    fmt(res$grand), cv_verdict(res$cv[length(res$cv)])))

  ## effect-by-effect
  ee <- character(0)
  inter_sig <- FALSE
  for (nm in names(res$effects)) {
    e <- res$effects[[nm]]
    if (is.na(e$p)) next
    s <- if (e$p < a / 5) "highly significant" else
         if (e$p < a) sprintf("significant at the %s level", lvl)
         else sprintf("not significant at the %s level", lvl)
    best <- e$means[which.max(e$means$Mean), ]
    top <- paste(vapply(e$vars, function(v) as.character(best[[v]]), character(1)),
                 collapse = " x ")
    if (effect_sig(e)) {
      if (length(e$vars) > 1) inter_sig <- TRUE
      cd_txt <- if (!is.null(e$slice))
        sprintf("Two means at the same level of %s must differ by at least <b>%s</b> (C.D. at %s) to be declared different; other comparisons have their own C.D., given with the table of means.",
                e$slice, fmt(e$cd), lvl)
      else if (!is.na(e$cd))
        sprintf("Two means of this effect must differ by at least <b>%s</b> (C.D. at %s) to be declared different.",
                fmt(e$cd), lvl)
      else
        sprintf("Because the data are unbalanced, the difference two means need to be declared different depends on which two are compared: the C.D. at %s ranges from <b>%s</b>.",
                lvl, err_text(e, "cd", 3, html = FALSE))
      ee <- c(ee, sprintf("<li><b>%s</b> is %s (F = %s, %s). The highest mean, %s, was recorded for <b>%s</b>. %s</li>",
        e$label, s, fmt(e$F, 2), p_eq(e$p), fmt(best$Mean), top, cd_txt))
    } else if (length(e$vars) > 1) {
      ## an interaction F-test asks whether one factor's effect depends on the
      ## other, not whether the cell means are all equal
      fx <- e$vars
      ee <- c(ee, sprintf("<li><b>%s</b> is %s (F = %s, %s). There is insufficient evidence at the %s level that the effect of %s depends on the level of %s. This is not evidence that the interaction is absent; a small one may have gone undetected.</li>",
        e$label, s, fmt(e$F, 2), p_eq(e$p), lvl, fx[1], paste(fx[-1], collapse = " and ")))
    } else {
      ee <- c(ee, sprintf("<li><b>%s</b> is %s (F = %s, %s). There is insufficient evidence to conclude that its means differ at the %s level, so no C.D. is quoted and no letters are given. This is not evidence that the means are equal; the experiment may have been too small to detect a real difference.</li>",
        e$label, s, fmt(e$F, 2), p_eq(e$p), lvl))
    }
  }

  ## In a pooled factorial the interaction of the environment with each single
  ## factor is an ANOVA row but not a table of means, so read it from the ANOVA.
  if (res$design %in% c("POOLFRCBD", "POOLFCRD")) {
    env <- setdiff(res$effects[[length(res$effects)]]$vars, res$facs)
    an <- res$anova
    rows <- an[startsWith(an$Source, paste0(env, " x ")) & !is.na(an$p), , drop = FALSE]
    full <- paste0(env, " x ", paste(res$facs, collapse = " x "))
    rows <- rows[rows$Source != full, , drop = FALSE]
    for (i in seq_len(nrow(rows))) {
      fac <- sub(paste0("^", env, " x "), "", rows$Source[i])
      if (rows$p[i] < a) {
        inter_sig <- TRUE
        ee <- c(ee, sprintf("<li><b>%s</b> is significant at the %s level (F = %s, %s): the effect of %s differs between environments, so its pooled means are averages over environments that behave differently and should be read with that in mind.</li>",
          rows$Source[i], lvl, fmt(rows$F[i], 2), p_eq(rows$p[i]), fac))
      } else {
        ee <- c(ee, sprintf("<li><b>%s</b> is not significant at the %s level (F = %s, %s): there is insufficient evidence that the effect of %s differs between environments.</li>",
          rows$Source[i], lvl, fmt(rows$F[i], 2), p_eq(rows$p[i]), fac))
      }
    }
  }
  p <- c(p, "<h4>3. Effect of each source</h4><ul>", ee, "</ul>")

  if (inter_sig) p <- c(p, "<p class='warn'><b>An interaction is significant.</b> The effect of one factor depends on the level of the other, so the main-effect means are averages over conditions that behave differently. Interpret the <i>interaction (cell) means</i> and the simple effects rather than the main effects, and use the interaction plot to describe the pattern.</p>")
  else if (length(res$facs) > 1) p <- c(p, sprintf("<p>No interaction was significant at the %s level, so there is insufficient evidence that the factors interact. The main-effect means can be interpreted directly and the best level of each factor chosen separately, bearing in mind that a small interaction may have gone undetected.</p>", lvl))

  ## assumptions, judged at the same level
  at <- character(0)
  if (!is.na(asm$p_norm)) at <- c(at, sprintf("<li>Shapiro-Wilk on residuals: W = %s, %s - %s.</li>",
    fmt(asm$shapiro$statistic, 3), p_eq(asm$p_norm),
    if (asm$p_norm > a) sprintf("insufficient evidence at the %s level that the residuals depart from normality", lvl)
    else sprintf("the residuals <b>depart from normality</b> at the %s level", lvl)))
  if (!is.na(asm$p_hov)) at <- c(at, sprintf("<li>Levene's test: %s - %s.</li>",
    p_eq(asm$p_hov),
    if (asm$p_hov > a) sprintf("insufficient evidence at the %s level that the treatments differ in variance", lvl)
    else sprintf("the variances are <b>heterogeneous</b> at the %s level", lvl)))
  if (length(asm$outliers)) at <- c(at, sprintf("<li>%d observation(s) have standardised residuals beyond +/-3 (rows %s) - check them for recording errors.</li>",
    length(asm$outliers), paste(asm$outliers, collapse = ", ")))
  at <- c(at, sprintf("<li>Recommendation: <b>%s</b>. %s</li>", TRANS[[sug$method]]$lab, sug$why))
  p <- c(p, "<h4>4. Assumptions of the ANOVA</h4><ul>", at, "</ul>")

  p <- c(p, sprintf("<h4>5. How to report this</h4><p>Present the ANOVA table, then the table of means with SEm&plusmn;, SEd, C.D. (%s) and CV(%%) at the foot%s. Means followed by a common letter are not significantly different at the %s level. For pairwise inference the least significant difference (LSD) is used only after a significant F-test (Fisher's protected LSD); Tukey's honestly significant difference (HSD) or Duncan's multiple range test (DMRT) may be preferred when many treatments are compared.</p>",
    lvl,
    if (res$balanced) "" else "; with unbalanced data give each mean its own SE and quote the range of the SEd and C.D. values",
    lvl))
  paste(p, collapse = "\n")
}

REPORT_CSS <- "
@page{size:A4;margin:16mm 13mm 20mm 13mm;
  @bottom-right{content:'DOEpro \\00b7 Shah, Khan & Jeelani \\2014 page ' counter(page);
                font-size:8pt;color:#666}}
body{font-family:Segoe UI,Helvetica,Arial,sans-serif;margin:28px;color:#222;line-height:1.5}
h1{border-bottom:3px solid #3B7DD8;padding-bottom:6px;margin-bottom:4px}
.rpt-head{display:flex;align-items:center;justify-content:space-between;gap:16px;
  border-bottom:3px solid #3B7DD8;margin-bottom:4px}
.rpt-head h1{border-bottom:none;margin:0;padding:0;flex:1}
.rpt-logo{height:52px;width:auto;flex:0 0 auto}
h2{color:#1B4F9C;border-bottom:1px solid #C8D8EE;margin-top:26px;page-break-after:avoid}
h3{color:#1B4F9C;margin-bottom:6px;page-break-after:avoid}
h4{margin-bottom:4px;color:#1B4F9C}
p.meta{color:#555;font-size:12px;margin-top:0}
table.doe{border-collapse:collapse;margin:8px 0 4px 0;font-size:13px;page-break-inside:avoid}
table.doe th,table.doe td{border:1px solid #b9c6d6;padding:5px 10px;text-align:right}
table.doe th{background:#EAF1FB;text-align:center}
table.doe td:first-child,table.doe th:first-child{text-align:left}
table.doe caption{caption-side:top;text-align:left;font-weight:600;padding:6px 0;color:#1B4F9C}
table.doe tfoot td{background:#FAFCFF;font-size:12px}
td.cdrow{text-align:left !important;background:#FAFCFF;font-size:12px}
table.foot{font-size:12px;margin-top:0;background:#FAFCFF}
span.sig{color:#C0392B;font-weight:600}
sup{color:#1B4F9C;font-weight:600}
.block{margin-bottom:26px}
.note{font-size:12px;color:#555;font-style:italic;margin:2px 0 14px 0}
.warn{background:#FFF6E5;border-left:4px solid #E8A33D;padding:8px 12px;margin:10px 0}
.authors{font-size:12px;color:#444;margin:10px 0 0 0}
.creditblock{margin-top:30px;padding-top:8px;border-top:1px solid #C8D8EE;
  font-size:11px;color:#666;text-align:right;page-break-inside:avoid}
.screencredit{position:fixed;right:12px;bottom:8px;font-size:10px;color:#8a8a8a;
  background:rgba(255,255,255,.85);padding:2px 6px;border-radius:3px}
@media print{.screencredit{display:none}}
"
