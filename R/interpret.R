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

interpret <- function(res, asm, sug, trans_lab = "None") {
  d <- res$data; p <- character(0)
  dn <- names(DESIGNS)[match(res$design, DESIGNS)]

  p <- c(p, sprintf("<h4>1. What was analysed</h4><p>A <b>%s</b> was analysed with <b>%s</b> as the response (%d observations, %s data).%s</p>",
    dn, res$resp, nrow(d),
    if (res$balanced) "balanced" else "<b>unbalanced</b>",
    if (trans_lab != "None") sprintf(" The response was transformed using <b>%s</b>; all means, SEd and CD values below are on the transformed scale (back-transformed means are shown alongside).", trans_lab) else ""))

  p <- c(p, sprintf("<h4>2. Precision of the experiment</h4><p>%s. Grand mean = %s. A CV of this magnitude is %s.</p>",
    paste(sprintf("%s = %s", names(res$cv), fmt(res$cv, 2)), collapse = "; "),
    fmt(res$grand), cv_verdict(res$cv[length(res$cv)])))

  ## effect-by-effect
  ee <- character(0)
  inter_sig <- FALSE
  for (nm in names(res$effects)) {
    e <- res$effects[[nm]]
    if (is.na(e$p)) next
    s <- if (e$p < 0.01) "highly significant (p &lt; 0.01)" else
         if (e$p < 0.05) "significant (p &lt; 0.05)" else "not significant"
    best <- e$means[which.max(e$means$Mean), ]
    lvl <- paste(vapply(e$vars, function(v) as.character(best[[v]]), character(1)),
                 collapse = " x ")
    if (e$p < 0.05) {
      if (length(e$vars) > 1) inter_sig <- TRUE
      ee <- c(ee, sprintf("<li><b>%s</b> is %s (F = %s). The highest mean, %s, was recorded for <b>%s</b>. Two means of this effect must differ by at least <b>%s</b> (CD at 5%%) to be declared different.</li>",
        e$label, s, fmt(e$F, 2), fmt(best$Mean), lvl, fmt(e$cd5)))
    } else {
      ee <- c(ee, sprintf("<li><b>%s</b> is %s (F = %s, p = %s). The observed spread among its means can be explained by experimental error alone, so no CD is quoted and the means should be treated as statistically alike.</li>",
        e$label, s, fmt(e$F, 2), pval(e$p)))
    }
  }
  p <- c(p, "<h4>3. Effect of each source</h4><ul>", ee, "</ul>")

  if (inter_sig) p <- c(p, "<p class='warn'><b>An interaction is significant.</b> The effect of one factor depends on the level of the other, so the main-effect means are averages over conditions that behave differently. Interpret the <i>interaction (cell) means</i> and the simple effects rather than the main effects, and use the interaction plot to describe the pattern.</p>")
  else if (length(res$facs) > 1) p <- c(p, "<p>No interaction was significant, so the factors act independently: the main-effect means can be interpreted directly and the best level of each factor can be chosen separately.</p>")

  ## assumptions
  a <- character(0)
  if (!is.na(asm$p_norm)) a <- c(a, sprintf("<li>Shapiro-Wilk on residuals: W = %s, p = %s - residuals %s normal.</li>",
    fmt(asm$shapiro$statistic, 3), pval(asm$p_norm),
    if (asm$p_norm > 0.05) "can be regarded as" else "<b>depart from</b>"))
  if (!is.na(asm$p_hov)) a <- c(a, sprintf("<li>Levene's test: p = %s - variances are %s across treatments.</li>",
    pval(asm$p_hov),
    if (asm$p_hov > 0.05) "homogeneous" else "<b>heterogeneous</b>"))
  if (length(asm$outliers)) a <- c(a, sprintf("<li>%d observation(s) have standardised residuals beyond +/-3 (rows %s) - check them for recording errors.</li>",
    length(asm$outliers), paste(asm$outliers, collapse = ", ")))
  a <- c(a, sprintf("<li>Recommendation: <b>%s</b>. %s</li>", TRANS[[sug$method]]$lab, sug$why))
  p <- c(p, "<h4>4. Assumptions of the ANOVA</h4><ul>", a, "</ul>")

  p <- c(p, sprintf("<h4>5. How to report this</h4><p>Present the ANOVA table, then the table of means with SEm&plusmn;, SEd, CD (5%%) and CV(%%) at the foot. Means followed by a common letter do not differ significantly at the %s%% level. For pairwise inference Fisher's protected LSD is used only after a significant F-test; Tukey's HSD or Duncan's DMRT may be preferred when many treatments are compared.</p>",
    fmt(res$alpha * 100, 0)))
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
