###############################################################################
##  UI

###############################################################################
##  MEAN TABLES  (layout follows the standard agronomy presentation)
###############################################################################

sup <- function(x) ifelse(nzchar(x), sprintf("<sup>%s</sup>", x), "")

## "mean +/- SE" cell, optionally carrying the grouping letter as a superscript.
## When the response was transformed the back-transformed mean is shown first and
## the transformed value - the scale on which SE, C.D. and C.V. are computed - in
## parentheses, following the usual journal convention.
ms_cell <- function(mu, se, letter = "", digits = 2, letters_on = TRUE, mu_bt = NULL) {
  lt <- if (letters_on) sup(letter) else ""
  if (is.null(mu_bt)) paste0(fmt(mu, digits), " &plusmn; ", fmt(se, digits), lt)
  else paste0(fmt(mu_bt, digits), " (", fmt(mu, digits), ")", lt)
}

TRANS_NOTE <- paste0("<div class='note'>Figures in parentheses are transformed values. ",
  "SE(m), SE(d), C.D. and C.V. refer to the transformed scale; the leading figure ",
  "is the back-transformed mean.</div>")

cd_or_ns <- function(e, digits = 2) if (!is.na(e$p) && e$p < 0.05) fmt(e$cd5, digits) else "NS"

## A raw HTML table: `head` is a character vector of <th> labels, `body` a list
## of character vectors (one per row), `foot` a list of character vectors.
raw_table <- function(head, body, foot = NULL, caption = NULL, cls = "doe") {
  th <- paste0("<th>", head, "</th>", collapse = "")
  mkrow <- function(r) if (length(r) == 1L && grepl("^<tr", r)) r else
    paste0("<tr>", paste0("<td>", r, "</td>", collapse = ""), "</tr>")
  tr <- vapply(body, mkrow, character(1))
  tf <- if (length(foot))
    paste0("<tfoot>", paste(vapply(foot, mkrow, character(1)), collapse = ""), "</tfoot>") else ""
  paste0("<table class='", cls, "'>",
         if (!is.null(caption)) paste0("<caption>", caption, "</caption>") else "",
         "<thead><tr>", th, "</tr></thead><tbody>",
         paste(tr, collapse = ""), "</tbody>", tf, "</table>")
}

## ---------------------------------------------------------------------------
## One-factor layout, consolidated over every response variable:
##
##      TREATMENT | PARAMETER 1        | PARAMETER 2        | ...
##      T1        | mean +/- SE        | mean +/- SE
##      ...
##      SE(m)+/-  |  ...
##      SE(d)+/-  |  ...
##      C.D. (P<=0.05)  <effect name>: ...
##      CV (%)    |  ...
## ---------------------------------------------------------------------------
sketch_main_html <- function(fits, fac, digits = 2, letters_on = TRUE) {
  ok <- vapply(fits, function(f) fac %in% names(f$final$effects), logical(1))
  fits <- fits[ok]
  if (!length(fits)) return("")
  resps <- names(fits)

  lv <- as.character(fits[[1]]$final$effects[[fac]]$means[[fac]])
  any_tr <- any(vapply(fits, function(f) !identical(f$trans, "none"), logical(1)))
  cells <- lapply(fits, function(f) {
    e <- f$final$effects[[fac]]
    m <- gate_letters(e)
    idx <- match(lv, as.character(m[[fac]]))
    lets <- if ("Letter" %in% names(m)) m$Letter[idx] else rep("", length(lv))
    bt <- if (!identical(f$trans, "none") && "Mean_bt" %in% names(m)) m$Mean_bt[idx] else NULL
    ms_cell(m$Mean[idx], e$sem, lets, digits, letters_on, mu_bt = bt)
  })

  body <- lapply(seq_along(lv), function(i)
    c(lv[i], vapply(cells, function(z) z[i], character(1))))

  ef <- lapply(fits, function(f) f$final$effects[[fac]])
  foot <- list(
    c("SE(m) &plusmn;",   vapply(ef, function(e) fmt(e$sem, digits), character(1))),
    c("SE(d) &plusmn;",   vapply(ef, function(e) fmt(e$sed, digits), character(1))),
    c(sprintf("C.D. (P&le;0.05) &nbsp; <b>%s</b>", fac),
                          vapply(ef, function(e) cd_or_ns(e, digits), character(1))),
    c("C.V. (%)",         vapply(fits, function(f) fmt(f$final$cv[length(f$final$cv)], 2),
                                 character(1))))

  hdr <- c(toupper(fac), vapply(fits, function(f) f$header, character(1)))
  paste0(raw_table(hdr, body, foot,
           caption = sprintf("Effect of <b>%s</b> (mean &plusmn; SE)", fac)),
         if (any_tr) TRANS_NOTE else "")
}

## ---------------------------------------------------------------------------
## Two-factor layout, one table per response:
##
##                Factor 2 ->   I1    I2    I3   | Mean
##      Factor 1  T1            ..    ..    ..   |  ..
##                T2
##                Mean          ..    ..    ..   |  ..
##      C.D. (P<=0.05)  Factor 1: .. ; Factor 2: .. ; Factor 1 x Factor 2: ..
## ---------------------------------------------------------------------------
sketch_twoway_html <- function(fit, f1, f2, digits = 2, letters_on = TRUE) {
  r  <- fit$final
  key <- paste0(f1, ":", f2)
  eI <- r$effects[[key]]; e1 <- r$effects[[f1]]; e2 <- r$effects[[f2]]
  if (is.null(eI) || is.null(e1) || is.null(e2)) return("")

  tr_on <- !identical(fit$trans, "none")
  bt <- function(e) if (tr_on && "Mean_bt" %in% names(e$means)) e$means$Mean_bt else e$means$Mean
  m  <- eI$means
  mu <- bt(eI)
  l1 <- as.character(e1$means[[f1]]); l2 <- as.character(e2$means[[f2]])
  grid <- matrix(NA_real_, length(l1), length(l2), dimnames = list(l1, l2))
  grid[cbind(match(as.character(m[[f1]]), l1), match(as.character(m[[f2]]), l2))] <- mu

  lets <- if (letters_on) {
    mlet <- gate_letters(eI)
    lc <- intersect(c("Letter", "Letter_within_MP", "Letter_within_A",
                      "Letter_within_env"), names(mlet))
    if (length(lc)) {
      g <- matrix("", length(l1), length(l2), dimnames = list(l1, l2))
      g[cbind(match(as.character(m[[f1]]), l1), match(as.character(m[[f2]]), l2))] <- mlet[[lc[1]]]
      g
    } else NULL
  } else NULL

  graw <- matrix(NA_real_, length(l1), length(l2), dimnames = list(l1, l2))
  graw[cbind(match(as.character(m[[f1]]), l1), match(as.character(m[[f2]]), l2))] <- m$Mean
  body <- lapply(seq_along(l1), function(i)
    c(l1[i],
      vapply(seq_along(l2), function(j)
        paste0(fmt(grid[i, j], digits),
               if (tr_on) paste0(" (", fmt(graw[i, j], digits), ")") else "",
               if (!is.null(lets)) sup(lets[i, j]) else ""),
        character(1)),
      fmt(bt(e1)[i], digits)))
  body[[length(body) + 1L]] <- c("<b>Mean</b>", fmt(bt(e2), digits), fmt(r$grand, digits))

  ncol <- length(l2) + 2L
  span <- function(txt) sprintf("<tr><td colspan='%d' class='cdrow'>%s</td></tr>", ncol, txt)
  trio <- function(f) sprintf("%s / %s / %s", fmt(f(e1), digits), fmt(f(e2), digits), fmt(f(eI), digits))
  foot <- list(
    span(sprintf("SE(m) &plusmn; &nbsp; %s", trio(function(e) e$sem))),
    span(sprintf("SE(d) &plusmn; &nbsp; %s", trio(function(e) e$sed))),
    span(sprintf("<b>C.D. (P&le;0.05)</b> &nbsp;&nbsp; %s: <b>%s</b> &nbsp;&nbsp; %s: <b>%s</b> &nbsp;&nbsp; %s &times; %s: <b>%s</b>",
                 f1, cd_or_ns(e1, digits), f2, cd_or_ns(e2, digits),
                 f1, f2, cd_or_ns(eI, digits))),
    span(sprintf("C.V. (%%) &nbsp; %s", paste(fmt(r$cv, 2), collapse = " / "))))

  hdr <- c(sprintf("%s \\ %s", f1, f2), l2, "Mean")
  tbl <- raw_table(hdr, body, foot,
    caption = sprintf("<b>%s</b> &mdash; %s &times; %s", fit$header, f1, f2))
  paste0(tbl, "<div class='note'>SE(m), SE(d) are given as ", f1, " / ", f2, " / ",
         f1, " &times; ", f2, ".</div>", if (tr_on) TRANS_NOTE else "")
}

## A compact summary card: design, observations, and grand mean + C.V. per response.
means_summary_card <- function(rr, digits = 2) {
  fits <- rr$fits
  f1 <- fits[[1]]$final
  chips <- vapply(names(fits), function(nm) {
    f <- fits[[nm]]; fin <- f$final
    cv <- paste(sprintf("%s = %s", names(fin$cv), fmt(fin$cv, 2)), collapse = " &middot; ")
    sprintf(paste0("<div class='ms-chip'><div class='ms-chip-name'>%s</div>",
                   "<div class='ms-chip-row'><span>Grand mean</span><b>%s</b></div>",
                   "<div class='ms-chip-row'><span>%s</span><b>%s</b></div></div>"),
            f$header, fmt(fin$grand, digits),
            if (length(fin$cv) > 1) "C.V." else "C.V. (%)", cv)
  }, character(1))
  sprintf(paste0("<div class='ms-summary'>",
                 "<div class='ms-summary-head'>%s &nbsp;&middot;&nbsp; %d observations",
                 "%s</div><div class='ms-chip-wrap'>%s</div></div>"),
          names(DESIGNS)[match(rr$design, DESIGNS)], nrow(f1$data),
          if (f1$balanced) sprintf(" &middot; %d replication(s), balanced", f1$reps)
          else " &middot; unbalanced",
          paste(chips, collapse = ""))
}

## A key to the notation, shown once at the foot of the section.
means_legend <- function(any_letters, any_trans) {
  items <- c(
    "<b>mean &plusmn; SE</b> &mdash; treatment mean with its standard error",
    if (any_letters) "<b><sup>a b c</sup></b> &mdash; means sharing a letter do not differ at P &le; 0.05" else NULL,
    "<b>SE(m)</b> standard error of a mean &nbsp; <b>SE(d)</b> standard error of a difference",
    "<b>C.D. (P&le;0.05)</b> critical difference; <b>NS</b> when the F-test is not significant",
    "<b>C.V.</b> coefficient of variation (%)",
    if (any_trans) "figures <b>in parentheses</b> are transformed values; the leading figure is the back-transformed mean" else NULL)
  paste0("<div class='ms-legend'><b>How to read these tables</b><ul><li>",
         paste(items, collapse = "</li><li>"), "</li></ul></div>")
}

## everything the Means tab (and the report) shows
means_section_html <- function(rr, digits = 2, letters_on = TRUE, detailed = FALSE) {
  fits <- rr$fits
  f1 <- rr$facs
  any_trans <- any(vapply(fits, function(f) !identical(f$trans, "none"), logical(1)))

  out <- c("<div class='means-wrap'>", means_summary_card(rr, digits))

  ## Section A: main-effect (treatment) means, every response side by side
  mains <- vapply(f1, function(fac)
    sketch_main_html(fits, fac, digits, letters_on), character(1))
  mains <- mains[nzchar(mains)]
  if (length(mains)) {
    lead <- if (length(f1) >= 2)
      "<p class='ms-lead'>Each factor averaged over the levels of the other factor(s). Compare within a column using that column's C.D.</p>"
    else ""
    out <- c(out,
      "<div class='ms-section'><div class='ms-h'><span class='ms-badge'>A</span>Treatment means (main effects)</div>",
      lead, mains, "</div>")
  }

  ## Section B: two-way tables, one per response
  if (length(f1) >= 2) {
    twoways <- vapply(names(fits), function(nm)
      sketch_twoway_html(fits[[nm]], f1[1], f1[2], digits, letters_on), character(1))
    twoways <- twoways[nzchar(twoways)]
    if (length(twoways)) {
      lead <- sprintf("<p class='ms-lead'>Cell (interaction) means of %s &times; %s, with marginal means in the last row and column. One table per response variable.</p>",
                      f1[1], f1[2])
      note <- if (length(f1) > 2)
        "<div class='warn'>Only the first two factors are shown as a two-way table. Every higher-order interaction is given in the detailed tables below (tick &ldquo;Show detailed tables&rdquo;).</div>"
        else ""
      out <- c(out,
        "<div class='ms-section'><div class='ms-h'><span class='ms-badge'>B</span>Two-way (interaction) means</div>",
        lead, note, twoways, "</div>")
    }
  }

  ## Section C: full per-effect detail (optional)
  if (detailed) {
    det <- vapply(names(fits), function(nm) paste0(
      "<div class='ms-detail-block'><div class='ms-detail-name'>", fits[[nm]]$header, "</div>",
      as.character(integrated_means_html(fits[[nm]]$final, digits)), "</div>"), character(1))
    out <- c(out,
      "<div class='ms-section'><div class='ms-h'><span class='ms-badge'>C</span>Detailed tables (every effect)</div>",
      det, "</div>")
  }

  out <- c(out, means_legend(letters_on, any_trans), "</div>")
  paste(out, collapse = "\n")
}

## combined ANOVA: mean squares of every response side by side
combined_anova_html <- function(rr) {
  fits <- rr$fits
  a1 <- fits[[1]]$final$anova
  src <- a1$Source
  ok <- all(vapply(fits, function(f) identical(f$final$anova$Source, src), logical(1)))
  if (!ok) return(NULL)
  body <- lapply(seq_along(src), function(i)
    c(src[i], if (is.na(a1$Df[i])) "-" else as.character(a1$Df[i]),
      vapply(fits, function(f) {
        an <- f$final$anova
        if (is.na(an$MS[i])) "-" else
          paste0(fmt(an$MS[i], 3), " ", "<span class='sig'>", star(an$p[i]), "</span>")
      }, character(1))))
  raw_table(c("Source of variation", "d.f.", vapply(fits, function(f) f$header, character(1))),
            body, caption = "Analysis of variance &mdash; mean squares",
            cls = "doe")
}


## Shared styling for the Means & C.D. section, injected into BOTH the live app
## (APP_CSS) and the downloadable report (REPORT_CSS) so the two look identical.
MEANS_CSS <- "
.means-wrap{margin-top:4px}
.ms-summary{background:linear-gradient(180deg,#F3F8FF,#EAF1FB);border:1px solid #CFE0F5;
  border-radius:8px;padding:12px 16px;margin:6px 0 18px 0}
.ms-summary-head{font-weight:600;color:#1B4F9C;font-size:14px;margin-bottom:10px}
.ms-chip-wrap{display:flex;flex-wrap:wrap;gap:10px}
.ms-chip{background:#fff;border:1px solid #D5E2F3;border-radius:6px;padding:8px 12px;min-width:150px}
.ms-chip-name{font-weight:600;color:#173F7D;font-size:12px;margin-bottom:4px;
  border-bottom:1px solid #EAF1FB;padding-bottom:3px}
.ms-chip-row{display:flex;justify-content:space-between;gap:14px;font-size:12px;color:#444;padding:1px 0}
.ms-chip-row b{color:#1B4F9C}
.ms-section{margin:0 0 22px 0}
.ms-h{display:flex;align-items:center;gap:10px;font-size:16px;font-weight:600;color:#1B4F9C;
  border-bottom:2px solid #3B7DD8;padding-bottom:6px;margin:18px 0 8px 0}
.ms-badge{display:inline-flex;align-items:center;justify-content:center;width:24px;height:24px;
  border-radius:50%;background:#3B7DD8;color:#fff;font-size:13px;font-weight:700;flex:0 0 auto}
.ms-lead{font-size:12.5px;color:#555;margin:2px 0 12px 0}
.ms-detail-block{margin:10px 0 6px 0}
.ms-detail-name{font-weight:600;color:#173F7D;background:#EAF1FB;border-left:4px solid #3B7DD8;
  padding:5px 10px;border-radius:3px;margin:14px 0 6px 0}
.ms-env-wrap{display:flex;flex-wrap:wrap;gap:14px;margin:6px 0 10px 0}
.ms-env-wrap table.doe{margin:0}
.ms-legend{background:#FAFCFF;border:1px solid #E1EAF6;border-radius:8px;padding:10px 16px;
  margin-top:18px;font-size:12px;color:#555}
.ms-legend b{color:#1B4F9C}
.ms-legend ul{margin:6px 0 0 0;padding-left:18px}
.ms-legend li{margin:3px 0}
/* polish on the shared table style */
table.doe{box-shadow:0 1px 2px rgba(27,79,156,.06)}
table.doe tbody tr:nth-child(even) td{background:#F7FAFF}
table.doe tbody tr:hover td{background:#EEF5FF}
table.doe caption{font-size:13.5px}
table.doe tfoot tr:first-child td{border-top:2px solid #9DBBE6}
table.doe tfoot td{color:#1B4F9C}
"
