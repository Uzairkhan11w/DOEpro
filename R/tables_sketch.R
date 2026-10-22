###############################################################################
##  MEAN TABLES  (layout follows the standard agronomy presentation)
###############################################################################
## What the tables show follows the comparison chosen (R/comparisons.R): the
## means alone, the means with the C.D. at the foot, or the means with the
## letters of a procedure, never the letters and the C.D. together.

sup <- function(x) ifelse(nzchar(x), sprintf("<sup>%s</sup>", x), "")

TRANS_NOTE <- paste0("<div class='note'>Figures in parentheses are transformed values. ",
  "SE(m), SE(d), C.D. and C.V. refer to the transformed scale; the leading figure ",
  "is the back-transformed mean.</div>")

## The C.V. of the error an effect is compared with: in a split or strip plot
## CV(a) under the main-plot (or horizontal) factor and CV(b) under the
## sub-plot (or vertical) one, named, since the last C.V. alone would give the
## main-plot factor a figure about three times too small
effect_cv <- function(fin, e) {
  cv <- fin$cv
  if (length(cv) == 1) return(fmt(cv, 2))
  hit <- names(fin$errors)[vapply(fin$errors, function(z) isTRUE(all.equal(z$ms, e$mse)), logical(1))]
  if (length(hit) == 1 && grepl("^Error [(].[)]$", hit)) {
    k <- substr(hit, 8, 8)
    return(sprintf("%s (%s)", fmt(cv[[sprintf("CV(%s) (%%)", k)]], 2), k))
  }
  paste(sprintf("%s = %s", sub(" [(]%[)]$", "", names(cv)), fmt(cv, 2)), collapse = "; ")
}

cd_or_ns <- function(e, digits = 2) if (effect_sig(e)) err_text(e, "cd", digits) else "NS"

## does any effect in the analysis have unequal replication?
any_unequal <- function(rr)
  any(vapply(rr$fits, function(f) any(vapply(f$final$effects, function(e)
    !isTRUE(e$equal_rep) && is.null(e$mix), logical(1))), logical(1)))

## A raw HTML table: `head` is a character vector of <th> labels, `body` a list
## of character vectors (one per row), `foot` a list of character vectors. A
## table with many columns is marked to print on a landscape page.
raw_table <- function(head, body, foot = NULL, caption = NULL, cls = "doe") {
  th <- paste0("<th>", head, "</th>", collapse = "")
  mkrow <- function(r) if (length(r) == 1L && grepl("^<tr", r)) r else
    paste0("<tr>", paste0("<td>", r, "</td>", collapse = ""), "</tr>")
  tr <- vapply(body, mkrow, character(1))
  tf <- if (length(foot))
    paste0("<tfoot>", paste(vapply(foot, mkrow, character(1)), collapse = ""), "</tfoot>") else ""
  if (length(head) > 8) cls <- paste(cls, "wide")
  paste0("<table class='", cls, "'>",
         if (!is.null(caption)) paste0("<caption>", caption, "</caption>") else "",
         "<thead><tr>", th, "</tr></thead><tbody>",
         paste(tr, collapse = ""), "</tbody>", tf, "</table>")
}

## ---------------------------------------------------------------------------
## One-factor layout, every response side by side:
##
##      TREATMENT | RESPONSE 1   | RESPONSE 2   | ...
##      T1        | mean[letter] | mean[letter]
##      ...
##      SE(m) +/- |  ...
##      SE(d) +/- |  ...
##      C.D. (5%) |  ...           (only when the comparison is the C.D.)
##      C.V. (%)  |  ...
## ---------------------------------------------------------------------------
sketch_main_html <- function(fits, fac, digits = 2, cmp = comparison(), readings = FALSE) {
  ok <- vapply(fits, function(f) fac %in% names(f$final$effects), logical(1))
  fits <- fits[ok]
  if (!length(fits)) return("")

  ## every level any response has, in the design's order: a level with no
  ## data for the first response must not vanish from the others' columns
  lvs <- lapply(fits, function(f) levels(f$final$data[[fac]]))
  lv <- lvs[[which.max(lengths(lvs))]]
  for (x in lvs) for (i in seq_along(x)) if (!x[i] %in% lv)
    lv <- append(lv, x[i], after = if (i == 1) 0 else match(x[i - 1], lv))
  alpha <- fits[[1]]$final$alpha
  any_tr <- any(vapply(fits, function(f) !identical(f$trans, "none"), logical(1)))
  ef <- lapply(fits, function(f) f$final$effects[[fac]])
  L <- lapply(fits, function(f) cmp_letters(f$final, fac, cmp))
  cells <- lapply(seq_along(fits), function(i) {
    f <- fits[[i]]; m <- ef[[i]]$means
    idx <- match(lv, as.character(m[[fac]]))
    bt <- if (!identical(f$trans, "none") && "Mean_bt" %in% names(m)) m$Mean_bt[idx] else NULL
    ifelse(is.na(idx), "&ndash;", cmp_cell(m$Mean[idx], L[[i]]$letters[idx], digits, bt))
  })
  body <- lapply(seq_along(lv), function(j) c(esc(lv[j]), vapply(cells, function(z) z[j], character(1))))

  foot <- c(
    list(c("SE(m) &plusmn;", vapply(ef, function(e) err_text(e, "sem", digits), character(1))),
         c("SE(d) &plusmn;", vapply(ef, function(e) err_text(e, "sed", digits), character(1)))),
    if (identical(cmp$kind, "cd"))
      list(c(cd_name(alpha), vapply(ef, function(e) cd_or_ns(e, digits), character(1)))),
    list(c("C.V. (%)", vapply(seq_along(fits), function(i) effect_cv(fits[[i]]$final, ef[[i]]), character(1)))))

  hdr <- c(esc(fac), vapply(fits, function(f) esc(f$header), character(1)))
  notes <- unlist(lapply(seq_along(fits), function(i)
    cmp_letters_note(cmp, ef[[i]], L[[i]], if (length(fits) > 1) fits[[i]]$header)))
  rd <- if (readings) rd_html(vapply(seq_along(fits), function(i)
    paste0(if (length(fits) > 1) paste0(fits[[i]]$header, ": ") else "",
           cmp_means_text(fits[[i]], fac, cmp, digits, L[[i]])), character(1))) else ""
  ## the title sits above the table rather than in a caption, which a
  ## narrow table would squeeze onto several lines
  paste0(sprintf("<div class='ms-tcap'>Effect of <b>%s</b>%s</div>", esc(fac), cmp_caption(cmp, alpha, any_tr)),
         raw_table(hdr, body, foot),
         if (length(notes)) paste0("<div class='note'>", paste(esc(notes), collapse = " "), "</div>") else "",
         if (any_tr) TRANS_NOTE else "", rd)
}

## ---------------------------------------------------------------------------
## Two-factor layout, one table per response:
##
##                Factor 2 ->   I1    I2    I3   | Mean
##      Factor 1  T1            ..    ..    ..   |  ..
##                T2
##                Mean          ..    ..    ..   |  ..
##      SE(m), SE(d) (and, with the C.D., the C.D.) of factor 1 / factor 2 / interaction
## ---------------------------------------------------------------------------
sketch_twoway_html <- function(fit, f1, f2, digits = 2, cmp = comparison(), readings = FALSE) {
  r  <- fit$final
  key <- paste0(f1, ":", f2)
  eI <- r$effects[[key]]; e1 <- r$effects[[f1]]; e2 <- r$effects[[f2]]
  if (is.null(eI) || is.null(e1) || is.null(e2)) return("")

  tr_on <- !identical(fit$trans, "none")
  L <- cmp_letters(r, key, cmp)
  m  <- eI$means
  l1 <- as.character(e1$means[[f1]]); l2 <- as.character(e2$means[[f2]])
  at <- cbind(match(as.character(m[[f1]]), l1), match(as.character(m[[f2]]), l2))
  grid <- matrix(NA_real_, length(l1), length(l2)); graw <- grid
  lets <- matrix("", length(l1), length(l2))
  grid[at] <- if (tr_on && "Mean_bt" %in% names(m)) m$Mean_bt else m$Mean
  graw[at] <- m$Mean
  lets[at] <- L$letters

  ## margins and corner are shown like the cells: back-transformed, with the
  ## transformed value in parentheses. The corner is the average of the cell
  ## means, which is what the (adjusted) margins average to.
  btf <- function(z) TRANS[[fit$trans]]$b(z, fit$lambda)
  marg <- function(z) if (tr_on) paste0(fmt(btf(z), digits), " (", fmt(z, digits), ")") else fmt(z, digits)
  body <- lapply(seq_along(l1), function(i)
    c(esc(l1[i]),
      vapply(seq_along(l2), function(j)
        paste0(fmt(grid[i, j], digits), if (tr_on) paste0(" (", fmt(graw[i, j], digits), ")") else "",
               sup(lets[i, j])), character(1)),
      marg(e1$means$Mean[i])))
  body[[length(body) + 1L]] <- c("<b>Mean</b>", vapply(e2$means$Mean, marg, character(1)),
                                 marg(mean(graw, na.rm = TRUE)))

  ncol <- length(l2) + 2L
  span <- function(txt) sprintf("<tr><td colspan='%d' class='cdrow'>%s</td></tr>", ncol, txt)
  trio <- function(what) sprintf("%s / %s / %s", err_text(e1, what, digits),
                                 err_text(e2, what, digits), err_text(eI, what, digits))
  ## in a split, strip or pooled design the interaction's single SE(d) and C.D.
  ## hold only within one level of its slicing factor; the others are listed
  within <- if (is.null(eI$slice)) "" else
    sprintf(" (%s within the same %s)", esc(paste(setdiff(eI$vars, eI$slice), collapse = " x ")), esc(eI$slice))
  xt <- cmp_extra(eI, cmp, digits)
  foot <- list(
    span(sprintf("SE(m) &plusmn; &nbsp; %s", trio("sem"))),
    span(sprintf("SE(d) &plusmn; &nbsp; %s", trio("sed"))),
    if (identical(cmp$kind, "cd"))
      span(sprintf("<b>%s</b> &nbsp;&nbsp; %s: <b>%s</b> &nbsp;&nbsp; %s: <b>%s</b> &nbsp;&nbsp; %s &times; %s%s: <b>%s</b>",
                   cd_name(r$alpha), esc(f1), cd_or_ns(e1, digits), esc(f2), cd_or_ns(e2, digits),
                   esc(f1), esc(f2), within, cd_or_ns(eI, digits))),
    if (length(xt)) span(paste(sprintf("%s: <b>%s</b>", esc(names(xt)), xt), collapse = " &nbsp;&middot;&nbsp; ")),
    span(sprintf("C.V. (%%) &nbsp; %s", paste(fmt(r$cv, 2), collapse = " / "))))
  foot <- Filter(Negate(is.null), foot)

  hdr <- c(paste(esc(f1), "&#92;", esc(f2)), vapply(l2, esc, ""), "Mean")
  tbl <- paste0(sprintf("<div class='ms-tcap'><b>%s</b> &mdash; %s &times; %s%s</div>", esc(fit$header), esc(f1), esc(f2),
                        cmp_caption(cmp, r$alpha)),
                raw_table(hdr, body, foot))
  scope <- if (!identical(cmp$kind, "letters") || !any(nzchar(L$letters))) ""
    else if (is.null(eI$slice)) sprintf(" Letters compare every %s &times; %s cell with every other.", esc(f1), esc(f2))
    else sprintf(" Letters compare %s means within the same level of %s only; the same letter at two levels of %s means nothing.",
                 esc(paste(setdiff(eI$vars, eI$slice), collapse = " x ")), esc(eI$slice), esc(eI$slice))
  lnote <- cmp_letters_note(cmp, eI, L)
  rd <- if (readings) rd_html(cmp_twoway_text(fit, f1, f2, cmp, digits, L)) else ""
  paste0(tbl, "<div class='note'>SE(m) and SE(d)", if (identical(cmp$kind, "cd")) " and the C.D." else "",
         " are given as ", esc(f1), " / ", esc(f2), " / ", esc(f1), " &times; ", esc(f2),
         if (nzchar(within)) paste0(within, ", the conventional SE(m) for those comparisons being SE(d) / &radic;2") else "",
         ".", scope, if (length(lnote)) paste0(" ", esc(lnote)) else "", "</div>",
         if (tr_on) TRANS_NOTE else "", rd)
}

## A compact summary card: design, observations, and grand mean + C.V. per response.
means_summary_card <- function(rr, digits = 2) {
  fits <- rr$fits
  chips <- vapply(names(fits), function(nm) {
    f <- fits[[nm]]; fin <- f$final
    cv <- if (length(fin$cv) == 1) fmt(fin$cv, 2)
          else paste(sprintf("%s = %s", names(fin$cv), fmt(fin$cv, 2)), collapse = " &middot; ")
    sprintf(paste0("<div class='ms-chip'><div class='ms-chip-name'>%s</div>",
                   "<div class='ms-chip-row'><span>Grand mean</span><b>%s</b></div>",
                   "<div class='ms-chip-row'><span>%s</span><b>%s</b></div></div>"),
            esc(f$header), fmt(fin$grand, digits),
            if (length(fin$cv) > 1) "C.V." else "C.V. (%)", cv)
  }, character(1))
  layout <- function(fin) paste0(nrow(fin$data), " observations",
    if (fin$balanced) sprintf(" &middot; %d replication(s), balanced", fin$reps)
    else if (length(fin$n_range) == 2L && fin$n_range[1] != fin$n_range[2])
      sprintf(" &middot; unequal replication (%d to %d per treatment)", fin$n_range[1], fin$n_range[2])
    else " &middot; unbalanced (the treatments do not appear equally often in every block)")
  ## one line for all the responses only when it is true of each of them
  lay <- vapply(fits, function(f) layout(f$final), "")
  shared <- length(unique(lay)) == 1L
  if (!shared)
    chips <- vapply(seq_along(chips), function(i)
      sub("</div></div>$", sprintf("</div><div class='ms-chip-row'><span>%s</span></div></div>", lay[i]), chips[i]), "")
  sprintf(paste0("<div class='ms-summary'>",
                 "<div class='ms-summary-head'>%s%s</div><div class='ms-chip-wrap'>%s</div></div>"),
          names(DESIGNS)[match(rr$design, DESIGNS)],
          if (shared) paste0(" &nbsp;&middot;&nbsp; ", lay[1]) else "",
          paste(chips, collapse = ""))
}

## A key to the notation, shown once at the foot of the section.
means_legend <- function(cmp, any_trans, alpha, unequal = FALSE, mixed = FALSE) {
  cd <- identical(cmp$kind, "cd")
  items <- c(
    if (any_trans) "<b>12.34 (3.51)</b> &mdash; the back-transformed mean, with the transformed value in parentheses"
    else "the figures are the treatment means",
    if (identical(cmp$kind, "letters"))
      sprintf("<b><sup>a b c</sup></b> &mdash; means sharing a letter do not differ significantly by %s at the %s%% level; no letters are given where the F-test is not significant",
              CMP_NAME[[cmp$method]], pct(alpha)),
    paste("<b>SE(m)</b> standard error of a mean &nbsp; <b>SE(d)</b> standard error of the difference between two means",
          "(standard errors, not the standard deviation of the observations)"),
    if (cd)
      sprintf("<b>%s</b> critical difference (Fisher's protected LSD) at the %s%% level: two means differ significantly when their difference is larger; <b>NS</b> when the F-test is not significant at that level",
              cd_name(alpha), pct(alpha)),
    if (unequal) paste0("<b>0.96&ndash;1.13</b> (a range of two numbers) &mdash; the data are unbalanced, ",
                        "so SE(d)", if (cd) " and the C.D." else "",
                        " depend on which two means are compared; the range is shown, and the CSV gives each mean its own SE"),
    if (mixed) paste0("in an interaction of a pooled factorial, SE(d)", if (cd) " and the C.D." else "",
                      " depend on the factors in which two means differ, since the environment is random; the range is ",
                      "shown, and each kind is listed under the detailed table"),
    "<b>C.V.</b> coefficient of variation (%)",
    if (cmp$kind %in% c("none", "dunnett")) cmp_rule(cmp, alpha))
  paste0("<div class='ms-legend'><b>How to read these tables</b><ul><li>",
         paste(items, collapse = "</li><li>"), "</li></ul></div>")
}

## Everything the Means & Comparisons tab (and the report) shows; with
## `readings`, each table is followed by a reading of what it shows.
means_section_html <- function(rr, digits = 2, cmp = comparison(), detailed = FALSE, readings = FALSE) {
  fits <- rr$fits
  f1 <- rr$facs
  any_trans <- any(vapply(fits, function(f) !identical(f$trans, "none"), logical(1)))

  out <- c("<div class='means-wrap'>", means_summary_card(rr, digits))

  ## Section A: main-effect (treatment) means, every response side by side
  mains <- vapply(f1, function(fac) sketch_main_html(fits, fac, digits, cmp, readings), character(1))
  mains <- mains[nzchar(mains)]
  if (length(mains)) {
    lead <- if (length(f1) >= 2)
      "<p class='ms-lead'>Each factor averaged over the levels of the other factor(s); read each column on its own.</p>"
    else ""
    out <- c(out,
      "<div class='ms-section'><div class='ms-h'><span class='ms-badge'>A</span>Treatment means (main effects)</div>",
      lead, mains, "</div>")
  }

  ## Section B: two-way tables, one per response
  if (length(f1) >= 2) {
    twoways <- vapply(names(fits), function(nm)
      sketch_twoway_html(fits[[nm]], f1[1], f1[2], digits, cmp, readings), character(1))
    twoways <- twoways[nzchar(twoways)]
    if (length(twoways)) {
      lead <- sprintf("<p class='ms-lead'>Cell (interaction) means of %s &times; %s, with marginal means in the last row and column. One table per response variable.</p>",
                      esc(f1[1]), esc(f1[2]))
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
      "<div class='ms-detail-block'><div class='ms-detail-name'>", esc(fits[[nm]]$header), "</div>",
      as.character(integrated_means_html(fits[[nm]]$final, digits, readings, cmp)), "</div>"), character(1))
    out <- c(out,
      "<div class='ms-section'><div class='ms-h'><span class='ms-badge'>C</span>Detailed tables (every effect)</div>",
      det, "</div>")
  }

  mixed <- any(vapply(rr$fits, function(f) any(vapply(f$final$effects, function(e) !is.null(e$mix), logical(1))), logical(1)))
  out <- c(out, means_legend(cmp, any_trans, rr$alpha, any_unequal(rr), mixed), "</div>")
  paste(out, collapse = "\n")
}


## Shared styling for the Means & Comparisons section, injected into BOTH the
## live app (APP_CSS) and the downloadable report (REPORT_CSS) so the two
## look identical.
MEANS_CSS <- "
.means-wrap{margin-top:4px}
.ms-summary{background:linear-gradient(180deg,#F3F8FF,#EAF1FB);border:1px solid #CFE0F5;
  border-radius:12px;padding:12px 16px;margin:6px 0 18px 0}
.ms-summary-head{font-weight:600;color:#1B4F9C;font-size:14px;margin-bottom:10px}
.ms-chip-wrap{display:flex;flex-wrap:wrap;gap:10px}
.ms-chip{background:#fff;border:1px solid #D5E2F3;border-radius:10px;padding:8px 12px;min-width:150px}
.ms-chip-name{font-weight:600;color:#173F7D;font-size:12px;margin-bottom:4px;
  border-bottom:1px solid #EAF1FB;padding-bottom:3px}
.ms-chip-row{display:flex;justify-content:space-between;gap:14px;font-size:12px;color:#444;padding:1px 0}
.ms-chip-row b{color:#1B4F9C}
.ms-section{margin:0 0 22px 0}
.ms-h{display:flex;align-items:center;gap:10px;font-size:16px;font-weight:600;color:#1B4F9C;
  border-bottom:2px solid #3B7DD8;padding-bottom:6px;margin:18px 0 8px 0;page-break-after:avoid;break-after:avoid}
.ms-badge{display:inline-flex;align-items:center;justify-content:center;width:24px;height:24px;
  border-radius:50%;background:#3B7DD8;color:#fff;font-size:13px;font-weight:700;flex:0 0 auto}
.ms-lead{font-size:12.5px;color:#555;margin:2px 0 12px 0}
.ms-tcap{font-size:13.5px;color:#14284B;margin:14px 0 2px 0;break-after:avoid;page-break-after:avoid}
.ms-tcap+table.doe{margin-top:4px}
.ms-detail-block{margin:10px 0 6px 0}
.ms-detail-name{font-weight:600;color:#173F7D;background:#EAF1FB;border-left:4px solid #3B7DD8;
  padding:5px 10px;border-radius:3px;margin:14px 0 6px 0}
.ms-env-wrap{display:flex;flex-wrap:wrap;gap:14px;margin:6px 0 10px 0}
.ms-env-wrap table.doe{margin:0}
.ms-legend{background:#FAFCFF;border:1px solid #E1EAF6;border-radius:10px;padding:10px 16px;
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
