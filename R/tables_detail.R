###############################################################################
##  INTEGRATED MEAN TABLES  (integrated agronomy-style output)
###############################################################################
## two-way table with marginal means
two_way <- function(d, resp, f1, f2) {
  tb <- tapply(d[[resp]], list(d[[f1]], d[[f2]]), mean)
  tb <- cbind(tb, `Mean` = rowMeans(tb, na.rm = TRUE))
  tb <- rbind(tb, `Mean` = colMeans(tb, na.rm = TRUE))
  out <- data.frame(rownames(tb), tb, check.names = FALSE)
  names(out)[1] <- f1
  out
}

## SE(m), SE(d) and C.D. under a table of means. With unequal replication they
## differ from mean to mean, and the range is given instead of one figure.
effect_footer <- function(e, cv = NULL) {
  ## a split, strip or pooled interaction's single figures hold only for means
  ## at the same level of its slicing factor; the others follow in e$extra
  within <- if (is.null(e$slice)) "" else sprintf(" (within the same %s)", e$slice)
  rows <- c(
    sprintf("<tr><td>SEm +/-%s</td><td>%s</td></tr>", within, err_text(e, "sem", 3)),
    sprintf("<tr><td>SEd%s</td><td>%s</td></tr>", within, err_text(e, "sed", 3)),
    sprintf("<tr><td>%s%s</td><td>%s</td></tr>", cd_head(e$alpha), within,
            if (effect_sig(e)) err_text(e, "cd", 3) else "NS"))
  if (!is.null(cv)) rows <- c(rows, sprintf("<tr><td>CV (%%)</td><td>%s</td></tr>",
                                            paste(fmt(cv, 2), collapse = " / ")))
  paste0("<table class='doe foot'>", paste(rows, collapse = ""), "</table>")
}

## compact contingency matrix of f1 x f2 cell means (from an effect's table of
## means) with the grouping letters attached as superscripts, plus marginal
## means. Used by the detailed Section C and by the per-environment breakdown.
## The margins and the corner are averages of the cell means, so they agree
## with the adjusted margins when replication is unequal; with a transformation
## they are shown like the cells, back-transformed with the transformed value in
## parentheses (`btf` is the back-transformation).
two_way_lettered <- function(mlet, f1, f2, letter_col, tr_on, digits, caption, btf = NULL) {
  l1 <- levels(factor(mlet[[f1]])); l2 <- levels(factor(mlet[[f2]]))
  disp <- if (tr_on && "Mean_bt" %in% names(mlet)) mlet$Mean_bt else mlet$Mean
  raw  <- mlet$Mean
  ix <- cbind(match(as.character(mlet[[f1]]), l1), match(as.character(mlet[[f2]]), l2))
  cD <- matrix(NA_real_, length(l1), length(l2), dimnames = list(l1, l2))
  cR <- cD; lg <- matrix("", length(l1), length(l2), dimnames = list(l1, l2))
  cD[ix] <- disp; cR[ix] <- raw
  if (!is.null(letter_col) && letter_col %in% names(mlet)) lg[ix] <- mlet[[letter_col]]
  rmean <- rowMeans(cR, na.rm = TRUE); cmean <- colMeans(cR, na.rm = TRUE)
  marg <- function(x) if (tr_on && is.function(btf))
    paste0(fmt(btf(x), digits), " (", fmt(x, digits), ")") else fmt(x, digits)
  body <- lapply(seq_along(l1), function(i)
    c(l1[i],
      vapply(seq_along(l2), function(j)
        paste0(fmt(cD[i, j], digits),
               if (tr_on) paste0(" (", fmt(cR[i, j], digits), ")") else "",
               sup(lg[i, j])), character(1)),
      marg(rmean[i])))
  body[[length(body) + 1L]] <- c("<b>Mean</b>", marg(cmean), marg(mean(cR, na.rm = TRUE)))
  raw_table(c(sprintf("%s \\ %s", f1, f2), l2, "Mean"), body, caption = caption)
}

## the note under a table whose letters restart in each level of a factor
slice_note <- function(e) {
  if (is.null(e$slice)) return("")
  sprintf(paste0("<p class='note'>Letters compare %s means within the same level of %s ",
                 "only; the same letter in two levels of %s means nothing.</p>"),
          paste(setdiff(e$vars, e$slice), collapse = " &times; "), e$slice, e$slice)
}

## per-environment breakdown of a *significant* environment x treatment
## interaction: for each environment, the treatment structure with letters that
## compare cells within that environment (pooled error). A 2-factor treatment
## structure is shown as a matrix; otherwise as a ranked table.
gxe_env_tables <- function(e, digits, grand) {
  E <- e$env; tf <- setdiff(e$vars, E)
  m <- e$means
  out <- vapply(levels(factor(m[[E]])), function(lv) {
    mi <- m[as.character(m[[E]]) == lv, , drop = FALSE]
    cap <- sprintf("<b>%s</b> &mdash; %s means (letters compare cells within %s)",
                   lv, paste(tf, collapse = " &times; "), lv)
    if (length(tf) == 2) {
      two_way_lettered(mi, tf[1], tf[2], "Letter_within_env", FALSE, digits, cap)
    } else {
      mm <- mi[order(-mi$Mean), c(tf, "Mean", "Letter_within_env"), drop = FALSE]
      names(mm)[names(mm) == "Letter_within_env"] <- "Group"
      df_html(mm, caption = cap)
    }
  }, character(1))
  paste0("<div class='ms-env-wrap'>", paste(out, collapse = ""), "</div>")
}

## With `readings`, each table is followed by a reading of what it shows.
integrated_means_html <- function(res, digits = 2, readings = FALSE) {
  d <- res$data; resp <- res$resp
  h <- c(sprintf("<p><b>Response:</b> %s &nbsp; | &nbsp; <b>Grand mean:</b> %s &nbsp; | &nbsp; <b>%s</b></p>",
                 resp, fmt(res$grand),
                 paste(sprintf("%s = %s", names(res$cv), fmt(res$cv, 2)), collapse = " | ")))

  tr_on <- !is.null(res$trans) && !identical(res$trans, "none")
  btf <- if (tr_on) function(z) TRANS[[res$trans]]$b(z, res$lambda) else NULL
  ## the unadjusted average, which with a transformation is on that scale too
  raw_lab <- if (tr_on) "Unadjusted mean (transformed scale)" else "Unadjusted mean"
  relabel <- function(x) sub("^Raw_mean$", raw_lab, sub("^Mean_bt$", "Back-transformed",
                             sub("^N$", "n", sub("^Letter$", "Group", x))))

  for (nm in names(res$effects)) {
    e <- res$effects[[nm]]
    sig <- effect_sig(e)
    ttl <- sprintf("Table of means: <b>%s</b> &nbsp;(F = %s, %s, %s)",
                   e$label, fmt(e$F, 2), p_eq(e$p, e$alpha), star(e$p, e$alpha))
    unequal <- !isTRUE(e$equal_rep)
    m <- gate_letters(e)          # letters blanked when the F-test is NS
    foot <- ""; extra <- ""; note <- ""

    if (isTRUE(e$is_gxe)) {
      ## environment x treatment stability table (Rec 3 + footnote gating)
      inner <- if (sig) gxe_env_tables(e, digits, res$grand) else ""
      body <- paste0("<div class='ms-detail-name'>", ttl, "</div>",
                     "<p class='note'>", e$notes, "</p>", inner)

    } else if (length(e$vars) == 1) {
      ## with unequal replication each mean carries its own SE, and an adjusted
      ## mean is shown beside the raw average it was adjusted from
      cols <- intersect(c(e$vars, "Mean", "Raw_mean", "Mean_bt", "N",
                          if (unequal) "SE", "Letter"), names(m))
      if (!has_groups(m)) cols <- setdiff(cols, "Letter")     # drop empty Group when NS
      mm <- m[, cols, drop = FALSE]
      names(mm) <- relabel(names(mm))
      body <- df_html(mm, caption = ttl)
      foot <- effect_footer(e, res$cv)

    } else if (length(e$vars) == 2) {
      ## Rec 4: a single contingency matrix with superscript letters
      lc <- intersect(c("Letter", "Letter_within_MP", "Letter_within_A",
                        "Letter_within_env"), names(m))
      body <- paste0(two_way_lettered(m, e$vars[1], e$vars[2],
                                      if (length(lc)) lc[1] else NULL, tr_on, digits, ttl, btf),
                     if (sig) slice_note(e) else "")
      foot <- effect_footer(e, res$cv)

    } else {
      ## Rec 2: 3+ way pure-treatment table, Group column only when significant
      lc <- intersect(c("Letter", "Letter_within_env"), names(m))
      cols <- c(e$vars, "Mean", intersect(c("Raw_mean", "Mean_bt"), names(m)),
                if (unequal) c("N", "SE"))
      ord <- do.call(order, m[e$vars])
      mm <- m[ord, cols, drop = FALSE]
      if (length(lc) && has_groups(m)) mm$Group <- m[ord, lc[1]]
      names(mm) <- relabel(names(mm))
      body <- df_html(mm, caption = ttl)
      foot <- effect_footer(e, res$cv)
    }

    ## the other comparisons of a split, strip or pooled interaction, each C.D.
    ## shown only where its own F-test is significant
    xt <- extra_text(e)
    if (length(xt))
      extra <- paste0("<table class='doe foot'>",
        paste0(sprintf("<tr><td>%s</td><td>%s</td></tr>", names(xt), xt), collapse = ""),
        "</table>")
    if (!isTRUE(e$is_gxe) && length(e$notes) && nzchar(e$notes[1]))
      note <- paste0("<p class='note'>", e$notes, "</p>")
    rd <- if (readings) rd_html(detail_text(res, e, digits, res$trans %||% "none")) else ""
    h <- c(h, "<div class='block'>", body, foot, extra, note, rd, "</div>")
  }
  HTML(paste(h, collapse = "\n"))
}
