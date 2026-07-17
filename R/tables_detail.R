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

effect_footer <- function(e, cv = NULL) {
  rows <- c(
    sprintf("<tr><td>SEm +/-</td><td>%s</td></tr>", fmt(e$sem)),
    sprintf("<tr><td>SEd</td><td>%s</td></tr>",     fmt(e$sed)),
    sprintf("<tr><td>CD (5%%)</td><td>%s</td></tr>",
            if (!is.na(e$p) && e$p < 0.05) fmt(e$cd5) else "NS"),
    sprintf("<tr><td>CD (1%%)</td><td>%s</td></tr>",
            if (!is.na(e$p) && e$p < 0.01) fmt(e$cd1) else "NS"))
  if (!is.null(cv)) rows <- c(rows, sprintf("<tr><td>CV (%%)</td><td>%s</td></tr>",
                                            paste(fmt(cv, 2), collapse = " / ")))
  paste0("<table class='doe foot'>", paste(rows, collapse = ""), "</table>")
}

## compact contingency matrix of f1 x f2 cell means (from an effect's table of
## means) with the grouping letters attached as superscripts, plus marginal
## means. Used by the detailed Section C and by the per-environment breakdown.
two_way_lettered <- function(mlet, f1, f2, letter_col, tr_on, digits, caption, grand) {
  l1 <- levels(factor(mlet[[f1]])); l2 <- levels(factor(mlet[[f2]]))
  disp <- if (tr_on && "Mean_bt" %in% names(mlet)) mlet$Mean_bt else mlet$Mean
  raw  <- mlet$Mean
  ix <- cbind(match(as.character(mlet[[f1]]), l1), match(as.character(mlet[[f2]]), l2))
  cD <- matrix(NA_real_, length(l1), length(l2), dimnames = list(l1, l2))
  cR <- cD; lg <- matrix("", length(l1), length(l2), dimnames = list(l1, l2))
  cD[ix] <- disp; cR[ix] <- raw
  if (!is.null(letter_col) && letter_col %in% names(mlet)) lg[ix] <- mlet[[letter_col]]
  rmean <- rowMeans(cR, na.rm = TRUE); cmean <- colMeans(cR, na.rm = TRUE)
  body <- lapply(seq_along(l1), function(i)
    c(l1[i],
      vapply(seq_along(l2), function(j)
        paste0(fmt(cD[i, j], digits),
               if (tr_on) paste0(" (", fmt(cR[i, j], digits), ")") else "",
               sup(lg[i, j])), character(1)),
      fmt(rmean[i], digits)))
  body[[length(body) + 1L]] <- c("<b>Mean</b>", fmt(cmean, digits), fmt(grand, digits))
  raw_table(c(sprintf("%s \\ %s", f1, f2), l2, "Mean"), body, caption = caption)
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
      two_way_lettered(mi, tf[1], tf[2], "Letter_within_env", FALSE, digits, cap, mean(mi$Mean))
    } else {
      mm <- mi[order(-mi$Mean), c(tf, "Mean", "Letter_within_env"), drop = FALSE]
      names(mm)[names(mm) == "Letter_within_env"] <- "Group"
      df_html(mm, caption = cap)
    }
  }, character(1))
  paste0("<div class='ms-env-wrap'>", paste(out, collapse = ""), "</div>")
}

integrated_means_html <- function(res, digits = 2) {
  d <- res$data; resp <- res$resp
  h <- c(sprintf("<p><b>Response:</b> %s &nbsp; | &nbsp; <b>Grand mean:</b> %s &nbsp; | &nbsp; <b>%s</b></p>",
                 resp, fmt(res$grand),
                 paste(sprintf("%s = %s", names(res$cv), fmt(res$cv, 2)), collapse = " | ")))

  for (nm in names(res$effects)) {
    e <- res$effects[[nm]]
    sig <- effect_sig(e)
    tr_on <- !is.null(res$trans) && !identical(res$trans, "none")
    ttl <- sprintf("Table of means: <b>%s</b> &nbsp;(F = %s, p = %s, %s)",
                   e$label, fmt(e$F, 2), if (is.na(e$p)) "-" else pval(e$p), star(e$p))
    m <- gate_letters(e)          # letters blanked when the F-test is NS
    foot <- ""; extra <- ""; note <- ""

    if (isTRUE(e$is_gxe)) {
      ## environment x treatment stability table (Rec 3 + footnote gating)
      inner <- if (sig) gxe_env_tables(e, digits, res$grand) else ""
      body <- paste0("<div class='ms-detail-name'>", ttl, "</div>",
                     "<p class='note'>", e$notes, "</p>", inner)
      if (!is.null(e$extra))
        extra <- paste0("<table class='doe foot'>",
          paste0(sprintf("<tr><td>%s</td><td>%s</td></tr>", names(e$extra),
                         fmt(unlist(e$extra))), collapse = ""), "</table>")

    } else if (length(e$vars) == 1) {
      cols <- intersect(c(e$vars, "Mean", "Mean_bt", "N", "Letter"), names(m))
      if (!has_groups(m)) cols <- setdiff(cols, "Letter")     # drop empty Group when NS
      mm <- m[, cols, drop = FALSE]
      names(mm) <- sub("^Mean_bt$", "Back-transformed", sub("^N$", "n",
                   sub("^Letter$", "Group", names(mm))))
      body <- df_html(mm, caption = ttl)
      foot <- effect_footer(e, res$cv)

    } else if (length(e$vars) == 2) {
      ## Rec 4: a single contingency matrix with superscript letters
      lc <- intersect(c("Letter", "Letter_within_MP", "Letter_within_A",
                        "Letter_within_env"), names(m))
      body <- two_way_lettered(m, e$vars[1], e$vars[2],
                               if (length(lc)) lc[1] else NULL, tr_on, digits, ttl, res$grand)
      foot <- effect_footer(e, res$cv)

    } else {
      ## Rec 2: 3+ way pure-treatment table, Group column only when significant
      lc <- intersect(c("Letter", "Letter_within_env"), names(m))
      cols <- c(e$vars, "Mean", intersect("Mean_bt", names(m)))
      ord <- do.call(order, m[e$vars])
      mm <- m[ord, cols, drop = FALSE]
      if (length(lc) && has_groups(m)) mm$Group <- m[ord, lc[1]]
      names(mm) <- sub("^Mean_bt$", "Back-transformed", names(mm))
      body <- df_html(mm, caption = ttl)
      foot <- effect_footer(e, res$cv)
    }

    if (!isTRUE(e$is_gxe) && length(e$notes) && nzchar(e$notes[1]))
      note <- paste0("<p class='note'>", e$notes, "</p>")
    h <- c(h, "<div class='block'>", body, foot, extra, note, "</div>")
  }
  HTML(paste(h, collapse = "\n"))
}
