###############################################################################
##  INTEGRATED MEAN TABLES  (every effect, in detail)
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

## SE(m), SE(d) and, when the comparison is the C.D., the C.D. under a table
## of means. With unequal replication they differ from mean to mean, and the
## range is given instead of one figure.
effect_footer <- function(e, cv = NULL, cmp = comparison()) {
  ## a split, strip or pooled interaction's single figures hold only for means
  ## at the same level of its slicing factor; the others follow in e$extra
  within <- if (is.null(e$slice)) "" else sprintf(" (within the same %s)", esc(e$slice))
  rows <- c(
    sprintf("<tr><td>SEm &plusmn;%s</td><td>%s</td></tr>", within, err_text(e, "sem", 3)),
    sprintf("<tr><td>SEd%s</td><td>%s</td></tr>", within, err_text(e, "sed", 3)),
    if (identical(cmp$kind, "cd"))
      sprintf("<tr><td>%s%s</td><td>%s</td></tr>", cd_name(e$alpha), within, cd_or_ns(e, 3)))
  if (!is.null(cv)) rows <- c(rows, sprintf("<tr><td>CV (%%)</td><td>%s</td></tr>",
                                            paste(fmt(cv, 2), collapse = " / ")))
  paste0("<table class='doe foot'>", paste(rows, collapse = ""), "</table>")
}

## compact matrix of f1 x f2 cell means (from an effect's table of means)
## with the letters, if any, as superscripts, plus marginal means. The
## margins and the corner are averages of the cell means, so they agree with
## the adjusted margins when replication is unequal; with a transformation
## they are shown like the cells, back-transformed with the transformed value
## in parentheses (`btf` is the back-transformation).
two_way_lettered <- function(m, f1, f2, lets, tr_on, digits, caption, btf = NULL) {
  l1 <- levels(factor(m[[f1]])); l2 <- levels(factor(m[[f2]]))
  disp <- if (tr_on && "Mean_bt" %in% names(m)) m$Mean_bt else m$Mean
  ix <- cbind(match(as.character(m[[f1]]), l1), match(as.character(m[[f2]]), l2))
  cD <- matrix(NA_real_, length(l1), length(l2))
  cR <- cD; lg <- matrix("", length(l1), length(l2))
  cD[ix] <- disp; cR[ix] <- m$Mean
  if (length(lets)) lg[ix] <- lets
  rmean <- rowMeans(cR, na.rm = TRUE); cmean <- colMeans(cR, na.rm = TRUE)
  marg <- function(x) if (tr_on && is.function(btf))
    paste0(fmt(btf(x), digits), " (", fmt(x, digits), ")") else fmt(x, digits)
  body <- lapply(seq_along(l1), function(i)
    c(esc(l1[i]),
      vapply(seq_along(l2), function(j)
        paste0(fmt(cD[i, j], digits),
               if (tr_on) paste0(" (", fmt(cR[i, j], digits), ")") else "",
               sup(lg[i, j])), character(1)),
      marg(rmean[i])))
  body[[length(body) + 1L]] <- c("<b>Mean</b>", marg(cmean), marg(mean(cR, na.rm = TRUE)))
  raw_table(c(paste(esc(f1), "&#92;", esc(f2)), vapply(l2, esc, ""), "Mean"), body, caption = caption)
}

## the note under a table whose letters restart in each level of a factor
slice_note <- function(e) {
  if (is.null(e$slice)) return("")
  sprintf(paste0("<p class='note'>Letters compare %s means within the same level of %s ",
                 "only; the same letter in two levels of %s means nothing.</p>"),
          esc(paste(setdiff(e$vars, e$slice), collapse = " x ")), esc(e$slice), esc(e$slice))
}

## The environment x treatment table of a pooled analysis: with a significant
## interaction, each environment's treatments, compared within that
## environment against the pooled error, by the comparison chosen. A
## two-factor treatment structure is shown as a matrix, otherwise as a
## ranked table.
gxe_env_tables <- function(e, lets, digits, cmp) {
  E <- e$env; tf <- setdiff(e$vars, E)
  m <- e$means; m$.let <- lets
  out <- vapply(levels(factor(m[[E]])), function(lv) {
    mi <- m[as.character(m[[E]]) == lv, , drop = FALSE]
    cap <- sprintf("<b>%s</b> &mdash; %s means%s", esc(lv), esc(paste(tf, collapse = " x ")),
                   if (identical(cmp$kind, "letters")) sprintf(" (letters compare cells within %s)", esc(lv)) else "")
    if (length(tf) == 2) {
      two_way_lettered(mi, tf[1], tf[2], mi$.let, FALSE, digits, cap)
    } else {
      mm <- mi[order(-mi$Mean), c(tf, "Mean"), drop = FALSE]
      if (identical(cmp$kind, "letters")) mm$Letters <- mi$.let[order(-mi$Mean)]
      df_html(mm, caption = cap)
    }
  }, character(1))
  paste0("<div class='ms-env-wrap'>", paste(out, collapse = ""), "</div>")
}

## What sits under an environment x treatment table: the verdict and, when
## the interaction is significant, how the cells are compared, by the
## comparison chosen
gxe_cmp_note <- function(res, e, cmp) {
  a <- e$alpha; E <- e$env; tf <- paste(setdiff(e$vars, E), collapse = " x ")
  if (!effect_sig(e))
    return(sprintf(paste("The %s x %s interaction is not significant at the %s%% level: there is insufficient evidence",
                         "that the treatment differences change from one environment to another, so the individual",
                         "environment cells are not compared."), esc(E), esc(tf), pct(a)))
  how <- switch(cmp$kind,
    cd = sprintf("Two cells in the same environment differ significantly when their difference is larger than the %s within an environment, %s (pooled error).",
                 cd_name(a), fmt(e$cd)),
    letters = sprintf("Letters, by %s, compare cells within one environment only (pooled error).", CMP_NAME[[cmp$method]]),
    "Choose a comparison method to compare the cells within each environment.")
  sprintf("The %s x %s interaction is significant at the %s%% level, so the cells are shown separately for each environment. %s",
          esc(E), esc(tf), pct(a), how)
}

## Every effect of one response in detail, as the comparison chosen shows
## it. With `readings`, each table is followed by a reading of what it shows.
integrated_means_html <- function(res, digits = 2, readings = FALSE, cmp = comparison()) {
  h <- c(sprintf("<p><b>Response:</b> %s &nbsp; | &nbsp; <b>Grand mean:</b> %s &nbsp; | &nbsp; <b>%s</b></p>",
                 esc(res$resp), fmt(res$grand),
                 paste(sprintf("%s = %s", names(res$cv), fmt(res$cv, 2)), collapse = " | ")))

  tr <- res$trans %||% "none"
  tr_on <- !identical(tr, "none")
  btf <- if (tr_on) function(z) TRANS[[tr]]$b(z, res$lambda) else NULL
  fit <- list(final = res, trans = tr, lambda = res$lambda, header = res$resp)
  ## the unadjusted average, which with a transformation is on that scale too
  raw_lab <- if (tr_on) "Unadjusted mean (transformed scale)" else "Unadjusted mean"
  relabel <- function(x) sub("^Raw_mean$", raw_lab, sub("^Mean_bt$", "Back-transformed", sub("^N$", "n", x)))

  for (nm in names(res$effects)) {
    e <- res$effects[[nm]]
    ttl <- sprintf("Table of means: <b>%s</b> &nbsp;(F = %s, %s, %s)",
                   esc(e$label), fmt(e$F, 2), p_eq(e$p, e$alpha), star(e$p, e$alpha))
    unequal <- !isTRUE(e$equal_rep)
    L <- cmp_letters(res, nm, cmp)
    lets <- if (identical(cmp$kind, "letters")) L$letters else rep("", nrow(e$means))
    m <- e$means
    foot <- ""; extra <- ""; note <- character(0)

    if (isTRUE(e$is_gxe)) {
      inner <- if (effect_sig(e)) gxe_env_tables(e, lets, digits, cmp) else ""
      body <- paste0("<div class='ms-detail-name'>", ttl, "</div>",
                     "<p class='note'>", gxe_cmp_note(res, e, cmp), "</p>", inner)

    } else if (length(e$vars) == 1) {
      ## with unequal replication each mean carries its own SE, and an adjusted
      ## mean is shown beside the raw average it was adjusted from
      cols <- intersect(c(e$vars, "Mean", "Raw_mean", "Mean_bt", "N", if (unequal) "SE"), names(m))
      mm <- m[, cols, drop = FALSE]
      if (any(nzchar(lets))) mm$Letters <- lets
      names(mm) <- relabel(names(mm))
      body <- df_html(mm, caption = ttl)
      foot <- effect_footer(e, res$cv, cmp)

    } else if (length(e$vars) == 2) {
      body <- paste0(two_way_lettered(m, e$vars[1], e$vars[2], lets, tr_on, digits, ttl, btf),
                     if (any(nzchar(lets))) slice_note(effect_by(e, L$x$by)) else "")
      foot <- effect_footer(e, res$cv, cmp)

    } else {
      ## a table of three or more factors: one row per combination
      cols <- c(e$vars, "Mean", intersect(c("Raw_mean", "Mean_bt"), names(m)), if (unequal) c("N", "SE"))
      ord <- do.call(order, unname(as.list(m[e$vars])))
      mm <- m[ord, cols, drop = FALSE]
      if (any(nzchar(lets))) mm$Letters <- lets[ord]
      names(mm) <- relabel(names(mm))
      body <- df_html(mm, caption = ttl)
      foot <- effect_footer(e, res$cv, cmp)
    }

    ## the other comparisons of a split, strip or pooled interaction: each
    ## SE(d), and each C.D. when the comparison is the C.D., shown only where
    ## its own F-test is significant
    xt <- cmp_extra(e, cmp)
    if (length(xt))
      extra <- paste0("<table class='doe foot'>",
        paste0(sprintf("<tr><td>%s</td><td>%s</td></tr>", esc(names(xt)), xt), collapse = ""),
        "</table>")
    if (!isTRUE(e$is_gxe)) {
      ## the adjusted-means note from the analysis; notes about letters and
      ## C.D.s are written here, to match what the table shows
      if (!is.null(m$Raw_mean) && is.null(e$slice) && is.null(e$mix)) note <- c(note, e$notes[1])
      if (!is.null(e$mix))
        note <- c(note, paste0("The environment is random, so the difference between two of these means carries the ",
                               "environment's interaction with each factor in which they differ: each kind of comparison ",
                               "has its own SE(d)", if (identical(cmp$kind, "cd")) " and C.D. (from Gomez and Gomez's weighted t)" else "",
                               ", listed above."))
      note <- c(note, cmp_letters_note(cmp, e, L))
    }
    note_html <- if (length(note)) paste0("<p class='note'>", paste(note, collapse = " "), "</p>") else ""
    rd <- if (readings) rd_html(cmp_detail_text(fit, nm, cmp, digits, L)) else ""
    h <- c(h, "<div class='block'>", body, foot, extra, note_html, rd, "</div>")
  }
  HTML(paste(h, collapse = "\n"))
}

## The reading under a detailed table, which prints one-factor and
## many-factor tables to three decimals, two-way tables to `digits`, and
## every C.D. to three. An environment x treatment table carries its own note.
cmp_detail_text <- function(fit, effect, cmp, digits = 2, L = NULL) {
  r <- fit$final; e <- r$effects[[effect]]
  if (isTRUE(e$is_gxe)) return("")
  if (exact_fit(r)) return(RD_EXACT)
  if (length(e$vars) == 1) cmp_means_text(fit, effect, cmp, 3, L)
  else if (length(e$vars) == 2) cmp_twoway_text(fit, e$vars[1], e$vars[2], cmp, digits, L, cd_digits = 3)
  else multi_text(e, 3, fit$trans)
}
