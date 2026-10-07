###############################################################################
##  POST-HOC
###############################################################################
## Post-hoc multiple comparisons, computed from the effect's own error term and
## from each pair's own standard error of a difference, taken from the effect's
## SE(d) matrix. With equal replication every pair has the same SE(d) and these
## are the textbook procedures. With unequal replication Tukey's HSD becomes the
## Tukey-Kramer procedure, the LSD, Bonferroni and Scheffe tests stay exact,
## and the two multiple-range tests use Kramer's (1956) pairwise adjustment;
## the output says which. Interaction effects of split plots, strip plots and
## pooled analyses are compared within one level of their slicing factor, the
## only comparisons that share one error term.
PH_METHODS <- c("LSD (Fisher's protected)", "LSD (Bonferroni-adjusted)",
                "Tukey HSD", "Duncan's DMRT", "Student-Newman-Keuls", "Scheffe")

## what each method is called wherever it is shown to the user
PH_LABELS <- c(
  "LSD (Fisher's protected)"  = "Least significant difference (LSD), Fisher's protected",
  "LSD (Bonferroni-adjusted)" = "Least significant difference (LSD), Bonferroni-adjusted",
  "Tukey HSD"                 = "Tukey's honestly significant difference (HSD)",
  "Duncan's DMRT"             = "Duncan's multiple range test (DMRT)",
  "Student-Newman-Keuls"      = "Student-Newman-Keuls (SNK) test",
  "Scheffe"                   = "Scheffe's test")

posthoc <- function(res, effect, method, alpha = res$alpha) {
  e <- res$effects[[effect]]
  if (is.null(e)) stop("Unknown effect.")
  if (!method %in% PH_METHODS) stop("Unknown method")
  m <- e$means
  lab <- apply(m[e$vars], 1, paste, collapse = " : ")
  fams <- if (is.null(e$slice)) list(seq_len(nrow(m)))
          else unname(split(seq_len(nrow(m)), factor(m[[e$slice]], levels = unique(m[[e$slice]]))))
  fams <- fams[lengths(fams) >= 2]
  if (!length(fams)) stop("This effect has fewer than two means.")
  df <- e$df
  ranged <- method %in% c("Student-Newman-Keuls", "Duncan's DMRT")
  if (method %in% c("Tukey HSD", "Student-Newman-Keuls", "Duncan's DMRT") && df < 2)
    stop(sprintf(paste0("%s needs at least 2 error degrees of freedom; this effect has %s. ",
                        "Use the LSD, Bonferroni or Scheffe test, or add replication."),
                 PH_LABELS[[method]], df_text(df)))
  kmax <- max(lengths(fams))
  ## the studentized range for p means spanned (p = 2..kmax), divided by
  ## sqrt(2) so that it multiplies a standard error of a difference; computed
  ## once per span, not once per pair
  qv <- if (ranged) {
    q <- vapply(2:kmax, function(p) {
      pr <- if (method == "Student-Newman-Keuls") 1 - alpha else (1 - alpha)^(p - 1)
      q_tukey(pr, p, df)
    }, numeric(1))
    ## Duncan's ranges must not shrink as more means are spanned; with few error
    ## df the raw quantiles at his protection levels do, so they are held level
    if (method == "Duncan's DMRT") q <- cummax(q)
    c(Inf, q / sqrt(2))
  } else NULL

  run <- function(ix) {
    mu <- m$Mean[ix]; k <- length(mu)
    S <- e$sed_mat[ix, ix, drop = FALSE]            # rows of sed_mat follow e$means
    nC <- k * (k - 1) / 2
    rk <- rank(-mu, ties.method = "first")
    span <- abs(outer(rk, rk, "-")) + 1
    mult <- switch(method,
      "LSD (Fisher's protected)"  = matrix(t_crit(e, alpha), k, k),
      "LSD (Bonferroni-adjusted)" = matrix(stats::qt(1 - alpha / (2 * nC), df), k, k),
      "Tukey HSD"                 = matrix(q_tukey(1 - alpha, k, df) / sqrt(2), k, k),
      "Scheffe"                   = matrix(sqrt((k - 1) * stats::qf(1 - alpha, k - 1, df)), k, k),
      matrix(qv[span], k, k))
    crit <- mult * S
    raw <- abs(outer(mu, mu, "-")) > crit
    diag(raw) <- FALSE
    sig <- if (ranged) step_down(mu, raw) else raw
    lets <- cld_from_sig(mu, sig)

    o <- order(-mu)
    ij <- which(upper.tri(diag(k)), arr.ind = TRUE)
    ij <- ij[order(ij[, 1], ij[, 2]), , drop = FALSE]
    ij <- cbind(o[ij[, 1]], o[ij[, 2]])                 # larger mean first
    pairs <- data.frame(
      Comparison = paste(lab[ix][ij[, 1]], "vs", lab[ix][ij[, 2]]),
      Difference = mu[ij[, 1]] - mu[ij[, 2]],
      SEd = S[ij],
      `Critical value` = mult[ij],
      `Critical difference` = crit[ij],
      Significant = ifelse(sig[ij], "Yes",
                    ifelse(raw[ij], "No (inside a non-significant range)", "No")),
      check.names = FALSE, stringsAsFactors = FALSE)
    list(lets = lets, pairs = pairs, k = k, crit = crit[upper.tri(crit)])
  }
  out <- lapply(fams, run)

  within <- if (is.null(e$slice)) NULL else
    vapply(fams, function(ix) as.character(m[[e$slice]][ix[1]]), character(1))
  groups <- do.call(rbind, lapply(seq_along(fams), function(f) {
    ix <- fams[[f]]
    g <- data.frame(Treatment = lab[ix], Mean = m$Mean[ix], row.names = NULL, check.names = FALSE)
    if (!is.null(m$Raw_mean)) g$`Unadjusted mean` <- m$Raw_mean[ix]
    g <- cbind(g, n = m$N[ix], SE = m$SE[ix], Group = out[[f]]$lets)
    if (!is.null(within)) g <- cbind(Within = within[f], g)
    g[order(-g$Mean), ]
  }))
  rownames(groups) <- NULL
  pairs <- do.call(rbind, lapply(seq_along(fams), function(f) {
    p <- out[[f]]$pairs
    if (!is.null(within)) p <- cbind(Within = within[f], p)
    p
  }))

  ## one critical difference for the whole family only if one really applies
  crit_all <- unlist(lapply(out, `[[`, "crit"))
  equal <- isTRUE(e$equal_rep)
  one_cd <- !ranged && diff(range(crit_all)) <= 1e-9 * max(abs(crit_all))
  ranges <- if (ranged && equal)
    data.frame(`Means apart (p)` = 2:kmax, `Critical range` = qv[2:kmax] * e$sed,
               check.names = FALSE)
  else NULL

  kramer <- !equal && (method == "Tukey HSD" || ranged)
  shown <- if (method == "Tukey HSD" && !equal)
             "Tukey-Kramer procedure (Tukey's HSD for unequal replication)"
           else if (kramer) paste(PH_LABELS[[method]], "with Kramer's adjustment for unequal replication")
           else PH_LABELS[[method]]
  k_txt <- if (is.null(within)) as.character(nrow(m))
           else sprintf("%s within each level of %s", paste(unique(vapply(out, `[[`, numeric(1), "k")),
                                                         collapse = " or "), e$slice)
  st <- data.frame(
    Item  = c("Effect", "Method", "Error mean square", "Error df",
              "SE of a mean (SEm)", "SE of a difference (SEd)",
              "Number of means (k)", "Significance level",
              if (one_cd) "Critical difference (C.D.)" else "Critical difference"),
    Value = c(e$label, shown, fmt(e$mse, 4), df_text(df),
              err_text(e, "sem", 3, html = FALSE), err_text(e, "sed", 3, html = FALSE),
              k_txt, p_lab(alpha),
              if (one_cd) fmt(crit_all[1])
              else if (ranged && equal) "varies with p - see the table of critical ranges"
              else "varies by pair - see the pairwise comparisons"),
    check.names = FALSE)

  f_sig <- !is.na(e$p) && e$p < alpha
  note <- c(
    if (!f_sig)
      sprintf(paste0("The F-test for %s is not significant at the %s%% level, so, as ",
                     "everywhere else in DOEpro, no letters are shown and no pair is ",
                     "declared different."), e$label, pct(alpha)),
    if (!is.null(e$slice))
      sprintf(paste0("Means are compared within each level of %s: these are the only ",
                     "comparisons that share one error term. Letters restart in each level, ",
                     "so the same letter in two levels of %s means nothing."), e$slice, e$slice),
    if (!is.null(e$error_desc)) e$error_desc,
    if (method == "Tukey HSD" && !equal)
      paste0("Replication is unequal, so this is the Tukey-Kramer procedure: Tukey's ",
             "studentized range applied to each pair's own standard error of a difference."),
    if (ranged && !equal)
      paste0("Replication is unequal, so each pair uses its own standard error of a ",
             "difference (Kramer, 1956). agricolae and SAS use the harmonic mean of the ",
             "replications instead, so their critical ranges will differ slightly."),
    if (ranged)
      paste0("As a multiple-range test this is a step-down procedure: two means inside a ",
             "range that is not significant are not declared different, even if their own ",
             "difference exceeds its critical range."),
    if (method %in% c("Tukey HSD", "Student-Newman-Keuls", "Duncan's DMRT"))
      paste0("The critical value in the pairwise table is the studentized range q divided ",
             "by &radic;2, so that critical difference = critical value &times; SEd. Tables of ",
             "q list it before that division."),
    ## notes written for the tables of means describe that table's letters and
    ## level, so they are repeated here only when they apply to this output
    if (is.null(e$slice) && isTRUE(all.equal(alpha, e$alpha))) e$notes,
    if (!isTRUE(all.equal(alpha, e$alpha)))
      sprintf(paste0("These comparisons are at the %s%% level, while the analysis and its ",
                     "tables of means are at the %s%% level."), pct(alpha), pct(e$alpha)))
  note <- note[nzchar(note)]

  list(groups = groups, stats = st, ranges = ranges, pairs = pairs,
       note = if (length(note)) note else NULL, f_sig = f_sig, alpha = alpha,
       method = PH_LABELS[[method]])
}

## What the user sees and exports: the procedure's letters and verdicts only
## when the effect's F-test is significant, as for every other table in
## DOEpro. posthoc() itself keeps them, as an effect keeps its letters.
gate_posthoc <- function(x) {
  if (isTRUE(x$f_sig)) return(x)
  x$groups$Group <- ""
  x$pairs$Significant <- sprintf("Not declared (F-test not significant at %s%%)", pct(x$alpha))
  x
}

## Multiple-range tests are step-down procedures: once a range of means is
## declared homogeneous, no pair inside it may be declared different. Working
## from the widest range inwards, a pair stays significant only if both ranges
## one step wider than it were significant, and therefore every range around it.
step_down <- function(mu, sig) {
  k <- length(mu); o <- order(mu, decreasing = TRUE)
  s <- sig[o, o, drop = FALSE]
  out <- matrix(FALSE, k, k)
  for (w in rev(seq_len(k - 1))) for (i in seq_len(k - w)) {
    j <- i + w
    out[i, j] <- s[i, j] && (i == 1 || out[i - 1, j]) && (j == k || out[i, j + 1])
  }
  out <- out | t(out)
  res <- matrix(FALSE, k, k)
  res[o, o] <- out
  res
}

## qtukey() fails to converge for the long ranges Duncan's test needs once about
## twenty means are compared (it returns NaN); inverting ptukey() does not
q_tukey <- function(pr, p, df) {
  q <- suppressWarnings(stats::qtukey(pr, p, df))
  if (is.finite(q)) return(q)
  f <- function(x) stats::ptukey(x, p, df) - pr
  if (!is.finite(f(1e-3)) || !is.finite(f(100)))
    stop(sprintf(paste0("The studentized range cannot be computed for %d means on %s ",
                        "error degrees of freedom. Use the LSD, Bonferroni or Scheffe test."),
                 p, df_text(df)), call. = FALSE)
  stats::uniroot(f, c(1e-3, 100), tol = 1e-12)$root
}

## degrees of freedom: whole numbers as they are, Satterthwaite's to one decimal
df_text <- function(df) if (abs(df - round(df)) < 1e-8) as.character(round(df)) else fmt(df, 1)
