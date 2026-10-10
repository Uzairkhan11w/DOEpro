###############################################################################
##  DUNNETT'S TEST
###############################################################################
## Dunnett's test compares each treatment with one control and nothing else.
## Its critical value is the quantile of the largest |t| (or the largest t,
## for one side) among those comparisons, which share the control's mean and
## the error mean square: a multivariate t whose correlations follow from
## the standard errors of the differences.
##
## When every correlation is lambda_i * lambda_j, which holds whenever the
## means are independent (equal or unequal replication) and in a blocked
## design with one missing plot, the comparisons can be written
##     t_i = (lambda_i Z0 + sqrt(1 - lambda_i^2) E_i) / S,
## with Z0 and the E_i independent standard normals and S^2 a chi-square over
## its degrees of freedom, so the probability is a two-dimensional integral
## over Z0 and S (Dunnett, 1955). It is done here by trapezoid rules on the
## whole line, over log S and over Z0, which converge geometrically for such
## smooth, fast-decaying integrands: with one comparison the value is
## Student's t to within 2e-10 from 1 to 5000 degrees of freedom, and
## Dunnett's tables are reproduced to their last decimal. Correlations not of
## that form (two or more missing plots in a blocked design) are given the
## nearest form of it, and the output says that this is an approximation; in
## a test with two missing plots it was within 0.002 of a simulation of the
## exact value.

## the integration grid for `df` error degrees of freedom: points (s, z)
## and their weights, without the points whose weights together come to
## less than 1e-14, which cannot move the probability by more than that
DUNNETT_GRIDS <- new.env(parent = emptyenv())
dunnett_grid <- function(df) {
  key <- format(df, digits = 15)
  if (!is.null(DUNNETT_GRIDS[[key]])) return(DUNNETT_GRIDS[[key]])
  ns <- if (df < 3) 400 else 80; nz <- 81; L <- 8.5
  lo <- 0.5 * log(stats::qchisq(1e-15, df) / df)
  hi <- 0.5 * log(stats::qchisq(1e-15, df, lower.tail = FALSE) / df)
  t <- seq(lo, hi, length.out = ns); ht <- t[2] - t[1]
  ## the density of log S, for S^2 = chi-square / df
  ws <- ht * exp(log(2) + (df / 2) * log(df / 2) - lgamma(df / 2) + df * t - df * exp(2 * t) / 2)
  z <- seq(-L, L, length.out = nz); wz <- (z[2] - z[1]) * stats::dnorm(z)
  w <- as.vector(outer(wz, ws))
  o <- order(w); small <- cumsum(w[o]) < 1e-14
  keep <- setdiff(seq_along(w), o[small])
  g <- list(s = rep(exp(t), each = nz)[keep], z = rep(z, times = ns)[keep], w = w[keep])
  DUNNETT_GRIDS[[key]] <- g
  g
}

## P(every comparison within c), for comparisons with loadings `lam`, each
## of the distinct loadings evaluated once and raised to its count
dunnett_prob <- function(c, lam, cnt, two_sided, g) {
  cs <- c * g$s; p_all <- 1
  for (j in seq_along(lam)) {
    sq <- sqrt(1 - lam[j]^2)
    p <- stats::pnorm((cs - lam[j] * g$z) / sq)
    if (two_sided) p <- p - stats::pnorm((-cs - lam[j] * g$z) / sq)
    p_all <- p_all * if (cnt[j] == 1) p else p^cnt[j]
  }
  sum(g$w * p_all)
}

## The correlations between the comparisons of every mean with the control
## `ctrl`, from the standard errors of the differences S: two comparisons
## share the control, and Cov = (S_i0^2 + S_j0^2 - S_ij^2) / 2.
dunnett_cor <- function(S, ctrl) {
  o <- setdiff(seq_len(nrow(S)), ctrl)
  a <- S[o, ctrl]
  R <- (outer(a^2, a^2, "+") - S[o, o, drop = FALSE]^2) / (2 * outer(a, a))
  diag(R) <- 1
  unname(R)
}

## The loadings of the nearest one-factor form lambda_i lambda_j to the
## correlations R, and how far R departs from it
dunnett_lambda <- function(R) {
  k <- nrow(R)
  if (k == 1) return(list(lambda = sqrt(0.5), departure = 0))
  off <- R; diag(off) <- 0
  lam <- sqrt(pmax(rowSums(off) / (k - 1), 1e-6))
  for (it in seq_len(500)) {
    M <- R; diag(M) <- lam^2
    ev <- eigen(M, symmetric = TRUE)
    new <- sqrt(max(ev$values[1], 0)) * abs(ev$vectors[, 1])
    done <- max(abs(new - lam)) < 1e-14
    lam <- new
    if (done) break
  }
  P <- outer(lam, lam); diag(P) <- 1
  list(lambda = pmin(lam, 1 - 1e-9), departure = max(abs(P - R)))
}

## Dunnett's critical value for comparisons with correlations R on df error
## degrees of freedom, at level alpha, two-sided or one-sided. It lies
## between one comparison's t and Bonferroni's t, whatever the correlations.
dunnett_crit <- function(R, df, alpha, two_sided = TRUE) {
  k <- nrow(R); a1 <- if (two_sided) alpha / 2 else alpha
  if (k == 1) return(list(value = stats::qt(1 - a1, df), exact = TRUE, departure = 0, equal = TRUE))
  fit <- dunnett_lambda(R)
  u <- table(round(fit$lambda, 12))
  lam <- as.numeric(names(u)); cnt <- as.integer(u)
  g <- dunnett_grid(df)
  lo <- stats::qt(1 - a1, df) * 0.999; hi <- stats::qt(1 - a1 / k, df) * 1.001
  root <- stats::uniroot(function(c) dunnett_prob(c, lam, cnt, two_sided, g) - (1 - alpha),
                         c(lo, hi), tol = 1e-9)$root
  list(value = root, exact = fit$departure <= 1e-8, departure = fit$departure,
       equal = length(lam) == 1)
}

## the levels a control can be chosen from: those that vary within one
## family of comparisons (for a sliced interaction, the levels of the other
## factor), in the order of the tables
dunnett_levels <- function(e) {
  vary <- setdiff(e$vars, e$slice)
  unique(apply(e$means[vary], 1, function(z) paste(trimws(z), collapse = " : ")))
}

DUNNETT_ALT <- c(two.sided = "two-sided: does each treatment differ from the control?",
                 greater = "one-sided: is each treatment higher than the control?",
                 less = "one-sided: is each treatment lower than the control?")

## Dunnett's test for effect `e`, in the shape of the other procedures'
## results: the means with each one's verdict against the control, every
## comparison with the control, and the parameters. `fams` are the families
## of means compared together (all of them, or one level of the slicing
## factor at a time).
posthoc_dunnett <- function(e, fams, alpha, control, alternative = "two.sided") {
  if (!alternative %in% names(DUNNETT_ALT)) stop("Unknown alternative.")
  m <- e$means
  vary <- setdiff(e$vars, e$slice)
  lab_vary <- apply(m[vary], 1, function(z) paste(trimws(z), collapse = " : "))
  lab_all <- apply(m[e$vars], 1, paste, collapse = " : ")
  ## as in the app, the first level is the control unless another is named:
  ## a level with no number (Control, Check) sorts first
  if (is.null(control)) control <- dunnett_levels(e)[1]
  if (!control %in% lab_vary)
    stop(sprintf("Choose the control from the levels of %s.", paste(vary, collapse = " x ")))
  two <- alternative == "two.sided"
  within <- if (is.null(e$slice)) NULL else
    vapply(fams, function(ix) as.character(m[[e$slice]][ix[1]]), character(1))
  word <- function(d, sig) if (!sig) switch(alternative, two.sided = "Not significantly different",
                                            greater = "Not significantly higher", less = "Not significantly lower")
                           else if (d > 0) "Significantly higher" else "Significantly lower"
  out <- lapply(seq_along(fams), function(f) {
    ix <- fams[[f]]
    c0 <- ix[lab_vary[ix] == control]
    if (length(c0) != 1) stop(sprintf("The control %s is not in every family of comparisons.", control))
    oth <- setdiff(ix, c0)
    S <- e$sed_mat[c(c0, oth), c(c0, oth), drop = FALSE]
    crit <- dunnett_crit(dunnett_cor(S, 1), e$df, alpha, two)
    d <- m$Mean[oth] - m$Mean[c0]
    sed <- S[-1, 1]
    cd <- crit$value * sed
    sig <- switch(alternative, two.sided = abs(d) > cd, greater = d > cd, less = -d > cd)
    pairs <- data.frame(
      Comparison = paste(lab_all[oth], "vs", lab_all[c0]),
      Difference = d, SEd = sed, `Critical value` = rep(crit$value, length(oth)),
      `Critical difference` = cd, Significant = ifelse(sig, "Yes", "No"),
      check.names = FALSE, stringsAsFactors = FALSE)
    verdict <- vapply(seq_along(oth), function(i) word(d[i], sig[i]), "")
    ord <- oth[order(-m$Mean[oth])]
    g <- data.frame(Treatment = lab_all[c(c0, ord)], Mean = m$Mean[c(c0, ord)],
                    row.names = NULL, check.names = FALSE)
    if (!is.null(m$Raw_mean)) g$`Unadjusted mean` <- m$Raw_mean[c(c0, ord)]
    g <- cbind(g, n = m$N[c(c0, ord)], SE = m$SE[c(c0, ord)],
               `Versus the control` = c("Control", verdict[match(ord, oth)]))
    if (!is.null(within)) { g <- cbind(Within = within[f], g); pairs <- cbind(Within = within[f], pairs) }
    list(groups = g, pairs = pairs, crit = crit, k = length(oth),
         family = list(lab = lab_vary[ix], mu = m$Mean[ix], ctrl = match(c0, ix),
                       d = d, sig = sig, others = match(oth, ix),
                       within = if (is.null(within)) NULL else within[f]))
  })
  groups <- do.call(rbind, lapply(out, `[[`, "groups")); rownames(groups) <- NULL
  pairs <- do.call(rbind, lapply(out, `[[`, "pairs")); rownames(pairs) <- NULL
  crits <- lapply(out, `[[`, "crit")
  cvals <- vapply(crits, `[[`, 0, "value")
  exact <- all(vapply(crits, `[[`, TRUE, "exact"))
  departure <- max(vapply(crits, `[[`, 0, "departure"))
  one_cd <- diff(range(pairs$`Critical difference`)) <= 1e-9 * max(abs(pairs$`Critical difference`))
  st <- data.frame(
    Item = c("Effect", "Method", "Control", "Error mean square", "Error df", "SE of a mean (SEm)",
             "SE of a difference (SEd)", "Comparisons with the control", "Significance level",
             "Critical value (multivariate t)", if (one_cd) "Critical difference (C.D.)" else "Critical difference"),
    Value = c(e$label, paste("Dunnett's test,", DUNNETT_ALT[[alternative]]), control, fmt(e$mse, 4), df_text(e$df),
              err_text(e, "sem", 3, html = FALSE), err_text(e, "sed", 3, html = FALSE),
              if (is.null(within)) as.character(out[[1]]$k)
              else sprintf("%d within each level of %s", out[[1]]$k, e$slice),
              p_lab(alpha),
              if (diff(range(cvals)) <= 1e-9 * max(cvals)) fmt(cvals[1]) else sprintf("%s to %s", fmt(min(cvals)), fmt(max(cvals))),
              if (one_cd) fmt(pairs$`Critical difference`[1]) else "varies by comparison - see the comparisons"),
    check.names = FALSE)
  how <- if (!exact)
    sprintf(paste("The missing plots make the correlations between the comparisons slightly uneven: they depart by up to %s",
                  "from the form the exact calculation needs, so the critical value is computed for the nearest correlations",
                  "of that form. This is an approximation; in a test with two missing plots it was within 0.002 of a",
                  "simulation of the exact value."), fmt(departure, 3))
    else if (all(vapply(crits, `[[`, TRUE, "equal")))
      "The comparisons share the control's mean, so they are correlated; the critical value is the quantile of their joint (multivariate t) distribution, computed exactly."
    else "The comparisons share the control's mean, so they are correlated, unequally since the standard errors differ; the critical value is the quantile of their joint (multivariate t) distribution, computed exactly for these correlations."
  list(groups = groups, pairs = pairs, stats = st, how = how, exact = exact, departure = departure,
       crit = cvals, families = lapply(out, `[[`, "family"), alternative = alternative, control = control)
}

## The notes under Dunnett's test, and the result in the shape posthoc()
## returns for every procedure
posthoc_notes <- function(d, e, method, alpha) {
  f_sig <- !is.na(e$p) && e$p < alpha
  note <- c(
    if (!f_sig)
      sprintf(paste0("The F-test for %s is not significant at the %s%% level, so, as everywhere else in DOEpro, ",
                     "no treatment is declared different from the control."), e$label, pct(alpha)),
    if (!is.null(e$slice))
      sprintf(paste0("Means are compared with the control within each level of %s: these are the only ",
                     "comparisons that share one error term."), e$slice),
    if (!is.null(e$error_desc)) e$error_desc,
    d$how,
    switch(d$alternative,
      greater = paste("One-sided: only a treatment higher than the control can be declared; one lower than the",
                      "control, however far, is reported as not significantly higher."),
      less = paste("One-sided: only a treatment lower than the control can be declared; one higher than the",
                   "control, however far, is reported as not significantly lower."),
      NULL),
    if (!isTRUE(all.equal(alpha, e$alpha)))
      sprintf(paste0("These comparisons are at the %s%% level, while the analysis and its ",
                     "tables of means are at the %s%% level."), pct(alpha), pct(e$alpha)))
  list(groups = d$groups, stats = d$stats, ranges = NULL, pairs = d$pairs,
       note = note, f_sig = f_sig, alpha = alpha, method = PH_LABELS[[method]], families = d$families,
       dunnett = list(alternative = d$alternative, control = d$control, crit = d$crit,
                      exact = d$exact, departure = d$departure))
}
