###############################################################################
##  FACTOR ANALYSIS
###############################################################################
## Maximum-likelihood factor analysis (stats::factanal) of the chosen numeric
## columns, from the rows that have every value: whether the data suit it
## (Kaiser-Meyer-Olkin's measure and Bartlett's test of sphericity), how many
## factors (parallel analysis, as on the principal components tab), the
## rotated loadings, communalities and uniquenesses, the variance each factor
## carries, the factor correlations after an oblique rotation, and the
## scores. As on the other Explore tabs, every sentence is one the tables or
## plots bear out: loadings are judged on their values as printed, and the
## readings never say what a factor is, only which variables load on it.

## a loading of this size or more, as printed, counts as salient
FA_SALIENT <- 0.40
## a communality below this, as printed, is low
FA_LOW_COMMUNALITY <- 0.40
## factanal() stops a uniqueness at this lower limit (a Heywood case)
FA_LOWER <- 0.005

#' Factor analysis of numeric variables
#'
#' Maximum-likelihood factor analysis (\code{\link[stats]{factanal}}) of the
#' chosen columns, from the rows with a value of every one, with the
#' Kaiser-Meyer-Olkin measure of sampling adequacy (overall and for each
#' variable) and Bartlett's test of sphericity. The number of factors, unless
#' given, comes from Horn's parallel analysis of the eigenvalues of the
#' correlation matrix (the same as on the principal components tab), within
#' the most the data allow. Text entries are read as the data check reads
#' them; see \code{\link{check_data}}.
#'
#' Each factor's sign is arbitrary; here it is chosen so that its largest
#' loading is positive, and the factors are ordered by the variance they
#' carry. With \code{rotation = "promax"} the loadings are pattern loadings
#' and the factors may correlate; their correlations are returned.
#'
#' @param d A data frame.
#' @param vars The names of three or more numeric columns.
#' @param factors The number of factors, or \code{NULL} for the number
#'   parallel analysis suggests.
#' @param rotation \code{"varimax"} (the default, uncorrelated factors),
#'   \code{"promax"} (factors allowed to correlate) or \code{"none"}.
#' @param alpha The significance level for Bartlett's test and the test that
#'   the number of factors is enough.
#'
#' @return A list of class \code{doepro_fa}: \code{kmo} (overall) and
#'   \code{msa} (each variable), \code{bartlett} (statistic, df, p),
#'   \code{eigen} (eigenvalues of the correlation matrix with their
#'   parallel-analysis thresholds), \code{suggested} and \code{factors},
#'   \code{loadings} (variables by factors), \code{communality},
#'   \code{uniqueness}, \code{variance} (each factor's sum of squared
#'   loadings, proportion and cumulative proportion), \code{phi} (the factor
#'   correlations), \code{fit} (the test that the number of factors is
#'   enough), \code{scores} (regression scores, rows by factors), and the
#'   settings.
#'
#' @examples
#' set.seed(1)
#' f1 <- rnorm(60); f2 <- rnorm(60)
#' d <- data.frame(Height = f1 + rnorm(60, 0, 0.5), Tillers = f1 + rnorm(60, 0, 0.5),
#'                 Yield = f1 + rnorm(60, 0, 0.6), Protein = f2 + rnorm(60, 0, 0.5),
#'                 Oil = f2 + rnorm(60, 0, 0.5), Moisture = f2 + rnorm(60, 0, 0.6))
#' fa <- fa_data(d, names(d))
#' fa$kmo
#' round(fa$loadings, 2)
#'
#' @export
fa_data <- function(d, vars, factors = NULL, rotation = c("varimax", "promax", "none"), alpha = 0.05) {
  rotation <- match.arg(rotation)
  if (!is.numeric(alpha) || length(alpha) != 1L || is.na(alpha) || alpha <= 0 || alpha >= 0.5)
    stop("The significance level must be a single number between 0 and 0.5, such as 0.05.")
  vars <- unique(vars[!is.na(vars) & nzchar(vars)])
  if (length(vars) < 3) stop("Factor analysis needs at least three variables.")
  gone <- setdiff(vars, names(d))
  if (length(gone))
    stop(sprintf("%s %s of the data.", join_and(sprintf("'%s'", gone)), pl(length(gone), "is not a column", "are not columns")))
  p <- length(vars)
  vals <- lapply(stats::setNames(vars, vars), function(v) response_values(d[[v]]))
  held <- lapply(stats::setNames(vars, vars), function(v) held_flags(d[[v]]))
  ok <- Reduce(`&`, lapply(vals, is.finite))
  would <- Reduce(`&`, Map(function(v, h) is.finite(v) | h, vals, held))
  held_rows <- which(would & !ok)
  held_eg <- if (length(held_rows)) {
    i <- held_rows[1]; v <- vars[vapply(held, function(h) h[i], logical(1))][1]
    trimws(as.character(d[[v]][i]))
  } else ""
  n <- sum(ok)
  X <- do.call(cbind, lapply(vals, function(v) v[ok])); colnames(X) <- vars
  flat <- vars[apply(X, 2, function(z) length(unique(z)) < 2)]
  if (length(flat))
    stop(sprintf("%s %s the same value in every row used, so %s nothing to the factors; leave %s out.",
                 join_and(sprintf("'%s'", flat)), pl(length(flat), "has", "each have"),
                 pl(length(flat), "it adds", "they add"), pl(length(flat), "it", "them")))
  ## the most factors maximum likelihood can fit: the degrees of freedom
  ## ((p - m)^2 - (p + m)) / 2 must not be negative
  mmax <- max(which(vapply(seq_len(p), function(m) ((p - m)^2 - (p + m)) / 2 >= 0, logical(1))), 0L)
  if (n <= p + 1)
    stop(sprintf("Only %d %s a value of every variable chosen; factor analysis of %d variables needs at least %d.%s",
                 n, pl(n, "row has", "rows have"), p, p + 2,
                 if (length(held_rows)) sprintf(" %d more %s an entry the data check has flagged (such as '%s'), which will count once corrected on the Data tab.",
                                                length(held_rows), pl(length(held_rows), "row has", "rows have"), held_eg) else ""))
  R <- stats::cor(X)
  if (min(eigen(R, symmetric = TRUE, only.values = TRUE)$values) < 1e-8)
    stop("Some variable can be worked out exactly (or almost exactly) from the others, so the correlations cannot be factored; leave one of them out.")

  ## Kaiser-Meyer-Olkin: the correlations against the partial correlations
  Ri <- solve(R)
  P <- -Ri / sqrt(outer(diag(Ri), diag(Ri)))
  r2 <- R^2; p2 <- P^2; diag(r2) <- 0; diag(p2) <- 0
  kmo <- sum(r2) / (sum(r2) + sum(p2))
  msa <- colSums(r2) / (colSums(r2) + colSums(p2))
  ## Bartlett's test of sphericity: are the correlations, together, zero?
  chi <- -(n - 1 - (2 * p + 5) / 6) * log(det(R))
  bdf <- p * (p - 1) / 2
  bart <- list(statistic = chi, df = bdf, p = stats::pchisq(chi, bdf, lower.tail = FALSE))

  ev <- eigen(R, symmetric = TRUE, only.values = TRUE)$values
  pp <- pca_parallel(n, p)
  above <- ev > pp$threshold
  pa_keep <- if (all(above)) p else which(!above)[1] - 1L
  suggested <- max(1L, min(pa_keep, mmax))
  if (is.null(factors) || length(factors) != 1 || is.na(factors)) factors <- suggested
  factors <- as.integer(factors)
  if (factors < 1 || factors > mmax)
    stop(sprintf("With %d variables, maximum likelihood can fit from 1 to %d %s.", p, mmax, pl(mmax, "factor", "factors")))

  fit <- tryCatch(stats::factanal(X, factors = factors, rotation = rotation, scores = "regression"),
                  error = function(e) e)
  if (inherits(fit, "error"))
    stop(sprintf(paste("The factor analysis with %d %s did not settle on a solution (%s); try fewer factors, or",
                       "leave out a variable that is almost a copy of another."),
                 factors, pl(factors, "factor", "factors"), conditionMessage(fit)))
  L <- unclass(fit$loadings)[, seq_len(factors), drop = FALSE]
  S <- fit$scores[, seq_len(factors), drop = FALSE]
  Tinv <- if (!is.null(fit$rotmat)) solve(fit$rotmat) else diag(factors)
  phi <- Tinv %*% t(Tinv)
  ## factors ordered by the variance they carry, each with its largest
  ## loading positive
  ss <- colSums(L^2)
  o <- order(-ss)
  L <- L[, o, drop = FALSE]; S <- S[, o, drop = FALSE]; phi <- phi[o, o, drop = FALSE]
  sgn <- vapply(seq_len(factors), function(k) if (L[which.max(abs(round(L[, k], 10))), k] < 0) -1 else 1, 0)
  L <- sweep(L, 2, sgn, "*"); S <- sweep(S, 2, sgn, "*"); phi <- phi * outer(sgn, sgn)
  fn <- paste0("F", seq_len(factors))
  colnames(L) <- colnames(S) <- rownames(phi) <- colnames(phi) <- fn
  rownames(L) <- vars
  ss <- colSums(L^2)
  test <- if (!is.null(fit$STATISTIC) && isTRUE(fit$dof > 0))
    list(statistic = unname(fit$STATISTIC), df = fit$dof, p = unname(fit$PVAL)) else list(statistic = NA_real_, df = fit$dof, p = NA_real_)
  structure(list(vars = vars, n = n, p = p, alpha = alpha, rotation = rotation, factors = factors, suggested = suggested,
                 pa_keep = pa_keep, mmax = mmax, kmo = kmo, msa = msa, bartlett = bart,
                 eigen = data.frame(Eigenvalue = ev, PA_threshold = pp$threshold), pa_sets = pp$sets,
                 loadings = L, uniqueness = fit$uniquenesses, communality = 1 - fit$uniquenesses,
                 variance = data.frame(Factor = fn, SS = ss, Proportion = ss / p, Cumulative = cumsum(ss) / p),
                 phi = phi, fit = test, scores = S, rows = row_ids(d)[ok], pos = which(ok), nrow = nrow(d),
                 held_rows = length(held_rows), held_eg = held_eg,
                 heywood = vars[fit$uniquenesses <= FA_LOWER + 1e-6]),
            class = "doepro_fa")
}

#' @rdname fa_data
#' @param x A result of \code{fa_data()}.
#' @param ... Not used.
#' @export
print.doepro_fa <- function(x, ...) {
  cat(sprintf("KMO %.3f; Bartlett chi-square %.2f on %d df, %s\n\n", x$kmo, x$bartlett$statistic, as.integer(x$bartlett$df),
              gsub("&lt;", "<", p_eq(x$bartlett$p, x$alpha), fixed = TRUE)))
  print(round(cbind(x$loadings, Communality = x$communality, Uniqueness = x$uniqueness), 3))
  invisible(x)
}

## ------------------------------------------------------------ bands ----

## Kaiser's (1974) words for a measure of sampling adequacy, judged on the
## value as printed
fa_kmo_word <- function(v) {
  v <- as.numeric(fmt(v, 2))
  if (v >= 0.90) "marvellous" else if (v >= 0.80) "meritorious" else if (v >= 0.70) "middling"
  else if (v >= 0.60) "mediocre" else if (v >= 0.50) "miserable" else "unacceptable"
}

## ------------------------------------------------------------ tables ----

fa_adequacy_html <- function(fa) {
  body <- c(list(c("All variables", fmt(fa$kmo, 2), fa_kmo_word(fa$kmo))),
            lapply(seq_len(fa$p), function(j) c(esc(fa$vars[j]), fmt(fa$msa[j], 2), fa_kmo_word(fa$msa[j]))))
  notes <- c(
    paste("Kaiser-Meyer-Olkin measure: how much of the correlation between the variables is shared rather than",
          "between pairs alone; Kaiser's words: 0.90 or more marvellous, 0.80s meritorious, 0.70s middling, 0.60s",
          "mediocre, 0.50s miserable, below 0.50 unacceptable."),
    sprintf("Bartlett's test of sphericity: chi-square = %s on %d degrees of freedom, p %s.", fmt(fa$bartlett$statistic, 2),
            as.integer(fa$bartlett$df), if (fa$bartlett$p < 1e-4) "< 0.0001" else paste("=", p_show(fa$bartlett$p, fa$alpha))))
  foot <- sprintf("<tr><td colspan='3' class='cdrow'>%s</td></tr>", esc(paste(notes, collapse = " ")))
  raw_table(c("", "KMO", "In Kaiser's words"), body, foot = foot, caption = "Are the data suited to factor analysis?")
}

fa_loadings_html <- function(fa) {
  L <- fa$loadings
  body <- lapply(seq_len(fa$p), function(i) c(esc(fa$vars[i]), vapply(seq_len(fa$factors), function(k) {
    s <- dfmt(L[i, k], 2)
    if (abs(as.numeric(s)) >= FA_SALIENT) paste0("<b>", s, "</b>") else s
  }, ""), fmt(fa$communality[i], 2), fmt(fa$uniqueness[i], 2)))
  ob <- fa$rotation == "promax"
  notes <- c(sprintf("%s. In bold: %s or more either way, taken here as salient.",
                     if (ob) "Promax pattern loadings: the direct part of each factor in each variable"
                     else "Loadings: the correlations between the variables and the factors", dfmt(FA_SALIENT, 2)),
             "Communality: the share of each variable's variance the factors account for; uniqueness: the rest (1 minus the communality).",
             sprintf("%s rotation. A factor's sign is arbitrary: reversing all its loadings changes nothing.",
                     c(varimax = "Varimax", promax = "Promax", none = "No")[[fa$rotation]]))
  foot <- sprintf("<tr><td colspan='%d' class='cdrow'>%s</td></tr>", fa$factors + 3, esc(paste(notes, collapse = " ")))
  raw_table(c("Variable", colnames(L), "Communality", "Uniqueness"), body, foot = foot, caption = "Loadings")
}

fa_variance_html <- function(fa) {
  v <- fa$variance; ob <- fa$rotation == "promax"
  body <- lapply(seq_len(nrow(v)), function(k) c(v$Factor[k], fmt(v$SS[k], 3), fmt(100 * v$Proportion[k], 1),
                                                 if (!ob) fmt(100 * v$Cumulative[k], 1)))
  notes <- c(sprintf("Sum of squared loadings: the variance each factor carries, out of %d (one for each standardised variable).", fa$p),
             if (ob) "With correlated (promax) factors these overlap, so they are not added up.")
  foot <- sprintf("<tr><td colspan='%d' class='cdrow'>%s</td></tr>", if (ob) 3 else 4, esc(paste(notes, collapse = " ")))
  raw_table(c("Factor", "Sum of squared loadings", "% of variance", if (!ob) "Cumulative %"), body, foot = foot,
            caption = "Variance accounted for")
}

fa_phi_html <- function(fa) {
  if (fa$rotation != "promax" || fa$factors < 2) return("")
  body <- lapply(seq_len(fa$factors), function(i) c(colnames(fa$phi)[i], vapply(seq_len(fa$factors), function(j)
    if (j == i) "1" else dfmt(fa$phi[i, j], 2), "")))
  raw_table(c("", colnames(fa$phi)), body, caption = "Factor correlations")
}

fa_scores_html <- function(fa, show = 10) {
  S <- fa$scores; i <- seq_len(min(show, nrow(S)))
  body <- lapply(i, function(r) c(as.character(fa$rows[r]), vapply(seq_len(fa$factors), function(k) dfmt(S[r, k], 2), "")))
  foot <- sprintf("<tr><td colspan='%d' class='cdrow'>%s</td></tr>", fa$factors + 1, esc(paste0(
    "Factor scores (the regression method): each row's estimated position on each factor, 0 being the average row. ",
    "They are estimates, not exact values.",
    if (nrow(S) > show) sprintf(" The first %d of %d rows; the CSV holds them all.", show, nrow(S)) else "")))
  raw_table(c("Row", colnames(S)), body, foot = foot, caption = "Factor scores")
}

## ------------------------------------------------------------ readings ----

## a factor in a sentence: its share and its salient loadings, as printed
fa_factor_text <- function(fa, k) {
  l <- fa$loadings[, k]; pr <- as.numeric(dfmt(l, 2))
  i <- which(abs(pr) >= FA_SALIENT); i <- i[order(-abs(pr[i]))]
  head_ <- sprintf("%s (%s%% of the variance)", colnames(fa$loadings)[k], fmt(100 * fa$variance$Proportion[k], 1))
  if (!length(i))
    return(sprintf("%s: no variable has a salient loading on it (every loading lies between %s and %s).", head_,
                   dfmt(-FA_SALIENT, 2), dfmt(FA_SALIENT, 2)))
  pos <- i[l[i] > 0]; neg <- i[l[i] < 0]
  part <- function(j) sprintf("%s (%s)", join_and(fa$vars[j]), join_and(dfmt(l[j], 2)))
  sprintf("%s: %s%s.", head_,
          if (length(pos)) sprintf("salient positive loadings for %s", part(pos)) else "",
          if (length(neg)) sprintf("%s%s", if (length(pos)) " and negative ones for " else "salient negative loadings for ", part(neg)) else "")
}

## The reading of the analysis as a whole.
fa_text <- function(fa) {
  alpha <- fa$alpha; lvl <- paste0(pct(alpha), "%")
  out <- sprintf("The analysis uses %d %s and %d variables.", fa$n, pl(fa$n, "row", "rows"), fa$p)
  out <- c(out, sprintf("The Kaiser-Meyer-Olkin measure is %s, which Kaiser called %s%s.", fmt(fa$kmo, 2), fa_kmo_word(fa$kmo),
                        if (as.numeric(fmt(fa$kmo, 2)) < 0.5) ": the variables share too little for factor analysis to be worth while" else ""))
  low_msa <- which(as.numeric(fmt(fa$msa, 2)) < 0.5)
  if (length(low_msa))
    out <- c(out, sprintf("%s %s a measure below 0.50, so %s too little with the others; consider leaving %s out.",
                          join_and(fa$vars[low_msa]), pl(length(low_msa), "has", "have"), pl(length(low_msa), "it shares", "they share"),
                          pl(length(low_msa), "it", "them")))
  b <- fa$bartlett
  out <- c(out, if (b$p < alpha)
    sprintf("Bartlett's test finds the correlations, taken together, larger than chance would give (%s): there is shared variation for factors to describe.",
            reg_p(b$p, alpha))
    else sprintf("Bartlett's test finds insufficient evidence at the %s level that the variables are correlated at all (%s), so factor analysis has little to work with.",
                 lvl, reg_p(b$p, alpha)))
  out <- c(out, sprintf("%s%s",
    if (fa$pa_keep == 0) "No eigenvalue of the correlations is larger than random data of this size would give (parallel analysis), so no factor stands out from chance"
    else sprintf("Parallel analysis keeps %d %s of the correlations above what random data of this size would give",
                 fa$pa_keep, pl(fa$pa_keep, "eigenvalue", "eigenvalues")),
    if (fa$factors == fa$suggested && fa$pa_keep >= 1 && fa$pa_keep <= fa$mmax) sprintf(", and %d %s fitted.", fa$factors, pl(fa$factors, "factor is", "factors are"))
    else if (fa$factors == fa$suggested) sprintf("; %d %s fitted, %s.", fa$factors, pl(fa$factors, "factor is", "factors are"),
                                                 if (fa$pa_keep == 0) "the fewest possible" else sprintf("the most %d variables allow", fa$p))
    else sprintf("; %d %s fitted, as chosen.", fa$factors, pl(fa$factors, "factor is", "factors are"))))
  ft <- fa$fit
  out <- c(out, if (is.na(ft$p))
    sprintf("With %d %s for %d variables no degrees of freedom are left, so there is no test of whether that is enough.",
            fa$factors, pl(fa$factors, "factor", "factors"), fa$p)
    else if (ft$p < alpha)
      sprintf("The test that %d %s enough is significant at the %s level (chi-square = %s on %d degrees of freedom, %s): some correlation is left that %s not account for, so more factors may be needed.",
              fa$factors, pl(fa$factors, "factor is", "factors are"), lvl, fmt(ft$statistic, 2), as.integer(ft$df), reg_p(ft$p, alpha),
              pl(fa$factors, "it does", "they do"))
    else sprintf("The test that %d %s enough finds insufficient evidence at the %s level that more are needed (chi-square = %s on %d degrees of freedom, %s).",
                 fa$factors, pl(fa$factors, "factor is", "factors are"), lvl, fmt(ft$statistic, 2), as.integer(ft$df), reg_p(ft$p, alpha)))
  out <- c(out, sprintf("%s %s%% of the variance of the variables (the average communality).",
                        if (fa$factors == 1) "The factor accounts for" else "Together the factors account for",
                        fmt(100 * mean(fa$communality), 1)))
  out <- c(out, vapply(seq_len(fa$factors), function(k) fa_factor_text(fa, k), ""))
  sal <- abs(matrix(as.numeric(dfmt(fa$loadings, 2)), nrow = fa$p)) >= FA_SALIENT
  cross <- which(rowSums(sal) >= 2)
  if (length(cross))
    out <- c(out, sprintf("%s %s salient on more than one factor, so %s not belong clearly to any one.",
                          join_and(fa$vars[cross]), pl(length(cross), "is", "are"), pl(length(cross), "it does", "they do")))
  none <- which(rowSums(sal) == 0)
  if (length(none))
    out <- c(out, sprintf("%s %s salient on no factor.", join_and(fa$vars[none]), pl(length(none), "is", "are")))
  lowc <- which(as.numeric(fmt(fa$communality, 2)) < FA_LOW_COMMUNALITY)
  if (length(lowc))
    out <- c(out, sprintf("%s %s a communality below %s: the factors account for little of %s variance.",
                          join_and(fa$vars[lowc]), pl(length(lowc), "has", "have"), dfmt(FA_LOW_COMMUNALITY, 2),
                          pl(length(lowc), "its", "their")))
  if (length(fa$heywood))
    out <- c(out, sprintf(paste("The uniqueness of %s stopped at its lower limit (%s), a Heywood case: the solution forced",
                                "%s to be almost wholly explained by the factors, which is seldom real; try fewer factors,",
                                "or treat the solution with caution."), join_and(fa$vars[match(fa$heywood, fa$vars)]),
                          dfmt(FA_LOWER, 3), pl(length(fa$heywood), "it", "them")))
  if (fa$rotation == "promax" && fa$factors >= 2) {
    ph <- fa$phi; ph[lower.tri(ph, diag = TRUE)] <- NA
    shown <- abs(printed2(ph))
    top <- which(shown == max(shown, na.rm = TRUE))[1]
    ij <- arrayInd(top, dim(ph))
    out <- c(out, sprintf("Promax lets the factors correlate; the largest correlation is between %s and %s (%s).",
                          colnames(ph)[ij[1]], colnames(ph)[ij[2]], dfmt(fa$phi[ij[1], ij[2]], 2)))
  } else if (fa$rotation == "varimax" && fa$factors >= 2)
    out <- c(out, "Varimax keeps the factors uncorrelated.")
  out <- c(out, paste("A factor's sign is arbitrary. Factors summarise the variation the variables share; naming them is",
                      "the reader's judgement, and the analysis does not show what they are or what causes them."))
  left <- fa$nrow - fa$n
  if (left > 0) {
    out <- c(out, sprintf("%d of the %d rows %s left out because %s no usable value of some variable.", left, fa$nrow,
                          pl(left, "is", "are"), pl(left, "it has", "they have")))
    if (fa$held_rows > 0)
      out <- c(out, sprintf("Of these, %d %s an entry the data check has flagged (such as '%s') and will count once corrected on the Data tab.",
                            fa$held_rows, pl(fa$held_rows, "has", "have"), fa$held_eg))
  }
  paste(out, collapse = " ")
}

## ------------------------------------------------------------ plots ----

plot_fa_scree <- function(fa) {
  e <- fa$eigen; df <- data.frame(k = seq_len(nrow(e)), ev = e$Eigenvalue, pa = e$PA_threshold)
  ggplot(df, aes(x = .data$k)) +
    geom_hline(yintercept = 1, linetype = 3, colour = "grey45") +
    geom_line(aes(y = .data$pa), linetype = 2, colour = "#C0392B") +
    geom_point(aes(y = .data$pa), shape = 4, colour = "#C0392B", size = 2.4) +
    geom_line(aes(y = .data$ev), colour = "#173F7D", linewidth = 0.9) +
    geom_point(aes(y = .data$ev), colour = "#3B7DD8", size = 2.6) +
    scale_x_continuous(breaks = df$k) +
    labs(title = "Scree plot of the correlations", x = "Eigenvalue number", y = "Eigenvalue",
         caption = paste(strwrap("Blue: eigenvalues of the correlation matrix. Red crosses: parallel analysis. Dotted line: 1.", 60),
                         collapse = "\n")) +
    theme_doe()
}

plot_fa_heat <- function(fa) {
  L <- fa$loadings
  df <- data.frame(var = factor(rep(fa$vars, fa$factors), levels = rev(fa$vars)),
                   f = factor(rep(colnames(L), each = fa$p), levels = colnames(L)), v = as.vector(L))
  df$lab <- dfmt(df$v, 2)
  df$strong <- abs(as.numeric(df$lab)) >= FA_SALIENT
  ggplot(df, aes(x = .data$f, y = .data$var, fill = .data$v)) +
    geom_tile(colour = "white") +
    geom_text(aes(label = .data$lab, fontface = ifelse(.data$strong, "bold", "plain")), size = 3.6) +
    scale_fill_gradient2(low = "#C0392B", mid = "white", high = "#2E6FD0", midpoint = 0,
                         limits = c(-1, 1) * max(1, max(abs(df$v))), name = "Loading") +
    labs(title = "Loadings", x = NULL, y = NULL,
         caption = sprintf("Bold: %s or more either way.", dfmt(FA_SALIENT, 2))) +
    theme_doe() + theme(panel.grid = element_blank())
}

plot_fa_loadings <- function(fa, ax = c(1, 2)) {
  df <- data.frame(x = fa$loadings[, ax[1]], y = fa$loadings[, ax[2]], v = fa$vars)
  lim <- max(1.2, 1.1 * max(abs(c(df$x, df$y))))
  g <- ggplot(df)
  if (fa$rotation != "promax") {
    circ <- data.frame(t = seq(0, 2 * pi, length.out = 200))
    g <- g + geom_path(data = circ, aes(x = cos(.data$t), y = sin(.data$t)), colour = "grey70")
  }
  g + geom_hline(yintercept = 0, colour = "grey85") + geom_vline(xintercept = 0, colour = "grey85") +
    geom_segment(aes(x = 0, y = 0, xend = .data$x, yend = .data$y), colour = "#173F7D",
                 arrow = arrow(length = unit(0.18, "cm"))) +
    geom_text(aes(x = .data$x * 1.12, y = .data$y * 1.12, label = .data$v), size = 3.4) +
    coord_equal(xlim = c(-lim, lim), ylim = c(-lim, lim)) +
    labs(title = "Loading plot", x = colnames(fa$loadings)[ax[1]], y = colnames(fa$loadings)[ax[2]],
         caption = if (fa$rotation != "promax") "Circle: a loading of 1." else "Promax pattern loadings: no circle, as they can pass 1.") +
    theme_doe()
}

## ------------------------------------------------------------ plot notes ----

fa_scree_text <- function(fa) {
  what <- paste("The eigenvalues of the correlation matrix in order; the red crosses show what random data of this size",
                "would give, and an eigenvalue above its cross carries more than chance alone would. The dotted line",
                "marks 1, Kaiser's rule.")
  reading <- sprintf("%s %s",
    if (fa$pa_keep == 0) "No eigenvalue lies above its cross."
    else if (fa$pa_keep == 1) "The first eigenvalue lies above its cross."
    else sprintf("The first %d eigenvalues lie above their crosses.", fa$pa_keep),
    "If the line drops steeply and then levels off, factors after the bend add little.")
  list(what = what, reading = reading)
}

fa_heat_text <- function(fa) {
  what <- sprintf(paste("Each square is a variable's loading on a factor: blue for positive, red for negative, deeper the",
                        "further from 0; loadings of %s or more either way are in bold."), dfmt(FA_SALIENT, 2))
  reading <- "Read across a row to see where a variable belongs, and down a column to see which variables a factor gathers."
  list(what = what, reading = reading)
}

fa_loadings_text <- function(fa, ax = c(1, 2)) {
  nm <- colnames(fa$loadings)
  what <- sprintf(paste("Each arrow is a variable, ending at its loadings on %s (across) and %s (up). Variables whose arrows",
                        "point the same way load together; an arrow along one axis loads on that factor alone."), nm[ax[1]], nm[ax[2]])
  reading <- if (fa$rotation == "promax")
    "With promax the factors are allowed to correlate, so the two axes are not truly at right angles; read the directions loosely."
  else "The factors are uncorrelated, so the axes are at right angles and an arrow's length shows how much of the variable these two factors carry."
  list(what = what, reading = reading)
}
