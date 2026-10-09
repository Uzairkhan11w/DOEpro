###############################################################################
##  REGRESSION
###############################################################################
## Simple and multiple linear regression of one numeric column on one or more
## others, by least squares, using the rows that have every value. As on the
## other Explore tabs, every sentence is one the tables or the plots on
## screen bear out: estimates are printed to the precision their standard
## errors support, a result short of significance is "insufficient
## evidence", the level named is the level chosen, and nothing is said about
## cause that the data cannot show.

## The rank tolerance for every fit and QR here. Powers of values far from 0
## (Year^4 beside Year^3 for 2001 to 2020) differ from the lower powers by
## about one part in 10^9 even when centred and scaled, which lm's default
## (1e-7) takes for an exact combination; at 1e-10 they fit as poly() fits
## them, while a column that is an exact copy in other units is still caught.
REG_TOL <- 1e-10

## Decimals for a coefficient: enough that its standard error shows about
## three significant figures, so the estimate, its SE and its interval are
## printed to the precision the data support and line up within a row. With
## no standard error (an exact fit) the estimate keeps four figures.
reg_dec <- function(se, est) {
  if (is.finite(se) && se > 0) return(as.integer(min(20, max(0, 2 - floor(log10(se))))))
  a <- signif(abs(est), 6)
  if (!is.finite(a) || a == 0) return(2L)
  as.integer(min(20, max(0, 3 - floor(log10(a)))))
}

## A number to s significant figures, never in scientific notation (MSE,
## sums of squares, the residual checks).
sig_fmt <- function(x, s = 4) vapply(x, function(z) {
  if (!is.finite(z)) return("-")
  if (z == 0) return("0")
  formatC(z, format = "f", digits = max(0, s - 1 - floor(log10(abs(z)))))
}, "")

## "p = 0.0234", or "p < 0.0001", with enough figures that a p below the
## chosen level never prints at or above it
reg_p <- function(p, alpha) if (is.na(p)) "p not available" else if (p < 1e-4) "p < 0.0001" else paste("p =", p_show(p, alpha))

## The column names as they appear in the text and the plots
reg_names <- function(v) sprintf("'%s'", v)

#' Linear regression of one numeric variable on one or more others
#'
#' Fits \code{response} on the \code{predictors} by least squares
#' (\code{\link[stats]{lm}}), using the rows with a value of every variable.
#' One predictor gives a simple linear regression, two or more a multiple
#' regression. Text entries are read as the data check reads them: an entry
#' that is not plainly a number counts as missing; see
#' \code{\link{check_data}}.
#'
#' The coefficient table gives each estimate with its standard error, t
#' statistic, two-sided p-value, a significance mark (\code{*} at
#' \code{alpha}, \code{**} at \code{alpha / 5}, \code{NS} otherwise) and a
#' confidence interval at level \code{1 - alpha}; with two or more
#' predictors, each slope's variance inflation factor. The model summary
#' gives N, R-squared, adjusted R-squared, the residual mean square (MSE, the
#' residual sum of squares divided by its N - k - 1 degrees of freedom), its
#' square root (RMSE, the residual standard error), the mean absolute residual
#' (MAE), and the F test of the whole model. Three checks on the residuals
#' go with them: Shapiro-Wilk's test of normality; the Breusch-Pagan test
#' (Koenker's studentised form) of a spread that changes with the predictors
#' (the squared residuals on the predictors, k degrees of freedom); and a test
#' of a curve, Ramsey's RESET (with one predictor its square added to the
#' model, with several the square of the fitted values). With up to 200 rows
#' the p-values of Shapiro-Wilk and Breusch-Pagan come from 2,000 sets of
#' normal data simulated on the same predictor values (10,000 near a
#' significance threshold), since for residuals from few rows the usual ones
#' can be far off; with one residual degree of freedom both are not
#' available.
#'
#' @param d A data frame.
#' @param response The name of the numeric column to be predicted.
#' @param predictors The names of one or more numeric columns to predict it
#'   from.
#' @param alpha The significance level, between 0 and 0.5.
#'
#' @return A list of class \code{doepro_regression}: \code{equation} (the
#'   fitted equation as printed), \code{coefficients} (a data frame:
#'   \code{Term}, \code{Estimate}, \code{SE}, \code{t}, \code{p}, \code{Mark},
#'   \code{CI_lower}, \code{CI_upper} and, with two or more predictors,
#'   \code{VIF}), \code{model} (a one-row data frame: \code{N}, \code{R2},
#'   \code{Adj_R2}, \code{MSE}, \code{RMSE}, \code{MAE}, \code{F},
#'   \code{df1}, \code{df2}, \code{p}, \code{Mark}), \code{anova} (the
#'   regression, residual and total sums of squares), \code{checks} (the
#'   three residual checks: \code{Check}, \code{Test}, \code{Statistic},
#'   \code{df}, \code{p}, \code{Simulated} (whether the p-value came from
#'   simulation) and \code{Verdict}, which gives the reason when a check is
#'   not available), \code{rows} (the rows used), \code{fit}
#'   (the \code{lm} fit, on columns \code{y}, \code{x1}, \code{x2}, ...: the
#'   predictors centred and scaled by \code{centre} and \code{scale}),
#'   \code{built} (predictors that are powers or products of others), and
#'   the settings. With an exact fit (every residual zero) the standard
#'   errors, tests and intervals are \code{NA}.
#'
#' @examples
#' d <- demo_data("RCBD")
#' set.seed(1)
#' d$Height <- 60 + 0.8 * d$Yield + rnorm(nrow(d), 0, 2)
#' r <- regress_data(d, "Height", "Yield")
#' r$equation
#' r$coefficients
#' r$model
#'
#' @export
regress_data <- function(d, response, predictors, alpha = 0.05) {
  if (!is.numeric(alpha) || length(alpha) != 1L || is.na(alpha) || alpha <= 0 || alpha >= 0.5)
    stop("The significance level must be a single number between 0 and 0.5, such as 0.05.")
  if (!is.character(response) || length(response) != 1L || is.na(response) || !nzchar(response))
    stop("Choose one response variable.")
  predictors <- unique(predictors[!is.na(predictors) & nzchar(predictors)])
  if (!length(predictors)) stop("Choose at least one predictor.")
  if (response %in% predictors)
    stop(sprintf("'%s' is the response, so it cannot also be a predictor.", response))
  vars <- c(response, predictors)
  gone <- setdiff(vars, names(d))
  if (length(gone))
    stop(sprintf("%s %s of the data.", join_and(reg_names(gone)), pl(length(gone), "is not a column", "are not columns")))
  k <- length(predictors)
  vals <- lapply(stats::setNames(vars, vars), function(v) response_values(d[[v]]))
  held <- lapply(stats::setNames(vars, vars), function(v) held_flags(d[[v]]))
  ok <- Reduce(`&`, lapply(vals, is.finite))
  ## rows that would have every value once a flagged entry is corrected
  would <- Reduce(`&`, Map(function(v, h) is.finite(v) | h, vals, held))
  held_rows <- which(would & !ok)
  held_eg <- if (length(held_rows)) {
    i <- held_rows[1]; v <- vars[vapply(held, function(h) h[i], logical(1))][1]
    trimws(as.character(d[[v]][i]))
  } else ""
  held_note <- if (length(held_rows))
    sprintf(" %d more %s an entry the data check has flagged (such as '%s'), which will count once corrected on the Data tab.",
            length(held_rows), pl(length(held_rows), "row has", "rows have"), held_eg) else ""
  n <- sum(ok)
  if (n < k + 2)
    stop(sprintf(paste0("Only %d %s a value of every variable chosen; a regression on %d %s needs at least %d, ",
                        "so that something is left over to measure the scatter about the fitted %s.%s"),
                 n, pl(n, "row has", "rows have"), k, pl(k, "predictor", "predictors"), k + 2,
                 if (k == 1) "line" else "equation", held_note))
  y <- vals[[1]][ok]
  X <- do.call(cbind, lapply(vals[-1], function(v) v[ok]))
  colnames(X) <- predictors
  flat <- predictors[apply(X, 2, function(z) length(unique(z)) < 2)]
  if (length(flat))
    stop(sprintf("%s %s the same value in every row used, so %s cannot be estimated; leave %s out.",
                 join_and(reg_names(flat)), pl(length(flat), "has", "each have"), pl(length(flat), "its effect", "their effects"),
                 pl(length(flat), "it", "them")))
  if (length(unique(y)) < 2)
    stop(sprintf("'%s' has the same value in every row used, so there is no variation for the predictors to account for.", response))

  ## The fit is made on the predictors centred and scaled, and its
  ## coefficients turned back to the scale of the data: raw powers of values
  ## far from 0 (Year, Year^2 and Year^3 for 2001 to 2020), or times written
  ## as large counts, are so alike in their leading digits that a fit on them
  ## drops a column as if it were a combination of the others, which it is not.
  safe <- paste0("x", seq_len(k))
  cen <- colMeans(X); scl <- apply(X, 2, stats::sd)
  Z <- sweep(sweep(X, 2, cen), 2, scl, "/")
  df <- data.frame(y = y, Z); names(df) <- c("y", safe)
  fit <- stats::lm(stats::reformulate(safe, "y"), data = df, tol = REG_TOL)
  g <- stats::coef(fit)
  if (anyNA(g)) {
    al <- predictors[is.na(g[safe])]
    others <- k - length(al)
    stop(sprintf(paste("%s can be worked out exactly from the other %s in the rows used (one may be the sum of others,",
                       "or the same measurement in other units), so %s cannot be told apart from %s; leave %s out."),
                 join_and(reg_names(al)), pl(others, "predictor", "predictors"),
                 pl(length(al), "its effect", "their effects"), pl(others, "the other predictor's", "the others'"),
                 pl(length(al), "it", "them")))
  }
  e <- stats::residuals(fit)
  ## residuals that are only the rounding left by an exact fit: nothing to
  ## test against, so the standard errors, tests and intervals are left out.
  ## They are judged against the spread of the response, with a floor for the
  ## rounding its size leaves (counts near 2e9 that scatter by 1 are not an
  ## exact fit)
  exact <- all(abs(e) <= max(1e-9 * max(abs(y - mean(y))), 1e3 * .Machine$double.eps * max(abs(y))))
  dfr <- fit$df.residual

  ## coefficients and their covariance on the scale of the data
  Tm <- diag(k + 1); Tm[1, -1] <- -cen / scl; Tm[cbind(2:(k + 1), 2:(k + 1))] <- 1 / scl
  est <- unname(drop(Tm %*% g))
  V <- Tm %*% suppressWarnings(stats::vcov(fit)) %*% t(Tm)
  se <- sqrt(pmax(diag(V), 0)); tt <- est / se
  pp <- 2 * stats::pt(-abs(tt), dfr)
  qa <- stats::qt(1 - alpha / 2, dfr)
  lo <- est - qa * se; hi <- est + qa * se
  if (exact) {
    se[] <- tt[] <- pp[] <- lo[] <- hi[] <- NA_real_
    ## an exact fit's zero coefficients come out as rounding residue (3e-15):
    ## any whose part in the fitted values is below that is 0
    part <- abs(est) * c(1, apply(abs(X), 2, max))
    est[part <= 1e-9 * max(abs(y))] <- 0
  }
  coefs <- data.frame(Term = c("Intercept", predictors), Estimate = est, SE = se, t = tt, p = pp,
                      Mark = ifelse(is.na(pp), "", star(pp, alpha)), CI_lower = lo, CI_upper = hi,
                      stringsAsFactors = FALSE)
  if (k >= 2) {
    v <- tryCatch(diag(solve(stats::cor(X))), error = function(err) rep(Inf, k))
    coefs$VIF <- c(NA_real_, unname(v))
  }

  fv <- stats::fitted(fit)
  sse <- sum(e^2); ssr <- sum((fv - mean(y))^2); sst <- sum((y - mean(y))^2)
  mse <- sse / dfr
  Fst <- if (exact) NA_real_ else (ssr / k) / mse
  pF <- if (exact) NA_real_ else stats::pf(Fst, k, dfr, lower.tail = FALSE)
  model <- data.frame(N = n, R2 = if (exact) 1 else ssr / sst,
                      Adj_R2 = if (exact) 1 else 1 - (sse / dfr) / (sst / (n - 1)),
                      MSE = if (exact) 0 else mse, RMSE = if (exact) 0 else sqrt(mse),
                      MAE = if (exact) 0 else mean(abs(e)),
                      F = Fst, df1 = k, df2 = dfr, p = pF, Mark = if (is.na(pF)) "" else star(pF, alpha))
  anova <- data.frame(Source = c("Regression", "Residual", "Total"), df = c(k, dfr, n - 1L),
                      SS = c(ssr, if (exact) 0 else sse, sst),
                      MS = c(ssr / k, if (exact) 0 else mse, NA), F = c(Fst, NA, NA), p = c(pF, NA, NA),
                      stringsAsFactors = FALSE)

  dec <- mapply(reg_dec, se, est)
  ## a significant coefficient's interval excludes 0, so neither end may
  ## print as 0: the row gains decimals until they do not
  for (i in which(!is.na(pp) & pp < alpha)) {
    d0 <- dec[i]
    while (dec[i] < d0 + 6 && (as.numeric(dfmt(lo[i], dec[i])) == 0 || as.numeric(dfmt(hi[i], dec[i])) == 0))
      dec[i] <- dec[i] + 1L
  }
  ## The printed equation must reproduce the fitted values: when predictors
  ## are nearly alike (powers of a calendar year) coefficients rounded one by
  ## one to their standard errors do not, so every row gains decimals until
  ## the printed equation is within 1% of the RMSE of the fit at every row.
  fv0 <- stats::fitted(fit)
  tolv <- if (exact) 1e-6 * stats::sd(y) else 0.01 * sqrt(sse / dfr)
  extra <- 0L
  printed_fit <- function(dd) drop(cbind(1, X) %*% vapply(seq_along(est), function(i) as.numeric(dfmt(est[i], dd[i])), 0))
  while (extra < 12 && max(abs(printed_fit(dec + extra) - fv0)) > tolv) extra <- extra + 1L
  dec <- dec + extra
  ## rows left out for an entry that is not a finite number (Inf from a
  ## division by zero), which the data check does not list
  inf_rows <- sum(!ok & Reduce(`|`, lapply(vals, is.infinite)))
  r <- list(response = response, predictors = predictors, alpha = alpha, k = k, n = n,
            rows = row_ids(d)[ok], y = y, X = X, fit = fit, centre = cen, scale = scl, exact = exact, dec = dec,
            coefficients = coefs, model = model, anova = anova,
            held_rows = length(held_rows), held_eg = held_eg, inf_rows = inf_rows, extra = extra,
            lacking = sum(!would), nrow = nrow(d))
  r$built <- reg_built(X)
  r$equation <- reg_equation(r)
  r$checks <- reg_checks(r)
  class(r) <- "doepro_regression"
  r
}

#' @rdname regress_data
#' @param x A result of \code{regress_data()}.
#' @param ... Not used.
#' @export
print.doepro_regression <- function(x, ...) {
  cat(x$equation, "\n\n")
  print(x$coefficients, row.names = FALSE)
  cat("\n")
  print(x$model, row.names = FALSE)
  invisible(x)
}

## Columns built from other predictors: powers (Dose2 = Dose^2, Dose3 =
## Dose^3, up to the fourth) and products of two (NP = N x P), as when a
## polynomial or a response surface is fitted. A value counts when it is
## the power or product to the decimals it was entered with (156.3 for
## 12.5^2, as a spreadsheet copies it) and to three significant figures.
## One row per built column: `kind` "power" or "product", built from
## predictor `a` (and `b` for a product), `p` the power. A built column
## cannot change without the predictors it is built from, so they are drawn
## and read together.
reg_built <- function(X) {
  k <- ncol(X)
  out <- data.frame(col = integer(0), kind = character(0), a = integer(0), b = integer(0), p = integer(0),
                    stringsAsFactors = FALSE)
  if (k < 2) return(out)
  decs <- lapply(seq_len(k), function(m) {
    s <- num_text(X[, m])
    ifelse(grepl(".", s, fixed = TRUE), nchar(sub("^[^.]*[.]", "", s)), 0)
  })
  ## Each entry must be the target to the decimals it was entered with. When
  ## some entry is only rounded, each entry large enough to carry three
  ## figures at those decimals must also match to three figures, and at
  ## least two distinct non-zero entries must be of that kind, so small whole
  ## numbers (1, 3, 4 beside 1.21, 2.56, 4.41) are not taken for squares.
  same <- function(m, target) {
    big <- max(abs(target))
    if (big == 0) return(FALSE)
    gap <- abs(X[, m] - target)
    if (!all(gap <= pmax(0.5 * 10^(-decs[[m]]), 1e-9 * big) * (1 + 1e-9))) return(FALSE)
    if (all(gap <= 1e-9 * big)) return(TRUE)
    three <- abs(target) >= 10^(2 - decs[[m]])
    all(gap[three] <= 5e-3 * abs(target[three]) + 1e-9 * big) && length(unique(target[three & target != 0])) >= 2
  }
  cand <- list()
  add <- function(m, kind, a, b, p)
    cand[[length(cand) + 1]] <<- data.frame(col = m, kind = kind, a = a, b = b, p = p, stringsAsFactors = FALSE)
  for (m in seq_len(k)) {
    v <- setdiff(seq_len(k), m)
    for (j in v) for (p in 2:4) if (same(m, X[, j]^p)) add(m, "power", j, NA_integer_, p)
    if (length(v) >= 2) for (pr in utils::combn(length(v), 2, simplify = FALSE))
      if (same(m, X[, v[pr[1]]] * X[, v[pr[2]]])) add(m, "product", v[pr[1]], v[pr[2]], NA_integer_)
  }
  if (!length(cand)) return(out)
  cand <- do.call(rbind, cand)
  ## a column built from a built column (Dose4 = Dose2^2) counts only through
  ## predictors that are not themselves built
  built <- unique(cand$col)
  cand <- cand[!cand$a %in% built & (is.na(cand$b) | !cand$b %in% built), , drop = FALSE]
  cand <- cand[!duplicated(cand$col), , drop = FALSE]
  cand <- cand[order(cand$col), , drop = FALSE]
  rownames(cand) <- NULL
  cand
}

reg_power_word <- function(p) c("square", "cube", "fourth power")[p - 1]
## the row of `built` for column m, NULL when m is not built
reg_built_row <- function(r, m) if (is.null(r$built) || !m %in% r$built$col) NULL else r$built[match(m, r$built$col), ]
reg_bases <- function(r) setdiff(seq_len(r$k), r$built$col)
## the power columns of predictor j, by increasing power, and its products
reg_powers_of <- function(r, j) {
  w <- r$built[r$built$kind == "power" & r$built$a == j, , drop = FALSE]
  w$col[order(w$p)]
}
reg_products_of <- function(r, j) r$built$col[r$built$kind == "product" & (r$built$a == j | (!is.na(r$built$b) & r$built$b == j))]
## the square of j when it is j's only power: a quadratic in j
reg_quad_of <- function(r, j) {
  pw <- reg_powers_of(r, j)
  if (length(pw) == 1 && reg_built_row(r, pw)$p == 2) pw else NA_integer_
}
## the predictor a column is shown through: itself, or the first it is
## built from
reg_source <- function(r, m) { w <- reg_built_row(r, m); if (is.null(w)) m else w$a }

## The groups of columns that change together: predictors joined by a
## product, with their powers and products. One vector of column indices
## per group of two or more columns, the predictors first.
reg_groups <- function(r) {
  if (is.null(r$built) || !nrow(r$built)) return(list())
  bases <- reg_bases(r)
  lab <- bases
  prod <- r$built[r$built$kind == "product", , drop = FALSE]
  repeat {
    changed <- FALSE
    for (i in seq_len(nrow(prod))) {
      la <- lab[match(prod$a[i], bases)]; lb <- lab[match(prod$b[i], bases)]
      if (la != lb) { lab[lab == max(la, lb)] <- min(la, lb); changed <- TRUE }
    }
    if (!changed) break
  }
  out <- lapply(unique(lab), function(L) {
    bs <- bases[lab == L]
    c(bs, sort(r$built$col[r$built$a %in% bs]))
  })
  out[vapply(out, length, 1L) > 1]
}
reg_group <- function(r, j) { for (g in reg_groups(r)) if (j %in% g) return(g); j }

## Every column of the design from the values of the predictors that are not
## built: powers and products worked out from them.
reg_design <- function(r, B) {
  for (i in seq_len(NROW(r$built))) {
    w <- r$built[i, ]
    B[, w$col] <- if (w$kind == "power") B[, w$a]^w$p else B[, w$a] * B[, w$b]
  }
  B
}

## predictions from the fit, which is on centred and scaled columns
reg_predict <- function(r, B, interval = FALSE) {
  Z <- sweep(sweep(B, 2, r$centre), 2, r$scale, "/")
  nd <- as.data.frame(Z); names(nd) <- paste0("x", seq_len(r$k))
  suppressWarnings(stats::predict(r$fit, nd, interval = if (interval) "confidence" else "none", level = 1 - r$alpha))
}



## a coefficient as the table prints it
reg_coef_txt <- function(r, i, what = "Estimate") {
  v <- r$coefficients[[what]][i]
  if (is.na(v)) "-" else dfmt(v, r$dec[i])
}

## The fitted equation, each number as the coefficient table prints it:
## Yield = 12.35 + 0.452 x Nitrogen - 1.20 x Rain. The terms are kept apart
## so a wrapped line never splits one.
reg_equation_terms <- function(r) {
  out <- paste(r$response, "=", reg_coef_txt(r, 1))
  for (j in seq_len(r$k)) {
    s <- reg_coef_txt(r, j + 1)
    out <- c(out, paste0(if (startsWith(s, "-")) "- " else "+ ", sub("^-", "", s), " \u00d7 ", r$predictors[j]))
  }
  out
}
reg_equation <- function(r) paste(reg_equation_terms(r), collapse = " ")

## ------------------------------------------------------------- checks ----

## Residuals are not independent values: part of their pattern is set by the
## predictor values, so with few rows the usual p-values of Shapiro-Wilk and
## Breusch-Pagan applied to them can be far off. In simulation (normal data,
## designs with small whole-number predictors or one far-out row) Shapiro-
## Wilk called 10-23% of them non-normal at 5% with 2-5 residual degrees of
## freedom, and Breusch-Pagan 15-28%; Breusch-Pagan settled near 5% only
## from about 10. Neither statistic depends on the coefficients or the SD,
## only on the predictor values, so up to REG_SIM_N rows each p-value is
## found by simulation instead: REG_SIM_B sets of normal data on these same
## predictor values, a fixed seed, and p = (k + 1) / (B + 1) from the sets at
## least as extreme, which holds the level for any design.
## A p-value within three standard errors of alpha or alpha / 5 is worked out
## again from REG_SIM_B_CLOSE sets, so a verdict near the threshold rests on
## a p-value good to about +/- 0.002 rather than 0.005.
REG_SIM_N <- 200
REG_SIM_B <- 2000
REG_SIM_B_CLOSE <- 10000

## residuals of B sets of standard normal data on the model's predictors
reg_sim_resid <- function(fit, B = REG_SIM_B) {
  Q <- qr.Q(fit$qr)
  n <- nrow(Q)
  Z <- with_fixed_seed(COR_PERM_SEED + B, matrix(stats::rnorm(n * B), n))
  Z - Q %*% crossprod(Q, Z)
}

## (k + 1) / (B + 1) from the simulated statistics at least as extreme as the
## observed one, worked out again from more sets when it falls close to a
## threshold; `stat` gives the statistic of each column of residuals and
## `extreme` says which are at least as extreme
reg_sim_p <- function(fit, alpha, E, stat, extreme) {
  p_of <- function(E) (sum(extreme(stat(E))) + 1) / (ncol(E) + 1)
  p <- p_of(E); B <- ncol(E)
  se <- sqrt(p * (1 - p) / B)
  if (any(abs(p - c(alpha, alpha / 5)) < 3 * se)) { p <- p_of(reg_sim_resid(fit, REG_SIM_B_CLOSE)); B <- REG_SIM_B_CLOSE }
  structure(p, sets = B)
}

## Breusch-Pagan's N R^2 of the squared residuals on the predictors, for one
## residual vector or each column of a matrix of them (Koenker's studentised
## form: no assumption of normal residuals in the statistic itself)
reg_bp_stat <- function(E, Q) {
  E <- as.matrix(E)
  S <- E^2
  Sc <- sweep(S, 2, colMeans(S))
  tot <- colSums(Sc^2)
  ## residuals all of one size: no change of spread at all
  flat <- tot <= (1e-8 * colMeans(S))^2 * nrow(S)
  fit <- colSums((Q %*% crossprod(Q, Sc))^2)
  ifelse(flat, 0, nrow(S) * fit / tot)
}

## The test of a curve in predictor j: the square of the (centred) predictor
## added to the full model and tested by F. With one predictor this is
## Ramsey's RESET; it stays defined when the fitted line is flat.
reg_curve <- function(r, j) {
  na <- function(why) list(stat = NA_real_, df = NA_character_, p = NA_real_, why = why)
  m <- reg_quad_of(r, j); pw <- reg_powers_of(r, j)
  if (!is.na(m))
    return(na(sprintf("%s, the square of %s, is already in the model, so a curve in %s is already allowed for",
                      r$predictors[m], r$predictors[j], r$predictors[j])))
  if (length(pw))
    return(na(sprintf("powers of %s (%s) are already in the model, so a curve in %s is already allowed for",
                      r$predictors[j], join_and(r$predictors[pw]), r$predictors[j])))
  if (r$exact) return(na("the model fits every value exactly, so there is no scatter to test"))
  if (r$fit$df.residual < 2)
    return(na(sprintf("it needs at least %d rows, one more than the regression itself", r$k + 3)))
  df <- r$fit$model
  x <- r$X[, j]
  df$q <- ((x - mean(x)) / stats::sd(x))^2
  f2 <- stats::lm(stats::update(stats::formula(r$fit), . ~ . + q), data = df, tol = REG_TOL)
  if (is.na(stats::coef(f2)[["q"]]))
    return(na(if (length(unique(x)) <= 2)
      sprintf("%s takes only two distinct values, so a curve cannot be told from a straight line", r$predictors[j])
      else sprintf(paste("the square of %s can already be worked out from the predictors in the model (a squared",
                         "column may be among them), so a curve in it is already allowed for"), r$predictors[j])))
  a <- stats::anova(r$fit, f2)
  list(stat = a$F[2], df = sprintf("1, %d", f2$df.residual), p = a$`Pr(>F)`[2], why = NULL)
}

## The three checks on the residuals, each with its statistic, degrees of
## freedom, p-value and verdict, or the reason it cannot be worked out:
##   normality: Shapiro-Wilk on the residuals;
##   constant spread: Breusch-Pagan of the squared residuals on the
##     predictors, N R^2 on k df;
##   a curve: with one predictor its square added (Ramsey's RESET); with
##     several, RESET proper, the square of the fitted values added, tested by
##     F on 1 and N - k - 2 df.
reg_checks <- function(r) {
  alpha <- r$alpha
  e <- stats::residuals(r$fit); fv <- stats::fitted(r$fit); n <- r$n
  dfr <- r$fit$df.residual
  Q <- qr.Q(r$fit$qr)
  na <- function(why) list(stat = NA_real_, df = NA_character_, p = NA_real_, why = why, sim = FALSE)
  exact_why <- "the model fits every value exactly, so there is no scatter to test"
  one_df <- "with one residual degree of freedom, the pattern of the residuals is fixed by the predictor values"
  sim <- !r$exact && dfr >= 2 && n <= REG_SIM_N
  ## with many rows a handful of sets is enough to spot a design that fixes
  ## the Breusch-Pagan statistic
  E <- if (!r$exact && dfr >= 2) reg_sim_resid(r$fit, if (sim) REG_SIM_B else 5)
  normal <- if (r$exact) na(exact_why)
    else if (dfr < 2) na(one_df)
    else if (n > 5000) na("Shapiro-Wilk's test takes at most 5,000 values")
    else {
      w <- unname(stats::shapiro.test(e)$statistic)
      p <- if (sim) reg_sim_p(r$fit, alpha, E, function(E) apply(E, 2, function(z) stats::shapiro.test(z)$statistic),
                              function(ws) ws <= w + 1e-12)
           else stats::shapiro.test(e)$p.value
      list(stat = w, df = "", p = as.numeric(p), why = NULL, sim = sim, sets = attr(p, "sets"))
    }
  ## A row that alone decides a coefficient (leverage 1) has a residual of 0
  ## whatever the data, which Breusch-Pagan would read as a change of spread:
  ## it is left out of that test, with the predictors as they are in the rows
  ## that remain.
  keep <- stats::hatvalues(r$fit) < 1 - 1e-8
  ## on the centred and scaled columns the fit used, so no direction is lost
  qk <- qr(cbind(1, as.matrix(r$fit$model[keep, -1, drop = FALSE])), tol = REG_TOL)
  Qk <- qr.Q(qk)[, seq_len(qk$rank), drop = FALSE]
  nk <- sum(keep)
  spread <- if (r$exact) na(exact_why)
    else if (dfr < 2) na(one_df)
    ## when every set's squared residuals are fitted exactly by the
    ## predictors (two rows at each setting, each setting fitted on its own),
    ## the statistic is N whatever the data
    else if (qk$rank < 2)
      na(paste("the rows that have a residual all share one setting of the predictors (each other row alone decides a",
               "coefficient and has no residual), so a change of spread cannot be measured"))
    else if (all(reg_bp_stat(E[keep, seq_len(min(5, ncol(E))), drop = FALSE], Qk) >= nk * (1 - 1e-8)))
      na(paste("the squared residuals are fitted exactly by the predictors whatever the data (as with two rows at each",
               "setting of the predictors), so a change of spread cannot be tested"))
    else {
      st <- reg_bp_stat(e[keep], Qk)
      p <- if (sim) reg_sim_p(r$fit, alpha, E, function(E) reg_bp_stat(E[keep, , drop = FALSE], Qk),
                              function(b) b >= st - 1e-9 * max(1, st))
           else stats::pchisq(st, qk$rank - 1, lower.tail = FALSE)
      list(stat = st, df = as.character(qk$rank - 1), p = as.numeric(p), why = NULL, sim = sim, sets = attr(p, "sets"))
    }
  curve <- if (r$k == 1) c(reg_curve(r, 1), sim = FALSE)
    else if (r$exact) na(exact_why)
    else if (dfr < 2) na(sprintf("it needs at least %d rows, one more than the regression itself", r$k + 3))
    ## a flat fit: its square is rounding residue, not a curve
    else if (stats::sd(fv) <= 1e-9 * stats::sd(r$y))
      na("the fitted values are all the same, so their square cannot show a curve")
    else {
      df <- r$fit$model
      df$q <- ((fv - mean(fv)) / stats::sd(fv))^2
      f2 <- stats::lm(stats::update(stats::formula(r$fit), . ~ . + q), data = df, tol = REG_TOL)
      if (is.na(stats::coef(f2)[["q"]]))
        na(if (length(unique(signif(fv, 10))) <= 2)
             "the fitted values take too few distinct values for a curve to be told from a straight line"
           else paste("the square of the fitted values can already be worked out from the predictors in the model,",
                      "so a curve along them is already allowed for"))
      else {
        a <- stats::anova(r$fit, f2)
        list(stat = a$F[2], df = sprintf("1, %d", f2$df.residual), p = a$`Pr(>F)`[2], why = NULL, sim = FALSE)
      }
    }
  out <- data.frame(Check = c("Normal residuals", "Constant spread", "Straight line"),
                    Test = c("Shapiro-Wilk W", "Breusch-Pagan chi-square",
                             if (r$k == 1) paste0("F, square of ", r$predictors, " added") else "RESET F"),
                    Statistic = c(normal$stat, spread$stat, curve$stat),
                    df = c(normal$df, spread$df, curve$df),
                    p = c(normal$p, spread$p, curve$p),
                    Simulated = c(normal$sim, spread$sim, FALSE),
                    Sets = c(normal$sets %||% NA_real_, spread$sets %||% NA_real_, NA_real_), stringsAsFactors = FALSE)
  out$Verdict <- ifelse(is.na(out$p), paste("not available:", c(normal$why %||% "", spread$why %||% "", curve$why %||% "")),
                        ifelse(out$p < alpha, "significant departure", "no significant departure"))
  out
}

## ------------------------------------------------------------ display ----

reg_mark_key <- function(alpha)
  sprintf("** significant at p < %s; * significant at p < %s; NS not significant at the %s%% level.",
          p_lab(alpha / 5), p_lab(alpha), pct(alpha))

## The coefficients: each estimate, its SE and its interval to the same
## decimals, so the row reads straight across.
reg_coef_html <- function(r) {
  cf <- r$coefficients; alpha <- r$alpha; multi <- r$k >= 2
  body <- lapply(seq_len(nrow(cf)), function(i) c(
    esc(cf$Term[i]), reg_coef_txt(r, i), reg_coef_txt(r, i, "SE"),
    if (is.na(cf$t[i])) "-" else dfmt(cf$t[i], 2),
    esc(p_show(cf$p[i], alpha)), cf$Mark[i],
    if (is.na(cf$CI_lower[i])) "-" else sprintf("%s to %s", reg_coef_txt(r, i, "CI_lower"), reg_coef_txt(r, i, "CI_upper")),
    if (multi) (if (i == 1) "" else if (is.finite(cf$VIF[i])) fmt(cf$VIF[i], 2) else "-")))
  head_ <- c("Term", "Estimate", "SE", "t", "p", "Mark", sprintf("%s%% CI", pct(1 - alpha)), if (multi) "VIF")
  notes <- c(
    sprintf(paste("Estimate: for the intercept, the predicted %s when %s 0; for a predictor, the change in the",
                  "predicted %s for one unit more of it%s. SE: its standard error. t = Estimate / SE, tested on",
                  "N - k - 1 = %d - %d - 1 = %d %s of freedom (N rows used, k predictors). CI: the %s%% confidence interval."),
            esc(r$response), if (multi) "every predictor is" else paste(esc(r$predictors), "is"), esc(r$response),
            if (multi) ", the other predictors held at the same values" else "", r$n, r$k, r$model$df2,
            pl(r$model$df2, "degree", "degrees"), pct(1 - alpha)),
    if (length(reg_groups(r))) paste(vapply(reg_groups(r), function(g) esc(reg_group_note(r, g)), ""), collapse = " "),
    esc(reg_mark_key(alpha)),
    if (multi) paste("VIF (variance inflation factor): how much a slope's variance is inflated because its predictor",
                     "is related to the others; 1 means unrelated, and 5 or more is commonly taken as a warning."),
    if (r$exact) "The model fits every value exactly, so there is no scatter to judge the estimates against: standard errors, tests and intervals are not available.")
  foot <- sprintf("<tr><td colspan='%d' class='cdrow'>%s</td></tr>", length(head_), paste(notes, collapse = " "))
  raw_table(head_, body, foot = foot, caption = "Coefficients")
}

## The model in one row: how well it fits, and its F test.
reg_model_html <- function(r) {
  m <- r$model; alpha <- r$alpha
  body <- list(c(as.character(m$N), fmt(m$R2, 3), dfmt(m$Adj_R2, 3), sig_fmt(m$MSE), sig_fmt(m$RMSE), sig_fmt(m$MAE),
                 if (is.na(m$F)) "-" else fmt(m$F, 2), sprintf("%d, %d", m$df1, m$df2), esc(p_show(m$p, alpha)), m$Mark))
  head_ <- c("N", "R\u00b2", "Adjusted R\u00b2", "MSE", "RMSE", "MAE", "F", "df", "p", "Mark")
  notes <- c(
    "R\u00b2: the share of the variation in the response about its mean that the fitted equation accounts for.",
    paste("Adjusted R\u00b2: the same, with an allowance for the number of predictors; it falls below 0 when the",
          "predictors account for less of the variation than chance alone would give."),
    "MSE: the residual mean square, the sum of squared residuals divided by N - k - 1 (N rows used, k predictors; as in the ANOVA below).",
    "RMSE: its square root, the residual standard error, which estimates the standard deviation of the scatter about the true equation.",
    "MAE: the mean of the absolute residuals.",
    sprintf("F tests the whole model at the %s%% level.", pct(alpha)))
  foot <- sprintf("<tr><td colspan='%d' class='cdrow'>%s</td></tr>", length(head_), paste(notes, collapse = " "))
  raw_table(head_, body, foot = foot, caption = "Model summary")
}

## The regression ANOVA: the total variation split into the part the
## equation accounts for and the residual.
reg_anova_html <- function(r) {
  a <- r$anova; alpha <- r$alpha
  body <- lapply(seq_len(nrow(a)), function(i) c(
    a$Source[i], as.character(a$df[i]), sig_fmt(a$SS[i], 5), if (is.na(a$MS[i])) "" else sig_fmt(a$MS[i], 5),
    if (i == 1 && !is.na(a$F[i])) fmt(a$F[i], 2) else if (i == 1) "-" else "",
    if (i == 1) esc(p_show(a$p[i], alpha)) else "", if (i == 1) r$model$Mark else ""))
  foot <- sprintf(paste("<tr><td colspan='7' class='cdrow'>The total variation in %s about its mean, split into the part",
                        "the fitted equation accounts for and the residual. F = regression mean square / residual mean",
                        "square: the model's test in the summary above. R\u00b2 = regression sum of squares / total.</td></tr>"),
                  esc(r$response))
  raw_table(c("Source", "df", "Sum of squares", "Mean square", "F", "p", "Mark"), body, foot = foot,
            caption = "Analysis of variance of the regression")
}

## The residual checks, with what each would show.
reg_checks_html <- function(r) {
  ck <- r$checks; alpha <- r$alpha
  fams <- NROW(r$built) > 0
  body <- lapply(seq_len(nrow(ck)), function(i) c(
    if (fams && ck$Check[i] == "Straight line") "Shape of the fit" else ck$Check[i], ck$Test[i],
    if (is.na(ck$Statistic[i])) "-" else fmt(ck$Statistic[i], if (i == 1) 3 else 2),
    if (is.na(ck$df[i]) || !nzchar(ck$df[i])) "" else ck$df[i],
    esc(p_show(ck$p[i], alpha)),
    if (grepl("^significant", ck$Verdict[i])) paste0("<b>", ck$Verdict[i], "</b>") else esc(ck$Verdict[i])))
  notes <- c(
    "Shapiro-Wilk: whether the residuals could come from a normal distribution.",
    if (r$k == 1) sprintf("Breusch-Pagan: whether the spread of the residuals changes with %s (a funnel in the residual plot).", esc(r$predictors))
    else "Breusch-Pagan: whether the spread of the residuals changes with the predictors (a band that widens along one of them).",
    if (r$k == 1) sprintf("Straight line: whether adding the square of %s fits significantly better than the straight line (with one predictor, Ramsey's RESET test).", esc(r$predictors))
    else if (fams) paste("RESET: whether adding the square of the fitted values fits significantly better, a pattern along",
                         "the fitted values that the fitted equation does not follow.")
    else paste("RESET: whether adding the square of the fitted values fits significantly better, a curve along the fitted",
               "values; a curve in one predictor is tested in the main plot's note."),
    if (any(ck$Simulated)) {
      sm <- which(ck$Simulated)
      nm <- c("Shapiro-Wilk", "Breusch-Pagan")[sm]
      sets <- vapply(ck$Sets[sm], function(b) formatC(b, format = "d", big.mark = ","), "")
      paste0(sprintf(paste("The %s of %s %s from sets of normal data simulated on these same predictor values (%s):",
                           "with few rows the usual formulas can be far off for residuals."),
                     pl(length(sm), "p-value", "p-values"), join_and(nm), pl(length(sm), "comes", "come"),
                     paste(sprintf("%s for %s", sets, nm), collapse = ", ")),
             if (any(ck$Sets[sm] > REG_SIM_B))
               sprintf(" %s sets are used when the first estimate from %s falls close to a threshold.",
                       formatC(REG_SIM_B_CLOSE, format = "d", big.mark = ","), formatC(REG_SIM_B, format = "d", big.mark = ","))
             else "")
    },
    sprintf("Verdicts at the %s%% level.", pct(alpha)))
  foot <- sprintf("<tr><td colspan='6' class='cdrow'>%s</td></tr>", paste(notes, collapse = " "))
  raw_table(c("Check", "Test", "Statistic", "df", "p", "Verdict"), body, foot = foot, caption = "Checks on the residuals")
}

## the range of a predictor in the rows used, as "40 to 160"
reg_range <- function(r, j) {
  x <- r$X[, j]
  ## whole numbers and short decimals as entered; a long binary fraction cut
  ## to six figures, outward, so the range printed still holds every value
  end <- function(v, up) {
    if (v == round(v) || v == signif(v, 10)) return(num_text(v))
    f <- 10^(5 - floor(log10(abs(v))))
    num_text(signif((if (up) ceiling(v * f) else floor(v * f)) / f, 10))
  }
  paste(end(min(x), FALSE), "to", end(max(x), TRUE))
}

## Where a quadratic in predictor j turns, x = -b1 / (2 b2), from the
## coefficients as fitted (rounded ones can move it by years when j is a
## calendar year), with any product of j taken at the other predictor's held
## value. Printed to a thousandth of the observed range, and judged inside,
## at the end of, or outside it on the printed value.
reg_turn_text <- function(r, j, held = colMeans(r$X)) {
  sq <- reg_quad_of(r, j); cf <- r$coefficients; nm <- r$predictors
  if (as.numeric(reg_coef_txt(r, sq + 1)) == 0)
    return(sprintf("The coefficient of %s prints as 0, so over the data the curve is close to a straight line.", nm[sq]))
  pr <- reg_products_of(r, j)
  b1 <- cf$Estimate[j + 1]
  oth <- integer(0)
  for (m in pr) {
    w <- reg_built_row(r, m); o <- if (w$a == j) w$b else w$a
    b1 <- b1 + cf$Estimate[m + 1] * held[o]; oth <- c(oth, o)
  }
  b2 <- cf$Estimate[sq + 1]
  x <- r$X[, j]; lo <- min(x); hi <- max(x)
  tp <- dfmt(-b1 / (2 * b2), max(0, 3 - floor(log10(hi - lo)))); tv <- as.numeric(tp)
  where <- if (tv > lo && tv < hi) sprintf("inside the observed range (%s)", reg_range(r, j))
           else if (tv == lo || tv == hi) sprintf("at the %s end of the observed range (%s)", if (tv == hi) "upper" else "lower", reg_range(r, j))
           else sprintf("outside the observed range (%s), so over the data the curve keeps %s", reg_range(r, j),
                        if ((b2 < 0) == (tv > hi)) "rising" else "falling")
  paste0(if (length(pr)) "With the other predictors at their means, the curve" else "The curve",
         sprintf(" is %s at %s = %s, where its slope is 0, %s.", if (b2 < 0) "highest" else "lowest", nm[j], tp, where),
         if (length(pr)) sprintf(" Its shape in %s changes with %s (through %s), so at other values it turns elsewhere.",
                                 nm[j], join_and(nm[unique(oth)]), join_and(nm[pr])) else "")
}

## the F test of a group of columns: the model against the model without them
reg_partial <- function(r, cols) {
  keep <- setdiff(seq_len(r$k), cols)
  df <- r$fit$model
  red <- if (length(keep)) stats::lm(stats::reformulate(paste0("x", keep), "y"), data = df, tol = REG_TOL) else stats::lm(y ~ 1, data = df)
  a <- stats::anova(red, r$fit)
  list(F = a$F[2], df1 = a$Df[2], df2 = a$Res.Df[2], p = a$`Pr(>F)`[2])
}

## how a group is named in the reading
reg_group_name <- function(r, g) {
  bs <- intersect(g, reg_bases(r))
  if (length(bs) == 1) sprintf("the curve in %s", r$predictors[bs]) else sprintf("the terms in %s", join_and(r$predictors[bs]))
}

## what each built column of a group is
reg_built_desc <- function(r, m) {
  w <- reg_built_row(r, m); nm <- r$predictors
  if (w$kind == "power") sprintf("%s is the %s of %s", nm[m], reg_power_word(w$p), nm[w$a])
  else sprintf("%s is %s \u00d7 %s", nm[m], nm[w$a], nm[w$b])
}

## the coefficient table's note on a group
reg_group_note <- function(r, g) {
  bs <- intersect(g, reg_bases(r)); built <- setdiff(g, bs); nm <- r$predictors
  if (length(bs) == 1) {
    words <- vapply(built, function(m) reg_power_word(reg_built_row(r, m)$p), "")
    sprintf("%s %s the %s of %s: %s describe one curve and cannot change separately, so %s alone is a change per unit.",
            join_and(nm[built]), pl(length(built), "is", "are"), join_and(words), nm[bs],
            if (length(built) == 1) "the two" else "with it they",
            if (length(built) == 1) "neither coefficient" else "no coefficient among them")
  } else sprintf(paste("%s: these terms describe one surface in %s and cannot change separately, so no coefficient among",
                       "them alone is a change per unit."),
                 cap1(join_and(vapply(built, function(m) reg_built_desc(r, m), ""))), join_and(nm[bs]))
}

## why a group's VIFs are large
reg_group_reason <- function(r, g) {
  bs <- intersect(g, reg_bases(r)); built <- setdiff(g, bs); nm <- r$predictors
  if (length(bs) > 1) return(sprintf("these terms being built from %s", join_and(nm[bs])))
  if (length(built) == 1) return(sprintf("%s being the %s of %s", nm[built], reg_power_word(reg_built_row(r, built)$p), nm[bs]))
  sprintf("these terms being powers of %s", nm[bs])
}

## The reading of a group: what its columns are, and the tests that mean
## something for it (the bend, an interaction, the group as a whole).
reg_group_text <- function(r, g) {
  alpha <- r$alpha; lvl <- paste0(pct(alpha), "%"); cf <- r$coefficients; nm <- r$predictors
  bs <- intersect(g, reg_bases(r)); built <- setdiff(g, bs)
  prods <- built[vapply(built, function(m) reg_built_row(r, m)$kind == "product", logical(1))]
  ## a significant RESET means the data bend in a way this curve misses, so a
  ## lower degree is not offered as an alternative
  missed <- !is.na(r$checks$p[3]) && r$checks$p[3] < alpha
  bend <- function(j) {
    pw <- reg_powers_of(r, j)
    if (!length(pw) || r$exact) return(NULL)
    top <- pw[length(pw)]; quad <- !is.na(reg_quad_of(r, j))
    inj <- if (length(prods)) paste(" in", nm[j]) else ""
    if (cf$p[top + 1] < alpha)
      sprintf("The coefficient of %s, %s, is significant at the %s level (%s): %s.", nm[top],
              if (quad) paste0("which measures the bend", inj) else paste("the highest power of", nm[j]),
              lvl, reg_p(cf$p[top + 1], alpha), if (quad) "the relation curves" else "the curve needs it")
    else sprintf("There is insufficient evidence at the %s level that %s (the coefficient of %s, %s)%s.", lvl,
                 if (quad) paste0("the relation bends", inj) else paste("the highest power of", nm[j], "is needed"),
                 nm[top], reg_p(cf$p[top + 1], alpha),
                 if (missed || length(prods)) "" else if (quad) sprintf(", so a straight line in %s may do as well", nm[j])
                 else ", so a curve of lower degree may do as well")
  }
  if (!length(prods)) {
    j <- bs[1]
    words <- vapply(built, function(m) reg_power_word(reg_built_row(r, m)$p), "")
    deg <- max(vapply(built, function(m) reg_built_row(r, m)$p, 1L))
    out <- c(if (length(built) == 1)
               sprintf(paste("%s is the %s of %s, so the two describe one curve and cannot change separately: neither",
                             "coefficient alone is the change for one unit more of %s."), nm[built], words, nm[j], nm[j])
             else sprintf(paste("%s are the %s of %s, so with it they describe one curve (a polynomial of degree %d) and",
                                "cannot change separately: no coefficient among them alone is the change for one unit more of %s."),
                          join_and(nm[built]), join_and(words), nm[j], deg, nm[j]),
             if (length(setdiff(seq_len(r$k), g))) "The other predictors raise or lower the curve without changing its shape.",
             if (!is.na(reg_quad_of(r, j))) reg_turn_text(r, j) else "The main plot draws the curve.",
             bend(j))
    return(paste(out, collapse = " "))
  }
  out <- sprintf(paste("%s describe one surface in %s: %s. The effect of each of %s therefore depends on the %s, and no",
                       "coefficient among these terms alone is a change per unit."),
                 join_and(nm[g]), join_and(nm[bs]), join_and(vapply(built, function(m) reg_built_desc(r, m), "")), join_and(nm[bs]),
                 if (length(bs) == 2) "other" else "others")
  if (!r$exact && length(g) < r$k) {
    pf <- reg_partial(r, g)
    out <- c(out, if (pf$p < alpha)
      sprintf("Together these terms are significant at the %s level (F = %s on %d and %d degrees of freedom, %s).",
              lvl, fmt(pf$F, 2), pf$df1, pf$df2, reg_p(pf$p, alpha))
      else sprintf("There is insufficient evidence at the %s level that these terms together are related to %s (F = %s on %d and %d degrees of freedom, %s).",
                   lvl, r$response, fmt(pf$F, 2), pf$df1, pf$df2, reg_p(pf$p, alpha)))
  }
  if (!r$exact) for (m in prods) {
    w <- reg_built_row(r, m)
    out <- c(out, if (cf$p[m + 1] < alpha)
      sprintf("The coefficient of %s, which measures how the effect of %s changes with %s, is significant at the %s level (%s).",
              nm[m], nm[w$a], nm[w$b], lvl, reg_p(cf$p[m + 1], alpha))
      else sprintf("There is insufficient evidence at the %s level that the effect of %s changes with %s (the coefficient of %s, %s).",
                   lvl, nm[w$a], nm[w$b], nm[m], reg_p(cf$p[m + 1], alpha)))
  }
  out <- c(out, unlist(lapply(bs, bend)),
           sprintf("The main plot draws the fitted %s for %s, with the other predictors held at their means.",
                   {
                     cv <- vapply(bs, function(j) length(reg_powers_of(r, j)) > 0, logical(1))
                     if (all(cv)) "curve" else if (!any(cv)) "line" else "line or curve"
                   },
                   if (length(bs) == 2) paste(nm[bs], collapse = " or ") else paste("any one of", join_and(nm[bs]))))
  paste(out, collapse = " ")
}

## The parts of the model with a test of their own: each predictor outside
## a group (its t test) and each group as a whole (its F test); the lower
## terms of a group have no meaning alone.
reg_units <- function(r) {
  gs <- reg_groups(r)
  singles <- setdiff(seq_len(r$k), unlist(gs))
  u <- c(lapply(singles, function(j) list(first = j, name = r$predictors[j], p = r$coefficients$p[j + 1])),
         lapply(gs, function(g) list(first = g[1], name = reg_group_name(r, g),
                                     p = if (length(g) == r$k) r$model$p else reg_partial(r, g)$p)))
  u[order(vapply(u, function(x) x$first, 1L))]
}


## "rises by 0.452", "falls by 1.20", or "changes by 0.000", from the
## estimate as printed
reg_move <- function(r, i) {
  s <- reg_coef_txt(r, i)
  if (as.numeric(s) > 0) paste("rises by", s) else if (as.numeric(s) < 0) paste("falls by", sub("^-", "", s))
  else paste("changes by", s)
}

## One slope in a sentence: its size and direction, its interval and its test.
reg_slope_text <- function(r, j) {
  i <- j + 1; cf <- r$coefficients; alpha <- r$alpha; lvl <- paste0(pct(alpha), "%")
  multi <- r$k >= 2
  base <- sprintf("For each unit more of %s, the predicted %s %s%s", r$predictors[j], r$response, reg_move(r, i),
                  if (multi) " when the other predictors are held at the same values" else "")
  if (r$exact) return(paste0(base, "."))
  test <- if (cf$p[i] < alpha)
    sprintf("; the slope is significant at the %s level (%s), with a %s%% confidence interval of %s to %s.", lvl,
            reg_p(cf$p[i], alpha), pct(1 - alpha), reg_coef_txt(r, i, "CI_lower"), reg_coef_txt(r, i, "CI_upper"))
  else sprintf("; there is insufficient evidence at the %s level that the slope differs from 0 (%s)%s.", lvl,
               reg_p(cf$p[i], alpha), if (multi) " once the other predictors are allowed for" else "")
  paste0(base, test)
}

## The reading of the whole model, sentence by sentence, each one checkable
## against the tables above it.
reg_text <- function(r) {
  m <- r$model; alpha <- r$alpha; lvl <- paste0(pct(alpha), "%"); multi <- r$k >= 2
  cf <- r$coefficients; nm <- r$predictors
  groups <- reg_groups(r); gcols <- unlist(groups); bases <- reg_bases(r)
  ## the whole model one curve or one surface
  one <- length(groups) == 1 && length(groups[[1]]) == r$k
  form <- if (one && length(bases) > 1) "surface" else "curve"
  out <- sprintf("The fitted equation is %s, from %d %s.", r$equation, r$n, pl(r$n, "row", "rows"))
  if (r$exact) {
    out <- c(out, paste("The equation passes through every point exactly, so there is no scatter left to judge it",
                        "against: R\u00b2 is 1, and the standard errors, tests and intervals are not available. An exact fit",
                        "usually means the response was worked out from the predictors, or there are hardly more rows than",
                        "coefficients."))
  } else {
    out <- c(out, sprintf("R\u00b2 is %s: %s for %s%% of the variation in %s about its mean%s.", fmt(m$R2, 3),
                          if (one) sprintf("the fitted %s accounts", form) else if (multi) "the predictors together account" else "the line accounts",
                          fmt(100 * m$R2, 1), r$response,
                          if (multi) sprintf(" (adjusted R\u00b2, which allows for the number of predictors, is %s%s)", dfmt(m$Adj_R2, 3),
                                             if (as.numeric(dfmt(m$Adj_R2, 3)) < 0)
                                               ", below 0: the predictors account for less of the variation than chance alone would give"
                                             else "") else ""))
    out <- c(out, if (m$p < alpha)
      sprintf("The regression is significant at the %s level (F = %s on %d and %d degrees of freedom, %s).", lvl,
              fmt(m$F, 2), m$df1, m$df2, reg_p(m$p, alpha))
      else if (one)
        sprintf("There is insufficient evidence at the %s level that %s is related to %s along the fitted %s (F = %s on %d and %d degrees of freedom, %s).",
                lvl, r$response, join_and(nm[bases]), form, fmt(m$F, 2), m$df1, m$df2, reg_p(m$p, alpha))
      else sprintf("There is insufficient evidence at the %s level of a %srelation between %s and %s (F = %s on %d and %d degrees of freedom, %s).",
                   lvl, if (!multi) "straight-line " else if (length(groups)) "" else "linear ", r$response,
                   if (multi) "the predictors taken together" else nm, fmt(m$F, 2), m$df1, m$df2, reg_p(m$p, alpha)))
    ## the F test and the slopes look only for straight lines
    cv <- r$checks[3, ]
    if (!is.na(cv$p) && cv$p < alpha)
      out <- c(out, if (length(groups))
        sprintf(paste("The check of the shape below is significant, though (%s): the data bend in a way the fitted",
                      "equation does not follow (such as a levelling off or an S-shape), so another shape may suit them better."),
                reg_p(cv$p, alpha))
        else sprintf(paste("The check for a curve below is significant, though (%s): the relation bends, which a",
                           "%s cannot show, so read the %s with care."),
                     reg_p(cv$p, alpha), if (multi) "linear equation" else "straight line", if (multi) "slopes" else "slope"))
  }
  ## an equation that needed many more figures than the standard errors to
  ## reproduce the fit: say why, and how to get one that is easier to read
  if (isTRUE(r$extra >= 2)) {
    far0 <- bases[vapply(bases, function(j) abs(mean(r$X[, j])) > 10 * stats::sd(r$X[, j]), logical(1))]
    far0 <- intersect(far0, unlist(groups))
    out <- c(out, paste0("The coefficients carry more figures than their standard errors need: the predictors are so alike",
      " that fewer figures would not reproduce the fitted values.",
      if (length(far0)) sprintf(paste(" Subtracting a starting value from %s (such as %s) before working out %s gives",
                                      "the same fit with coefficients that are easier to read."),
                                join_and(nm[far0]), join_and(vapply(far0, function(j) num_text(floor(min(r$X[, j]) / 10) * 10), "")),
                                pl(length(far0), "its powers", "their powers")) else ""))
  }
  ## each predictor outside a group in a sentence; each group read together
  out <- c(out, unlist(lapply(seq_len(r$k), function(j) {
    g <- reg_group(r, j)
    if (length(g) == 1) reg_slope_text(r, j) else if (j == g[1]) reg_group_text(r, g) else NULL
  })))
  ## A group's own columns are related by construction: their VIFs are no
  ## warning as far as the group explains them (up to twice the VIF among
  ## its own columns); beyond that the other predictors make its position
  ## imprecise.
  hv <- if (multi && !r$exact) which(is.finite(cf$VIF) & round(cf$VIF, 2) >= 5 | is.infinite(cf$VIF)) else integer(0)
  own <- rep(NA_real_, r$k + 1)
  for (g in groups) own[g + 1] <- tryCatch(diag(solve(stats::cor(r$X[, g]))), error = function(err) rep(Inf, length(g)))
  big <- which(is.finite(cf$VIF) & !is.na(own) & cf$VIF > 2 * own)
  raised <- intersect(hv, big)
  ## significant together, none alone; significant alone, not together. The
  ## parts are the predictors outside groups and the groups as a whole.
  units <- reg_units(r)
  if (length(units) >= 2 && !r$exact) {
    up <- vapply(units, function(u) u$p, 0); un <- vapply(units, function(u) u$name, "")
    if (m$p < alpha && all(up >= alpha)) {
      lead <- if (length(units) == 2) sprintf("neither %s nor %s is", un[1], un[2]) else sprintf("none of %s is", join_and(un))
      related <- any(round(cf$VIF[setdiff(seq_len(r$k), gcols) + 1], 2) >= 2, na.rm = TRUE) || length(intersect(big, gcols + 1)) > 0
      out <- c(out, paste0("The predictors are significant together, but ", lead, " significant on its own: ",
        if (related) "this can happen when the predictors are related to each other (see the VIF column), so that each one's share cannot be told apart."
        else "each falls short of significance by itself, while together they account for more of the variation than chance readily gives."))
    }
    if (m$p >= alpha && any(up < alpha)) {
      sg <- un[up < alpha]
      out <- c(out, sprintf(paste("%s %s significant on %s own although the model as a whole is not: with several predictors,",
                                  "one may reach significance by chance, which is what the overall F test guards against,",
                                  "so treat %s with care."),
                            cap1(join_and(sg)), pl(length(sg), "is", "are"), pl(length(sg), "its", "their"), pl(length(sg), "it", "them")))
    }
  }
  if (length(hv)) {
    for (g in groups) {
      ex <- setdiff(intersect(hv, g + 1), raised)
      if (length(ex))
        out <- c(out, sprintf("The large %s of %s %s from %s, and %s no warning about the %s.",
                              pl(length(ex), "VIF", "VIFs"), join_and(cf$Term[ex]), pl(length(ex), "comes", "come"),
                              reg_group_reason(r, g), pl(length(ex), "is", "are"),
                              if (length(intersect(g, bases)) > 1) "surface" else "curve"))
    }
    if (length(raised))
      out <- c(out, sprintf(paste("%s %s (%s) %s also raised by the other predictors, which are related to it, so the",
                                  "position of the curve is imprecise and can change markedly if a predictor is added or removed."),
                            if (length(raised) == 1) paste0(cf$Term[raised], "'s") else paste("The VIFs of", join_and(cf$Term[raised])),
                            if (length(raised) == 1) "VIF" else "", join_and(fmt(cf$VIF[raised], 2)), pl(length(raised), "is", "are")))
    hv <- setdiff(hv, gcols + 1)
    if (length(hv))
      out <- c(out, sprintf(paste("%s %s %s of 5 or more (%s): %s closely related to the other predictors, so %s",
                                  "imprecise and can change markedly if a predictor is added or removed."),
                            join_and(cf$Term[hv]), pl(length(hv), "has", "have"), pl(length(hv), "a VIF", "VIFs"),
                            join_and(ifelse(is.finite(cf$VIF[hv]), fmt(cf$VIF[hv], 2), "-")),
                            pl(length(hv), "it is", "they are"), pl(length(hv), "its slope is", "their slopes are")))
  }
  ## the intercept, and whether 0 lies inside the data, judged on the
  ## predictors themselves (a power or product is 0 when they are)
  outside <- bases[vapply(bases, function(j) min(r$X[, j]) > 0 || max(r$X[, j]) < 0, logical(1))]
  out <- c(out, sprintf("The intercept, %s, is the predicted %s when %s 0%s", reg_coef_txt(r, 1), r$response,
                        if (multi) "every predictor is" else paste(nm, "is"),
                        if (!length(outside)) "."
                        else sprintf(paste0("; but 0 lies outside the observed range of %s, so the intercept is an ",
                                            "extrapolation: it positions the %s rather than describing any row."),
                                     join_and(sprintf("%s (%s)", nm[outside], vapply(outside, function(j) reg_range(r, j), ""))),
                                     if (multi) "fitted equation" else "line")))
  out <- c(out, sprintf("The equation describes the data only over the observed %s (%s); beyond %s, %s is an assumption the data cannot check.",
                        pl(length(bases), "range", "ranges"),
                        join_and(sprintf("%s %s", nm[bases], vapply(bases, function(j) reg_range(r, j), ""))),
                        pl(length(bases), "it", "them"), if (multi) "the fitted relation" else "a straight-line relation"))
  left <- r$nrow - r$n
  if (left > 0) {
    out <- c(out, sprintf("%d of the %d rows %s left out because %s no usable value of %s.", left, r$nrow,
                          pl(left, "is", "are"), pl(left, "it has", "they have"),
                          if (multi) "the response or a predictor" else "one of the two variables"))
    if (r$held_rows > 0)
      out <- c(out, sprintf("Of these, %d %s an entry the data check has flagged (such as '%s') and will count once corrected on the Data tab.",
                            r$held_rows, pl(r$held_rows, "has", "have"), r$held_eg))
    if (r$inf_rows > 0)
      out <- c(out, sprintf("%s %d %s a value that is not a finite number (such as Inf, from a division by zero).",
                            if (r$held_rows > 0) "And" else "Of these,", r$inf_rows, pl(r$inf_rows, "holds", "hold")))
  }
  out <- c(out, sprintf(paste("A regression describes how %s goes with %s in these data; it can show a cause only when %s",
                              "set by the experimenter and assigned to the plots at random."),
                        r$response, join_and(nm[bases]),
                        pl(length(bases), "the predictor was", "the predictors were")))
  paste(out, collapse = " ")
}

## ------------------------------------------------------------- plots ----

## The line (or curve) for one predictor with the others at their means, its
## band at 1 - alpha, and each row's response adjusted to those means (the
## line's value plus the row's residual). Powers and products of the shown
## predictor move with it; those of the others are worked out from their
## means (75 for N gives N2 5625, a setting that can occur). With one
## predictor this is the ordinary fitted line and the observed points.
reg_effect <- function(r, j) {
  k <- r$k; mu <- colMeans(r$X)
  others <- setdiff(reg_bases(r), j)
  g <- seq(min(r$X[, j]), max(r$X[, j]), length.out = 101)
  Bg <- matrix(mu, length(g), k, byrow = TRUE); Bg[, j] <- g; Bg <- reg_design(r, Bg)
  Ba <- matrix(mu, r$n, k, byrow = TRUE); Ba[, j] <- r$X[, j]; Ba <- reg_design(r, Ba)
  pr <- reg_predict(r, Bg, TRUE)
  pts <- data.frame(x = r$X[, j], y = if (!length(others)) r$y else unname(reg_predict(r, Ba)) + unname(stats::residuals(r$fit)))
  list(band = data.frame(x = g, fit = pr[, 1], lwr = pr[, 2], upr = pr[, 3]), pts = pts,
       moving = c(reg_powers_of(r, j), reg_products_of(r, j)), others = others,
       held = reg_design(r, matrix(mu, 1, k))[1, ])
}

## The settings the other predictors are held at, as the note names them:
## "Rain 40.5", or "N 75 and Rain 40.5, so N2 5625". Each mean is printed to
## six figures and the powers and products worked out from the printed
## means, so the reader can check one against the other (Year 2010.5, so
## Year2 4042110).
reg_held_text <- function(r, ef) {
  zap <- reg_zap_means(r)
  shown <- vapply(zap, function(v) num_text(signif(v, 6)), "")
  hb <- setdiff(seq_len(r$k), c(reg_bases(r), ef$moving))
  hv <- reg_design(r, matrix(as.numeric(shown), 1, r$k))[1, hb]
  paste0(join_and(sprintf("%s %s", r$predictors[ef$others], shown[ef$others])),
         if (length(hb)) sprintf(", so %s", join_and(sprintf("%s %s", r$predictors[hb],
                                                              vapply(hv, function(v) num_text(signif(v, 7)), ""))))
         else "")
}

## the predictor shown in the main plot: the one asked for (a power or a
## product is shown through the predictor it is built from), else the first
reg_show <- function(r, show) {
  j <- if (!is.null(show) && show %in% r$predictors) match(show, r$predictors) else reg_bases(r)[1]
  reg_source(r, j)
}

plot_reg_main <- function(r, show = NULL) {
  j <- reg_show(r, show); ef <- reg_effect(r, j); adj <- length(ef$others) > 0
  xl <- r$predictors[j]
  pw <- reg_powers_of(r, j); pr <- reg_products_of(r, j); curve <- length(pw) > 0
  words <- vapply(pw, function(m) reg_power_word(reg_built_row(r, m)$p), "")
  what <- paste0(r$response, " against ", xl,
                 if (length(pw) && !length(pr)) sprintf(" (with %s, its %s)", join_and(r$predictors[pw]), join_and(words))
                 else if (length(c(pw, pr))) sprintf(" (with %s moving with it)", join_and(r$predictors[c(pw, pr)])) else "",
                 if (adj) ", other predictors at their means" else "")
  words <- reg_equation_terms(r)
  if (!r$exact) {
    words[length(words)] <- paste0(words[length(words)], ";")
    words <- c(words, paste("R\u00b2 =", fmt(r$model$R2, 3)))
  }
  sub <- wrap_words(words, 70)
  ggplot() +
    (if (!r$exact) geom_ribbon(data = ef$band, aes(x = .data$x, ymin = .data$lwr, ymax = .data$upr), fill = "#BBD3F2", alpha = 0.7)) +
    geom_point(data = ef$pts, aes(x = .data$x, y = .data$y), colour = "#3B7DD8", size = 2.4, alpha = 0.85) +
    geom_line(data = ef$band, aes(x = .data$x, y = .data$fit), colour = "#173F7D", linewidth = 1) +
    ## titles and caption wrapped, so a long name or equation is not cut off
    ## in a narrow panel
    labs(title = paste(strwrap(what, 50), collapse = "\n"),
         subtitle = sub, x = xl, y = if (adj) paste(r$response, "(adjusted)") else r$response,
         caption = if (!r$exact) paste(strwrap(sprintf("Shaded band: %s%% confidence interval for the average %s along the %s.",
                                         pct(1 - r$alpha), r$response, if (curve) "curve" else "line"), 60), collapse = "\n")) +
    theme_doe()
}

## Words joined into lines of at most `width` characters, each kept whole, so
## "R2 = 0.979" never breaks across lines.
wrap_words <- function(words, width) {
  lines <- character(0); cur <- ""
  for (w in words) {
    if (nzchar(cur) && nchar(cur) + 1 + nchar(w) > width) { lines <- c(lines, cur); cur <- w }
    else cur <- if (nzchar(cur)) paste(cur, w) else w
  }
  paste(c(lines, cur), collapse = "\n")
}

## standardised residuals: each residual over its own standard error; a row
## that fixes the fit on its own (leverage 1) has none
reg_std <- function(r) {
  s <- suppressWarnings(stats::rstandard(r$fit))
  s[!is.finite(s)] <- NA_real_
  s
}

## rows whose standardised residual lies beyond -3 or 3, with room for
## rounding: one at exactly 3 is not beyond it
reg_far <- function(s) which(!is.na(s) & abs(s) > 3 + 1e-8)

plot_reg_resid <- function(r) {
  if (r$exact) return(NULL)
  df <- data.frame(fit = stats::fitted(r$fit), std = reg_std(r), row = r$rows)
  df <- df[!is.na(df$std), , drop = FALSE]
  far <- df[reg_far(df$std), , drop = FALSE]
  ggplot(df, aes(x = .data$fit, y = .data$std)) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_hline(yintercept = c(-3, 3), linetype = 3, colour = "grey45") +
    geom_point(colour = "#3B7DD8", size = 2.2) +
    (if (nrow(far)) geom_text(data = far, aes(label = paste("row", .data$row), vjust = ifelse(.data$std > 0, -0.8, 1.8)),
                              size = 3.4, colour = "#C0392B")) +
    ## room above and below for the row labels
    scale_y_continuous(expand = expansion(mult = 0.12)) +
    labs(title = "Residuals against fitted values", x = paste("Fitted", r$response),
         y = "Standardised residual") +
    theme_doe()
}

plot_reg_qq <- function(r) {
  if (r$exact) return(NULL)
  df <- data.frame(std = reg_std(r)); df <- df[!is.na(df$std), , drop = FALSE]
  if (nrow(df) < 3) return(NULL)
  ggplot(df, aes(sample = .data$std)) + stat_qq(colour = "#3B7DD8") + stat_qq_line(colour = "#C0392B") +
    labs(title = "Normal Q-Q plot of the residuals", x = "Where the residuals would be if they were normal",
         y = "Standardised residual") +
    theme_doe()
}

plot_reg_obs <- function(r) {
  df <- data.frame(pred = stats::fitted(r$fit), obs = r$y)
  lim <- range(c(df$pred, df$obs))
  ggplot(df, aes(x = .data$pred, y = .data$obs)) +
    geom_abline(intercept = 0, slope = 1, linetype = 2, colour = "grey35") +
    geom_point(colour = "#3B7DD8", size = 2.2) +
    coord_cartesian(xlim = lim, ylim = lim) +
    labs(title = "Observed against predicted", x = paste("Predicted", r$response), y = paste("Observed", r$response),
         subtitle = if (r$exact) "Every point on the line" else sprintf("R\u00b2 = %s;  RMSE = %s", fmt(r$model$R2, 3), sig_fmt(r$model$RMSE)),
         caption = "Dashed line: observed = predicted.") +
    theme_doe()
}

## ------------------------------------------------------- plot notes ----

## rows that sit exactly on another row's point, hidden under it
reg_hidden <- function(x, y) {
  dup <- sum(duplicated(data.frame(x, y)))
  if (dup) sprintf(" %d %s exactly on another row's point and %s hidden under it.", dup,
                   pl(dup, "row sits", "rows sit"), pl(dup, "is", "are")) else ""
}

reg_main_text <- function(r, show = NULL) {
  j <- reg_show(r, show); ef <- reg_effect(r, j); adj <- length(ef$others) > 0
  nm <- r$predictors; cf <- r$coefficients; lvl <- paste0(pct(r$alpha), "%")
  pw <- reg_powers_of(r, j); pr <- reg_products_of(r, j)
  curve <- length(pw) > 0; shape <- if (curve) "curve" else "line"
  quad <- !is.na(reg_quad_of(r, j))
  band <- sprintf("the %s%% confidence interval for the average %s", pct(1 - r$alpha), r$response)
  ## "with Dose2 moving with it as its square", "with N2 moving with it as
  ## its square and NP as N x P"
  mv <- c(pw, pr)
  roles <- c(vapply(pw, function(m) paste("its", reg_power_word(reg_built_row(r, m)$p)), ""),
             vapply(pr, function(m) { w <- reg_built_row(r, m); sprintf("%s \u00d7 %s", nm[w$a], nm[w$b]) }, ""))
  moving <- if (!length(mv)) ""
            else sprintf(", with %s moving with it as %s%s,", nm[mv[1]], roles[1],
                         if (length(mv) > 1) paste0(" and ", join_and(sprintf("%s as %s", nm[mv[-1]], roles[-1]))) else "")
  what <- if (!adj)
    paste0(sprintf("Each point is a row. The %s is the fitted equation%s", shape, sub(",$", "", moving)),
           if (r$exact) "."
           else sprintf(paste(", and the shaded band is %s at each value of %s: it shows where the true %s could lie,",
                              "not where single rows will fall."), band, nm[j], shape))
  else sprintf(paste("The %s shows how the predicted %s changes with %s%s when the other predictors are held at their",
                     "means (%s). Each point is a row's %s adjusted to those means: the %s's value at that row's %s",
                     "plus the row's residual, so the points scatter about the %s exactly as the residuals do about",
                     "the full equation.%s"),
               shape, r$response, nm[j], moving, reg_held_text(r, ef), r$response, shape, nm[j], shape,
               if (r$exact) "" else sprintf(" The shaded band is %s along the %s.", band, shape))
  hidden <- reg_hidden(ef$pts$x, signif(ef$pts$y, 12))
  few <- if (r$n < 10 && !r$exact) sprintf(" With only %d rows, a single point can move the %s a great deal.", r$n, shape) else ""
  ## the tests of the products the shown predictor enters
  inter <- if (r$exact) "" else paste(vapply(pr, function(m) {
    w <- reg_built_row(r, m)
    if (cf$p[m + 1] < r$alpha)
      sprintf(" The coefficient of %s, which measures how the effect of %s changes with %s, is significant at the %s level (%s).",
              nm[m], nm[w$a], nm[w$b], lvl, reg_p(cf$p[m + 1], r$alpha))
    else sprintf(" There is insufficient evidence at the %s level that the effect of %s changes with %s (the coefficient of %s, %s).",
                 lvl, nm[w$a], nm[w$b], nm[m], reg_p(cf$p[m + 1], r$alpha))
  }, ""), collapse = "")
  if (curve) {
    top <- pw[length(pw)]
    reading <- paste0(
      if (quad) reg_turn_text(r, j, ef$held)
      else sprintf("The curve is the fitted polynomial of degree %d in %s%s.", reg_built_row(r, top)$p, nm[j],
                   if (length(pr)) ", drawn with the other predictors at their means" else ""),
      if (r$exact) "" else if (cf$p[top + 1] < r$alpha)
        sprintf(" The coefficient of %s, %s, is significant at the %s level (%s).", nm[top],
                if (quad) "which measures the bend" else "the highest power", lvl, reg_p(cf$p[top + 1], r$alpha))
      else sprintf(" There is insufficient evidence at the %s level that %s (the coefficient of %s, %s).", lvl,
                   if (quad) "the relation bends" else "the highest power is needed", nm[top], reg_p(cf$p[top + 1], r$alpha)),
      inter, hidden, few,
      if (r$exact) "" else " If the points stray from the curve in a pattern, or one point sits far from the rest at either end, the curve can mislead.")
    return(list(what = what, reading = reading))
  }
  i <- j + 1
  reading <- if (length(pr)) {
    ## the slope at the other predictors' means, through the products
    s_eff <- cf$Estimate[i]; oth <- integer(0)
    for (m in pr) {
      w <- reg_built_row(r, m); o <- if (w$a == j) w$b else w$a
      s_eff <- s_eff + cf$Estimate[m + 1] * ef$held[o]; oth <- c(oth, o)
    }
    paste0(sprintf("With the other predictors at their means, the slope is %s (%s per unit of %s); it changes with %s (through %s).",
                   dfmt(s_eff, r$dec[i]), r$response, nm[j], join_and(nm[unique(oth)]), join_and(nm[pr])), inter)
  } else {
    slope <- sprintf("The slope is %s (%s per unit of %s)", reg_coef_txt(r, i), r$response, nm[j])
    if (r$exact) paste0(slope, "; the line passes through every point.")
    else paste0(slope, if (cf$p[i] < r$alpha) sprintf(", significant at the %s level (%s).", lvl, reg_p(cf$p[i], r$alpha))
                else sprintf(", with insufficient evidence at the %s level that it differs from 0 (%s).", lvl, reg_p(cf$p[i], r$alpha)))
  }
  reading <- paste0(reading, hidden, few)
  if (!r$exact) {
    cv <- reg_curve(r, j)
    reading <- paste(reading,
      "If the points bend along a curve about the line, or one point sits far from the rest at either end, the straight line can mislead.",
      if (is.na(cv$p)) sprintf("A curve in %s cannot be tested: %s.", nm[j], cv$why)
      else if (cv$p < r$alpha)
        sprintf("Adding the square of %s to the model fits significantly better at the %s level (%s): the relation bends, so a curve may suit the data better.",
                nm[j], lvl, reg_p(cv$p, r$alpha))
      else sprintf("There is insufficient evidence at the %s level that adding the square of %s fits better than the straight line (%s).",
                   lvl, nm[j], reg_p(cv$p, r$alpha)))
  }
  list(what = what, reading = reading)
}

## the predictors' means for the main plot's note, with a mean that is only
## rounding residue (a centred column) shown as 0
reg_zap_means <- function(r) {
  mu <- colMeans(r$X)
  mu[abs(mu) <= 1e-9 * apply(abs(r$X), 2, max)] <- 0
  mu
}

## the verdict of one check as a sentence: `yes` and `no` hold "{p}" where
## the p-value goes
reg_check_name <- function(r, which)
  switch(which, "Normal residuals" = "Shapiro-Wilk", "Constant spread" = "Breusch-Pagan",
         "Straight line" = if (r$k == 1) "curve" else "RESET")
reg_check_clause <- function(r, which, yes, no) {
  ck <- r$checks[r$checks$Check == which, ]
  if (is.na(ck$p))
    return(sprintf("The %s test is not available: %s.", reg_check_name(r, which), sub("^not available: ", "", ck$Verdict)))
  sub("{p}", reg_p(ck$p, r$alpha), if (ck$p < r$alpha) yes else no, fixed = TRUE)
}

reg_resid_text <- function(r) {
  built <- NROW(r$built) > 0
  what <- paste("Each point is a row: its fitted value across, and its residual (observed minus fitted) divided by",
                sprintf("the residual's standard error up the plot. If %s suits the data, the points form an even",
                        if (built) "the fitted equation" else "a straight line"),
                "band about the dashed line at 0, with no curve and no funnel. The dotted lines mark -3 and 3.")
  if (r$exact) return(list(what = "", reading = "The model fits every value exactly, so every residual is 0 and there is nothing to plot."))
  lvl <- paste0(pct(r$alpha), "%")
  s <- reg_std(r)
  far <- reg_far(s)
  nolev <- sum(is.na(s))
  with_what <- if (r$k == 1) r$predictors else "the predictors"
  dfr <- r$fit$df.residual
  fv <- stats::fitted(r$fit)
  ## a flat fit stands every point in one column of this plot, so a bend or a
  ## widening shows only in the main plot
  flat <- stats::sd(fv) <= 1e-9 * stats::sd(r$y)
  look_bend <- if (flat) "look for the bend in the main plot, since every fitted value here is the same"
               else "look for the points to bend in a U or an arch"
  look_wide <- if (flat) "look for the band of points to widen along the main plot, since every fitted value here is the same"
               else if (r$k == 1) "look for the band of points to widen towards one side"
               else if (built && length(reg_bases(r)) == 1)
                 "look for the band of points to widen along the main plot, where they scatter about the curve by the residuals"
               else if (built)
                 paste("the change need not show against the fitted values here, so choose each of",
                       join_and(r$predictors[reg_bases(r)]), "in turn on the main plot, where the points scatter about",
                       "the fitted line or curve by the residuals, and look for the band to widen along one")
               else paste("the change need not show against the fitted values here, so choose each predictor in turn on the main",
                          "plot, where the points scatter about the line by the residuals, and look for the band to widen along one")
  out <- c(
    if (r$k == 1) reg_check_clause(r, "Straight line",
      paste0("The check for a curve finds that adding the square of ", r$predictors, " fits significantly better than the straight line at the ",
             lvl, " level ({p}): ", look_bend, "; a curve may suit the data better."),
      paste0("The check for a curve finds insufficient evidence at the ", lvl, " level that adding the square of ",
             r$predictors, " fits better than the straight line ({p})."))
    else if (built) reg_check_clause(r, "Straight line",
      paste0("The RESET test finds a pattern along the fitted values that the fitted equation does not follow, significant at the ",
             lvl, " level ({p}): ", look_bend, "; another shape may suit the data better."),
      paste0("The RESET test finds insufficient evidence at the ", lvl,
             " level of a pattern along the fitted values that the fitted equation does not follow ({p})."))
    else reg_check_clause(r, "Straight line",
      paste0("The RESET test finds a curve along the fitted values that fits significantly better than the straight line at the ",
             lvl, " level ({p}): ", look_bend, "; a curve may suit the data better."),
      paste0("The RESET test finds insufficient evidence at the ", lvl, " level of a curve along the fitted values ({p}).")),
    reg_check_clause(r, "Constant spread",
      paste0("The Breusch-Pagan test finds that the spread of the residuals changes with ", with_what, ", significant at the ",
             lvl, " level ({p}): ", look_wide, ". The tests and intervals above are then less trustworthy, and a transformation such as a log often helps."),
      paste0("The Breusch-Pagan test finds insufficient evidence at the ", lvl, " level that the spread of the residuals changes with ",
             with_what, " ({p}).")),
    if (length(far)) paste0(sprintf("%s %s more than three standard errors from 0, which normal residuals seldom are; check %s for a recording error.",
                                    cap1(rows_text(r$rows[far])), pl(length(far), "lies", "lie"), pl(length(far), "it", "them")),
      ## among many rows a few go beyond 3 by chance alone
      if (r$n >= 100) sprintf(" Among %d rows, about %s would lie beyond 3 by chance even if every residual were normal.",
                              r$n, fmt(2 * stats::pnorm(-3) * r$n, 1)) else "")
    ## a standardised residual cannot exceed the square root of the residual
    ## degrees of freedom, so with nine or fewer the lines can flag nothing
    else if (dfr <= 9)
      sprintf(paste("With %d residual %s of freedom, no standardised residual can exceed 3, so the dotted lines cannot",
                    "pick out a stray point here; look instead for one point well apart from the rest."),
              dfr, pl(dfr, "degree", "degrees"))
    else "No point lies beyond -3 or 3.",
    if (nolev) sprintf("%s %s the fit on %s own (%s alone decides a coefficient), so %s no residual to plot.",
                       cap1(rows_text(r$rows[is.na(s)])), pl(nolev, "fixes", "fix"), pl(nolev, "its", "their"),
                       pl(nolev, "it", "each"), pl(nolev, "it has", "they have")))
  list(what = what, reading = paste(out, collapse = " "))
}

reg_qq_text <- function(r) {
  what <- paste("Each point is a residual, placed against where it would fall if the residuals followed a normal",
                "distribution. Points close to the straight line mean the residuals look normal, as the t tests and",
                "intervals assume.")
  if (r$exact) return(list(what = "", reading = "The model fits every value exactly, so there are no residuals to plot."))
  s <- reg_std(r); s <- s[!is.na(s)]
  e <- stats::residuals(r$fit)
  pattern <- if (r$fit$df.residual < 2) "one"
             else if (length(s) < 5) NA
             else if (diff(stats::quantile(s, c(0.25, 0.75), names = FALSE)) == 0) "heaped"
             else if (qq_coarse(e)) "coarse" else qq_pattern(s)
  plot_read <- if (identical(pattern, "one"))
      paste("With one residual degree of freedom every standardised residual is +1 or -1, so the plot cannot show",
            "the shape of the scatter.")
    else if (is.na(pattern)) "There are too few points to judge."
    else if (pattern == "heaped") "At least half the residuals are the same value, so many points sit in one flat run."
    else if (pattern == "coarse") paste("The residuals sit on a coarse grid, as happens when the response is recorded in large steps,",
                                        "so the points form flat runs and the ends of this plot cannot be read reliably.")
    else qq_reading(s)
  plot_read <- gsub("values", "residuals", plot_read, fixed = TRUE)
  ## whether the plot's ends were read at all
  read <- !is.na(pattern) && !pattern %in% c("heaped", "coarse", "one")
  ## A residual the residual plot flags sits far out at one end of this plot;
  ## the reading of the ends averages the outer tenth, which one point moves
  ## little, so the point is named rather than left to contradict it.
  sall <- reg_std(r); far <- reg_far(sall)
  if (read && length(far)) {
    up <- sall[far] > 0
    at_end <- if (all(up)) "far above the line at the top end" else if (!any(up)) "far below the line at the bottom end"
              else "far from the line at the two ends"
    lead <- sprintf("%s %s %s, as the residual plot flags.", cap1(rows_text(r$rows[far])), pl(length(far), "sits", "sit"), at_end)
    plot_read <- if (identical(pattern, "ok ok"))
      paste(lead, sprintf("Apart from %s, %s", pl(length(far), "it", "them"),
                          sub("^Neither", "neither", plot_read)))
    else paste(lead, plot_read)
  }
  lvl <- paste0(pct(r$alpha), "%")
  test <- reg_check_clause(r, "Normal residuals",
    paste0("The Shapiro-Wilk test finds a significant departure from normal at the ", lvl, " level ({p})."),
    paste0("The Shapiro-Wilk test finds insufficient evidence at the ", lvl, " level that the residuals are not normal ({p})."))
  ck <- r$checks[1, ]
  ## the plot's ends and the test can disagree; say why rather than leave both
  calm <- identical(pattern, "ok ok")
  clash <- !is.na(ck$p) && read && ((ck$p < r$alpha && calm) || (ck$p >= r$alpha && !calm))
  out <- c(plot_read, test,
           if (clash) {
             if (length(far)) sprintf(paste("The two need not agree: the test weighs every point, %s among them, while the reading",
                                            "of the plot looks at how far the outer tenth of the points at each end bends on average,",
                                            "which one point moves little."), rows_text(r$rows[far]))
             else paste("The two need not agree: the test weighs every point, while the reading of the plot looks at the outer",
                        "tenth of the points at each end.")
           },
           if (read && length(s) < 20) sprintf("With only %d residuals, a Q-Q plot can show only a large departure from normal.", length(s)),
           if (r$fit$df.residual >= 30 && max(stats::hatvalues(r$fit)) <= 0.2)
             "With this many rows, the tests and intervals for the coefficients are little affected by a moderate departure.")
  list(what = what, reading = paste(out, collapse = " "))
}

reg_obs_text <- function(r) {
  what <- paste("Each point is a row: its predicted value across and its observed value up. Points on the dashed line",
                "are predicted exactly; the closer the points lie to it, the better the fit.")
  if (r$exact) return(list(what = what, reading = "Every point lies on the line: the model fits every value exactly."))
  if (stats::sd(stats::fitted(r$fit)) <= 1e-9 * stats::sd(r$y))
    return(list(what = what, reading = sprintf(paste("Every predicted value is the same (the fitted equation is flat), so the",
      "points stand in one column: the equation accounts for none of the variation in %s (R\u00b2 %s)."), r$response, fmt(r$model$R2, 3))))
  reading <- sprintf(paste("R\u00b2 (%s) is the square of the correlation between the observed and the predicted values.",
                           "The points lie on average %s from the line, vertically (MAE); RMSE (%s) estimates the",
                           "standard deviation of the scatter about the true equation.%s"),
                     fmt(r$model$R2, 3), sig_fmt(r$model$MAE), sig_fmt(r$model$RMSE),
                     reg_hidden(signif(stats::fitted(r$fit), 12), r$y))
  list(what = what, reading = reading)
}
