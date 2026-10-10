###############################################################################
##  CORRELATION
###############################################################################
## Pearson's, Spearman's or Kendall's coefficient for every pair of the chosen
## numeric variables, each with N, its p-value, a significance mark at the
## user's level and a short reading; a matrix, a heat map, and a scatter plot
## of one pair. As with the descriptives, every sentence is one the numbers on
## screen bear out: strength and direction are judged on the coefficient as
## printed, a result short of significance is "insufficient evidence", never
## "no correlation", nothing is said about cause, and a p-value that is only
## an approximation is marked as one.

COR_NAMES  <- c(pearson = "Pearson's r", spearman = "Spearman's rho", kendall = "Kendall's tau")
COR_SYMBOL <- c(pearson = "r", spearman = "rho", kendall = "tau")

## Where an association is called weak, moderate or strong. For Pearson's r
## these are Cohen's (1988) 0.1, 0.3 and 0.5. Spearman's rho and Kendall's tau
## run on other scales for the same association: for bivariate normal data
## rho = (6 / pi) asin(r / 2) and tau = (2 / pi) asin(r), which carry Cohen's
## points to 0.10, 0.29, 0.48 for rho and 0.06, 0.19, 0.33 for tau (rounded to
## the two decimals the coefficients are printed to).
COR_CUTS <- list(pearson = c(0.10, 0.30, 0.50), spearman = c(0.10, 0.29, 0.48),
                 kendall = c(0.06, 0.19, 0.33))

## the bands as a reader checks them against printed values, without overlap
cor_bands <- function(method) {
  k <- COR_CUTS[[method]]; f <- function(z) fmt(z, 2)
  sprintf("under %s negligible, %s to %s weak, %s to %s moderate, %s or more strong",
          f(k[1]), f(k[1]), f(k[2] - 0.01), f(k[2]), f(k[3] - 0.01), f(k[3]))
}

## a coefficient as the tables print it (two decimals), NA where there is none
printed2 <- function(x) {
  out <- rep(NA_real_, length(x)); ok <- !is.na(x)
  out[ok] <- as.numeric(dfmt(x[ok], 2))
  out
}

## the strength of a coefficient, judged on its value as printed
cor_strength <- function(coef, method) {
  a <- abs(printed2(coef)); k <- COR_CUTS[[method]]
  ifelse(is.na(a), NA_character_, ifelse(a < k[1], "negligible", ifelse(a < k[2], "weak",
         ifelse(a < k[3], "moderate", "strong"))))
}

## ------------------------------------------------- rank p-values ----
## Spearman's and Kendall's p-values. If the two variables are unrelated,
## every ordering of one variable's values against the other's is equally
## likely, so the p-value is the share of orderings that give a coefficient at
## least as far from 0 as the one observed. That holds with tied values too,
## where the large-sample approximations cor.test() falls back on can be far
## off: three tied rows rising together came out "significant at 1%" (the
## exact p is 0.33), and ten plots of mostly-zero counts with one shared
## non-zero plot came out "**" (the exact p is 0.10).
##
## So the p-value is exact whenever the distinct orderings of one of the two
## variables can be listed (ties make them few: twelve plots with ten zeros
## have 132), estimated from random orderings when the values have ties but
## too many orderings to list, and taken from cor.test() otherwise: exact
## without ties where it is (Kendall below 50 rows, Spearman at 9), a
## large-sample approximation beyond, which is accurate at those sizes.

## at most this many orderings are listed, and at most this many cells held
COR_EXACT_MAX <- 50000
COR_CELLS_MAX <- 4e6
## random orderings for an estimate; a fixed seed keeps it the same each time.
## An estimate within three standard errors of a significance threshold is
## worked out again from ten times as many, so that a mark near the
## threshold rests on a p-value good to about +/- 0.0005 rather than 0.0015.
COR_PERM_B <- 20000
COR_PERM_B_CLOSE <- 200000
COR_PERM_SEED <- 20261009

## the number of distinct orderings of a variable's values
n_orders <- function(v) {
  cnt <- tabulate(match(v, unique(v)))
  exp(lfactorial(length(v)) - sum(lfactorial(cnt)))
}

## Every distinct ordering of a variable's values, one per row, as indices
## into its sorted distinct values (so the indices order the rows exactly as
## the values do): positions are chosen for the first value, then for the
## next among those left, and so on. Every partial ordering has the same
## number of free positions at each step, so each step is one matrix
## operation over all of them.
distinct_orders <- function(v) {
  u <- sort(unique(v)); cnt <- tabulate(match(v, u), length(u)); n <- length(v)
  A <- matrix(0L, 1, n)
  for (k in seq_len(length(u) - 1)) {
    R <- nrow(A)
    free <- matrix(((which(t(A == 0L)) - 1L) %% n) + 1L, nrow = R, byrow = TRUE)
    cmb <- utils::combn(ncol(free), cnt[k])
    J <- ncol(cmb)
    r_of <- rep(seq_len(R), each = J); j_of <- rep(seq_len(J), times = R)
    cols <- free[cbind(rep(r_of, each = cnt[k]), as.vector(cmb[, j_of]))]
    A <- A[r_of, , drop = FALSE]
    A[cbind(rep(seq_len(R * J), each = cnt[k]), cols)] <- k
  }
  A[A == 0L] <- length(u)
  A
}

## Without ties a variable's terms depend only on the number of rows, so they
## are built once for each size and coefficient and shared by every variable.
untied_cache <- new.env(parent = emptyenv())

## the upper-triangle pairs (i < j), in the order sx[upper.tri(sx)] lists them
upper_pairs <- function(n) which(upper.tri(diag(n)), arr.ind = TRUE)

## For one variable `v` and one coefficient, the statistic's ingredient for
## every ordering (rows of `orders`, indices as distinct_orders() gives them):
## centred ranks for rho, the signs of every pairwise difference for tau.
order_terms <- function(v, orders, method) {
  u <- sort(unique(v))
  if (method == "spearman") {
    rk <- rank(v)[match(u, v)]
    matrix(rk[orders], nrow(orders)) - mean(rank(v))
  } else {
    ij <- upper_pairs(ncol(orders))
    sign(matrix(orders[, ij[, 1]], nrow(orders)) - matrix(orders[, ij[, 2]], nrow(orders)))
  }
}

## the fixed variable's side of the statistic, and the observed value
fixed_terms <- function(x, method) {
  if (method == "spearman") rank(x) - mean(rank(x))
  else { s <- sign(outer(x, x, "-")); s[upper.tri(s)] }
}

## Run `expr` with a fixed seed and put the user's random number stream back
## afterwards, so an estimate is the same every time and nothing else changes.
## The generator is fixed as well as the seed (R's defaults), so a user who
## has chosen another kind of generator gets the same answer; the saved
## stream carries the user's kind back with it. One thing R code cannot put
## back: under Box-Muller normals, the second of a pair drawn but not yet
## used is lost, so the user's next normal draw moves on by one.
with_fixed_seed <- function(seed, expr) {
  had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old <- if (had) get(".Random.seed", envir = globalenv(), inherits = FALSE)
  kinds <- RNGkind()
  on.exit(if (had) assign(".Random.seed", old, envir = globalenv()) else {
    RNGkind(kinds[1], kinds[2], kinds[3])
    if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv())
  })
  set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  expr
}

## The p-value of rho or tau for one pair and how it was found: "exact" (every
## ordering listed, or cor.test() exact), "estimate" (random orderings) or
## "approximation" (large-sample). `cache` keeps each variable's terms within
## one table, since every pair a variable is in reuses them.
rank_p <- function(x, y, method, cache = NULL, keys = c("x", "y"), alpha = NULL) {
  n <- length(x)
  ties <- anyDuplicated(x) > 0 || anyDuplicated(y) > 0
  width <- if (method == "spearman") n else choose(n, 2)
  m <- c(n_orders(x), n_orders(y))
  ## permute the variable with fewer orderings; list them if few enough
  k <- which.min(m)
  if (m[k] <= COR_EXACT_MAX && m[k] * width <= COR_CELLS_MAX) {
    v <- if (k == 1) x else y; w <- if (k == 1) y else x
    untied <- !anyDuplicated(v)
    store <- if (untied) untied_cache else cache
    key <- if (untied) paste(n, method) else paste(keys[k], method, sep = "\u001f")
    terms <- if (!is.null(store) && !is.null(store[[key]])) store[[key]] else {
      t <- order_terms(if (untied) seq_len(n) else v, distinct_orders(if (untied) seq_len(n) else v), method)
      if (!is.null(store)) assign(key, t, envir = store)
      t
    }
    ## without ties the terms were built from ranks 1..n, so the variable
    ## being ordered enters through its ranks
    if (untied) v <- rank(v)
    a <- fixed_terms(w, method)
    obs <- sum(fixed_terms(v, method) * a)
    s <- drop(terms %*% a)
    return(list(p = mean(abs(s) >= abs(obs) - 1e-9 * max(1, abs(obs))), how = "exact"))
  }
  if (!ties) {
    exact <- (method == "spearman" && n <= 9) || (method == "kendall" && n < 50)
    p <- suppressWarnings(stats::cor.test(x, y, method = method, exact = if (exact) TRUE else NULL))$p.value
    return(list(p = p, how = if (exact) "exact" else "approximation"))
  }
  ## ties, but too many orderings to list: an estimate from random orderings
  ## (Spearman's rho at any size, Kendall's tau up to 20 rows; from 21 rows
  ## Kendall's large-sample approximation held its level in simulation, at
  ## most 5.7% at 5% and 1.2% at 1% for sparse counts and three-level
  ## scores, and the estimate would cost n^2 work per ordering), as
  ## (k + 1) / (B + 1), which keeps the test's level at or below alpha; on any
  ## one data set the estimate falls either side of the exact value, so one
  ## near a threshold is worked out again more closely
  if (method == "spearman" || n <= 20) {
    p <- perm_estimate(x, y, method, COR_PERM_B, cache, keys[1])
    se <- sqrt(p * (1 - p) / COR_PERM_B)
    if (!is.null(alpha) && any(abs(p - c(alpha, alpha / 5)) < 3 * se))
      p <- perm_estimate(x, y, method, COR_PERM_B_CLOSE, cache, keys[1])
    return(list(p = p, how = "estimate"))
  }
  p <- suppressWarnings(stats::cor.test(x, y, method = method, exact = FALSE))$p.value
  list(p = p, how = "approximation")
}

## B random orderings of 1..n under the fixed seed, one per row, drawn by
## sorting uniform numbers within each row: one sort for the whole block.
## Blocks within the usual count are the same for every pair of the same
## size, so they are drawn once and kept: for one size at a time, and only
## while they fit in COR_ORDERS_MAX cells, which bounds the memory held.
## Blocks beyond the usual count (a closer count) are new each time.
COR_ORDERS_MAX <- 1e7
perm_cache <- new.env(parent = emptyenv())
random_orders <- function(n, B, first = 1) {
  keep <- first + B - 1 <= COR_PERM_B && n * COR_PERM_B <= COR_ORDERS_MAX
  key <- paste(B, first)
  if (keep && isTRUE(perm_cache$n == n) && !is.null(perm_cache[[key]])) return(perm_cache[[key]])
  o <- with_fixed_seed(COR_PERM_SEED + first, order(rep(seq_len(B), each = n), stats::runif(n * B)))
  P <- matrix(o - rep((seq_len(B) - 1L) * as.integer(n), each = n), nrow = B, byrow = TRUE)
  if (keep) {
    if (!isTRUE(perm_cache$n == n)) {
      rm(list = ls(perm_cache), envir = perm_cache)
      perm_cache$n <- n
    }
    assign(key, P, envir = perm_cache)
  }
  P
}

## The p-value from B random orderings of x against y, as (k + 1) / (B + 1):
## x's terms for every ordering (as order_terms() builds them for the exact
## count) times y's fixed terms. The orderings come in blocks of at most
## COR_CELLS_MAX cells. In a table x stays the same over consecutive pairs,
## so its terms for the usual first block are kept for the next pair (one
## variable at a time, which bounds the memory held).
perm_estimate <- function(x, y, method, B, cache = NULL, key = NULL) {
  n <- length(x)
  ix <- match(x, sort(unique(x)))
  a <- fixed_terms(y, method)
  obs <- sum(fixed_terms(x, method) * a)
  tol <- 1e-9 * max(1, abs(obs))
  width <- if (method == "spearman") n else choose(n, 2)
  block <- min(B, COR_PERM_B, max(1, floor(COR_CELLS_MAX / width)))
  tk <- paste(key, method, sep = "\u001f")
  hits <- 0; done <- 0
  while (done < B) {
    b <- min(block, B - done)
    keep <- !is.null(cache) && !is.null(key) && done == 0 && b == COR_PERM_B
    terms <- if (keep && identical(cache$perm_key, tk)) cache$perm_terms else {
      t <- order_terms(x, matrix(ix[random_orders(n, b, first = done + 1)], b), method)
      if (keep) { cache$perm_key <- tk; cache$perm_terms <- t }
      t
    }
    s <- drop(terms %*% a)
    hits <- hits + sum(abs(s) >= abs(obs) - tol)
    done <- done + b
  }
  (hits + 1) / (B + 1)
}

## One pair: the coefficient, its p-value, how the p-value was found and (for
## Pearson) the confidence interval at the level that matches alpha, from
## every row with both values.
cor_pair <- function(x, y, method, alpha, xname = "the first variable", yname = "the second variable",
                     cache = NULL) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]; n <- length(x)
  out <- list(N = n, coef = NA_real_, p = NA_real_, low = NA_real_, high = NA_real_,
              how = NA_character_, why = NULL)
  if (n < 3) {
    out$why <- if (n == 0) "no row has both values" else sprintf("only %d %s both values", n, pl(n, "row has", "rows have"))
    return(out)
  }
  flat <- c(xname, yname)[c(stats::sd(x) == 0, stats::sd(y) == 0)]
  if (length(flat)) {
    out$why <- sprintf("%s %s not vary in the rows that have both values",
                       join_and(sprintf("'%s'", flat)), pl(length(flat), "does", "do"))
    return(out)
  }
  if (method == "pearson") {
    ct <- stats::cor.test(x, y, conf.level = 1 - alpha)
    out$coef <- unname(ct$estimate); out$p <- ct$p.value; out$how <- "exact"
    if (!is.null(ct$conf.int)) { out$low <- ct$conf.int[1]; out$high <- ct$conf.int[2] }
    return(out)
  }
  out$coef <- stats::cor(x, y, method = method)
  rows <- paste(which(ok), collapse = ",")
  rp <- rank_p(x, y, method, cache, keys = paste(c(xname, yname), rows, sep = "\u001f"), alpha = alpha)
  out$p <- rp$p; out$how <- rp$how
  out
}

## the reading of one pair, in a line; the direction comes from the sign of
## the coefficient, so a significant 0.00 still says which way it leans
cor_reading <- function(coef, p, strength, alpha, why = NULL) {
  if (length(why) && nzchar(why)) return(paste0("Not available: ", why, "."))
  dir <- if (coef > 0) "positive" else if (coef < 0) "negative" else ""
  lvl <- paste0(pct(alpha), "%")
  if (p < alpha) {
    if (strength == "negligible")
      sprintf(paste("Negligible %s association, though significant at the %s level: with many rows even a very",
                    "small coefficient can be significant."), dir, lvl)
    else sprintf("%s %s association, significant at the %s level.", cap1(strength), dir, lvl)
  } else sprintf("%s; insufficient evidence of an association at the %s level.",
                 if (printed2(coef) != 0) sprintf("%s %s coefficient", cap1(strength), dir) else "A coefficient of 0.00", lvl)
}

## Rows left out of a pair only because an entry is written in a form the
## data check flags (36,2; 45%): they would count once corrected.
held_flags <- function(raw)
  if (is.numeric(raw)) rep(FALSE, length(raw)) else read_entries(raw)$kind %in% setdiff(NUMBER_KINDS, "number")

#' Correlation between pairs of numeric variables
#'
#' For every pair of the variables named: the number of rows with both values
#' (N), the coefficient (Pearson's r, Spearman's rho or Kendall's tau-b), its
#' two-sided p-value, a significance mark at the chosen level (\code{*} at
#' \code{alpha}, \code{**} at \code{alpha / 5}, \code{NS} otherwise), for
#' Pearson's r a confidence interval at level \code{1 - alpha} (Fisher's
#' approximation), the strength of the association, and a one-line reading.
#' Text entries are read as the data check reads them: an entry that is not
#' plainly a number counts as missing; see \code{\link{check_data}}.
#'
#' Pearson's p-value is the usual t test. Spearman's and Kendall's are exact
#' whenever the distinct orderings of one variable's values can be listed (up
#' to 50,000, which covers eight rows and many more when values are tied);
#' with ties and more orderings than that, Spearman's at any size and
#' Kendall's up to 20 rows are estimated from 20,000 random orderings
#' (200,000 near a significance threshold; a fixed seed, and the user's
#' random number stream is left as it was), and Kendall's above 20 rows use
#' the large-sample approximation, which holds its level there; without ties
#' they come from \code{\link[stats]{cor.test}}, exact for Kendall's tau below
#' 50 rows and Spearman's rho at nine, and a large-sample approximation
#' beyond.
#' Strength uses Cohen's 0.1, 0.3 and 0.5 for r, carried to rho and tau
#' through their relation to r for bivariate normal data (0.10, 0.29, 0.48 for
#' rho; 0.06, 0.19, 0.33 for tau).
#'
#' @param d A data frame.
#' @param vars The names of two or more numeric columns.
#' @param method \code{"pearson"}, \code{"spearman"} or \code{"kendall"}.
#' @param alpha The significance level, between 0 and 0.5.
#'
#' @return A data frame with one row per pair: \code{Variable1},
#'   \code{Variable2}, \code{N}, \code{Coefficient}, \code{p}, \code{Mark},
#'   \code{CI_lower} and \code{CI_upper} (Pearson only; \code{NA} otherwise),
#'   \code{Strength}, \code{P_method} (\code{"exact"}, \code{"estimate"} from
#'   random orderings, or \code{"approximation"}), \code{Approximate}
#'   (\code{TRUE} unless exact), \code{Held} (rows left out only because an
#'   entry waits for a data-check correction) and \code{Reading}. A pair that cannot be
#'   computed has \code{NA} coefficient and p-value and says why in
#'   \code{Reading}.
#'
#' @examples
#' d <- demo_data("RCBD")
#' set.seed(1)
#' d$Height <- 60 + 0.8 * d$Yield + rnorm(nrow(d), 0, 2)
#' correlate_data(d, c("Yield", "Height"))
#' correlate_data(d, c("Yield", "Height"), method = "spearman")
#'
#' @export
correlate_data <- function(d, vars, method = c("pearson", "spearman", "kendall"), alpha = 0.05) {
  method <- match.arg(method)
  if (!is.numeric(alpha) || length(alpha) != 1L || is.na(alpha) || alpha <= 0 || alpha >= 0.5)
    stop("The significance level must be a single number between 0 and 0.5, such as 0.05.")
  vars <- unique(vars)
  if (length(vars) < 2) stop("Choose at least two variables to correlate.")
  gone <- setdiff(vars, names(d))
  if (length(gone))
    stop(sprintf("%s %s of the data.", join_and(sprintf("'%s'", gone)),
                 pl(length(gone), "is not a column", "are not columns")))
  vals <- lapply(stats::setNames(vars, vars), function(v) response_values(d[[v]]))
  held <- lapply(stats::setNames(vars, vars), function(v) held_flags(d[[v]]))
  pr <- utils::combn(length(vars), 2)
  cache <- new.env(parent = emptyenv())
  gained <- rep(FALSE, nrow(d))
  out <- do.call(rbind, lapply(seq_len(ncol(pr)), function(k) {
    a <- vars[pr[1, k]]; b <- vars[pr[2, k]]
    cp <- cor_pair(vals[[a]], vals[[b]], method, alpha, a, b, cache)
    ## rows that would have both values once flagged entries are corrected
    has_a <- is.finite(vals[[a]]) | held[[a]]; has_b <- is.finite(vals[[b]]) | held[[b]]
    h <- which((held[[a]] | held[[b]]) & has_a & has_b)
    gained[h] <<- TRUE
    eg <- if (length(h)) trimws(as.character(if (held[[a]][h[1]]) d[[a]][h[1]] else d[[b]][h[1]])) else ""
    why <- cp$why
    if (length(h) && cp$N < 3)
      why <- sprintf("%s as plain numbers, and %d %s an entry the data check has flagged (such as '%s'), which will count once corrected on the Data tab",
                     why, length(h), pl(length(h), "row has", "rows have"), eg)
    st <- cor_strength(cp$coef, method)
    reading <- cor_reading(cp$coef, cp$p, st, alpha, why)
    if (length(h) && cp$N >= 3)
      reading <- paste(reading, sprintf("%d %s an entry the data check has flagged (such as '%s') and %s left out until %s corrected on the Data tab.",
        length(h), pl(length(h), "row has", "rows have"), eg, pl(length(h), "is", "are"), pl(length(h), "it is", "they are")))
    ## Pearson's interval is Fisher's approximation and can disagree with the
    ## t test near the boundary; the reading says so where it does
    if (method == "pearson" && !is.na(cp$low) && (cp$p < alpha) != (cp$low > 0 || cp$high < 0))
      reading <- paste(reading, "Near the boundary the interval, an approximation, and the test disagree; the mark follows the test.")
    data.frame(Variable1 = a, Variable2 = b, N = cp$N, Coefficient = cp$coef, p = cp$p,
               Mark = star(cp$p, alpha), CI_lower = cp$low, CI_upper = cp$high,
               Strength = st, P_method = cp$how, Approximate = !is.na(cp$how) & cp$how != "exact",
               Held = length(h),
               Reading = reading, stringsAsFactors = FALSE)
  }))
  rownames(out) <- NULL
  attr(out, "method") <- method; attr(out, "alpha") <- alpha
  ## variables with fewer than two distinct values have no correlation even
  ## with themselves
  attr(out, "flat") <- vars[vapply(vals, function(v) length(unique(v[is.finite(v)])) < 2, logical(1))]
  ## rows that some pair would gain once a flagged entry is corrected, and one
  ## such entry to show
  attr(out, "held_rows") <- sum(gained)
  attr(out, "held_eg") <- if (any(gained)) {
    i <- which(gained)[1]; v <- vars[vapply(held, function(h) h[i], logical(1))][1]
    trimws(as.character(d[[v]][i]))
  } else ""
  out
}

## ------------------------------------------------------------ display ----

## a coefficient with its mark, as the matrix and the heat map print it
cor_label <- function(coef, mark) ifelse(is.na(coef), "-", paste0(dfmt(coef, 2), ifelse(mark == "NS", "", mark)))

## the key to the marks as the matrix and heat map print them
cor_key <- function(alpha)
  sprintf("** significant at p < %s; * significant at p < %s; no mark: not significant at the %s%% level; -: cannot be worked out (the pairs table says why).",
          p_lab(alpha / 5), p_lab(alpha), pct(alpha))

## the label on the diagonal: 1, or - for a variable that does not vary
cor_diag <- function(tab, v) if (v %in% attr(tab, "flat")) "-" else "1"

## The matrix: each pair once, below the diagonal, so nothing is printed twice.
cor_matrix_html <- function(tab, vars) {
  method <- attr(tab, "method")
  cell <- function(a, b) {
    i <- which((tab$Variable1 == a & tab$Variable2 == b) | (tab$Variable1 == b & tab$Variable2 == a))
    cor_label(tab$Coefficient[i], tab$Mark[i])
  }
  body <- lapply(seq_along(vars), function(i) c(esc(vars[i]), vapply(seq_along(vars), function(j)
    if (j < i) cell(vars[i], vars[j]) else if (j == i) cor_diag(tab, vars[i]) else "", "")))
  raw_table(c("", esc(vars)), body, caption = sprintf("Correlation matrix (%s)", COR_NAMES[[method]]))
}

## Every pair in a table with N, p, the mark, the interval and the reading;
## an approximate p-value carries a note.
cor_pairs_html <- function(tab) {
  method <- attr(tab, "method"); alpha <- attr(tab, "alpha")
  ci <- method == "pearson"
  body <- lapply(seq_len(nrow(tab)), function(i) c(
    esc(tab$Variable1[i]), esc(tab$Variable2[i]), as.character(tab$N[i]),
    dfmt(tab$Coefficient[i], 2),
    paste0(esc(p_show(tab$p[i], alpha)),
           if (identical(tab$P_method[i], "approximation")) "<sup>a</sup>"
           else if (identical(tab$P_method[i], "estimate")) "<sup>b</sup>" else ""),
    if (is.na(tab$p[i])) "" else tab$Mark[i],
    if (ci) (if (is.na(tab$CI_lower[i])) "-" else sprintf("%s to %s", dfmt(tab$CI_lower[i], 2), dfmt(tab$CI_upper[i], 2))),
    esc(tab$Reading[i])))
  head_ <- c("Variable 1", "Variable 2", "N", COR_SYMBOL[[method]], "p", "Mark",
             if (ci) sprintf("%s%% CI", pct(1 - alpha)), "Reading")
  notes <- c(
    if (any(tab$P_method == "approximation", na.rm = TRUE))
      "a: a large-sample approximation, used when the orderings of the values are too many to count; it is accurate at these sizes unless the values are very unevenly spread.",
    if (any(tab$P_method == "estimate", na.rm = TRUE))
      sprintf(paste("b: estimated from %s random orderings of the values, which have ties and too many orderings",
                    "to count (from %s when it falls close to a significance threshold); it can fall either side of",
                    "the exact value, by about 0.003 near p = 0.05, or 0.001 after the closer count."),
              format(COR_PERM_B, big.mark = ","), format(COR_PERM_B_CLOSE, big.mark = ",", scientific = FALSE)),
    if (method != "pearson" && any(tab$P_method == "exact", na.rm = TRUE))
      "Unmarked p-values are exact: every distinct ordering of the values is counted, ties included.",
    if (ci) "The interval is Fisher's approximation; the p-value and the mark come from the exact t test.",
    if (ci && any(!is.na(tab$p) & is.na(tab$CI_lower))) "An interval needs at least four rows.")
  foot <- if (length(notes)) sprintf("<tr><td colspan='%d' class='cdrow'>%s</td></tr>", length(head_), paste(notes, collapse = " "))
  raw_table(head_, body, foot = foot, caption = "Every pair")
}

## what each coefficient measures, in a sentence a student can follow
cor_about <- function(method) switch(method,
  pearson = paste("Pearson's r measures how closely two variables rise and fall together along a straight line,",
                  "from -1 (a perfect falling line) through 0 to +1 (a perfect rising line)."),
  spearman = paste("Spearman's rho is Pearson's r worked out on the ranks of the values: it measures how",
                   "consistently one variable rises (or falls) as the other rises, whether or not along a straight line."),
  kendall = paste("Kendall's tau (tau-b, which allows for tied values) compares every two rows: it is positive when",
                  "more pairs of rows are put in the same order by both variables than in opposite orders (pairs tied",
                  "on either variable count for neither), and negative when the reverse holds. For values scattered",
                  "evenly about a straight line it runs closer to 0 than r or rho."))

## pairs named for the text: "Yield and Height (0.82**)"
cor_pair_names <- function(tab, i)
  sprintf("%s and %s (%s)", tab$Variable1[i], tab$Variable2[i], cor_label(tab$Coefficient[i], tab$Mark[i]))

## The reading of the whole table: what the coefficient measures, which pairs
## are significant, and the cautions that apply to any correlation table.
cor_text <- function(tab) {
  method <- attr(tab, "method"); alpha <- attr(tab, "alpha")
  lvl <- paste0(pct(alpha), "%")
  ok <- !is.na(tab$p)
  sig <- which(ok & tab$p < alpha)
  out <- cor_about(method)
  if (any(!ok))
    out <- c(out, sprintf("%d %s could not be worked out; the table says why.", sum(!ok),
                          pl(sum(!ok), "pair", "pairs")))
  if (any(ok)) {
    sig <- sig[order(-abs(printed2(tab$Coefficient[sig])))]
    n_ok <- sum(ok); n_ns <- n_ok - length(sig)
    out <- c(out,
      if (n_ok == 1 && length(sig))
        sprintf("The pair shows evidence of an association at the %s level: %s.", lvl, cor_pair_names(tab, sig))
      else if (n_ok == 1)
        sprintf("For %s there is insufficient evidence of an association at the %s level.", cor_pair_names(tab, which(ok)), lvl)
      else if (!length(sig))
        sprintf("None of the %d pairs shows evidence of an association at the %s level.", n_ok, lvl)
      else if (length(sig) == n_ok)
        sprintf("All %d pairs show evidence of an association at the %s level: %s.", n_ok, lvl,
                join_and(head_more(cor_pair_names(tab, sig), 6)))
      else sprintf("%d of the %d pairs %s evidence of an association at the %s level: %s.", length(sig), n_ok,
                   pl(length(sig), "shows", "show"), lvl, join_and(head_more(cor_pair_names(tab, sig), 6))))
    if (n_ns > 0 && length(sig))
      out <- c(out, sprintf("For the other %s there is insufficient evidence of an association at the %s level.",
                            if (n_ns == 1) "pair" else paste(n_ns, "pairs"), lvl))
    if (n_ok >= 5 && length(sig)) {
      e <- round(n_ok * alpha, 10)
      out <- c(out, sprintf(paste("Even if no two variables were related, testing %d pairs at the %s level would make,",
                                  "on average, up to %s %s significant by chance (%d x %s), so treat a lone significant",
                                  "pair with care."), n_ok, lvl, num_text(e), if (e == 1) "pair" else "pairs", n_ok, p_lab(alpha)))
    }
  }
  ns <- range(tab$N)
  if (ns[1] != ns[2])
    out <- c(out, sprintf("Each pair uses every row that has both values, so N runs from %d to %d.", ns[1], ns[2]))
  hr <- attr(tab, "held_rows") %||% 0
  if (hr > 0)
    out <- c(out, sprintf(paste("%d %s an entry the data check has flagged (such as '%s') and %s left out of %s pairs",
                                "until corrected on the Data tab; each pair's reading gives the number it loses."),
                          hr, pl(hr, "row has", "rows have"), attr(tab, "held_eg"), pl(hr, "is", "are"),
                          pl(hr, "its", "their")))
  out <- c(out,
    "A correlation shows how two variables vary together; it does not show that one causes the other.",
    paste("In a designed experiment these coefficients use every plot, so they combine differences between",
          "treatments with variation within them: two variables can be correlated simply because both respond",
          "to the same treatments."))
  paste(out, collapse = " ")
}

## the full square of coefficients, each pair shown twice and each variable
## with itself on the diagonal, for the heat map
cor_square <- function(tab, vars) {
  g <- expand.grid(Row = vars, Col = vars, stringsAsFactors = FALSE)
  key <- function(a, b) paste(pmin(a, b), pmax(a, b), sep = "\u001f")
  i <- match(key(g$Row, g$Col), key(tab$Variable1, tab$Variable2))
  g$diag <- g$Row == g$Col
  flat <- g$diag & g$Row %in% attr(tab, "flat")
  g$coef <- ifelse(g$diag, ifelse(flat, NA, 1), tab$Coefficient[i])
  g$label <- ifelse(g$diag, ifelse(flat, "-", "1"), cor_label(tab$Coefficient[i], tab$Mark[i]))
  g$Row <- factor(g$Row, levels = rev(vars)); g$Col <- factor(g$Col, levels = vars)
  g
}

## Up to this many variables each square carries its coefficient; beyond it
## the squares are too small for the labels, which would overlap.
COR_HEAT_LABELS <- 15

plot_cor_heat <- function(tab, vars) {
  method <- attr(tab, "method")
  g <- cor_square(tab, vars)
  g$fill <- ifelse(g$diag, NA_real_, g$coef)
  ## dark tiles take white text, so every label can be read
  g$ink <- ifelse(!g$diag & !is.na(g$coef) & abs(g$coef) >= 0.6, "white", "black")
  n <- length(vars)
  ## the diagonal (each variable with itself) is drawn in white with an
  ## outline, so the grey of pairs that cannot be worked out means only that
  one <- g[g$diag & g$label == "1", , drop = FALSE]
  p <- ggplot(g, aes(x = .data$Col, y = .data$Row)) +
    geom_tile(aes(fill = .data$fill), colour = "white", linewidth = 0.8) +
    geom_tile(data = one, fill = "white", colour = "#B9C6D6", linewidth = 0.5) +
    scale_fill_gradient2(low = "#C0392B", mid = "#F7F7F7", high = "#1B4F9C", midpoint = 0,
                         limits = c(-1, 1), na.value = "#E3E3E3", name = COR_SYMBOL[[method]]) +
    labs(title = sprintf("Correlation heat map (%s)", COR_NAMES[[method]]), x = NULL, y = NULL,
         caption = "Blue: positive. Red: negative. Deeper colour: further from 0. White: each variable with itself. Grey: cannot be worked out.") +
    theme_doe() + theme(axis.text.x = element_text(angle = 30, hjust = 1, size = if (n > 12) 7 else 9),
                        axis.text.y = element_text(size = if (n > 12) 7 else 9), panel.grid = element_blank())
  if (n <= COR_HEAT_LABELS)
    p <- p + geom_text(aes(label = .data$label, colour = .data$ink), size = min(3.6, 40 / n)) + scale_colour_identity()
  p
}

## What the heat map shows, and the largest coefficients either way, named
## as printed (ties named together); the sign decides positive or negative.
cor_heat_text <- function(tab, nvars = NA) {
  alpha <- attr(tab, "alpha")
  what <- paste("Each square holds the coefficient for the variables on its row and column: blue for a positive",
                "coefficient, red for a negative one, and the deeper the colour the further it is from 0. The white",
                "diagonal is each variable with itself.",
                if (!is.na(nvars) && nvars > COR_HEAT_LABELS)
                  sprintf("With more than %d variables the squares are too small to label; read the values from the tables above.", COR_HEAT_LABELS)
                else cor_key(alpha))
  ok <- !is.na(tab$Coefficient)
  if (!any(ok)) return(list(what = what, reading = "No pair could be worked out."))
  pc <- printed2(tab$Coefficient)
  ## a coefficient that prints 0.00 and is not significant leans neither way,
  ## as its reading says
  lean <- ok & !(pc == 0 & tab$Mark == "NS")
  pos <- which(lean & tab$Coefficient > 0); neg <- which(lean & tab$Coefficient < 0)
  top <- function(i, fun) i[pc[i] == fun(pc[i])]
  out <- c(
    if (length(pos)) sprintf("The largest positive coefficient is for %s.", join_and(head_more(cor_pair_names(tab, top(pos, max)), 6)))
    else "No pair has a positive coefficient.",
    if (length(neg)) sprintf("The largest negative coefficient is for %s.", join_and(head_more(cor_pair_names(tab, top(neg, min)), 6)))
    else "No pair has a negative coefficient.")
  list(what = what, reading = paste(out, collapse = " "))
}

## one pair as a scatter plot, every row with both values a point
plot_cor_scatter <- function(d, x, y, method, alpha) {
  if (is.null(x) || is.null(y) || !nzchar(x) || !nzchar(y) || x == y) return(NULL)
  df <- data.frame(x = response_values(d[[x]]), y = response_values(d[[y]]))
  df <- df[is.finite(df$x) & is.finite(df$y), , drop = FALSE]
  if (nrow(df) < 2) return(NULL)
  tab <- correlate_data(d, c(x, y), method, alpha)
  ggplot(df, aes(x = .data$x, y = .data$y)) +
    geom_point(colour = "#3B7DD8", size = 2.4, alpha = 0.8) +
    labs(title = sprintf("%s against %s", y, x), x = x, y = y,
         subtitle = if (is.na(tab$Coefficient)) sprintf("N = %d; %s cannot be worked out", tab$N, COR_NAMES[[method]])
                    else sprintf("%s = %s, N = %d", COR_NAMES[[method]], cor_label(tab$Coefficient, tab$Mark), tab$N)) +
    theme_doe()
}

## What the scatter plot shows, the pair's reading, and the rule for curves
## and stray points left for the reader to apply.
cor_scatter_text <- function(d, x, y, method, alpha) {
  if (is.null(x) || is.null(y) || !nzchar(x) || !nzchar(y) || x == y)
    return(list(what = "", reading = "Choose two different variables to plot."))
  tab <- correlate_data(d, c(x, y), method, alpha)
  xs <- response_values(d[[x]]); ys <- response_values(d[[y]])
  ok <- is.finite(xs) & is.finite(ys)
  if (sum(ok) < 2 || is.na(tab$Coefficient)) return(list(what = if (sum(ok) < 2) "" else "Each point is a row with values of both variables.",
                                                        reading = tab$Reading))
  dup <- sum(duplicated(data.frame(xs[ok], ys[ok])))
  out <- c(sprintf("%s is %s.", COR_NAMES[[method]], cor_label(tab$Coefficient, tab$Mark)),
           tab$Reading,
           if (dup) sprintf("%d %s exactly on another row's point and %s hidden under it.", dup,
                            pl(dup, "row sits", "rows sit"), pl(dup, "is", "are")),
           if (tab$N < 10) sprintf("With only %d rows, a single point can change the coefficient a great deal.", tab$N),
           if (method == "pearson")
             paste("If the points bend along a curve that keeps rising (or keeps falling), or one or two sit far from",
                   "the rest, Pearson's r can mislead: Spearman's rho and Kendall's tau use only the order of the values",
                   "and are less affected. A curve that rises and then falls (or the reverse) can give a coefficient",
                   "near 0 whichever coefficient is used.")
           else paste("Rho and tau depend only on the order of the values, so a curve that keeps rising (or falling)",
                      "can still give a coefficient near +1 (or -1), while one that rises and then falls can give one near 0."))
  list(what = "Each point is a row with values of both variables.", reading = paste(out, collapse = " "))
}

## The variables an Explore tab starts with: the columns read as numbers,
## without the columns mapped as design factors or blocks and without those
## that usually label rows (Rep, Plot No, S. No.).
explore_defaults <- function(num, used = character(0)) {
  def <- setdiff(num, used)
  def <- def[!is_id_name(def)]
  if (length(def)) def else num
}
