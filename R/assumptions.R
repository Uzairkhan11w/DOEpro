###############################################################################
##  ASSUMPTIONS  &  TRANSFORMATIONS
###############################################################################
## Levene's test, median-centred (Brown-Forsythe): a one-way ANOVA on the
## absolute deviations from the cell medians.  Identical to car::leveneTest.
levene_test <- function(y, g) {
  g <- droplevels(factor(g))
  if (nlevels(g) < 2L || length(y) <= nlevels(g)) return(NULL)
  z <- abs(y - stats::ave(y, g, FUN = stats::median))
  a <- stats::anova(stats::lm(z ~ g))
  list(F = a[1, "F value"], p = a[1, "Pr(>F)"], df1 = a[1, "Df"], df2 = a[2, "Df"])
}

## Box-Cox log-likelihood profile.  For a positive response y and design matrix X,
##   z(lambda) = (y^lambda - 1) / (lambda * gm^(lambda-1)),   z(0) = gm * log(y)
## with gm the geometric mean of y, and  l(lambda) = -n/2 * log(RSS(z)/n).
## Computed directly, so it never depends on re-evaluating a stored model call.
boxcox_profile <- function(y, X, lambda = seq(-2, 2, 0.02)) {
  if (any(!is.finite(y)) || any(y <= 0) || is.null(X)) return(NULL)
  n <- length(y); gm <- exp(mean(log(y))); qrx <- qr(X)
  ll <- vapply(lambda, function(l) {
    z <- if (abs(l) < 1e-9) gm * log(y) else (y^l - 1) / (l * gm^(l - 1))
    rss <- sum(qr.resid(qrx, z)^2)
    if (!is.finite(rss) || rss <= 0) return(NA_real_)
    -n / 2 * log(rss / n)
  }, numeric(1))
  if (all(is.na(ll))) return(NULL)
  best <- lambda[which.max(ll)]
  ## 95% CI: lambda values within qchisq(.95,1)/2 of the maximum log-likelihood
  inside <- lambda[!is.na(ll) & ll > max(ll, na.rm = TRUE) - 0.5 * stats::qchisq(0.95, 1)]
  list(x = lambda, y = ll, lambda = best, ci = range(inside))
}

check_assumptions <- function(res) {
  r <- res$resid
  d <- res$data; resp <- res$resp
  cells <- interaction(d[res$facs], drop = TRUE)

  sw <- if (length(r) >= 3 && length(r) <= 5000) stats::shapiro.test(r) else NULL
  lev  <- tryCatch(levene_test(d[[resp]], cells), error = function(e) NULL)
  bart <- tryCatch(stats::bartlett.test(d[[resp]], cells), error = function(e) NULL)

  ## mean-variance relationship -> Taylor's power law slope
  mv <- data.frame(m = tapply(d[[resp]], cells, mean),
                   v = tapply(d[[resp]], cells, stats::var))
  mv <- mv[stats::complete.cases(mv) & mv$m > 0 & mv$v > 0, ]
  slope <- if (nrow(mv) >= 3)
    unname(stats::coef(stats::lm(log(v) ~ log(m), data = mv))[2]) else NA_real_

  bc <- tryCatch(boxcox_profile(d[[resp]], res$X), error = function(e) NULL)

  std <- r / stats::sd(r)
  outliers <- which(abs(std) > 3)

  list(shapiro = sw, levene = lev, bartlett = bart, slope = slope,
       bc = bc, lambda = if (is.null(bc)) NA_real_ else bc$lambda,
       mv = mv, outliers = outliers,
       p_norm = if (is.null(sw))  NA_real_ else sw$p.value,
       p_hov  = if (is.null(lev)) NA_real_ else lev$p)
}

suggest_transform <- function(res, asm, dtype = "auto") {
  y <- res$data[[res$resp]]
  y <- y[!is.na(y)]
  pn <- asm$p_norm; ph <- asm$p_hov; b <- asm$slope; lam <- asm$lambda

  nm <- if (!is.null(res$orig_resp)) res$orig_resp else res$resp
  has_frac    <- any(abs(y - round(y)) > 1e-8)
  looks_prop  <- min(y) >= 0 && max(y) <= 1 && has_frac
  looks_count <- min(y) >= 0 && !has_frac
  in_pct_range <- min(y) >= 0 && max(y) <= 100 && !looks_prop
  ## a 0-100 range alone does NOT make a variable a percentage (most yields and
  ## heights live there too), so auto-detection also needs the column name to say so
  pct_name <- grepl("perc|pct|%|incid|sever|infest|germinat|surviv|mortal|infect|damage",
                    tolower(nm))
  looks_pct   <- in_pct_range && pct_name
  count_slope <- !is.na(b) && b >= 0.5 && b < 1.5

  ok <- (is.na(pn) || pn > 0.05) && (is.na(ph) || ph > 0.05)

  ## When the diagnostics are satisfactory we still name the conventional
  ## transformation for data that are plainly counts or percentages, flagged as
  ## optional - agronomic convention transforms them, the diagnostics do not
  ## demand it, and the analyst should decide knowingly.
  if (ok && dtype == "auto") {
    if (looks_prop)
      return(list(method = "arcsine01", optional = TRUE,
        why = "The residuals are normal and the variances homogeneous, so no transformation is strictly required. The response is a proportion, however, and convention is to analyse proportions on the angular (arcsine square-root) scale."))
    if (looks_count && count_slope)
      return(list(method = if (min(y) < 1) "sqrt0.5" else "sqrt", optional = TRUE,
        why = sprintf("The residuals are normal and the variances homogeneous, so no transformation is strictly required. The response is nevertheless integer-valued with variance proportional to the mean (Taylor slope b = %.2f) - i.e. count data, for which the square root is conventional.", b)))
    if (looks_pct)
      return(list(method = "arcsine", optional = TRUE,
        why = sprintf("The residuals are normal and the variances homogeneous, so no transformation is strictly required. '%s' is bounded by 0 and 100 and named as a percentage, for which the angular (arcsine square-root) transformation is conventional.", nm)))
    return(list(method = "none", optional = FALSE,
      why = "Residuals are normal and the variances are homogeneous - no transformation is needed."))
  }

  ## user-declared data type wins over any guessing
  if (dtype == "percent")
    return(list(method = if (max(y) <= 1) "arcsine01" else "arcsine",
      why = "You declared the response to be a percentage/proportion, so the angular (arcsine square-root) transformation applies."))
  if (dtype == "count")
    return(list(method = if (min(y) < 1) "sqrt0.5" else "sqrt",
      why = "You declared the response to be a count, so the square-root transformation applies (sqrt(y+0.5) when zeros are present)."))

  if (looks_prop)
    return(list(method = "arcsine01",
      why = "The response lies between 0 and 1 and is not integer - it behaves like a proportion, so the angular (arcsine square-root) transformation applies."))

  if (looks_count && count_slope)
    return(list(method = if (min(y) < 1) "sqrt0.5" else "sqrt",
      why = sprintf("The response is integer-valued and the variance rises in proportion to the mean (Taylor slope b = %.2f) - the classic signature of count data, so the square root is indicated.", b)))

  if (looks_pct && !(looks_count && count_slope))
    return(list(method = "arcsine",
      why = sprintf("'%s' is bounded between 0 and 100 and its name suggests a percentage, so the angular (arcsine square-root) transformation applies.", nm)))

  if (!is.na(b)) {
    if (b >= 0.5 && b < 1.5)
      return(list(method = if (min(y) < 1) "sqrt0.5" else "sqrt",
        why = sprintf("Variance rises roughly in proportion to the mean (Taylor slope b = %.2f) - square root is indicated.", b)))
    if (b >= 1.5 && b < 2.5)
      return(list(method = if (min(y) <= 0) "log1" else "log",
        why = sprintf("Variance rises with the square of the mean (b = %.2f) - the logarithmic transformation is indicated.", b)))
    if (b >= 2.5)
      return(list(method = "reciprocal",
        why = sprintf("Variance rises faster than the square of the mean (b = %.2f) - the reciprocal transformation is indicated.", b)))
  }
  if (!is.na(lam)) {
    m <- if (abs(lam) < 0.15) "log" else if (abs(lam - 0.5) < 0.2) "sqrt" else
         if (abs(lam + 1) < 0.25) "reciprocal" else "boxcox"
    return(list(method = m, why = sprintf("The Box-Cox profile peaks at lambda = %.2f.", lam)))
  }
  hint <- if (in_pct_range && !pct_name)
    " Note: your response lies between 0 and 100. If it really is a percentage, set 'Nature of the response' to 'Percentage / proportion' and the angular transformation will be applied." else ""
  list(method = "none",
       why = paste0("No clear transformation is indicated. If normality is badly violated, consider a non-parametric test (Kruskal-Wallis / Friedman).", hint))
}

#' The transformations DOEpro offers
#'
#' A named list of the variance-stabilising transformations. Each entry holds a
#' label (\code{lab}), the transformation (\code{f}) and its inverse
#' (\code{inv}), which is what lets DOEpro report means on the original scale of
#' measurement alongside the transformed ones.
#'
#' @format A named list of length 9. The names are the keys used in the
#'   \code{trans} argument of \code{\link{run_all}}: \code{"none"},
#'   \code{"log"}, \code{"log1"}, \code{"sqrt"}, \code{"sqrt0.5"},
#'   \code{"arcsine"}, \code{"arcsine01"}, \code{"reciprocal"} and
#'   \code{"boxcox"}.
#'
#' @examples
#' names(TRANS)
#' vapply(TRANS, `[[`, character(1), "lab")
#'
#' @export
TRANS <- list(
  none       = list(lab = "None",                       f = function(y, l) y,
                    b = function(z, l) z),
  log        = list(lab = "log(y)",                     f = function(y, l) log(y),
                    b = function(z, l) exp(z)),
  log1       = list(lab = "log(y + 1)",                 f = function(y, l) log(y + 1),
                    b = function(z, l) exp(z) - 1),
  sqrt       = list(lab = "sqrt(y)",                    f = function(y, l) sqrt(y),
                    b = function(z, l) z^2),
  sqrt0.5    = list(lab = "sqrt(y + 0.5)",              f = function(y, l) sqrt(y + 0.5),
                    b = function(z, l) z^2 - 0.5),
  arcsine    = list(lab = "arcsine sqrt(y/100), degrees",
                    f = function(y, l) asin(sqrt(y / 100)) * 180 / pi,
                    b = function(z, l) (sin(z * pi / 180))^2 * 100),
  arcsine01  = list(lab = "arcsine sqrt(y), degrees",
                    f = function(y, l) asin(sqrt(y)) * 180 / pi,
                    b = function(z, l) (sin(z * pi / 180))^2),
  reciprocal = list(lab = "1 / y",                      f = function(y, l) 1 / y,
                    b = function(z, l) 1 / z),
  boxcox     = list(lab = "Box-Cox (optimal lambda)",
                    f = function(y, l) if (abs(l) < 1e-6) log(y) else (y^l - 1) / l,
                    b = function(z, l) if (abs(l) < 1e-6) exp(z) else (z * l + 1)^(1 / l))
)
