###############################################################################
##  MULTI-RESPONSE DRIVER
###############################################################################

## Analyse every selected response variable with the same design and mapping.
## `trans` is a named character vector: response -> transformation key.
#' Analyse several responses from a designed experiment
#'
#' Runs \code{\link{analyze}} for every response variable in turn, using the same
#' design and mapping, and optionally applying a variance-stabilising
#' transformation to each. This is what the application calls when more than one
#' response is selected.
#'
#' @param d A data frame in long format.
#' @param design The design code; see \code{\link{DESIGNS}}.
#' @param map A named list mapping roles to columns; see \code{\link{analyze}}.
#'   The \code{response} element is set for each response in turn and need not be
#'   supplied.
#' @param responses A character vector of response column names.
#' @param alpha The significance level. Defaults to 0.05.
#' @param trans Either \code{NULL} (analyse every response untransformed) or a
#'   named list or character vector giving a transformation for each response.
#'   The names are the response columns and the values are keys of
#'   \code{\link{TRANS}}, for example \code{list(Incidence = "arcsine")}.
#' @param dtype Passed to the transformation adviser; \code{"auto"} lets DOEpro
#'   judge the type of each response from its name and its mean-variance
#'   behaviour.
#'
#' @return A list with \code{fits} (one entry per response, each containing the
#'   fitted analysis in \code{final}), together with the design, the mapping and
#'   the significance level.
#'
#' @examples
#' d <- demo_data("FRCBD")
#' rr <- run_all(d, "FRCBD",
#'               list(factors = c("Nitrogen", "Variety"), block = "Block"),
#'               responses = "Yield")
#' rr$fits[["Yield"]]$final$anova
#'
#' @export
run_all <- function(d, design, map, responses, alpha = 0.05,
                    trans = NULL, dtype = "auto") {
  fits <- list()
  for (v in responses) {
    b <- fit_base(d, design, map, v, alpha, dtype_for(dtype, v))
    tr <- if (is.null(trans)) "none" else {
      x <- trans[[v]]; if (is.null(x) || !nzchar(x)) "none" else x
    }
    fits[[v]] <- fit_final(b, d, design, map, tr)
  }
  rr_of(design, alpha, map, responses, fits)
}

## the analysis of several responses as one object, from their fits
rr_of <- function(design, alpha, map, responses, fits, errors = NULL)
  list(design = design, alpha = alpha, map = map, responses = responses,
       facs = if (length(fits)) fits[[1]]$final$facs else NULL, fits = fits, errors = errors)

## the nature of response `v`: one choice for every response, or one each,
## named by the responses
dtype_for <- function(dtype, v) {
  if (is.null(dtype)) return("auto")
  if (!is.null(names(dtype))) { x <- dtype[[v]]; return(if (is.null(x) || !nzchar(x)) "auto" else x) }
  dtype[[1]]
}

## The analysis of one response as entered, its checks and the adviser's
## suggestion. The screening on the Data tab shows these, and the analysis
## starts from them (fit_final), so the two never disagree and the model is
## fitted once.
fit_base <- function(d, design, map, v, alpha = 0.05, dtype = "auto") {
  base <- analyze(d, design, modifyList(map, list(response = v)), alpha)
  asm0 <- check_assumptions(base)
  list(resp = v, base = base, asm0 = asm0, sug = suggest_transform(base, asm0, dtype))
}

## The transformations that can be applied to response `v` of the data: those
## defined for every value, and Box-Cox only when its profile has an optimum
## inside the range searched (fit_final() refuses it otherwise).
trans_ok <- function(b) {
  y <- b$base$data[[b$resp]]
  ok <- vapply(names(TRANS), function(m) trans_defined(m, y), logical(1))
  lam <- b$asm0$lambda
  ok[["boxcox"]] <- ok[["boxcox"]] && !is.na(lam) && abs(lam) < 2 - 1e-9
  names(TRANS)[ok]
}

## The analysis of one response on the scale chosen: the model as entered
## when `tr` is "none", else the analysis of the transformed values, with
## every mean also given back-transformed.
fit_final <- function(b, d, design, map, tr = "none") {
  v <- b$resp; base <- b$base; asm0 <- b$asm0; alpha <- base$alpha
  mv <- modifyList(map, list(response = v))
  lam <- if (!is.na(asm0$lambda)) asm0$lambda else 1

  if (identical(tr, "none")) {
    fin <- base; asm <- asm0
  } else {
    if (!tr %in% names(TRANS)) stop(sprintf("Unknown transformation '%s'.", tr))
    ## Box-Cox needs positive values for its profile; without one there is
    ## no optimal lambda, and applying lambda = 1 would only subtract 1
    if (identical(tr, "boxcox") && is.na(asm0$lambda))
      stop(sprintf("The Box-Cox transformation needs every value of '%s' to be positive, so no optimal lambda can be found; choose another transformation.", v))
    if (identical(tr, "boxcox") && abs(lam) >= 2 - 1e-9)
      stop(sprintf(paste("The Box-Cox profile of '%s' is still rising at lambda = %s, the end of the range searched (-2 to 2),",
                         "so no optimal lambda was found there; choose another transformation, such as 1 / y."), v, fmt(lam, 0)))
    y <- response_values(d[[v]])
    y[!is.finite(y)] <- NA          # Inf is left out, as analyze() leaves it out
    z <- suppressWarnings(TRANS[[tr]]$f(y, lam))
    if (any(!is.finite(z[!is.na(y)])))   # entries that are not numbers are left out, not "out of range"
      stop(sprintf("The %s transformation is not defined for '%s' - the column has values outside its permitted range (e.g. zero or negative).",
                   TRANS[[tr]]$lab, v))
    d2 <- d; d2[[v]] <- z
    fin <- analyze(d2, design, mv, alpha)
    fin$trans <- tr; fin$lambda <- lam
    fin$excluded <- base$excluded      # reasons in terms of the data as entered
    fin$effects <- lapply(fin$effects, function(e) {
      e$means$Mean_bt <- TRANS[[tr]]$b(e$means$Mean, lam); e
    })
    asm <- check_assumptions(fin)
  }
  list(resp = v, base = base, final = fin, asm = asm, asm0 = asm0, sug = b$sug,
       trans = tr, lambda = lam,
       header = if (identical(tr, "none")) v else sprintf("%s (%s)", v, TRANS[[tr]]$lab))
}
