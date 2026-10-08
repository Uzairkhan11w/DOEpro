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
    mv   <- modifyList(map, list(response = v))
    base <- analyze(d, design, mv, alpha)
    asm0 <- check_assumptions(base)
    sug  <- suggest_transform(base, asm0, dtype)

    tr  <- if (is.null(trans)) "none" else {
      x <- trans[[v]]; if (is.null(x) || !nzchar(x)) "none" else x
    }
    lam <- if (!is.na(asm0$lambda)) asm0$lambda else 1

    if (identical(tr, "none")) {
      fin <- base; asm <- asm0
    } else {
      y <- response_values(d[[v]])
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
    fits[[v]] <- list(resp = v, base = base, final = fin, asm = asm, sug = sug,
                      trans = tr, lambda = lam,
                      header = if (identical(tr, "none")) v
                               else sprintf("%s (%s)", v, TRANS[[tr]]$lab))
  }
  list(design = design, alpha = alpha, map = map, responses = responses,
       facs = fits[[1]]$final$facs, fits = fits)
}

## quick pre-flight scan: what transformation does each numeric column want?
auto_scan <- function(d, design, map, candidates, alpha = 0.05, dtype = "auto") {
  do.call(rbind, lapply(candidates, function(v) {
    tryCatch({
      r <- analyze(d, design, modifyList(map, list(response = v)), alpha)
      a <- check_assumptions(r); s <- suggest_transform(r, a, dtype)
      data.frame(Variable = v,
                 Key = s$method,
                 Optional = isTRUE(s$optional),
                 `Shapiro-Wilk p` = fmt(a$p_norm, 3),
                 `Equal variances p` = if (is.na(a$p_hov)) "-" else
                   paste0(fmt(a$p_hov, 3), if (identical(a$hov_test, "Bartlett")) " (Bartlett)"
                          else if (a$levene$dropped > 0) " (Levene, some cells)" else ""),
                 `Taylor b` = fmt(a$slope, 2),
                 `Box-Cox lambda` = fmt(a$lambda, 2),
                 `CV (%)` = fmt(r$cv[length(r$cv)], 2),
                 `Suggested transformation` = TRANS[[s$method]]$lab,
                 Why = s$why, check.names = FALSE, stringsAsFactors = FALSE)
    }, error = function(e) NULL)
  }))
}

CREDIT_ASCII <- "DOEpro | Shah, Khan & Jeelani"

## HTML text as plain text for the monospaced PDF: tags dropped, entities spelt out
plain_text <- function(x) {
  x <- gsub("<[^>]*>", " ", x)
  ent <- c("&lt;" = "<", "&gt;" = ">", "&le;" = "<=", "&plusmn;" = "+/-",
           "&times;" = "x", "&radic;" = "sqrt", "&nbsp;" = " ", "&mdash;" = "-",
           "&ndash;" = "-", "&middot;" = ".", "&chi;" = "chi", "&amp;" = "&")
  for (k in names(ent)) x <- gsub(k, ent[[k]], x, fixed = TRUE)
  x
}

## Plain monospaced PDF, drawn on the base graphics device.  Used when no
## HTML-to-PDF renderer is installed, so the PDF button always works.
pdf_plain <- function(rr, file, letters_on = TRUE) {
  L <- c(sprintf("ANALYSIS OF VARIANCE REPORT   %s", format(Sys.Date(), "%d %B %Y")),
         strrep("=", 92), "",
         sprintf("Design                : %s", names(DESIGNS)[match(rr$design, DESIGNS)]),
         sprintf("Response variable(s)  : %s",
                 paste(vapply(rr$fits, function(f) f$header, character(1)), collapse = ", ")),
         sprintf("Significance level    : %s", rr$alpha), "",
         "Developed by",
         vapply(seq_along(AUTHORS), function(i) sprintf("  %d. %s - %s, %s", i,
                AUTHORS[[i]]$name, AUTHORS[[i]]$role, AUTHORS[[i]]$aff), character(1)), "")

  txt <- function(x) utils::capture.output(print(x, row.names = FALSE))
  for (nm in names(rr$fits)) {
    f <- rr$fits[[nm]]
    L <- c(L, strrep("-", 92), paste("RESPONSE:", f$header), strrep("-", 92), "",
           "ANALYSIS OF VARIANCE", txt(anova_display(f$final$anova, rr$alpha)),
           strwrap(plain_text(anova_note(f$final)), width = 90), "")
    for (en in names(f$final$effects)) {
      e <- f$final$effects[[en]]
      m <- gate_letters(e)
      keep <- intersect(c(e$vars, "Mean", "Raw_mean", "Mean_bt", "N",
                          if (!isTRUE(e$equal_rep)) "SE", LETTER_COLS), names(m))
      mm <- m[, keep, drop = FALSE]
      names(mm) <- sub("^Raw_mean$", "Unadjusted_mean", sub("^Mean_bt$", "Back_transformed", names(mm)))
      within <- if (is.null(e$slice)) "" else sprintf(" (within the same %s)", e$slice)
      xt <- extra_text(e)
      L <- c(L, sprintf("MEANS: %s", e$label), txt(mm),
             sprintf("  SE(m)%s = %s   SE(d) = %s   %s = %s", within,
                     err_text(e, "sem", 3, html = FALSE), err_text(e, "sed", 3, html = FALSE),
                     cd_name(e$alpha),
                     if (effect_sig(e)) err_text(e, "cd", 3, html = FALSE) else "NS"),
             if (length(xt)) sprintf("  %s = %s", names(xt), xt), "")
    }
    L <- c(L, sprintf("C.V. : %s",
                      paste(sprintf("%s = %s", names(f$final$cv), fmt(f$final$cv, 2)),
                            collapse = "   ")), "",
           "INTERPRETATION",
           strwrap(plain_text(interpret(f$final, f$asm, f$sug, TRANS[[f$trans]]$lab)),
                   width = 90), "")
  }

  per <- 62L
  pages <- split(L, ceiling(seq_along(L) / per))
  grDevices::pdf(file, width = 8.27, height = 11.69)
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::par(oldpar)
    grDevices::dev.off()
  }, add = TRUE)
  graphics::par(mar = c(2, 1, 1, 1), family = "mono")
  for (pg in pages) {
    graphics::plot.new()
    graphics::text(0, 1, paste(pg, collapse = "\n"), adj = c(0, 1), cex = 0.55)
    graphics::mtext(CREDIT_ASCII, side = 1, adj = 1, cex = 0.5, col = "grey40", line = 0.5)
  }
  invisible(TRUE)
}
