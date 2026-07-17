#' Run the DOEpro application
#'
#' Launches the DOEpro Shiny application, which analyses designed agricultural
#' experiments: paste or upload your data, choose the design, and press
#' \strong{Run analysis}.
#'
#' The application returns the analysis of variance, tables of means with
#' standard errors and critical differences, checks of the assumptions with
#' advice on transformations, post-hoc comparisons, diagnostic plots, a
#' plain-English interpretation, and a downloadable report.
#'
#' @param ... Further arguments passed to \code{\link[shiny]{shinyApp}}, for
#'   example \code{options = list(port = 8080)}.
#'
#' @return An object of class \code{shiny.appobj}. Called for its side effect of
#'   starting the application.
#'
#' @examples
#' if (interactive()) {
#'   run_DOEpro()
#' }
#'
#' @export
run_DOEpro <- function(...) {
  shiny::shinyApp(ui = doepro_ui(), server = doepro_server, ...)
}
