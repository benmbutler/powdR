#' Run the background fitting shiny app
#'
#' A wrapper for \code{shiny::runApp} to start the \code{powdR}
#' background fitting Shiny app.
#'
#' @param ... further arguments to pass to \code{shiny::runApp}
#'
#' @examples
#'
#' \dontrun{
#'
#' run_powdR()
#'
#' }
#'
#' @md
#' @export
run_bkg <- function(...) {

  app_dir <- system.file(paste0("shiny/background/"), package = "powdR")
  app <- shiny::runApp(app_dir, launch.browser = TRUE,  ...)

}
