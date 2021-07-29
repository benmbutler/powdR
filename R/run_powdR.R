#' Run the powdR shiny app
#'
#' A wrapper for \code{shiny::runApp} to start the Shiny app for \code{powdR}.
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
run_powdR <- function(...) {

  app_dir <- system.file(paste0("shiny/quant/"), package = "powdR")
  app <- shiny::runApp(app_dir, launch.browser = TRUE,  ...)

}
