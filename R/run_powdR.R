#' Run the powdRcran shiny app
#'
#' A wrapper for [`runApp`] to start interactive shiny apps for the R package rxylib.
#' The `app_RLum()` function is just a wrapper for [`runApp`].
#' Via the `...` argument further arguments can be directly passed to [`runApp`].
#' See `?shiny::runApp` for further details on valid arguments.
#'
#' @param ... further arguments to pass to [`runApp`]
#'
#' @author Johannes Friedrich, University of Bayreuth (Germany)
#'
#' @examples
#'
#' \dontrun{
#'
#' run_powdR()
#'
#' }
#'
#' @seealso [`runApp`]
#' @md
#' @export
run_powdR <- function(...) {

  # start application
  app_dir <- system.file(paste0("shiny/"), package = "powdR")
  app <- shiny::runApp(app_dir, launch.browser = TRUE,  ...)
}
