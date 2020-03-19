#' Run the powdR shiny app
#'
#' A wrapper for \code{\link[shiny]{runApp}} to start the Shiny app for \code{powdR}.
#'
#' @param ... further arguments to pass to \code{\link[shiny]{runApp}}
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
  if (packageVersion("shiny") > 1.4) {

    cat("Warning: Parts of this application may not work with shiny versions > 1.3.2.
        \nArchived versions of Shiny can be installed using:
        \ninstall.packages('http://cran.r-project.org/src/contrib/Archive/shiny/shiny_1.3.2.tar.gz', repos=NULL, type='source') \n")

  }

  app_dir <- system.file(paste0("shiny/"), package = "powdR")
  app <- shiny::runApp(app_dir, launch.browser = TRUE,  ...)

}
