#' Create an XY object
#'
#' \code{as_xy} takes a data frame of XY XRPD data and ensures that it
#' meets the criteria for an XY object. These requirements
#' include that the data contains 2 columns of numeric data in a dataframe.
#' Once an XY object has been created, it can easily be plotted using the
#' associated \code{plot.XY} method.
#'
#' @param x a data frame (column 1 = 2theta, column 2 = counts)
#'
#' @return an XY object.
#'
#' @examples
#' # Load soils xrd data
#' data(rockjock_mixtures)
#'
#' xy <- as_xy(rockjock_mixtures$Mix1)
#'
#' class(xy)
#'
#' \dontrun{
#' plot(xy, wavelength = "Cu")
#' plot(xy, wavelength = "Cu", interactive = TRUE)
#' }
#'
#' @export
as_xy <- function(x) {

  #Check that x is a list
  if(!is.data.frame(x)) {

    stop("Data supplied to x must be a data frame.",
         call. = FALSE)

  }

  if(!.check_xy(x)) {

    stop("The data frame must contain two columns of numeric data.",
         call. = FALSE)

  }

  #change column names to tth and counts
  names(x) <- c("tth", "counts")
  class(x) <- c("XY", "data.frame")

  return(x)

}
