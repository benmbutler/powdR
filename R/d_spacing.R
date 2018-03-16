#' Calculate d-spacing
#'
#' \code{d.spacing} returns a vector of d-spacing computed from the supplied
#' 2theta vector and monochromatic wavelength.
#'
#' @param x a vector of 2theta measurement intervals
#' @param wavelength the monochromatic X-ray wavelength
#'
#' @examples
#' # Load soil xrd data
#' data(D8_soil)
#'
#' #Use second sample in list (mineral soil)
#' xrd <- D8_soil[[2]]
#'
#' xrd$d <- d.spacing(x = xrd$tth, wavelength = 1.5406)
d.spacing <- function(x, wavelength) {

d <- wavelength/(2*sin((x/2)*pi/180))

return(d)

}
