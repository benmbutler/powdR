#' Adjust 2theta of XRPD data based on monochromatic wavelength
#'
#' \code{wavelength.transform} returns a new 2theta vector based
#' on a different monochromatic X-ray wavelength from which the
#' measurements were made.
#'
#' @param x a vector of 2theta measurement intervals
#' @param measured.wavelength the monochromatic X-ray wavelength used
#' for the original measurement
#' @param new.wavelength the monochromatic X-ray wavelength used for
#' the transformation
#'
#' @return a vector a transformed 2theta intervals
#'
#' @examples
#' # Load soil xrd data
#' data(soils)
#'
#' xrd <- soils$sandstone
#'
#' #This was measured using Cu radiation (wavelength = 1.54056 Angstroms),
#' #and will be converted to Co radiation (wavelength = 1.78897 Angstroms)
#'
#' xrd$tth <- wavelength.transform(xrd$tth, measured.wavelength = 1.54056,
#'                                 new.wavelength = 1.78897)
wavelength.transform <- function(x, measured.wavelength, new.wavelength) {

  #Calculating d-spacing
  d <- measured.wavelength/(2*sin((x/2)*pi/180))

  #Calculating new TTH based on a new wavelength
  new.tth <- NISTunits::NISTradianTOdeg((asin(new.wavelength/(2*d))*2))

  return(new.tth)
}
