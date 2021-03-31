#' Calculate the Delta value for a fitted pattern
#'
#' \code{delta} computes the absolute difference between a measured and fitted pattern.
#' See equation for Delta in section 2.1 of Butler and Hillier (2021).
#'
#' @param measured a vector of count intensities for a measured pattern
#' @param fitted a vector of count intensities for a fitted pattern
#' @param weighting an optional weighting vector of the same length as those specified
#' in \code{measured} and \code{fitted}, which specifies areas of the pattern
#' to either emphasise (values > 1) or omit (values = 0) from the calculation.
#' Use with caution. Default is simply a weighting vector where all values are 1, which
#' hence has no effect on the computed value.
#'
#' @return a single numeric value
#'
#' @examples
#' # Load soils xrd data
#' data(soils)
#'
#' #Load minerals library
#' data(minerals)
#'
#' \dontrun{
#' #Produce a fit
#' fps_sand <-  fps(lib = minerals,
#'                  smpl = soils$sandstone,
#'                  refs = minerals$phases$phase_id,
#'                  std = "QUA.1",
#'                  align = 0.2)
#'
#' delta(measured = fps_sand$measured,
#'       fitted = fps_sand$fitted)
#'
#' }
#'
#' @references
#' Butler, B.M., Hillier, S., 2021. powdR: An R package for quantitative mineralogy using full pattern summation
#' of X-ray powder diffraction data. Computers and Geosciences. 147, 104662. doi:10.1016/j.cageo.2020.104662
#'
#' @export
delta <- function(measured, fitted, weighting) {

  if (!class(measured) %in% c("numeric", "integer")) {

    stop("\n-Data supplied to delta must be a numeric or integer vector")

  }

  if (!class(fitted) %in% c("numeric", "integer")) {

    stop("\n-Data supplied to delta must be a numeric or integer vector")

  }

  if (missing(weighting)) {

    weighting <- rep(1, length(measured))

  }

  if (!class(weighting) %in% c("numeric", "integer")) {

    stop("\n-Data supplied to delta must be a numeric or integer vector")

  }

  sum(abs(measured - fitted) * weighting)

}

#' Calculate the R value for a fitted pattern
#'
#' \code{r} computes the difference between a measured and fitted pattern. See
#' equation for R in section 2.1 of Butler and Hillier (2021).
#'
#' @param measured a vector of count intensities for a measured pattern
#' @param fitted a vector of count intensities for a fitted pattern
#' @param weighting an optional weighting vector of the same length as those specified
#' in \code{measured} and \code{fitted}, which specifies areas of the pattern
#' to either emphasise (values > 1) or omit (values = 0) from the calculation.
#' Use with caution. Default is simply a weighting vector where all values are 1, which
#' hence has no effect on the computed value.
#'
#' @return a single numeric value
#'
#' @examples
#' # Load soils xrd data
#' data(soils)
#'
#' #Load minerals library
#' data(minerals)
#'
#' \dontrun{
#' #Produce a fit
#' fps_sand <-  fps(lib = minerals,
#'                  smpl = soils$sandstone,
#'                  refs = minerals$phases$phase_id,
#'                  std = "QUA.1",
#'                  align = 0.2)
#'
#' r(measured = fps_sand$measured,
#'   fitted = fps_sand$fitted)
#'
#' }
#'
#' @references
#' Butler, B.M., Hillier, S., 2021. powdR: An R package for quantitative mineralogy using full pattern summation
#' of X-ray powder diffraction data. Computers and Geosciences. 147, 104662. doi:10.1016/j.cageo.2020.104662
#'
#' @export
r <- function(measured, fitted, weighting) {

  if (!class(measured) %in% c("numeric", "integer")) {

    stop("\n-Data supplied to r must be a numeric or integer vector")

  }

  if (!class(fitted) %in% c("numeric", "integer")) {

    stop("\n-Data supplied to r must be a numeric or integer vector")

  }

  if (missing(weighting)) {

    weighting <- rep(1, length(measured))

  }

  if (!class(weighting) %in% c("numeric", "integer")) {

    stop("\n-Data supplied to r must be a numeric or integer vector")

  }

  sqrt(sum((measured - fitted)^2 * weighting)/sum(measured^2))

}

#' Calculate the Rwp value for a fitted pattern
#'
#' \code{rwp} computes the difference between a measured and fitted pattern. See
#' equation for Rwp in section 2.1 of Butler and Hillier (2021).
#'
#' @param measured a vector of count intensities for a measured pattern
#' @param fitted a vector of count intensities for a fitted pattern
#' @param weighting an optional weighting vector of the same length as those specified
#' in \code{measured} and \code{fitted}, which specifies areas of the pattern
#' to either emphasise (values > 1) or omit (values = 0) from the calculation.
#' Use with caution. Default is simply a weighting vector where all values are 1, which
#' hence has no effect on the computed value.
#'
#' @return a single numeric value
#'
#' @examples
#' # Load soils xrd data
#' data(soils)
#'
#' #Load minerals library
#' data(minerals)
#'
#' \dontrun{
#' #Produce a fit
#' fps_sand <-  fps(lib = minerals,
#'                  smpl = soils$sandstone,
#'                  refs = minerals$phases$phase_id,
#'                  std = "QUA.1",
#'                  align = 0.2)
#'
#' rwp(measured = fps_sand$measured,
#'     fitted = fps_sand$fitted)
#'
#' }
#'
#' @references
#' Butler, B.M., Hillier, S., 2021. powdR: An R package for quantitative mineralogy using full pattern summation
#' of X-ray powder diffraction data. Computers and Geosciences. 147, 104662. doi:10.1016/j.cageo.2020.104662
#'
#' @export
rwp <- function(measured, fitted, weighting) {

  if (!class(measured) %in% c("numeric", "integer")) {

    stop("\n-The measured argument for rwp must be a numeric or integer vector")

  }

  if (!class(fitted) %in% c("numeric", "integer")) {

    stop("\n-The fitted argument for rwp must be a numeric or integer vector")

  }


  if (missing(weighting)) {

    weighting <- rep(1, length(measured))

  }

  if (!class(weighting) %in% c("numeric", "integer")) {

    stop("\n-The weighting argument for rwp must be a numeric or integer vector")

  }

  sqrt(sum((1/measured) * ((measured - fitted)^2) * weighting) / sum((1/measured) * (measured^2)))

}
