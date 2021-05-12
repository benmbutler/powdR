#' Transform a two theta axis between wavelengths
#'
#' \code{tth_transform} converts the two theta axis from one wavelength to another via Bragg's law.
#' Use this function with caution if intending the apply \code{fps()} or \code{afps()} to wavelength
#' transformed samples or libraries because background signals can vary with wavelength which may
#' therefore affect the quality of the fit.
#'
#' @param tth the 2theta vector to be transformed
#' @param from numeric value defining the wavelength (Angstroms) to transform from
#' @param to numeric value defining the wavelength (Angstroms) to transform to
#'
#' @return a transformed 2theta vector
#'
#' @examples
#' data(soils)
#' sandstone2 <- soils$sandstone
#'
#' #Convert from Cu (1.54056 Angstroms) to Co (1.78897 Angstroms)
#' sandstone2$tth <- tth_transform(sandstone2$tth,
#'                                 from = 1.54056,
#'                                 to = 1.78897)
#'
#' sandstone_list <- as_multi_xy(list("sandstone" = soils$sandstone,
#'                                    "sandstone2" = sandstone2))
#' #plot the change
#' plot(sandstone_list, wavelength = "Cu")
#'
#' #Alternatively convert the 2theta axis of a library
#' data(minerals)
#'
#' minerals2 <- minerals
#' minerals2$tth <- tth_transform(minerals2$tth,
#'                                 from = 1.54056,
#'                                 to = 1.78897)
#'
#' #Plot the difference
#' plot(x = minerals$tth, y = minerals$xrd$QUA.1,
#'      type = "l", xlim = c(0, 85))
#' lines(x = minerals2$tth, y = minerals2$xrd$QUA.1,
#'       col = "red")
#' @export
tth_transform <- function(tth, from, to) {

  #Calculating d-spacing
  d <- from/(2*sin((tth/2)*pi/180))

  #Calculating new TTH based on a new wavelength
  new_tth <- suppressWarnings((asin(to/(2*d))*2)/0.01745329) #0.01745329 converts radians to degrees

  na_ids <- which(is.na(new_tth))

  if (length(na_ids) > 0) {

  stop(paste("\nWavelength transform cannot be computed for supplied 2theta values of",
             tth[min(na_ids)], "\nor greater because the associated d-spacing exceeds the",
             "maximum that can be measured \nwithin a 180 degree 2theta range at",
             to, "Angstroms.", "\n", "\nPlease subset the data to values below", tth[min(na_ids)],
             "degrees 2theta and recompute."),
       call. = FALSE)

  }

  return(new_tth)
}
