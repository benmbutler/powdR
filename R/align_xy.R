#' Align XRPD data to a given standard
#'
#' See \code{?align_xy.XY} and \code{align_xy.multiXY} for
#' method-specific details.
#'
#' @param x an \code{XY} or \code{multiXY} object.
#' @param std a dataframe of the chosen standard that each
#' sample is aligned to (column 1 = 2theta, column 2 = counts)
#' @param xmin the minimum 2theta value used during alignment
#' @param xmax the maximum 2theta value used during alignment
#' @param xshift the maximum (positive and negative) 2theta shift
#' that is allowed during alignment
#' @param ... other arguments
#'
#' @return an \code{XY} or \code{multiXY} object.
#'
#' @examples
#' # Load soils xrd data
#' data(soils)
#'
#' #Load minerals library
#' data(minerals)
#'
#' #Create a standard quartz pattern to align to
#' quartz <- data.frame(tth = minerals$tth,
#'                      counts = minerals$xrd$QUA.1)
#'
#' #Plot the main quartz peak prior to alignment
#' plot(soils, wavelength = "Cu",
#'      xlim = c(26,27),
#'      normalise = TRUE)
#'
#' #align data
#' aligned <- align_xy(soils,
#'                     std = quartz,
#'                     xmin = 10,
#'                     xmax = 60,
#'                     xshift = 0.2)
#'
#' #replot data
#' plot(aligned, wavelength = "Cu",
#'      xlim = c(26,27),
#'      normalise = TRUE)
#'
#' #Alternatively try with a single XY objects
#'
#' unaligned <- as_multi_xy(list("quartz" = quartz,
#'                              "sandstone" = soils$sandstone))
#'
#' plot(unaligned, wav = "Cu",
#'      xlim = c(26,27), normalise = T)
#'
#' sandstone_a <- align_xy(soils$sandstone,
#'                         std = quartz,
#'                         xmin = 10,
#'                         xmax = 60,
#'                         xshift = 0.3)
#'
#' aligned <- as_multi_xy(list("quartz" = quartz,
#'                             "sandstone" = sandstone_a))
#'
#' plot(aligned, wav = "Cu",
#'      xlim = c(26,27), normalise = T)
#'
#' @export
align_xy <- function(x, std,
                     xmin, xmax, xshift, ...) {
  UseMethod("align_xy")
}


#' Align XRPD data in a multiXY object to a given standard
#'
#' \code{align_xy.multiXY} takes a multiXY object and aligns
#' each of the XY data frames within it to a given standard.
#' An optimisation routine is used that computes a suitable
#' linear shift. After all samples have been aligned,
#' the function harmonises the data to a single 2theta scale.
#'
#' @param x a \code{multiXY} object.
#' @param std a dataframe of the chosen standard that each
#' sample is aligned to (column 1 = 2theta, column 2 = counts)
#' @param xmin the minimum 2theta value used during alignment
#' @param xmax the maximum 2theta value used during alignment
#' @param xshift the maximum (positive and negative) 2theta shift
#' that is allowed during alignment
#' @param ... other arguments
#'
#' @return a \code{multiXY} object.
#'
#' @examples
#' # Load soils xrd data
#' data(soils)
#'
#' #Load minerals library
#' data(minerals)
#'
#' #Create a standard quartz pattern to align to
#' quartz <- data.frame(tth = minerals$tth,
#'                      counts = minerals$xrd$QUA.1)
#'
#' #Plot the main quartz peak prior to alignment
#' plot(soils, wavelength = "Cu",
#'      xlim = c(26,27),
#'      normalise = TRUE)
#'
#' #align data
#' aligned <- align_xy(soils,
#'                     std = quartz,
#'                     xmin = 10,
#'                     xmax = 60,
#'                     xshift = 0.2)
#'
#' #replot data
#' plot(aligned, wavelength = "Cu",
#'      xlim = c(26,27),
#'      normalise = TRUE)
#'
#' @export
align_xy.multiXY <- function(x, std,
                             xmin, xmax,
                             xshift, ...) {

  #-----------------------------------
  #Conditions
  #-----------------------------------

  #Make sure the standard data is provided
  if (missing(std)) {

    stop("Supply a dataframe for the standard (std) comprised of the
         2theta axis and counts",
         call. = FALSE)

  }

  #Make sure the xmin value is defined
  if (missing(xmin) | xmin > max(std[[1]]) | xmin < min(std[[1]])) {

    stop("Specify a numeric value for the xmin argument that is within the
         2theta range of your standard",
         call. = FALSE)

  }

  #Make sure the xmin value is defined
  if (missing(xmax) | xmax > max(std[[1]]) | xmax < min(std[[1]])) {

    stop("Specify a numeric value for the xmax argument that is within the
         2theta range of your standard",
         call. = FALSE)

  }

  if (xmax <= xmin) {

    stop("The value specified in xmax must exceed that of xmin",
         call. = FALSE)

  }

  if (missing(xshift) | !is.numeric(xshift)) {

    stop("Specify a numeric value for xshift.",
         call. = FALSE)

  }

  #---------------------------------------------
  #Alignment
  #---------------------------------------------

  #Make sure xshift is not negative
  xshift <- abs(xshift)

  #Aligned data will end up in this list
  xrpd_aligned <- list()

  #The minimum and maximum 2theta value from each alignment will
  #end up in these vectors
  min_tth <- c()
  max_tth <- c()

  for (i in 1:length(x)) {

    xa <- .xrd_align(smpl = x[[i]],
                     standard = std,
                     xmin = xmin,
                     xmax = xmax,
                     xshift = xshift,
                     manual = FALSE)

    xrpd_aligned[[i]] <- xa

    min_tth[i] <- min(xa[[2]][,1])
    max_tth[i] <- max(xa[[2]][,1])

  }

  #Define the 2theta resolution that a new scale will be built upon. Based on
  #the first pattern in the list
  tth_interval <- (max(std[[1]]) - min(std[[1]])) / nrow(std)

  #Create a new 2th scale based on the shifts of the data so that no NA
  #values result from the subsequent linear spline
  tth <- seq(from = max(min_tth), to = min(max_tth), by = tth_interval)

  #Harmonise the aligned data to the new 2TH scale
  xrpd_harm <- list()

  for (i in 1:length(xrpd_aligned)) {

    xrpd_harm[[i]] <- data.frame(stats::spline(x = xrpd_aligned[[i]][[2]][, 1],
                                               y = xrpd_aligned[[i]][[2]][, 2],
                                               method = "natural",
                                               xout = tth))

    names(xrpd_harm[[i]]) <- c("tth", "counts")
    class(xrpd_harm[[i]]) <- c("XY", "data.frame")

  }

  #preserve names
  names(xrpd_harm) <- names(x)

  class(xrpd_harm) <- c("multiXY", "list")

  return(xrpd_harm)


}


#' Align XRPD data in an XY object to a given standard
#'
#' \code{align_xy.XY} takes an XY object and aligns
#' it to a given standard. An optimisation routine is used
#' that computes a suitable linear shift.
#'
#' @param x an \code{XY} object.
#' @param std a dataframe of the chosen standard that each
#' sample is aligned to (column 1 = 2theta, column 2 = counts)
#' @param xmin the minimum 2theta value used during alignment
#' @param xmax the maximum 2theta value used during alignment
#' @param xshift the maximum (positive and negative) 2theta shift
#' that is allowed during alignment
#' @param ... other arguments
#'
#' @return a \code{XY} object.
#'
#' @examples
#' # Load soils xrd data
#' data(soils)
#'
#' #Load minerals library
#' data(minerals)
#'
#' #Create a standard quartz pattern to align to
#' quartz <- data.frame(tth = minerals$tth,
#'                      counts = minerals$xrd$QUA.1)
#'
#'unaligned <- as_multi_xy(list("quartz" = quartz,
#'                              "sandstone" = soils$sandstone))
#'
#' plot(unaligned, wav = "Cu",
#'      xlim = c(26,27), normalise = T)
#'
#' sandstone_a <- align_xy(soils$sandstone,
#'                         std = quartz,
#'                         xmin = 10,
#'                         xmax = 60,
#'                         xshift = 0.3)
#'
#' aligned <- as_multi_xy(list("quartz" = quartz,
#'                             "sandstone" = sandstone_a))
#'
#' plot(aligned, wav = "Cu",
#'      xlim = c(26,27), normalise = T)
#'
#' @export
align_xy.XY <- function(x, std,
                        xmin, xmax,
                        xshift, ...) {

  #-----------------------------------
  #Conditions
  #-----------------------------------

  #Make sure the standard data is provided
  if (missing(std)) {

    stop("Supply a dataframe for the standard (std) comprised of the
         2theta axis and counts",
         call. = FALSE)

  }

  #Make sure the xmin value is defined
  if (missing(xmin) | xmin > max(std[[1]]) | xmin < min(std[[1]])) {

    stop("Specify a numeric value for the xmin argument that is within the
         2theta range of your standard",
         call. = FALSE)

  }

  #Make sure the xmin value is defined
  if (missing(xmax) | xmax > max(std[[1]]) | xmax < min(std[[1]])) {

    stop("Specify a numeric value for the xmax argument that is within the
         2theta range of your standard",
         call. = FALSE)

  }

  if (xmax <= xmin) {

    stop("The value specified in xmax must exceed that of xmin",
         call. = FALSE)

  }

  if (missing(xshift) | !is.numeric(xshift)) {

    stop("Specify a numeric value for xshift.",
         call. = FALSE)

  }

  #---------------------------------------------
  #Alignment
  #---------------------------------------------

  #Make sure xshift is not negative
  xshift <- abs(xshift)

  xa <- .xrd_align(smpl = x,
                   standard = std,
                   xmin = xmin,
                   xmax = xmax,
                   xshift = xshift,
                   manual = FALSE)

  x <- xa$aligned

  class(x) <- c("XY", "data.frame")

  return(x)


}
