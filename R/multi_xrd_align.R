#' Align and harmonise multiple XRPD patterns
#'
#' \code{multi.xrd.align} takes a list of XRPD data and aligns them
#' relative to a chosen standard. It uses an optimisation routine that
#' computes a suitable linear shift. After all samples have been aligned,
#' the function harmonises the data to a single 2theta scale.
#'
#' @param xrd a list XRPD dataframes (2theta and counts)
#' @param xrd.standard a dataframe of the chosen standard that each
#' sample is aligned to (2theta and counts)
#' @param xmin the minimum 2theta value used during alignment
#' @param xmax the maximum 2theta value used during alignment
#' @param xshift the maximum (positive and negative) 2theta shift
#' that is allowed during alignment
#'
#' @return a list of aligned and harmonised XRPD data
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
#' #Plot the main quartz peak prior to alignment (scale the counts using rng.nm)
#' plot(x = soils$sandstone$tth, y = rng.nm(soils$sandstone$counts),
#'      xlim = c(26, 27), type = "l")
#' lines(x = soils$granite$tth, y = rng.nm(soils$granite$counts), col = "red")
#' lines(x = soils$limestone$tth, y = rng.nm(soils$limestone$counts), col = "green")
#' lines(x = quartz$tth, y = rng.nm(quartz$counts), col = "blue")
#'
#' #align data
#' aligned <- multi.xrd.align(xrd = soils, xrd.standard = quartz, xmin = 10,
#'                            xmax = 60, xshift = 0.2)
#' #replot data
#' plot(x = aligned$sandstone$tth, y = rng.nm(aligned$sandstone$counts),
#'      xlim = c(26, 27), type = "l")
#' lines(x = aligned$granite$tth, y = rng.nm(aligned$granite$counts), col = "red")
#' lines(x = aligned$limestone$tth, y = rng.nm(aligned$limestone$counts), col = "green")
#' lines(x = quartz$tth, y = rng.nm(quartz$counts), col = "blue")
multi.xrd.align <- function(xrd, xrd.standard, xmin, xmax, xshift) {

  #Aligned data will end up in this list
  xrd_aligned <- list()

  #The minimum and maximum 2theta value from each alignment will
  #end up in these vectors
  min_tth <- c()
  max_tth <- c()

  for (i in 1:length(xrd)) {
    x <- xrd.align(xrd.sample = xrd[[i]], xrd.standard = xrd.standard,
                      xmin = xmin,
                      xmax = xmax,
                      xshift = xshift)

    xrd_aligned[[i]] <- x

    min_tth[i] <- min(x[[2]][,1])
    max_tth[i] <- max(x[[2]][,1])
  }

  #Define the 2theta resolution that a new scale will be built upon. Based on
  #the first pattern in the list
  tth_interval <- (max(xrd[[1]][, 1]) - min(xrd[[1]][, 1])) / nrow(xrd[[1]])

  #Create a new 2th scale based on the shifts of the data so that no NA
  #values result from the subsequent linear spline
  tth <- seq(from = max(min_tth), to = min(max_tth), by = tth_interval)

  #Harmonise the aligned data to the new 2TH scale
  xrd_harm <- list()
  for (i in 1:length(xrd_aligned)) {
    xrd_harm[[i]] <- data.frame(stats::approx(x = xrd_aligned[[i]][[2]][, 1],
                                       y = xrd_aligned[[i]][[2]][, 2],
                                       method = "linear",
                                       xout = tth))
    names(xrd_harm[[i]]) <- c("tth", "counts")
  }

  #preserve names
  names(xrd_harm) <- names(xrd)

  return(xrd_harm)

}
