#' Fit a background to XRPD data
#'
#' \code{xrd.bkg} fits a background to xrpd data
#'
#' This function bins the data into chunks and takes the minimum counts
#' from each chunk. A loess polynomial is then fitted between the minimum
#' values and the polynomial predicted onto the original 2theta scale.
#'
#' @param tth the 2theta scale
#' @param counts the XRPD counts
#' @param width the bin width
#' @param res the span of the loess polynomial
#'
#' @return a vector of the fitted background
#'
#' @examples
#' data(D8_soil)
#' xrd <- D8_soil$mineral
#' background <- xrd.bkg(tth = xrd$tth, counts = xrd$counts, width = 50, res = 0.1)
#'
#' plot(xrd, type = "l")
#' lines(x = xrd$tth, y = background, col = "red")
xrd.bkg <- function(tth, counts, width, res) {

  #splitting the tth and counts vectors into a list of chunks
  tth_cut <- split(tth, ceiling(seq_along(tth)/width))
  counts_cut <- split(counts, ceiling(seq_along(counts)/width))

  #Extract the median and min values of these chunks
  tth_cut_min <- unname(unlist(lapply(tth_cut, median)))
  counts_cut_min <- unname(unlist(lapply(counts_cut, min)))

  #add the first and last data points of the original so that
  #extrapolation is not required
  tth_cut_min <- c(tth[1], tth_cut_min, tth[length(tth)])
  counts_cut_min <- c(counts[1], counts_cut_min, counts[length(counts)])

  #local polynomial smoothing
  bkg.loess <- loess(counts_cut_min ~ tth_cut_min, span = res)

  #predict onto original tth resolution
  bkg.out <- predict(bkg.loess, tth)

  return(bkg.out)
}
