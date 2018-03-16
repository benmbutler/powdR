#' Bin XRPD data
#'
#' \code{xrd.bin} applies binning to an XRPD dataframe using a specified bin width.
#'
#' The original values within an XRPD dataframe (2theta and counts) which fall within a
#' given bin width are replaced by their means for that interval.
#'
#' @param smpl a data frame of the XRPD sample to be binned.
#' @param bin.size the number of data points to be used in each bin
#'
#' @return a binned XRPD data frame (2theta and counts)
#'
#' @examples
#' data(D8_soil)
#'
#' xrd <- D8_soil$mineral
#' binned <- xrd.bin(smpl = xrd, bin.size = 5)
xrd.bin <- function(smpl, bin.size) {

  #Split the x-axis into chunks defined by bin.size
  x <- split(smpl[[1]], ceiling(seq_along(smpl[[1]])/bin.size))

  #calculate the mean of each chunk
  x_mean <- c()
  for(i in 1:length(x)) {
    x_mean[i] <- mean(x[[i]])
  }

  #Split the y-axis into chunks defined by bin.size
  y <- split(smpl[[2]], ceiling(seq_along(smpl[[2]])/bin.size))

  #calculate the mean of each chunk
  y_mean <- c()
  for(i in 1:length(y)) {
    y_mean[i] <- mean(y[[i]])
  }

  #create a dataframe of binned data
  out <- data.frame("tth" = x_mean,
                    "counts" = y_mean)

  return(out)

}
