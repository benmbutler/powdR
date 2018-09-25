#' Fit a background to XRPD data
#'
#' \code{bkg} fits a background to xrpd data
#'
#' A wrapper for the \code{baseline.fillPeaks} in the \code{baseline} package.
#'
#' @param xrd an xy data frame of the data to fit a background to. First column
#' is the 2theta scale, second column is count intensities
#' @param lambda 2nd derivative penalty for primary smoothing
#' @param hwi Half width of local windows
#' @param it Number of iterations in suppression loop
#' @param int Number of buckets to divide the data into
#'
#' @return a list of 3 vectors
#' \item{tth}{The 2theta axis of the measurement}
#' \item{counts}{The count intensities of the measurement}
#' \item{background}{The fitted background}
#'
#' @examples
#' data(soils)
#' fit_bkg <- bkg(soils$granite)
#' @export
bkg <- function(xrd, lambda, hwi, it, int) {

  if(missing(lambda)) {
    lambda <- 0.5
  }

  if(missing(hwi)) {
    hwi <- 50
  }

  if(missing(it)) {
    it <- 25
  }

  if(missing(int)) {
    int <- 1000
  }

  fp <- baseline::baseline(t(xrd[2]),
                 lambda = lambda,
                 hwi = hwi, it = it,
                 int = int, method='fillPeaks')

  out <- list("tth" = xrd[,1],
            "counts" = xrd[,2],
            "background" = c(fp@baseline))

  class(out) <- "powdRbkg"

  return(out)
}
