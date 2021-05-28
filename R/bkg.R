#' Fit a background to XRPD data
#'
#' \code{bkg} fits a background to X-Ray Powder Diffraction data
#'
#' A wrapper for the \code{baseline.fillPeaks} in the \code{baseline} package.
#'
#' @param xrd an xy data frame of the data to fit a background to. First column
#' is the 2theta scale, second column is count intensities
#' @param lambda second derivative penalty for primary smoothing. Default = 0.5.
#' @param hwi Half width of local windows. Default = 25.
#' @param it Number of iterations in suppression loop. Default = 50.
#' @param int Number of buckets to divide the data into. Default = \code{round(nrow(xrd)/4)}.
#'
#' @return a list of 3 vectors
#' \item{tth}{The 2theta axis of the measurement}
#' \item{counts}{The count intensities of the measurement}
#' \item{background}{The fitted background}
#'
#' @examples
#' data(soils)
#' \dontrun{
#' fit_bkg <- bkg(soils$granite)
#' }
#' @export
bkg <- function(xrd, lambda, hwi, it, int) {

  if (utils::packageVersion("baseline") > "1.2.1") {

    stop("The bkg function crashes when using baseline versions > 1.2.1.
         Please install version 1.2.1 of the baseline package from source using:
         install.packages('http://cran.r-project.org/src/contrib/Archive/baseline/baseline_1.2-1.tar.gz',
         repos=NULL, type='source')")

  }

  if(missing(lambda)) {
    lambda <- 0.5
  }

  if(missing(hwi)) {
    hwi <- 25
  }

  if(missing(it)) {
    it <- 50
  }

  if(missing(int)) {
    int <- round(nrow(xrd)/4, 0)
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
