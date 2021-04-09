#' Close the phase concentration data within a \code{powdRfps} or
#' \code{powdRafps} object
#'
#' \code{close_quant} closes the quantitative data witih a \code{powdRfps} or
#' \code{powdRafps} object (derived from \code{fps()} and \code{afps()}, respectively) by
#' ensuring that the composition sums to 100 percent. See \code{?close_quant.powdRfps} and
#' \code{?close_quant.powdRafps}.
#'
#' @param x A \code{powdRfps} or \code{powdRafps} object..
#' @param ... other arguments
#'
#' @return a \code{powdRfps} or \code{powdRafps} object with components:
#' \item{tth}{a vector of the 2theta scale of the fitted data}
#' \item{fitted}{a vector of the fitted XRPD pattern}
#' \item{measured}{a vector of the original XRPD measurement (aligned)}
#' \item{residuals}{a vector of the residuals (fitted vs measured)}
#' \item{phases}{a dataframe of the phases used to produce the fitted pattern}
#' \item{phases_grouped}{the phases dataframe grouped by phase_name and summed}
#' \item{obj}{named vector of the objective parameters summarising the quality of the fit}
#' \item{weighted_pure_patterns}{a dataframe of reference patterns used to produce the fitted pattern.
#' All patterns have been weighted according to the coefficients used in the fit}
#' \item{coefficients}{a named vector of coefficients used to produce the fitted pattern}
#' \item{inputs}{a list of input arguments used in the function call}
#'
#' @examples
#' \dontrun{
#' data(rockjock)
#' data(rockjock_mixtures)
#'
#' rockjock_1 <- fps(lib = rockjock,
#'                   smpl = rockjock_mixtures$Mix1,
#'                   refs = c("ORDERED_MICROCLINE",
#'                            "LABRADORITE",
#'                            "KAOLINITE_DRY_BRANCH",
#'                            "MONTMORILLONITE_WYO",
#'                            "ILLITE_1M_RM30",
#'                            "CORUNDUM"),
#'                  std = "CORUNDUM",
#'                  align = 0.3,
#'                  std_conc = 20)
#'
#' rockjock_1c <- close_quant(rockjock_1)
#'
#' rockjock_a1 <- afps(lib = rockjock,
#'                     smpl = rockjock_mixtures$Mix1,
#'                     std = "CORUNDUM",
#'                     align = 0.3,
#'                     lod = 1,
#'                     std_conc = 20)
#'
#' rockjock_a1c <- close_quant(rockjock_a1)
#'
#' }
#' @export
close_quant <- function(x, ...) {
  UseMethod("close_quant")
}

#' Close the phase concentration data within a \code{powdRfps} object
#'
#' \code{close_quant} closes the quantitative data within a \code{powdRfps} object
#' (derived from \code{fps()}) by ensuring that the composition sums to 100 percent.
#'
#' @param x A \code{powdRfps} object derived from \code{fps()}.
#' @param ... other arguments
#'
#' @return a \code{powdRfps} object with components:
#' \item{tth}{a vector of the 2theta scale of the fitted data}
#' \item{fitted}{a vector of the fitted XRPD pattern}
#' \item{measured}{a vector of the original XRPD measurement (aligned)}
#' \item{residuals}{a vector of the residuals (fitted vs measured)}
#' \item{phases}{a dataframe of the phases used to produce the fitted pattern and their concentrations}
#' \item{phases_grouped}{the phases dataframe grouped by phase_name and concentrations summed}
#' \item{obj}{named vector of the objective parameters summarising the quality of the fit}
#' \item{weighted_pure_patterns}{a dataframe of reference patterns used to produce the fitted pattern.
#' All patterns have been weighted according to the coefficients used in the fit}
#' \item{coefficients}{a named vector of coefficients used to produce the fitted pattern}
#' \item{inputs}{a list of input arguments used in the function call}
#'
#' @examples
#' \dontrun{
#' data(rockjock)
#' data(rockjock_mixtures)
#'
#' rockjock_1 <- fps(lib = rockjock,
#'                   smpl = rockjock_mixtures$Mix1,
#'                   refs = c("ORDERED_MICROCLINE",
#'                            "LABRADORITE",
#'                            "KAOLINITE_DRY_BRANCH",
#'                            "MONTMORILLONITE_WYO",
#'                            "ILLITE_1M_RM30",
#'                            "CORUNDUM"),
#'                  std = "CORUNDUM",
#'                  align = 0.3,
#'                  std_conc = 20)
#'
#' rockjock_1c<- close_quant(rockjock_1)
#'
#' }
#' @export
close_quant.powdRfps <- function(x, ...) {

  if (sum(x$phases$phase_percent) == 100) {

    stop("The phase concentration data already sum to 100 percent.",
         call. = FALSE)

  }

  x$phases$phase_percent <- (x$phases$phase_percent/sum(x$phases$phase_percent)) * 100
  x$phases_grouped$phase_percent <- (x$phases_grouped$phase_percent/
                                       sum(x$phases_grouped$phase_percent)) * 100

  x$inputs$closed <- TRUE

  return(x)

}

#' Close the phase concentration data within a \code{powdRafps} object
#'
#' \code{close_quant} closes the quantitative data within a \code{powdRafps} object
#' (derived from \code{afps()}) by ensuring that the composition sums to 100 percent.
#'
#' @param x A \code{powdRafps} object derived from \code{afps()}.
#' @param ... other arguments
#'
#' @return a \code{powdRafps} object with components:
#' \item{tth}{a vector of the 2theta scale of the fitted data}
#' \item{fitted}{a vector of the fitted XRPD pattern}
#' \item{measured}{a vector of the original XRPD measurement (aligned)}
#' \item{residuals}{a vector of the residuals (fitted vs measured)}
#' \item{phases}{a dataframe of the phases used to produce the fitted pattern and their concentrations}
#' \item{phases_grouped}{the phases dataframe grouped by phase_name and concentrations summed}
#' \item{obj}{named vector of the objective parameters summarising the quality of the fit}
#' \item{weighted_pure_patterns}{a dataframe of reference patterns used to produce the fitted pattern.
#' All patterns have been weighted according to the coefficients used in the fit}
#' \item{coefficients}{a named vector of coefficients used to produce the fitted pattern}
#' \item{inputs}{a list of input arguments used in the function call}
#'
#' @examples
#' \dontrun{
#' data(rockjock)
#' data(rockjock_mixtures)
#'
#' rockjock_a1 <- afps(lib = rockjock,
#'                     smpl = rockjock_mixtures$Mix1,
#'                     std = "CORUNDUM",
#'                     align = 0.3,
#'                     lod = 1,
#'                     std_conc = 20)
#'
#' rockjock_a1c <- close_quant(rockjock_a1)
#'
#' }
#' @export
close_quant.powdRafps <- function(x, ...) {

  if (sum(x$phases$phase_percent) == 100) {

    stop("The phase concentration data already sum to 100 percent.",
         call. = FALSE)

  }

  x$phases$phase_percent <- (x$phases$phase_percent/sum(x$phases$phase_percent)) * 100
  x$phases_grouped$phase_percent <- (x$phases_grouped$phase_percent/
                                       sum(x$phases_grouped$phase_percent)) * 100

  x$inputs$closed <- TRUE

  return(x)

}

