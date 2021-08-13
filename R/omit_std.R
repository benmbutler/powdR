#' Omit the internal standard from phase concentration data within a \code{powdRfps} or
#' \code{powdRafps} object
#'
#' \code{omit_std} adjusts phase concentrations in a \code{powdRfps} or \code{powdRafps} object
#' (derived from \code{fps()} and \code{afps()}, respectively) by removing the concentrations of
#' the internal standard. Relevant information for the calculation is automatically
#'  extracted from \code{x$inputs$std} and \code{x$inputs$std_conc}. For more information see
#'  \code{?omit_std.powdRfps} and \code{omit_std.powdRafps}.
#'
#' @param x A \code{powdRfps} or \code{powdRafps} object..
#' @param ... other arguments
#'
#' @return a \code{powdRfps} or \code{powdRafps} object with components:
#' \item{tth}{a vector of the 2theta scale of the fitted data}
#' \item{fitted}{a vector of the fitted XRPD pattern}
#' \item{measured}{a vector of the original XRPD measurement (aligned and harmonised)}
#' \item{residuals}{a vector of the residuals (measured minus fitted)}
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
#' rockjock_1o <- omit_std(rockjock_1)
#'
#' rockjock_a1 <- afps(lib = rockjock,
#'                     smpl = rockjock_mixtures$Mix1,
#'                     std = "CORUNDUM",
#'                     align = 0.3,
#'                     lod = 1,
#'                     std_conc = 20)
#'
#' rockjock_a1o <- omit_std(rockjock_a1)
#'
#' }
#' @export
omit_std <- function(x, ...) {
  UseMethod("omit_std")
}

#' Omit the internal standard from phase concentration data within a \code{powdRfps} object
#'
#' \code{omit_std.powdRfps} adjusts phase concentrations in a \code{powdRfps} object by removing
#' the concentrations of the internal standard. Relevant information for the calculation is
#' automatically extracted from \code{x$inputs$std} and \code{x$inputs$std_conc}.
#'
#' @param x A \code{powdRfps} object derived from \code{fps()}.
#' @param ... other arguments
#'
#' @return a \code{powdRfps} object with components:
#' \item{tth}{a vector of the 2theta scale of the fitted data}
#' \item{fitted}{a vector of the fitted XRPD pattern}
#' \item{measured}{a vector of the original XRPD measurement (aligned and harmonised)}
#' \item{residuals}{a vector of the residuals (measured minus fitted)}
#' \item{phases}{a dataframe of the phases used to produce the fitted pattern and their concentrations}
#' \item{phases_grouped}{the phases dataframe grouped by phase_name and concentrations summed}
#' \item{obj}{named vector of the objective parameters summarising the quality of the fit}
#' \item{weighted_pure_patterns}{a dataframe of reference patterns used to produce the fitted pattern.
#' All patterns have been weighted according to the coefficients used in the fit}
#' \item{coefficients}{a named vector of coefficients used to produce the fitted pattern}
#' \item{inputs}{a list of input arguments used in the function call}
#'
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
#' rockjock_1o <- omit_std(rockjock_1)
#'
#' }
#' @export
omit_std.powdRfps <- function(x, ...) {

  if (!"omit_std" %in% names(x$inputs)) {

    stop("The powdRfps object was derived using an old version of powdR. Please compute the
         the output again using at least version 1.3.",
         call. = FALSE)

  }

  if (x$inputs$omit_std == TRUE) {

    stop("The internal standard has already been omitted from these results.",
         call. = FALSE)

  }

  if (is.na(x$inputs$std_conc)) {

    stop("Cannot recompute phase concentrations because the std_conc supplied to
         derive the powdRfps object is NA",
         call. = FALSE)

  }

  std <- x$inputs$std
  std_name <- x$phases$phase_name[which(x$phases$phase_id == std)]

  std_conc <- x$inputs$std_conc

  x$phases$phase_percent[which(x$phases$phase_name == std_name)] <- NA
  x$phases_grouped$phase_percent[which(x$phases_grouped$phase_name == std_name)] <- NA

  x$phases$phase_percent <- (x$phases$phase_percent/(100-std_conc)) * 100
  x$phases_grouped$phase_percent <- (x$phases_grouped$phase_percent/(100-std_conc)) * 100

  x$inputs$omit_std <- TRUE

  return(x)

}

#' Omit the internal standard from phase concentration data within a \code{powdRafps} object
#'
#' \code{omit_std.powdRafps} adjusts phase concentrations in a \code{powdRafps} object by removing
#' the concentrations of the internal standard. Relevant information for the calculation is
#' automatically extracted from \code{x$inputs$std} and \code{x$inputs$std_conc}.
#'
#' @param x A \code{powdRafps} object derived from \code{afps()}.
#' @param ... other arguments
#'
#' @return a \code{powdRafps} object with components:
#' \item{tth}{a vector of the 2theta scale of the fitted data}
#' \item{fitted}{a vector of the fitted XRPD pattern}
#' \item{measured}{a vector of the original XRPD measurement (aligned and harmonised)}
#' \item{residuals}{a vector of the residuals (measured minus fitted)}
#' \item{phases}{a dataframe of the phases used to produce the fitted pattern and their concentrations}
#' \item{phases_grouped}{the phases dataframe grouped by phase_name and concentrations summed}
#' \item{obj}{named vector of the objective parameters summarising the quality of the fit}
#' \item{weighted_pure_patterns}{a dataframe of reference patterns used to produce the fitted pattern.
#' All patterns have been weighted according to the coefficients used in the fit}
#' \item{coefficients}{a named vector of coefficients used to produce the fitted pattern}
#' \item{inputs}{a list of input arguments used in the function call}
#'
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
#' rockjock_a1o <- omit_std(rockjock_a1)
#'
#' }
#' @export
omit_std.powdRafps <- function(x, ...) {

  if (!"omit_std" %in% names(x$inputs)) {

    stop("The powdRafps object was derived using an old version of powdR. Please compute the
         the output again using at least version 1.3.",
         call. = FALSE)

  }

  if (x$inputs$omit_std == TRUE) {

    stop("The internal standard has already been omitted from these results.",
         call. = FALSE)

  }

  if (is.na(x$inputs$std_conc)) {

    stop("Cannot recompute phase concentrations because the std_conc supplied to
         derive the powdRafps object is NA",
         call. = FALSE)

  }

  std <- x$inputs$std
  std_name <- x$phases$phase_name[which(x$phases$phase_id == std)]

  std_conc <- x$inputs$std_conc

  x$phases$phase_percent[which(x$phases$phase_name == std_name)] <- NA
  x$phases_grouped$phase_percent[which(x$phases_grouped$phase_name == std_name)] <- NA

  x$phases$phase_percent <- (x$phases$phase_percent/(100-std_conc)) * 100
  x$phases_grouped$phase_percent <- (x$phases_grouped$phase_percent/(100-std_conc)) * 100

  x$inputs$omit_std <- TRUE

  return(x)

}
