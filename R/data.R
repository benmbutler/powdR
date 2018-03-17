#' Example soil XRPD data
#'
#' 3 soil samples from different parent materials measured by XRPD on a
#' Bruker D8.
#'
#' @format A list of 3 dataframes (named according to rock type),
#' with each dataframe containing two columns of:
#' \describe{
#' \item{tth}{The 2theta measurement intervals}
#' \item{counts}{The count intensities}
#' }
"soils"

#' A selection of pure minerals measured on a Bruker D8 arranged into a reference
#' library.
#'
#' The reference library contains a range of measured XRPD data along with reference
#' intensity ratios needed for full pattern fitting of the \code{soils} data.
#'
#' @format A list of 3
#' \describe{
#' \item{xrd}{A dataframe of all xrd data (counts only). Column names denote the
#' reference sample}
#' \item{tth}{A vector of 2theta intervals of all measurements in the library}
#' \item{minerals}{A dataframe the mineral ID's, names and RIR's}
#' }
"minerals"
