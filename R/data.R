#' D8 library of XRPD patterns
#'
#' The D8 library contains all the reference patterns and reference intensity
#' ratios needed for full pattern fitting D8 data.
#'
#' @format A list of 3
#' \describe{
#' \item{XRD}{A dataframe of all xrd data. Column names denote the reference sample}
#' \item{TTH}{A vector of 2theta intervals of all measurements in the library}
#' \item{MINERALS}{A dataframe the mineral ID's, names and RIR's}
#' }
"D8"

#' D5000 library of XRPD patterns
#'
#' The D5000 library contains all the reference patterns and reference intensity
#' ratios needed for full pattern fitting D5000 data.
#'
#' @format A list of 3
#' \describe{
#' \item{XRD}{A dataframe of all xrd data. Column names denote the reference sample}
#' \item{TTH}{A vector of 2theta intervals of all measurements in the library}
#' \item{MINERALS}{A dataframe the mineral ID's, names and RIR's}
#' }
"D5000"

#' Xpert library of XRPD patterns
#'
#' The Xpert library contains all the reference patterns and reference intensity
#' ratios needed for full pattern fitting Xpert data.
#'
#' @format A list of 3
#' \describe{
#' \item{XRD}{A dataframe of all xrd data. Column names denote the reference sample}
#' \item{TTH}{A vector of 2theta intervals of all measurements in the library}
#' \item{MINERALS}{A dataframe the mineral ID's, names and RIR's}
#' }
"Xpert"

#' Soil samples measured by Xpert
#'
#' Two soil samples measured on a Panalytical Xpert. These are used in examples
#'
#' @format A list of 2 xy dataframes
#' \describe{
#' \item{organic}{An organic soil measured by XRPD}
#' \item{mineral}{A mineral soil measured by XRPD}
#' }
"Xpert_soil"

#' Xpert library of XRPD patterns relevant to NSIS soils
#'
#' The Xpert library contains all the reference patterns and reference intensity
#' ratios needed for full pattern fitting of the NSIS Xpert data.
#'
#' @format A list of 3
#' \describe{
#' \item{XRD}{A dataframe of all xrd data. Column names denote the reference sample}
#' \item{TTH}{A vector of 2theta intervals of all measurements in the library}
#' \item{MINERALS}{A dataframe the mineral ID's, names and RIR's}
#' }
"Xpert_NSIS"