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

#' D8 library of XRPD patterns relevant to NSIS soils
#'
#' The D8 library contains all the reference patterns and reference intensity
#' ratios needed for full pattern fitting of the NSIS D8 data.
#'
#' @format A list of 3
#' \describe{
#' \item{XRD}{A dataframe of all xrd data. Column names denote the reference sample}
#' \item{TTH}{A vector of 2theta intervals of all measurements in the library}
#' \item{MINERALS}{A dataframe the mineral ID's, names and RIR's}
#' }
"D8_NSIS"

#' Soil samples measured by D8
#'
#' Two soil samples measured on a Bruker D8. These are used in examples
#'
#' @format A list of 2 xy dataframes
#' \describe{
#' \item{organic}{An organic soil measured by XRPD}
#' \item{mineral}{A mineral soil measured by XRPD}
#' }
"D8_soil"

#' NSIS topsoil samples
#'
#' 204 samples measured on Bruker D8. Data have had aluminium signal
#' stripped from them, which was an artefact of the sample holders used
#' on this instrument.
#'
#' @format A list of 204 dataframes (named according to the sample ID's),
#' with each dataframe containing two columns of:
#' \describe{
#' \item{tth}{The 2theta measurement intervals}
#' \item{counts}{The count intensities}
#' }
"topsoils"
