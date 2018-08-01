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

#' A selection of pure minerals measured on a Bruker D8 arranged into a powdRlib
#' S3 object
#'
#' The reference library contains a range of measured XRPD data along with reference
#' intensity ratios needed for full pattern fitting of the \code{soils} data.
#'
#' @format A list of 3
#' \describe{
#' \item{xrd}{A dataframe of all xrd data (counts only). Column names denote the
#' reference sample}
#' \item{tth}{A vector of 2theta intervals of all measurements in the library}
#' \item{phases}{A dataframe the phase ID's, names and reference intensity
#' ratios (RIR)}
#' }
"minerals"

#' A table of 13 reference patterns and their corresponding two theta scale that can
#' be combined with a meta data table to create a powdRlib object when using
#' the \code{create_xrd_lib} function. Use the same layout to create custom
#' reference libraries.
#'
#' @format A dataframe
#' \describe{
#' The first column defines the two theta scale, and remaining columns individual
#' reference patterns of pure minerals or amorphous phases. Each column title
#' should be a unique mineral ID
#' }
"minerals_xrd"

#' A table of associated data for the minerals_xrd table, which can be be combined with a
#' xrd data table to create a powdRlib object when using the
#' \code{create_xrd_lib} function. Use the same layout to create custom reference
#' libraries.
#'
#' @format A 3 column dataframe
#' \describe{
#' The first column is a character string defining the unique mineral ID's that
#' should match those defined as column names of the minerals table.
#'
#' The second column is a character string defining the mineral group that each
#' reference pattern belongs to.
#'
#' The third column is a numeric vector defining the reference intensity ratios
#' of each reference pattern.
#' }
"minerals_phases"
