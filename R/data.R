#' Example soil XRPD data
#'
#' 3 soil samples from different parent materials measured by XRPD
#'
#' @format A list of 3 dataframes (named according to rock type),
#' with each dataframe containing two columns of:
#' \describe{
#' \item{tth}{The 2theta measurement intervals}
#' \item{counts}{The count intensities}
#' }
"soils"

#' An example powdRlib reference library
#'
#' This \code{powdRlib} object, built using the \code{powdRlib} constructor function,
#' contains a range of measured XRPD data along with their reference intensity ratios.
#' The library can be used with the \code{soils} example data for full pattern summation.
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

#' A table of 14 reference patterns and their corresponding two theta scale that can
#' be combined with the \code{minerals_phases} table to create a \code{powdRlib} object when using
#' the \code{powdRlib} constructor function. Use the same layout to create custom
#' reference libraries.
#'
#' @format A dataframe
#' \describe{
#' The first column defines the two theta scale, and remaining columns are individual
#' reference patterns of pure minerals or amorphous phases. Each column title
#' should be a unique mineral ID
#' }
"minerals_xrd"

#' A table of associated data for the minerals_xrd table, which can be be combined with a
#' xrd data table to create a \code{powdRlib} object when using the
#' \code{powdRlib} constructor function. Use the same layout to create custom reference
#' libraries.
#'
#' @format A 3 column dataframe
#' \describe{
#' The first column is a character string defining the unique mineral ID's that
#' should match those defined as column names of the minerals table
#' (e.g. \code{minerals_xrd}).
#'
#' The second column is a character string defining the mineral group that each
#' reference pattern belongs to.
#'
#' The third column is a numeric vector defining the reference intensity ratios
#' of each reference pattern.
#' }
"minerals_phases"

#' RockJock reference library
#'
#' A \code{powdRlib} object of 168 pure reference patterns from the RockJock
#' library along with reference intensity ratios. Can be used with the \code{fps()}
#' and \code{afps()} functions for quantitative analysis.
#'
#' @format A list of 3 components
#' \describe{
#' \item{xrd}{A dataframe of all xrd data (counts only). Column names denote the
#' reference sample}
#' \item{tth}{A vector of 2theta intervals of all measurements in the library}
#' \item{phases}{A dataframe the phase ID's, names and reference intensity
#' ratios (RIR)}
#' }
#'
#' @author Dennis Eberl
#' @references \url{https://pubs.er.usgs.gov/publication/ofr200378}
"rockjock"
