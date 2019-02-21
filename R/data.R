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

#' RockJock synthetic mixtures
#'
#' A list containing 8 XRPD measurements of synthetic mixtures that can be used to
#' assess accuracy of quantitative analysis from the \code{fps()} and \code{afps()} functions.
#'
#' @format A list of 8 components, each comprised of two columns. Column \code{tth} specifies
#' the 2theta axis and \code{counts} specifies the count intensities
#' \describe{
#' \item{Mix1}{Contains: 4 \% K-feldspar, 20 \% plagioclase, 12 \% kaolinite, 36 \% dioctahedral
#' smectite, 8 \% illite and 20 \% corundum.}
#' \item{Mix2}{Contains: 4 \% quartz, 8 \% K-feldspar, 36 \% plagioclase, 20 \% kaolinite,
#' 12 \% illite and 20 \% corundum.}
#' \item{Mix3}{Contains: 8 \% quartz, 12 \% K-feldspar, 36 \% kaolinite, 4 \% dioctahedral
#' smectite, 20 \% illite and 20 \% corundum.}
#' \item{Mix4}{Contains: 12 \% quartz, 20 \% K-feldspar, 4 \% plagioclase, 8 \% dioctahedral
#' smectite, 36 \% illite and 20 \% corundum.}
#' \item{Mix5}{Contains: 20 \% quartz, 36 \% K-feldspar, 8 \% plagioclase, 4 \% kaolinite,
#' 12 \% dioctahedral smectite and 20 \% corundum.}
#' \item{Mix6}{Contains: 36 \% quartz, 12 \% plagioclase, 8 \% kaolinite, 20 \% dioctahedral
#' smectite, 4 \% illite and 20 \% corundum.}
#' \item{Mix7}{Contains: 8 \% K-feldspar, 40 \% plagioclase, 4 \% kaolinite, 12 \% dioctahedral
#' smectite, 16 \% illite and 20 \% corundum.}
#' \item{Mix8}{Contains: 8 \% quartz, 4 \% K-feldspar, 4 \% plagioclase, 24 \% dioctahedral
#' smectite, 40 \% illite and 20 \% corundum.}
#' }
#'
#' @author Dennis Eberl
#' @references \url{https://pubs.er.usgs.gov/publication/ofr200378}
"rockjock_mixtures"
