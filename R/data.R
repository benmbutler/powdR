#' Example soil XRPD data
#'
#' 3 soil samples from different parent materials measured by XRPD (Cu K-alpha radiation)
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
#' contains a range of measured XRPD data (Cu K-alpha radiation) along with their reference
#' intensity ratios. The library can be used with the \code{soils} example data for full
#' pattern summation.
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


#' Example regrouping structure for the \code{minerals} data
#'
#' @format A 2 column data frame.
#' \describe{First column contains the unique phase ID's of all phases
#' in the \code{minerals} data. Second column contains the grouping structure for the data
#' (Non-clay, Clay or Amorphous).}
"minerals_regroup_structure"

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
#' library (Cu K-alpha radiation) along with reference intensity ratios. Note that compared
#' to same library supplied with RockJock the powdR patterns have been normalised to 10,000
#' counts and reference intensity ratios transformed so that all are relative
#' to that of corundum, which has been set to a value of 1.0.
#' Can be used with the \code{fps()} and \code{afps()} functions for quantitative
#' analysis. Example mixtures for testing the \code{rockjock} library with known
#' concentrations are available in the \code{rockjock_mixtures} data. See
#' \code{?rockjock_mixtures}.
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
#' @references
#' Eberl, D.D., 2003. User's guide to RockJock - A program for determining quantitative mineralogy from
#' powder X-ray diffraction data. Boulder, CA.
"rockjock"

#' RockJock synthetic mixtures
#'
#' A list containing 8 XRPD measurements (Cu K-alpha radiation) of synthetic mixtures
#' that can be used to assess accuracy of quantitative analysis from the \code{fps()}
#' and \code{afps()} functions. The mixtures contain various amounts of quartz (QUARTZ
#' standard in of the \code{rockjock} library), K-feldspar (ORDERED_MICROCLINE),
#' plagioclase (LABRADORITE), kaolinite (KAOLINITE_DRY_BRANCH), dioctahedral smectite
#' (MONTMORILLIONITE_WYO), illite (ILLITE_1M_RM30) and corundum (CORUNDUM).
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
#' @references
#' Eberl, D.D., 2003. User's guide to RockJock - A program for determining quantitative mineralogy from
#' powder X-ray diffraction data. Boulder, CA.
"rockjock_mixtures"

#' PCA loadings of \code{rockjock_mixtures} data
#'
#' A list containing 3 loadings of Dimensions 1 to 3 from principal components analysis of
#' the \code{rockjock_mixtures} data.
#'
#' @format A list of 3 components, each comprised of two columns. Column \code{tth} specifies
#' the 2theta axis and \code{loading} specifies the count intensities.
"rockjock_loadings"

#' Mineral concentrations of the \code{rockjock_mixtures} data
#'
#' A dataframe summarising the weighed mineral concentrations of the \code{rockjock_mixtures} data,
#' all in units of weight percent.
#'
#' @format An 8 column dataframe, with each row detailing the composition of a sample.
#'
#' @author Dennis Eberl
#' @references
#' Eberl, D.D., 2003. User's guide to RockJock - A program for determining quantitative mineralogy from
#' powder X-ray diffraction data. Boulder, CA.
"rockjock_weights"
