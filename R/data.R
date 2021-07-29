#' Example soil XRPD data
#'
#' 3 soil samples from different parent materials measured by XRPD (Cu K-alpha radiation)
#'
#' @format A multiXY list of 3 XY dataframes (named according to parent material type),
#' with each XY dataframe containing two columns of:
#' \describe{
#' \item{tth}{The 2theta measurement intervals}
#' \item{counts}{The count intensities}
#' }
"soils"

#' An example powdRlib reference library
#'
#' This \code{powdRlib} object, built using the \code{powdRlib} constructor function,
#' contains a range of measured XRPD data (collected using Cu K-alpha radiation) along with
#' their reference intensity ratios. The library is designed for simple examples only and
#' can be used with the \code{soils} data for relatively fast tests of \code{fps} and
#' \code{afps}.
#'
#' @format A powdRlib object of 3 components
#' \describe{
#' \item{xrd}{A dataframe of all the count intensities of all reference patterns. Column names denote the
#' unique phase ID of each reference pattern}
#' \item{tth}{A vector of the 2theta scale for all reference patterns in the library}
#' \item{phases}{A dataframe the phase IDs, names and reference intensity
#' ratios (RIR)}
#' }
"minerals"


#' Example regrouping structure for the \code{minerals} data
#'
#' @format A 2 column data frame.
#' \describe{First column contains the unique phase IDs of all phases
#' in the \code{minerals} data. Second column contains the grouping structure for the data
#' (Non-clay, Clay or Amorphous).}
"minerals_regroup_structure"

#' Example xrd table for a reference library
#'
#' A table of 14 reference patterns and their corresponding two theta scale that can
#' be combined with the \code{minerals_phases} table to create a \code{powdRlib} object using
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

#' Example phases table for a reference library
#'
#' A data frame of associated phase information for the \code{minerals_xrd} data.
#' Together these two data frames can be combined with the \code{powdRlib} constructor
#' function to create an example reference library (see \code{?powdRlib}). Use the same
#' layout to create custom reference libraries.
#'
#' @format A 3 column data frame consisting of:
#' \describe{
#' \item{phase_id}{A string defining the unique phase IDs that
#' should match those defined as column names of the minerals table
#' (e.g. \code{minerals_xrd}).}
#' \item{phase_name}{A string defining the mineral group that each
#' reference pattern belongs to.}
#' \item{rir}{A vector defining the reference intensity ratios
#' of each reference pattern.}
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
#' @format A powdRlib object of 3 components
#' \describe{
#' \item{xrd}{A dataframe of all the count intensities of all reference patterns. Column names denote the
#' unique phase ID of each reference pattern}
#' \item{tth}{A vector of the 2theta scale for all reference patterns in the library}
#' \item{phases}{A dataframe the phase IDs, names and reference intensity
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
#' A multiXY list containing 8 XRPD measurements (Cu K-alpha radiation) of synthetic mixtures
#' that can be used to assess accuracy of quantitative analysis from the \code{fps()}
#' and \code{afps()} functions. The mixtures contain various amounts of quartz (QUARTZ
#' standard of the \code{rockjock} library), K-feldspar (ORDERED_MICROCLINE),
#' plagioclase (LABRADORITE), kaolinite (KAOLINITE_DRY_BRANCH), dioctahedral smectite
#' (MONTMORILLIONITE_WYO), illite (ILLITE_1M_RM30) and corundum (CORUNDUM).
#'
#' @format A multiXY list of 8 components, each comprised of two columns. Column \code{tth} specifies
#' the 2theta axis and \code{counts} specifies the count intensities. The mixtures have to following
#' compositions that are also tabulated in the \code{rockjock_weights} data.
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

#' Original codes for the \code{afsis} reference patterns
#'
#' A data frame detailing the original codes associated with the \code{afsis} reference
#' patterns prior to their addition to powdR.
#'
#' @format An 2 column data frame. First column contains the phase IDs from \code{afsis$phase_id}
#' and the second column the original IDs prior to the inclusion in powdR.
#'
"afsis_codes"

#' Africa Soil Information Service (AfSIS) XRPD reference library
#'
#' A \code{powdRlib} object of 21 pure reference patterns and associated reference
#' intensity ratios for a range of common soil minerals. Data were collected on a Bruker D2
#' Phaser using Cu K-alpha radiation. All patterns have been normalised to 10,000
#' counts and reference intensity ratios transformed so that all are relative
#' to that of corundum.
#'
#' @format A powdRlib object of 3 components
#' \describe{
#' \item{xrd}{A dataframe of all the count intensities of all reference patterns. Column names denote the
#' unique phase ID of each reference pattern}
#' \item{tth}{A vector of the 2theta scale for all reference patterns in the library}
#' \item{phases}{A dataframe the phase IDs, names and reference intensity
#' ratios (RIR)}
#' }
#'
"afsis"

#' Regrouping structure for the Africa Soil Information Service (AfSIS) XRPD
#' reference library
#'
#' A data frame containing an example re-grouping structure for the \code{afsis}
#' reference library, which results in a slightly coarser description of clay
#' minerals and Fe/Ti-(hydr)oxides in \code{powdRfps} or \code{powdRafps} objects
#' when used with \code{regroup()}.
#'
#' @format A data frame. First column contains the phase IDs present in
#' \code{afsis$phases$phase_id}. Second column contains the phase names that
#' constitute the regrouping structure.
#'
"afsis_regroup"

#' Regrouping structure for the rockjock reference library
#'
#' A data frame containing an example re-grouping structure for the \code{rockjock}
#' reference library, which results in a slightly coarser description of clay
#' minerals and Fe/Ti-(hydr)oxides in \code{powdRfps} or \code{powdRafps} objects
#' when used with \code{regroup()}.
#'
#' @format A data frame
#'
"rockjock_regroup"
