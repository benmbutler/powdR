#' regroup
#'
#' \code{regroup} allows an alternative grouping structure to be applied to \code{powdRfps}
#' and \code{powdRafps} objects. For more details see \code{?regroup.powdRfps} or
#' \code{?regroup.powdRafps}.
#'
#' \code{powdRfps} and \code{powdRafps} objects contain a data frame called \code{phases_grouped}
#' that summarises phase concentrations based on defined mineral groups from the \code{powdRlib}
#' reference library. \code{regroup} allows you to change this grouping structure by supplying
#' new group identities.
#'
#' @param x A \code{powdRfps} or \code{powdRafps} object
#' @param ... Other parameters passed to methods e.g. \code{fps.powdRlib}
#'
#' @return a \code{powdRfps} or \code{powdRafps} object with components:
#' \item{tth}{a vector of the 2theta scale of the fitted data}
#' \item{fitted}{a vector of the count intensities of fitted XRPD pattern}
#' \item{measured}{a vector of the count intensities of original XRPD measurement (aligned)}
#' \item{residuals}{a vector of the residuals (measured minus fitted)}
#' \item{phases}{a dataframe of the phases used to produce the fitted pattern}
#' \item{phases_grouped}{the phases dataframe grouped and summed by phase_name}
#' \item{obj}{named vector of the objective parameters summarising the quality of the fit}
#' \item{weighted_pure_patterns}{a dataframe of reference patterns used to produce the fitted pattern.
#' All patterns have been weighted according to the coefficients used in the fit}
#' \item{coefficients}{a named vector of coefficients used to produce the fitted pattern}
#' \item{inputs}{a list of input arguments used in the function call}
#'
#' @examples
#' #Load the minerals library
#' data(minerals)
#'
#' #Load the soils data
#' data(soils)
#'
#' #Load the regrouping structure
#' data(minerals_regroup_structure)
#'
#' \dontrun{
#' fps_sandstone <- fps(lib = minerals,
#'                      smpl = soils$sandstone,
#'                      refs = minerals$phases$phase_id,
#'                      std = "QUA.1",
#'                      align = 0.2)
#'
#' fps_sandstone_regrouped <- regroup(fps_sandstone,
#'                                    minerals_regroup_structure)
#'
#' fps_sandstone_regrouped$phases_grouped
#'
#' }
#' @export
regroup <- function(x, ...) {
  UseMethod("regroup")
}


#' regroup
#'
#' \code{regroup.powdRfps} allows an alternative grouping structure to be applied to \code{powdRfps}
#' objects.
#'
#' \code{powdRfps} objects contain a data frame called \code{phases_grouped}
#' that summarises phase concentrations based on defined mineral groups from the \code{powdRlib}
#' reference library. \code{regroup} allows you to change this grouping structure by supplying
#' new group identities.
#'
#' @param x A \code{powdRfps} object
#' @param y A data frame. First column contains the phase IDs covering all those present in
#' \code{x$phases$phase_id}. Second column contains the desired grouping of each phase.
#' @param ... other arguments
#'
#' @return a \code{powdRfps} object with components:
#' \item{tth}{a vector of the 2theta scale of the fitted data}
#' \item{fitted}{a vector of the count intensities of fitted XRPD pattern}
#' \item{measured}{a vector of the count intensities of original XRPD measurement (aligned)}
#' \item{residuals}{a vector of the residuals (measured minus fitted)}
#' \item{phases}{a dataframe of the phases used to produce the fitted pattern}
#' \item{phases_grouped}{the phases dataframe grouped and summed by phase_name}
#' \item{obj}{named vector of the objective parameters summarising the quality of the fit}
#' \item{weighted_pure_patterns}{a dataframe of reference patterns used to produce the fitted pattern.
#' All patterns have been weighted according to the coefficients used in the fit}
#' \item{coefficients}{a named vector of coefficients used to produce the fitted pattern}
#' \item{inputs}{a list of input arguments used in the function call}
#'
#' @examples
#' #Load the minerals library
#' data(minerals)
#'
#' #Load the soils data
#' data(soils)
#'
#' #Load the regrouping structure
#' data(minerals_regroup_structure)
#'
#' \dontrun{
#' fps_sandstone <- fps(lib = minerals,
#'                      smpl = soils$sandstone,
#'                      refs = minerals$phases$phase_id,
#'                      std = "QUA.1",
#'                      align = 0.2)
#'
#' fps_sandstone_regrouped <- regroup(fps_sandstone,
#'                                    minerals_regroup_structure)
#'
#' fps_sandstone_regrouped$phases_grouped
#'
#' }
#' @export
regroup.powdRfps <- function(x, y, ...) {

  #name columns in y
  names(y)[1:2] <- c("phase_id", "phase_group")
  y <- y[1:2]

  #Check that all names in x$phases$phase_id are covered in y$phases$phase_id
  if (!length(which(x$phases$phase_id %in% y$phase_id)) == length(x$phases$phase_id)) {

    stop("Not all phase IDs in the powdRfps object are accounted for in the grouping structure.",
         call. = FALSE)

  }

  #Join the group onto the phases dataframe
  x$phases <- plyr::join(x$phases, y, by = "phase_id")

  x$phases$phase_name <- x$phases$phase_group
  x$phases$phase_group <- NULL


  #Use the new grouping structure to aggregate.
  df <- data.frame(stats::aggregate(phase_percent ~ phase_name, data = x$phases, FUN = sum),
                   stringsAsFactors = FALSE)

  x$phases_grouped <- df

  return(x)

}


#' regroup
#'
#' \code{regroup.powdRafps} allows an alternative grouping structure to be applied to
#' \code{powdRafps} objects.
#'
#' \code{powdRafps} objects contain a data frame called \code{phases_grouped}
#' that summarises phase concentrations based on defined mineral groups from the \code{powdRlib}
#' reference library. \code{regroup} allows you to change this grouping structure by supplying
#' new group identities.
#'
#' @param x A \code{powdRafps} object
#' @param y A data frame. First column contains the phase IDs covering all those present in
#' \code{x$phases$phase_id}. Second column contains the desired grouping of each phase.
#' @param ... other arguments
#'
#' @return a \code{powdRafps} object with components:
#' \item{tth}{a vector of the 2theta scale of the fitted data}
#' \item{fitted}{a vector of the count intensities of fitted XRPD pattern}
#' \item{measured}{a vector of the count intensities of original XRPD measurement (aligned)}
#' \item{residuals}{a vector of the residuals (measured minus fitted)}
#' \item{phases}{a dataframe of the phases used to produce the fitted pattern}
#' \item{phases_grouped}{the phases dataframe grouped and summed by phase_name}
#' \item{obj}{named vector of the objective parameters summarising the quality of the fit}
#' \item{weighted_pure_patterns}{a dataframe of reference patterns used to produce the fitted pattern.
#' All patterns have been weighted according to the coefficients used in the fit}
#' \item{coefficients}{a named vector of coefficients used to produce the fitted pattern}
#' \item{inputs}{a list of input arguments used in the function call}
#'
#' @examples
#' #Load the minerals library
#' data(minerals)
#'
#' # Load the soils data
#' data(soils)
#'
#' #Load the regrouping structure
#' data(minerals_regroup_structure)
#'
#' \dontrun{
#' afps_sandstone <- afps(lib = minerals,
#'                        smpl = soils$sandstone,
#'                        std = "QUA.2",
#'                        align = 0.2,
#'                        lod = 0.2,
#'                        amorphous = "ORG",
#'                        amorphous_lod = 1)
#'
#' afps_sandstone_regrouped <- regroup(afps_sandstone,
#'                                     minerals_regroup_structure)
#'
#' afps_sandstone_regrouped$phases_grouped
#' }
#' @export
regroup.powdRafps <- function(x, y, ...) {

  #name columns in y
  names(y)[1:2] <- c("phase_id", "phase_group")
  y <- y[1:2]

  #Check that all names in x$phases$phase_id are covered in y$phases$phase_id
  if (!length(which(x$phases$phase_id %in% y$phase_id)) == length(x$phases$phase_id)) {

    stop("Not all phase IDs in the powdRafps object are accounted for in the grouping structure.",
         call. = FALSE)

  }

  #Join the group onto the phases dataframe
  x$phases <- plyr::join(x$phases, y, by = "phase_id")

  x$phases$phase_name <- x$phases$phase_group
  x$phases$phase_group <- NULL


  #Use the new grouping structure to aggregate.
  df <- data.frame(stats::aggregate(phase_percent ~ phase_name, data = x$phases, FUN = sum),
                   stringsAsFactors = FALSE)

  x$phases_grouped <- df

  return(x)

}
