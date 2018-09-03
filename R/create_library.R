#' Create an XRD reference library
#'
#' A constructor function for creating a \code{powdRlib} object from two tables of
#' data. The resulting reference library object is required when
#' using \code{\link{fps}}.
#'
#' @param xrd_table A data frame of the count intensities of the xrd reference patterns,
#' all scaled to same maximum intensity, with their 2theta axis as the first column.
#' Must all be on the same 2theta scale.
#' @param phases_table A data frame of the required data (phase ID, phase name, and reference
#' intensity ratio) for each reference pattern.
#'
#' @return a list with components:
#' \item{xrd}{a data frame of reference patterns}
#' \item{tth}{a vector of the 2theta measurement intervals}
#' \item{phases}{a data frame of the id, name and reference
#' intensity ratio of each reference pattern}
#'
#' @examples
#' #load an example xrd_table
#' data(minerals_xrd)
#' #load an example phases_table
#' data(minerals_phases)
#'
#' #Create a reference library object
#' xrd_lib <- powdRlib(xrd_table = minerals_xrd,
#'                     phases_table = minerals_phases)
#' @export
powdRlib <- function(xrd_table, phases_table) {

  if(!names(table(is.na(xrd_table))) == FALSE) {
    stop("Please ensure that the xrd_table does not contain any
         NA's")
  }

  tth <- xrd_table[[1]]
  xrd <- xrd_table[-1]
  phases <- phases_table

  #Check that all phase id's match the column names
  if(!length(which(names(xrd) %in% phases[[1]])) == ncol(xrd)) {
    stop("The id's in the phase_id column of the phases_table do not match the id's
         provided as column names in the xrd_table")
  }

  #Ensure the order of the xrd_table and phases_table are the same
  xrd <- xrd[order(names(xrd))]
  phases <- phases[order(phases[[1]]), ]

  if(!ncol(xrd) == length(which((names(xrd) == phases[[1]]) == TRUE))) {
    stop("Please check that the first column in the phases_table matches the
         phase ids provided as column names in the xrd_table.")
  }

  if(is.factor(phases[[1]])) {
    phases[[1]] <- as.character(phases[[1]])
  }

  if(!is.character(phases[[1]])) {
    stop("Please make sure that the first column in phases_table is a character
         string.")
  }

  if(is.factor(phases[[2]])) {
    phases[[2]] <- as.character(phases[[2]])
  }

  if(!is.character(phases[[2]])) {
    stop("Please make sure that the second column in phases_table is a character
         string.")
  }

  if(!is.numeric(phases[[3]])) {
    stop("Please make sure that the third column in phases_table is a numeric vector.")
  }

  names(phases) <- c("phase_id", "phase_name", "rir")

  #Ensure that the data frame names are suitable and rename phase ID's
  xrd <- data.frame(xrd)
  phases[[1]] <- names(xrd)

  out <- list("xrd" = xrd,
              "tth" = tth,
              "phases" = phases)

  class(out) <- "powdRlib"

  return(out)
}
