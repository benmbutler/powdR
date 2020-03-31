#' Create an XRPD reference library
#'
#' A constructor function for creating a \code{powdRlib} object from two tables of
#' data. The resulting \code{powdRlib} object is required when
#' using \code{\link{fps}} or \code{\link{afps}}.
#'
#' @param xrd_table A data frame of the count intensities of the XRPD reference patterns,
#' all scaled to same maximum intensity, with their 2theta axis as the first column.
#' @param phases_table A data frame of the required data (phase ID, phase name, and reference
#' intensity ratio) for each reference pattern.
#'
#' @return a list with components:
#' \item{xrd}{a data frame of reference patterns}
#' \item{tth}{a vector of the 2theta axis}
#' \item{phases}{a 3 column data frame of the IDs, names and reference
#' intensity ratios of the reference pattern}
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

  #Checks on the xrd table
  if(!names(table(is.na(xrd_table))) == FALSE) {
    stop("Please ensure that the xrd_table does not contain any
         NA's")
  }

  #Check all columns in xrd_table are numeric
  if(!ncol(xrd_table) == as.numeric(summary(unlist(lapply(xrd_table, is.numeric)))[[2]])) {

    stop("All columns in the xrd_table should be numeric")

  }

  tth <- xrd_table[[1]]
  xrd <- data.frame(xrd_table[-1])
  phases <- phases_table

  #Checks on the phases table
  if(!is.character(phases[[1]])) {
    stop("Please make sure that the first column in phases_table is a character
         string.")
  }

  if(!is.character(phases[[2]])) {
    stop("Please make sure that the second column in phases_table is a character
         string.")
  }

  if(!is.numeric(phases[[3]])) {
    stop("Please make sure that the third column in phases_table is a numeric vector.")
  }

  #R can adjust the names of a data frame to ensure they meet the requirements
  #by replacing spaces and dashes with dots. Here I can ensure this is done.
  xrd2 <- xrd
  names(xrd2) <- phases[[1]]
  xrd2 <- data.frame(xrd2)

  #Check that all phase id's match the column names
  if(!length(which(names(xrd) %in% names(xrd2))) == ncol(xrd)) {
    stop("The id's in the phase_id column of the phases_table do not match the id's
         provided as column names in the xrd_table")
  }

  phases[[1]] <- names(xrd)

  #Ensure the order of the xrd_table and phases_table are the same
  xrd <- xrd[order(names(xrd))]
  phases <- phases[order(phases[[1]]), ]

  #if(!ncol(xrd) == length(which((names(xrd) == phases[[1]]) == TRUE))) {
  #  stop("Please check that the first column in the phases_table matches the
  #       phase ids provided as column names in the xrd_table.")
  #}

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
