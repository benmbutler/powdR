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
#' @param check_names A logical argument defining whether the column names in the data
#' supplied in \code{xrd_table} are syntactically valid variable names and are not
#' duplicated. Default = \code{TRUE}.
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
powdRlib <- function(xrd_table, phases_table, check_names) {

  if (missing(check_names)) {

    check_names <- TRUE

  }

  #Checks on the xrd table
  if(!names(table(is.na(xrd_table))) == FALSE) {
    stop("Please ensure that the xrd_table does not contain any
         NA's",
         call. = FALSE)
  }

  #Check all columns in xrd_table are numeric
  if(!ncol(xrd_table) == as.numeric(summary(unlist(lapply(xrd_table, is.numeric)))[[2]])) {

    stop("All columns in the xrd_table should be numeric",
         call. = FALSE)

  }

  #Checks on the phases table
  if(!is.character(phases_table[[1]])) {
    stop("Please make sure that the first column in phases_table is a character
         string.",
         call. = FALSE)
  }

  if(!is.character(phases_table[[2]])) {
    stop("Please make sure that the second column in phases_table is a character
         string.",
         call. = FALSE)
  }

  if(!is.numeric(phases_table[[3]])) {
    stop("Please make sure that the third column in phases_table is a numeric vector.",
         call. = FALSE)
  }

  #Check the column names of xrd_data match the phases_table[[1]]
  if (!identical(names(xrd_table[-1]), phases_table[[1]])) {

    stop("The column names of the xrd_table (excluding column 1, which is 2theta),
          do not match the first column of the phases_table",
         call. = FALSE)

  }

  #Extract 2theta from the xrd_table
  tth <- xrd_table[[1]]
  xrd <- data.frame(xrd_table[-1],
                    check.names = FALSE)
  phases <- phases_table


  #Name the columns of phases and ensure it's a dataframe
  names(phases) <- c("phase_id", "phase_name", "rir")
  phases <- data.frame(phases)

  if (check_names == TRUE) {

    xrd <- data.frame(xrd,
                      check.names = TRUE)

    phases$phase_id <- names(xrd)

  }

  out <- list("xrd" = xrd,
              "tth" = tth,
              "phases" = phases)

  class(out) <- "powdRlib"

  return(out)
}
