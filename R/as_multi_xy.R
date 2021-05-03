.check_xy <- function(x) {

if (is.numeric(x[[1]]) & is.numeric(x[[2]])) {

  check_out <- TRUE

} else {

  check_out <- FALSE

}

}

#' Create a multiXY object
#'
#' \code{as_multi_xy} takes a list of XRPD data and ensures that they meet
#' various requirements to create a multiXY object. These requirements
#' include that the data is in list format, with each items in the list
#' containing 2 columns of numeric data in a dataframe. \code{as_multi_xy}
#' also checks that all names are unique. Once a multiXY object has been
#' created, it can easily be plotted using the associated \code{plot.multiXY}
#' method.
#'
#' @param x a list XRPD dataframes (column 1 = 2theta, column 2 = counts)
#'
#' @return a multiXY object which is a list of xy dataframes.
#'
#' @examples
#' # Load soils xrd data
#' data(rockjock_mixtures)
#'
#' multi_xy <- as_multi_xy(rockjock_mixtures)
#'
#' class(multi_xy)
#'
#' @export
as_multi_xy <- function(x) {

  #Check that x is a list
  if(!is.list(x)) {

    stop("Data supplied to x must be a list of xy dataframes.",
         call. = FALSE)

  }

  #Check that each item in x is a dataframe
  df_condition <- which(unlist(lapply(x, is.data.frame)) == TRUE)

  if(!length(df_condition) == length(x)) {

    stop("Not all items in the list are dataframes.",
         call. = FALSE)

  }

  #Check that the first two columns in each dataframe are numeric
  check_xy_condition <- which(unlist(lapply(x, .check_xy)) == TRUE)

  if(!length(check_xy_condition) == length(x)) {

    stop("Not all items in the list contains two columns of numeric data.",
         call. = FALSE)

  }

  #Check that each item in the list has a name
  if (is.null(names(x)) | (length(which(is.na(names(x)))) > 0)) {

    stop("All items in the list must have a name.",
         call. = FALSE)

  }

  #Check that all names are unique
  if (!length(unique(names(x))) == length(x)) {

    stop("All items in the list must have a unique name.",
         call. = FALSE)

  }

  #change column names of each item to tth and counts
  for (i in 1:length(x)) {

    names(x[[i]]) <- c("tth", "counts")

  }

  #Change the class of x to multiXY
  class(x) <- c("multiXY", "list")

  return(x)

}
