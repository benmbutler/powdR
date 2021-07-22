.check_xy <- function(x) {

if (is.numeric(x[[1]]) & is.numeric(x[[2]])) {

  check_out <- TRUE

} else {

  check_out <- FALSE

}

  return(check_out)

}

#' Create a multiXY object
#'
#' \code{as_multi_xy} takes a list or data frame of XRPD data and ensures
#' that the data meet various requirements to create a multiXY object.
#' Once a multiXY object has been created, it can easily be plotted using
#' the associated \code{plot.multiXY} method.
#'
#' @param x a list or data frame of XRPD data
#' @param ... other arguments
#'
#' @return a \code{multiXY} object.
#'
#' @examples
#'
#' #EXAMPLE 1
#'
#' #load soils data
#' data(soils)
#'
#' #extract first two samples from the list
#' soils <- soils[c(1:2)]
#'
#' #convert to multiXY
#' soils <- as_multi_xy(soils)
#'
#' #EXAMPLE 2
#' #load the soils data
#' data(soils)
#'
#' #Convert to data frame
#' soils_df <- multi_xy_to_df(soils,
#'                            tth = TRUE)
#'
#' #Convert back to multiXY object
#' soils2 <- as_multi_xy(soils_df)
#' @export
as_multi_xy <- function(x, ...) {
  UseMethod("as_multi_xy")
}

#' Create a multiXY object from a list of XRPD data
#'
#' \code{as_multi_xy.list} takes a list of XRPD data and ensures that they meet
#' various requirements to create a multiXY object. These requirements
#' include that each item in the list contains 2 columns of numeric data in a
#' data frame. \code{as_multi_xy.list} also checks that all names are unique.
#' Once a \code{multiXY} object has been created, it can easily be plotted using
#' the associated \code{plot.multiXY} method.
#'
#' @param x a list of XRPD data frames (column 1 = 2theta, column 2 = counts)
#' @param ... other arguments
#'
#' @return a \code{multiXY} object.
#'
#' @examples
#' #' #load soils data
#' data(soils)
#'
#' #extract first two samples from the list
#' soils <- soils[c(1:2)]
#'
#' #convert to multiXY
#' soils <- as_multi_xy(soils)
#' @export
as_multi_xy.list <- function(x, ...) {

  #Check that each item in x is a dataframe
  df_condition <- which(unlist(lapply(x, is.data.frame)) == TRUE)

  if(!length(df_condition) == length(x)) {

    stop("Not all items in the list are dataframes.",
         call. = FALSE)

  }

  #Check that the first two columns in each dataframe are numeric
  check_xy_condition <- which(unlist(lapply(x, .check_xy)) == TRUE)

  if(!length(check_xy_condition) == length(x)) {

    stop("Not all items in the list contain two columns of numeric data.",
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
    class(x[[i]]) <- c("XY", "data.frame")

  }

  #Change the class of x to multiXY
  class(x) <- c("multiXY", "list")

  return(x)

}

#This function is needed in as_multi_xy.data.frame
.create_xy <- function(x, y) {

  out <- data.frame("tth" = x,
                    "counts" = y)

  class(out) <- c("XY", "data.frame")

  return(out)

}


#' Create a multiXY object from a list of XRPD data
#'
#' \code{as_multi_xy.data.frame} takes a data frame of XRPD data from multiple
#' samples and ensures that it meets various requirements to create a multiXY object.
#' Once a \code{multiXY} object has been created, it can easily be plotted using
#' the associated \code{plot.multiXY} method.
#'
#' @param x a data frame of XRPD data, with the first column as the 2theta
#' axis and subsequent columns of count intensities.
#' @param ... other arguments
#'
#' @return a \code{multiXY} object.
#'
#' @examples
#' #load the soils data
#' data(soils)
#'
#' #Convert to data frame
#' soils_df <- multi_xy_to_df(soils,
#'                            tth = TRUE)
#'
#' #Convert back to multiXY object
#' soils2 <- as_multi_xy(soils_df)
#'
#' @export
as_multi_xy.data.frame <- function(x, ...) {

  #Check all variables are numeric
  if (!all(unlist(lapply(x, is.numeric)))) {

    stop("All variables in x must be numeric",
         call. = FALSE)

  }

  #Check that there are more than two columns
  if (ncol(x) <= 2) {

    stop("At least 2 columns of count intensities are required
         to create a multiXY object.",
         call. = FALSE)

  }

  #Extract the 2theta
  tth <- x[[1]]

  #Check that the 2theta axis is valid
  if (!all(diff(tth) >= 0)) {

    stop("The data supplied in column 1 is not a valid 2theta
         axis because it does not continually ascend.")

  }

  #create the list
  l <- lapply(x[-1], .create_xy, x = tth)

  class(l) <- c("multiXY", "list")

  return(l)


}
