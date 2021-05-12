.check_tth_length <- function(x, y) {

  tth1 <- x[[1]]

  tth2 <- y[[1]]

  identical(length(tth1), length(tth2))

}

.check_tth <- function(x, y) {

  tth1 <- x[[1]]

  tth2 <- y[[1]]

  identical(tth1, tth2)

}

.remove_tth <- function(x) {

  x <- x[[2]]

}

#' Convert a multiXY object to a data frame.
#'
#' \code{multi_xy_to_df} converts multiXY objects to a column-wise data frame.
#'
#' @param x a \code{multiXY} object.
#' @param tth a logical value denoting whether the 2theta scale is appended as the
#' first column. Default \code{= TRUE}.
#' @param ... other arguments
#'
#' @return a \code{data.frame}.
#'
#' @examples
#' #Load the minerals library
#' data(soils)
#'
#' soils_df1 <- multi_xy_to_df(soils, tth = TRUE)
#' soils_df2 <- multi_xy_to_df(soils, tth = FALSE)
#' @export
multi_xy_to_df <- function(x, tth, ...) {
  UseMethod("multi_xy_to_df")
}


#' Convert a multiXY object to a data frame.
#'
#' \code{multi_xy_to_df.multiXY} converts multiXY objects to a column-wise data frame.
#'
#' @param x a \code{multiXY} object.
#' @param tth a logical value denoting whether the 2theta scale is appended as the
#' first column. Default \code{= TRUE}.
#' @param ... other arguments
#'
#' @return a \code{data.frame}.
#'
#' @examples
#' #Load the minerals library
#' data(soils)
#'
#' soils_df1 <- multi_xy_to_df(soils, tth = TRUE)
#' soils_df2 <- multi_xy_to_df(soils, tth = FALSE)
#' @export
multi_xy_to_df.multiXY <- function(x, tth, ...) {

  if(missing(tth)) {

    tth <- TRUE

  }

  if(!is.logical(tth)) {

    stop("The tth argument must be logical.",
         call. = FALSE)

  }

  #Check that the 2theta scales of each list item are identical
  check_tth_length_condition <- which(unlist(lapply(x,
                                                    .check_tth_length,
                                                    y = x[[1]])) == FALSE)

  if(length(check_tth_length_condition) > 0) {

    stop("Not all 2theta scales have identical lengths in the multiXY object.",
         call. = FALSE)

  }

  #Check that the 2theta scales of each list item are identical
  check_tth_condition <- which(unlist(lapply(x, .check_tth, y = x[[1]])) == FALSE)

  if(length(check_tth_condition) > 0) {

    stop("Not all 2theta scales are identical in the multiXY object.",
         call. = FALSE)

  }

  if (tth == TRUE) {

    x <- data.frame("tth" = x[[1]][[1]],
                    data.frame(lapply(x, .remove_tth)))

  } else {

    x <- data.frame(lapply(x, .remove_tth))

  }

  return(x)

}
