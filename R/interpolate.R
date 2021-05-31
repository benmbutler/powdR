#' Interpolate an XY, multiXY or powdRlib object to a given 2theta scale.
#'
#' \code{interpolate} takes an XY, multiXY or powdRlib object and the date
#' onto a new 2theta scale using a cubic spline.
#'
#' @param x an \code{XY} or \code{multiXY} object.
#' @param new_tth a numeric vector of the new 2theta scale.
#' @param ... other arguments
#'
#' @return an \code{XY} or \code{multiXY} object.
#'
#' @examples
#' #Define a new 2theta scale:
#' data(rockjock_mixtures)
#' tth <- seq(10, 60, 0.04)
#'
#' #interpolate list of data onto new scale
#' i1 <- interpolate(rockjock_mixtures, new_tth = tth)
#'
#' #interpolate single pattern onto new scale
#' i2 <- interpolate(rockjock_mixtures$Mix1, new_tth = tth)
#' @export
interpolate <- function(x, new_tth, ...) {
  UseMethod("interpolate")
}


#' Interpolate a multiXY object onto a given 2theta scale.
#'
#' \code{interpolate} takes a \code{multiXY} object, which may contain
#' XY data frames with varying 2theta scales, and interpolates all
#' data frames onto the same scale using cubic splines.
#'
#' @param x a \code{multiXY} object.
#' @param new_tth a numeric vector of the new 2theta scale.
#' @param ... other arguments
#'
#' @return a \code{multiXY} object.
#'
#' @examples
#' data(rockjock_mixtures)
#'
#' #Define a new 2theta scale:
#' tth <- seq(10, 60, 0.04)
#'
#' #interpolate data onto new scale
#' i1 <- interpolate(rockjock_mixtures, new_tth = tth)
#' @export
interpolate.multiXY <- function(x, new_tth, ...) {

  if(!is.numeric(new_tth)) {

    stop("Data supplied to new_tth must be numeric",
         call. = FALSE)

  }

  min_tth <- max(unlist(lapply(x, function(x) min(x[[1]]))))
  max_tth <- min(unlist(lapply(x, function(x) max(x[[1]]))))

  if(min(new_tth) < min_tth) {

    stop(paste0("Please set the lower limit of your new_tth vector to ",
                min_tth, " or greater to avoid the introduction of NA",
                " values during interpolation."),
         call. = FALSE)

  }

  if(max(new_tth) > max_tth) {

    stop(paste0("Please set the upper limit of your new_tth vector to ",
                max_tth, " or less to avoid the introduction of NA",
                " values during interpolation."),
         call. = FALSE)

  }

  x <- lapply(x,
              function(x) data.frame(stats::spline(x = x[[1]],
                                                   y = x[[2]],
                                                   method = "natural",
                                                   xout = new_tth)))

  x <- lapply(x, stats::setNames, c("tth", "counts"))

  for (i in 1:length(x)) {

    class(x[[i]]) <- c("XY", "data.frame")

  }

  class(x) <- "multiXY"

  return(x)

}


#' Interpolate an \code{XY} object onto a given 2theta scale.
#'
#' \code{interpolate} takes an \code{XY} object and interpolates the
#' 2theta axis onto a new scale using a cubic spline.
#'
#' @param x an \code{XY} object.
#' @param new_tth a numeric vector of the new 2theta scale.
#' @param ... other arguments
#'
#' @return an \code{XY} object.
#'
#' @examples
#' data(rockjock_mixtures)
#'
#' #Define a new 2theta scale:
#' tth <- seq(10, 60, 0.04)
#'
#' #interpolate data onto new scale
#' i1 <- interpolate(rockjock_mixtures$Mix1, new_tth = tth)
#' @export
interpolate.XY <- function(x, new_tth, ...) {

  if(!is.numeric(new_tth)) {

    stop("Data supplied to new_tth must be numeric",
         call. = FALSE)

  }

  min_tth <- min(x[[1]])
  max_tth <- max(x[[1]])

  if(min(new_tth) < min_tth) {

    stop(paste0("Please set the lower limit of your new_tth vector to ",
                min_tth, " or greater to avoid the introduction of NA",
                " values during interpolation."),
         call. = FALSE)

  }

  if(max(new_tth) > max_tth) {

    stop(paste0("Please set the upper limit of your new_tth vector to ",
                max_tth, " or less to avoid the introduction of NA",
                " values during interpolation."),
         call. = FALSE)

  }

  x <- data.frame(stats::spline(x = x[[1]],
                                y = x[[2]],
                                method = "natural",
                                xout = new_tth))

  names(x) <- c("tth", "counts")

  class(x) <- c("XY", "data.frame")

  return(x)

}

#' Interpolate a \code{powdRlib} object onto a given 2theta scale.
#'
#' \code{interpolate} takes a \code{powdRlib} object and interpolates the
#' data onto a new 2theta scale using a cubic spline.
#'
#' @param x a \code{powdRlib} object.
#' @param new_tth a numeric vector of the new 2theta scale.
#' @param ... other arguments
#'
#' @return a \code{powdRlib} object.
#'
#' @examples
#' data(minerals)
#'
#' #Define a new 2theta scale:
#' tth <- seq(10, 60, 0.04)
#'
#' #interpolate data onto new scale
#' i1 <- interpolate(minerals, new_tth = tth)
#' @export
interpolate.powdRlib <- function(x, new_tth, ...) {

  if(!is.numeric(new_tth)) {

    stop("Data supplied to new_tth must be numeric",
         call. = FALSE)

  }

  min_tth <- min(x$tth)
  max_tth <- max(x$tth)

  if(min(new_tth) < min_tth) {

    stop(paste0("Please set the lower limit of your new_tth vector to ",
                min_tth, " or greater to avoid the introduction of NA",
                " values during interpolation."),
         call. = FALSE)

  }

  if(max(new_tth) > max_tth) {

    stop(paste0("Please set the upper limit of your new_tth vector to ",
                max_tth, " or less to avoid the introduction of NA",
                " values during interpolation."),
         call. = FALSE)

  }

  s <- lapply(x$xrd, stats::spline, x = x$tth,
              method = "natural", xout = new_tth)

  s <- data.frame(lapply(s, function(x) x[[2]]))

  x$xrd <- s
  x$tth <- new_tth

  class(x) <- "powdRlib"

  return(x)

}

