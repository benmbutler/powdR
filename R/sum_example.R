#' Summ of vector elements
#'
#' \code{summ} returns the sum of all the values present in its arguments
#'
#' This is a generic function: methods can be defined for it directly or via the
#' \code{\link{Summary}} group generic. For this to work properly, the arguments
#' \code{...} should be unnamed, and dispatch is on the first argument.
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' summ(1, 1)
#' summ(10, 1)
summ <- function(x, y) {
 x + y
}


#' Range normalisation
#'
#' \code{rngnorm} returns a scaled vector ranging from 0 to 1
#'
#' This is a generic function: methods can be defined for it directly or via the
#' \code{\link{Summary}} group generic. For this to work properly, the arguments
#' \code{...} should be unnamed, and dispatch is on the first argument.
#'
#' @param x A vector.
#' @return A scaled representation of \code{x}.
#' @examples
#' x <- rnorm(100, mean = 0, sd = 100)
#' rgnorm(x)
rngnorm <- function(x) {
  (x-min(x))/(max(x)-min(x))
}
