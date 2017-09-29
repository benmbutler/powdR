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
