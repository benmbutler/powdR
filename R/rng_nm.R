#' Range normalisation
#'
#' \code{rng.nm} returns a range normalised vector, where the maximum is normalised
#' to 1, and the minimum is normalised to 0.
#'
#' @param x a vector to be normalised
#' @return a normalised vector
rng.nm <- function(x){(x-min(x))/(max(x)-min(x))}
