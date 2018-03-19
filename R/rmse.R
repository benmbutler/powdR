#' Root mean square error
#'
#' \code{rmse} computes the root mean square error of a vector of predicted values
#' relative to a vector of observed values
#' @param pred a vector a predicted values
#' @param obs a vector of observed values
rmse <- function(pred, obs) {
  sqrt(mean((pred - obs)^2))
}
