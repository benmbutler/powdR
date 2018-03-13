rmse <- function(pred, obs) {
  sqrt(mean((pred - obs)^2))
}
