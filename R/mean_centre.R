mean.centre <- function(smpl) {

  smpl[,2] <- smpl[[2]] - mean(smpl[[2]])

  return(smpl)
}
