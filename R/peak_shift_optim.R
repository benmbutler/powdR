pso <- function(a, par, xout, standard_shorter) {
  res1 <- a
  res2 <- res1
  res2[,1] <- res1[,1] + par
  res3 <- data.frame(powdharm(x = res2[,1], y = res2[,2], xout = xout))
  res4 <- 1-(cor(standard_shorter[[1]][,2],res3[,2]))
  return(res4)
}
