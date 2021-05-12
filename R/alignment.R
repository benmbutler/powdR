.align_optim <- function(a, par, xout, std) {

  a1 <- a
  a1[,1] <- a1[,1] + par
  a2 <- data.frame(stats::spline(x = a1[,1], y = a1[,2], method = "natural", xout = xout),
                   check.names = FALSE)
  a3 <- 1-(stats::cor(std[,2],a2[,2]))

  return(a3)
}

#Align XRPD data
.xrd_align <- function(smpl, standard,
                       xmin, xmax, xshift, manual) {

  #if manual == FALSE then make sure the xshift is an absolute valune
  if (manual == FALSE) {

  xshift <- abs(xshift)

 #Optimise alignment
  smpl_optim_out <- suppressWarnings(stats::optim(a = smpl[which(smpl[[1]] >= xmin &
                                                                   smpl[[1]] <= xmax), ],
                                                  par = 0,
                                                  xout = standard[which(standard[[1]] >= xmin &
                                                                          standard[[1]] <= xmax), 1],
                                                  std = standard[which(standard[[1]] >= xmin &
                                                                         standard[[1]] <= xmax), ],
                                                  .align_optim, method = "Brent",
                                                  lower = -xshift, upper = xshift))

  #extract the optimised shift (i.e. what to add/subtract from the sample 2theta)
  smpl_optim <- smpl_optim_out$par

  } else {

  smpl_optim <- xshift

  }

  #shift the pattern
  smpl_aligned <- smpl
  smpl_aligned[,1] <- smpl[,1] + smpl_optim

  #create a new tth
  tth_min <- max(c(min(smpl_aligned[[1]]), min(smpl[[1]])))
  tth_max <- min(c(max(smpl_aligned[[1]]), max(smpl[[1]])))
  tth_res <- mean(diff(smpl[[1]]))

  tth_new <- seq(from = tth_min, to = tth_max, by = tth_res)

  #Final harmonisation
  smpl_aligned_harm <- data.frame(stats::spline(x = smpl_aligned[[1]],
                                               y = smpl_aligned[[2]],
                                               method = "natural",
                                               xout = tth_new),
                                  check.names = FALSE)

  names(smpl_aligned_harm) <- c("tth", "counts")

  out <- list("shift" = smpl_optim, "aligned" = smpl_aligned_harm)

  return(out)
}
