.rng_nm <- function(x){(x-min(x))/(max(x)-min(x))}

.align_optim <- function(a, par, xout, std) {

  #res1 <- a
  a1 <- a
  a1[,1] <- a1[,1] + par
  a2 <- data.frame(stats::spline(x = a1[,1], y = a1[,2], method = "natural", xout = xout))
  a3 <- 1-(stats::cor(std[,2],a2[,2]))

  return(a3)
}

#Align XRPD data
.xrd_align <- function(smpl, standard,
                       xmin, xmax, xshift, manual) {

  #if manual == FALSE then make sure the xshift is an absolute valune
  if (manual == FALSE) {

  xshift <- abs(xshift)


  #Shorten the data to account for the xmin and xmax used during alignment
  #standard_short <- standard[which(standard[[1]] > xmin &
  #                           standard[[1]] < xmax), ]

  #shorten the data even further to also account for the potential shifting
  #xmin_short <- xmin - -xshift
  #xmax_short <- xmax - xshift

  #standard_shorter <- standard[which(standard[[1]] > xmin_short &
  #                             standard[[1]] < xmax_short), ]

  #TTH_short <- standard_short[,1]
  #TTH_shorter <- standard_shorter[,1]

  #Create the data frame that just contains shortened data
  smpl_short <- data.frame(stats::spline(x = smpl[[1]],
                                        y = smpl[[2]],
                                        method = "natural",
                                        xout = standard[[1]]))

  #Detecting the peak shift required for each sample
  #First define the number that's going to get minimised by the optim routine
  smpl_optim_out <- suppressWarnings(stats::optim(a = smpl_short, par = 0,
                                xout = standard[[1]], std = standard,
                                .align_optim, method = "Brent", lower = -xshift, upper = xshift))

  #extract the optimised shift (i.e. what to add/subtract from the sample 2theta)
  smpl_optim <- smpl_optim_out$par

  } else {

  smpl_optim <- xshift

  }

  #shift the pattern
  smpl_aligned <- smpl
  smpl_aligned[,1] <- smpl[,1] + smpl_optim

  ### Harmonising the data after the shifts
  #xmax_harm <- max(smpl[,1])
  #xmin_harm <- min(smpl[,1])
  #int_TTH <- ((smpl[nrow(smpl),1] - smpl[1,1])/(nrow(smpl)-1))

  #Create a vector that is nearly identical to the original measurements,
  #but with intervals that are exactly equal
  #TTH_constant <- seq(xmin_harm, xmax_harm, int_TTH)

  #new_xmin <- xmin_harm + smpl_optim
  #new_xmax <- xmax_harm + smpl_optim

  #Now I create a slightly shortened version of the constant vector which will allow me to
  #create a new 2theta resolution appropriate for this alignment
  #TTH_constant_short <- TTH_constant[which(TTH_constant > new_xmin & TTH_constant < new_xmax)]

  #Final harmonisation
  smpl_aligned_harm <- data.frame(stats::spline(x = smpl_aligned[,1],
                                               y = smpl_aligned[,2],
                                               method = "natural",
                                               xout = standard[[1]]))

  names(smpl_aligned_harm) <- c("tth", "counts")

  out <- list("shift" = smpl_optim, "aligned" = smpl_aligned_harm)

  return(out)
}
