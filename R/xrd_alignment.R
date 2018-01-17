align.optim <- function(a, par, xout, xrd.standard_shorter) {
  res1 <- a
  res2 <- res1
  res2[,1] <- res1[,1] + par
  res3 <- data.frame(approx(x = res2[,1], y = res2[,2], xout = xout))
  res4 <- 1-(cor(xrd.standard_shorter[,2],res3[,2]))
  return(res4)
}

rng.nm <- function(x){(x-min(x))/(max(x)-min(x))}

xrd.align <- function(xrd.sample, xrd.standard, xmin, xmax, xshift) {

  #I want to apply this function to the lists I've made using a loop
  xrd.sample_scaled <- xrd.sample
  xrd.sample_scaled[,2] <- rng.nm(xrd.sample[,2])

  xrd.standard_scaled <- xrd.standard
  xrd.standard_scaled[,2] <- rng.nm(xrd.standard[,2])

  #Now I want to increase the resolution of the data by 4 times so that the peaks can be
  #shifted more accurately

  xrd.sample_scaled_approx <- data.frame(approx(x = xrd.sample_scaled[,1],
                                                y = xrd.sample_scaled[,2],
                                                n = nrow(xrd.sample_scaled)*4))

  xrd.standard_scaled_approx <- data.frame(approx(x = xrd.standard_scaled[,1],
                                                  y = xrd.standard_scaled[,2],
                                                  n = nrow(xrd.standard_scaled)*4))

  ### Aligning the xrd.samples using optim

  #First I create lists of files that are approximated to the same scale as a shortened xrd.standard

  xrd.standard_short <- xrd.standard_scaled_approx
  xrd.standard_short <- subset(xrd.standard_short, x > xmin & x < xmax)

  xmin_short <- xmin - -xshift
  xmax_short <- xmax - xshift

  xrd.standard_shorter <- xrd.standard_scaled_approx
  xrd.standard_shorter <- subset(xrd.standard_shorter, x > xmin_short & x < xmax_short)

  #I use the shortened TTH to refine more approximation functions
  TTH_short <- xrd.standard_short[,1]
  TTH_shorter <- xrd.standard_shorter[,1]

  #Create the list that just contains shortened data

  xrd.sample_short <- data.frame(approx(x = xrd.sample_scaled_approx[,1],
                                        y = xrd.sample_scaled_approx[,2],
                                        xout = TTH_short))

  ### Detecting the peak shift required for each xrd.sample

  #First define the number that's going to get minimised by the optim routine

  xrd.sample_optim_out <- optim(a = xrd.sample_short, par = 0,
                                xout = TTH_shorter, xrd.standard_shorter = xrd.standard_shorter,
                                align.optim, method = "Brent", lower = -xshift, upper = xshift)

  xrd.sample_optim <- xrd.sample_optim_out$par

  # NOW TO SHIFT THE DIFFRACTOGRAM
  xrd.sample_aligned <- xrd.sample

  xrd.sample_aligned[,1] <- xrd.sample_aligned[,1] + xrd.sample_optim

  ### Harmonising the data after the shifts
  all_shifts <- xrd.sample_optim
  TTH_length <- nrow(xrd.sample)

  xmax_harm <- max(xrd.sample[,1])
  xmin_harm <- min(xrd.sample[,1])
  int_TTH <- ((xrd.sample[TTH_length,1] - xrd.sample[1,1])/(TTH_length-1))

  #This creates a vector that is pretty much identical to the original measurements, but with intervals that are exactly equal
  TTH_constant <- seq(xmin_harm, xmax_harm, int_TTH)

  new_xmin <- xmin_harm + all_shifts
  new_xmax <- xmax_harm + all_shifts

  #Now I create a slightly shortened version of the constant vector which will allow me to harmonise all of the data
  TTH_constant_short <- subset(TTH_constant,
                               TTH_constant > new_xmin & TTH_constant < new_xmax)

  ### FINAL HARMONISATION

  xrd.sample_aligned_harm <- data.frame(approx(x = xrd.sample_aligned[,1],
                                               y = xrd.sample_aligned[,2],
                                               xout = TTH_constant_short))

  names(xrd.sample_aligned_harm) <- c("TTH", "COUNTS")

  out <- list("SHIFT" = all_shifts, "ALIGNED" = xrd.sample_aligned_harm)

  return(out)
}
