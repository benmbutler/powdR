.rng_nm <- function(x){(x-min(x))/(max(x)-min(x))}

.align_optim <- function(a, par, xout, xrd_std_shorter) {

  res1 <- a
  res2 <- res1
  res2[,1] <- res1[,1] + par
  res3 <- data.frame(stats::approx(x = res2[,1], y = res2[,2], xout = xout))
  res4 <- 1-(stats::cor(xrd_std_shorter[,2],res3[,2]))

  return(res4)
}

#Align XRPD data
.xrd_align <- function(xrd_sample, xrd_standard,
                       xmin, xmax, xshift, resolution) {

  #Apply the range normalisation to samples and standard
  xrd_sample_scaled <- xrd_sample
  xrd_sample_scaled[,2] <- .rng_nm(xrd_sample[,2])

  xrd_standard_scaled <- xrd_standard
  xrd_standard_scaled[,2] <- .rng_nm(xrd_standard[,2])

  #Increase the resolution of the data so that the peaks can be
  #shifted more accurately
  xrd_sample_scaled_approx <- data.frame(stats::approx(x = xrd_sample_scaled[,1],
                                                y = xrd_sample_scaled[,2],
                                                method = "linear",
                                                n = nrow(xrd_sample_scaled)*resolution))

  xrd_standard_scaled_approx <- data.frame(stats::approx(x = xrd_standard_scaled[,1],
                                                  y = xrd_standard_scaled[,2],
                                                  method = "linear",
                                                  n = nrow(xrd_standard_scaled)*resolution))

  #Shorten the data to account for the xmin and xmax used during alignment
  xrd_standard_short <- xrd_standard_scaled_approx

  xrd_standard_short <- xrd_standard_short[which(xrd_standard_short[[1]] > xmin &
                                                 xrd_standard_short[[1]] < xmax), ]

  #shorten the data even further to also account for the potential shifting
  xmin_short <- xmin - -xshift
  xmax_short <- xmax - xshift

  xrd_standard_shorter <- xrd_standard_scaled_approx

  xrd_standard_shorter <- xrd_standard_shorter[which(xrd_standard_shorter[[1]] > xmin_short &
                                                     xrd_standard_shorter[[1]] < xmax_short), ]

  TTH_short <- xrd_standard_short[,1]
  TTH_shorter <- xrd_standard_shorter[,1]

  #Create the list that just contains shortened data
  xrd_sample_short <- data.frame(stats::approx(x = xrd_sample_scaled_approx[,1],
                                        y = xrd_sample_scaled_approx[,2],
                                        xout = TTH_short))

  #Detecting the peak shift required for each xrd_sample
  #First define the number that's going to get minimised by the optim routine
  xrd_sample_optim_out <- stats::optim(a = xrd_sample_short, par = 0,
                                xout = TTH_shorter, xrd_std_shorter = xrd_standard_shorter,
                                .align_optim, method = "Brent", lower = -xshift, upper = xshift)

  #extract the optimised shift (i.e. what to add/subtract from the sample 2theta)
  xrd_sample_optim <- xrd_sample_optim_out$par

  #shift the pattern
  xrd_sample_aligned <- xrd_sample
  xrd_sample_aligned[,1] <- xrd_sample_aligned[,1] + xrd_sample_optim

  ### Harmonising the data after the shifts
  TTH_length <- nrow(xrd_sample)

  xmax_harm <- max(xrd_sample[,1])
  xmin_harm <- min(xrd_sample[,1])
  int_TTH <- ((xrd_sample[TTH_length,1] - xrd_sample[1,1])/(TTH_length-1))

  #Create a vector that is nearly identical to the original measurements,
  #but with intervals that are exactly equal
  TTH_constant <- seq(xmin_harm, xmax_harm, int_TTH)

  new_xmin <- xmin_harm + xrd_sample_optim
  new_xmax <- xmax_harm + xrd_sample_optim

  #Now I create a slightly shortened version of the constant vector which will allow me to
  #create a new 2theta resolution appropriate for this alignment
  TTH_constant_short <- TTH_constant[which(TTH_constant > new_xmin & TTH_constant < new_xmax)]

  #Final harmonisation
  xrd_sample_aligned_harm <- data.frame(stats::approx(x = xrd_sample_aligned[,1],
                                               y = xrd_sample_aligned[,2],
                                               method = "linear",
                                               xout = TTH_constant_short))

  names(xrd_sample_aligned_harm) <- c("tth", "counts")

  out <- list("shift" = xrd_sample_optim, "aligned" = xrd_sample_aligned_harm)

  return(out)
}
