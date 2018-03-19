#' Maximisisation of correlation
#'
#' \code{align.optim} is a function that gets optimised during alignment
#' when using \code{xrd.align} and \code{multi.xrd.align}. Optimisation
#' maximises the correlation between a sample and standard.
#'
#' @param a a dataframe of the XRPD sample to be aligned
#' @param par the shift to be applied (this gets optimised)
#' @param xout the tth vector re-approximate the sample to after aligment
#' @param xrd.standard_shorter a shortened data frame of an XRPD standard
#' used to accomodate for clipping of data during alignment
align.optim <- function(a, par, xout, xrd.standard_shorter) {

  res1 <- a
  res2 <- res1
  res2[,1] <- res1[,1] + par
  res3 <- data.frame(stats::approx(x = res2[,1], y = res2[,2], xout = xout))
  res4 <- 1-(stats::cor(xrd.standard_shorter[,2],res3[,2]))

  return(res4)
}

#' Align XRPD data
#'
#' \code{xrd.align} returns a list containing the shift extent (2theta) and a
#' data frame of the shifted xrpd pattern.
#'
#' This function applies a linear alignment to an xrpd sample pattern relative to a
#' chosen standard. The function using brent optimisation to maximise the correlation
#' between the 2 patterns. Appropriate choice of internal standard is required for
#' accurate alignment.
#'
#' @param xrd.sample a data frame of the XRPD sample to be aligned. The first column
#' must be 2theta, and the second must be counts
#' @param xrd.standard a  data frame of the XRPD sample to be used as the standard. The
#' first column must be 2theta, and the second must be counts.
#' @param xmin The minimum 2theta value to be used during alignment
#' @param xmax the maximum 2theta value to be used during alignment
#' @param xshift the maximum 2theta shift that can be applied
#'
#' @return a list with components:
#' \item{shift}{the 2theta shift applied to the sample}
#' \item{aligned}{a dataframe of the aligned sample (2theta and counts)}
#'
#' @examples
#' data(soils)
#' data(minerals)
#'
#' #Create a quartz standard from the minerals library
#' quartz <- data.frame(tth = minerals$tth,
#'                      counts = minerals$xrd$QUARTZ.STRATH.P.1142250)
#'
#' #Create a soil XRPD pattern to align
#' soil <- soils$sandstone
#'
#' #Plot unaligned data. Note that rng.nm is used to normalise count intensities
#' plot(x = quartz$tth, y = rng.nm(quartz$counts), xlim = c(26, 27), type = "l")
#' lines(x = soil$tth, y = rng.nm(soil$counts), col = "red")
#'
#' #Align the soil sample to the quartz
#' soil_a <- xrd.align(xrd.sample = soil, xrd.standard = quartz, xmin = 10,
#'                      xmax = 60, xshift = 0.2)
#' #Replot
#' plot(x = quartz$tth, y = rng.nm(quartz$counts), xlim = c(26, 27), type = "l")
#' lines(x = soil_a$aligned$tth, y = rng.nm(soil_a$aligned$counts), col = "red")
xrd.align <- function(xrd.sample, xrd.standard, xmin, xmax, xshift) {

  #Apply the range normalisation to samples and standard
  xrd.sample_scaled <- xrd.sample
  xrd.sample_scaled[,2] <- rng.nm(xrd.sample[,2])

  xrd.standard_scaled <- xrd.standard
  xrd.standard_scaled[,2] <- rng.nm(xrd.standard[,2])

  #Increase the resolution of the data by 4 times so that the peaks can be
  #shifted more accurately
  xrd.sample_scaled_approx <- data.frame(stats::approx(x = xrd.sample_scaled[,1],
                                                y = xrd.sample_scaled[,2],
                                                method = "linear",
                                                n = nrow(xrd.sample_scaled)*4))

  xrd.standard_scaled_approx <- data.frame(stats::approx(x = xrd.standard_scaled[,1],
                                                  y = xrd.standard_scaled[,2],
                                                  method = "linear",
                                                  n = nrow(xrd.standard_scaled)*4))

  #Shorten the data to account for the xmin and xmax used during alignment
  xrd.standard_short <- xrd.standard_scaled_approx
  #xrd.standard_short <- subset(xrd.standard_short, x > xmin & x < xmax)
  xrd.standard_short <- xrd.standard_short[which(xrd.standard_short[[1]] > xmin &
                                                 xrd.standard_short[[1]] < xmax), ]

  #shorten the data even further to also account for the potential shifting
  xmin_short <- xmin - -xshift
  xmax_short <- xmax - xshift

  xrd.standard_shorter <- xrd.standard_scaled_approx
  #xrd.standard_shorter <- subset(xrd.standard_shorter, x > xmin_short & x < xmax_short)
  xrd.standard_shorter <- xrd.standard_shorter[which(xrd.standard_shorter[[1]] > xmin_short &
                                                     xrd.standard_shorter[[1]] < xmax_short), ]


  TTH_short <- xrd.standard_short[,1]
  TTH_shorter <- xrd.standard_shorter[,1]

  #Create the list that just contains shortened data
  xrd.sample_short <- data.frame(stats::approx(x = xrd.sample_scaled_approx[,1],
                                        y = xrd.sample_scaled_approx[,2],
                                        xout = TTH_short))

  #Detecting the peak shift required for each xrd.sample
  #First define the number that's going to get minimised by the optim routine
  xrd.sample_optim_out <- stats::optim(a = xrd.sample_short, par = 0,
                                xout = TTH_shorter, xrd.standard_shorter = xrd.standard_shorter,
                                align.optim, method = "Brent", lower = -xshift, upper = xshift)

  #extract the optimised shift (i.e. what to add/subtract from the sample 2theta)
  xrd.sample_optim <- xrd.sample_optim_out$par

  #shift the pattern
  xrd.sample_aligned <- xrd.sample
  xrd.sample_aligned[,1] <- xrd.sample_aligned[,1] + xrd.sample_optim

  ### Harmonising the data after the shifts
  #all_shifts <- xrd.sample_optim
  TTH_length <- nrow(xrd.sample)

  xmax_harm <- max(xrd.sample[,1])
  xmin_harm <- min(xrd.sample[,1])
  int_TTH <- ((xrd.sample[TTH_length,1] - xrd.sample[1,1])/(TTH_length-1))

  #Create a vector that is nearly identical to the original measurements,
  #but with intervals that are exactly equal
  TTH_constant <- seq(xmin_harm, xmax_harm, int_TTH)

  new_xmin <- xmin_harm + xrd.sample_optim
  new_xmax <- xmax_harm + xrd.sample_optim

  #Now I create a slightly shortened version of the constant vector which will allow me to
  #create a new 2theta resolution appropriate for this alignment
  #TTH_constant_short <- subset(TTH_constant,
  #                            TTH_constant > new_xmin & TTH_constant < new_xmax)

  TTH_constant_short <- TTH_constant[which(TTH_constant > new_xmin & TTH_constant < new_xmax)]

  #Final harmonisation
  xrd.sample_aligned_harm <- data.frame(stats::approx(x = xrd.sample_aligned[,1],
                                               y = xrd.sample_aligned[,2],
                                               method = "linear",
                                               xout = TTH_constant_short))

  names(xrd.sample_aligned_harm) <- c("tth", "counts")

  out <- list("shift" = xrd.sample_optim, "aligned" = xrd.sample_aligned_harm)

  return(out)
}
