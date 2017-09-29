#' Align XRD data
#'
#' \code{powdalign} returns a list of aligned XRPD patterns
#'
#' This function aligns a list of XRPD sample patterns to a known internal standard
#' using a linear optimisation.
#'
#' @param fd A directory containing .xy files of XRPD patterns.
#' @param std The file location of the internal standard XRPD pattern.
#' @param xmin The minimum 2theta value used during alignment
#' @param xmax The maximum 2theta value used during alignment
#' @param nshift The negative shift allowance (must be negative)
#' @param pshift The positive shift allowance (must be positive)
#' @return A list of aligned XRPD patterns.
powdalign <- function(fd, std, xmin, xmax, nshift, pshift) {

  sample_files <- dir(fd)
  sample_locations <- paste0(fd, "/", sample_files)

  sample <- lapply(sample_locations, read.csv, header = FALSE, sep = "")
  names(sample) <- sample_files

  #Load a reference standard pattern that I will align everything to
  standard <- list(read.csv(file = std, header = FALSE, sep = ""))


  #The data are then scaled

  #I want to apply this function to the lists I've made using a loop

  sample_scaled <- sample
  for (i in 1:length(sample)) {
    sample_scaled[[i]][2] <- rngnorm(sample_scaled[[i]][2])
  }

  standard_scaled <- standard
  for (i in 1:length(standard_scaled)) {
    standard_scaled[[i]][2] <- rngnorm(standard_scaled[[i]][2])
  }



  #Now I want to increase the resolution of the data by 4 times so that the peaks can be
  #shifted more accurately

  sample_scaled_approx <- sample_scaled
  for (i in 1:length(sample_scaled_approx)) {
    sample_scaled_approx[[i]] <- data.frame(extend(x = unlist(sample_scaled[[i]][1]),
                                                                   y = unlist(sample_scaled[[i]][2]),
                                                                   n = nrow(sample_scaled[[i]])*4))
  }

  standard_scaled_approx <- standard_scaled
  for (i in 1:length(standard_scaled_approx)) {
    standard_scaled_approx[[i]] <- data.frame(extend(x = unlist(standard_scaled_approx[[i]][1]),
                                                                     y = unlist(standard_scaled_approx[[i]][2]),
                                                                     n = nrow(standard_scaled_approx[[i]])*4))
  }



  #
  ##
  ### Aligning the samples using optim
  ##
  #

  #First I create lists of files that are approximated to the same scale as a shortened standard standard

  standard_short <- standard_scaled_approx
  standard_short[[1]] <- subset(standard_short[[1]], x > xmin & x < xmax)

  xmin_short <- xmin - nshift
  xmax_short <- xmax - pshift

  standard_shorter <- standard_scaled_approx
  standard_shorter[[1]] <- subset(standard_shorter[[1]], x > xmin_short & x < xmax_short)

  #I use the shortened TTH to refine more approximation functions
  TTH_short <- unlist(standard_short[[1]][,1])
  TTH_shorter <- unlist(standard_shorter[[1]][,1])


  #Create the list that just contains shortened data

  sample_short <- list()
  for (i in 1:length(sample_scaled_approx)) {
    sample_short[[i]] <- data.frame(powdharm(x = sample_scaled_approx[[i]][,1],
                                                             y = sample_scaled_approx[[i]][,2],
                                                             xout = TTH_short))
  }
  names(sample_short) <- names(sample)


  #
  ##
  ### Detecting the peak shift required for each sample
  ##
  #

  #sample
  sample_optim <- list()
  for (i in 1:length(sample_short)) {
    sample_optim_out <- optim(a = sample_short[[i]], par = 0, xout = TTH_shorter, standard_shorter = standard_shorter,
                              pso, method = "Brent", lower = nshift, upper = pshift)
    sample_optim[[i]] <- sample_optim_out$par
  }
  names(sample_optim) <- names(sample_short)



  # NOW TO SHIFT THE DIFFRACTOGRAMS

  #sample
  sample_aligned <- sample
  for (i in 1:length(sample)) {
    sample_aligned[[i]][,1] <- sample_aligned[[i]][,1] + sample_optim[[i]]
  }

  ### Same for scaled data

  #sample scaled
  sample_scaled_aligned <- sample_scaled
  for (i in 1:length(sample_scaled)) {
    sample_scaled_aligned[[i]][,1] <- sample_scaled_aligned[[i]][1] + sample_optim[[i]]
  }



  ### Harmonising the data after the shifts

  #I want to find the maximum and minimum of the shifts

  all_shifts <- c(unlist(sample_optim))

  max_shift <- max(all_shifts)
  min_shift <- min(all_shifts)

  TTH_length <- nrow(sample[[1]])

  xmax_harm <- max(sample[[1]][,1])
  xmin_harm <- min(sample[[1]][,1])
  int_TTH <- ((sample[[1]][TTH_length,1] - sample[[1]][1,1])/(TTH_length-1))




  #This creates a vector that is pretty much identical to the original measurements, but with intervals that are exactly equal
  TTH_constant <- seq(xmin_harm, xmax_harm, int_TTH)

  new_xmin <- xmin_harm + max_shift
  new_xmax <- xmax_harm + min_shift


  #Now I create a slightly shortened version of the constant vector which will allow me to harmonise all of the data
  TTH_constant_short <- subset(TTH_constant,
                               TTH_constant > new_xmin & TTH_constant < new_xmax)

  #Now I finally create a new approximation function with the TTH_constant_short vector

  #
  ##
  ### FINAL HARMONISATION!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ##
  #

  # sample
  sample_aligned_harm <- list()
  for (i in 1:length(sample_aligned)) {
    sample_aligned_harm[[i]] <- data.frame(powdharm(x = sample_aligned[[i]][,1],
                                                                    y = sample_aligned[[i]][,2], xout = TTH_constant_short))
  }
  names(sample_aligned_harm) <- names(sample_aligned)


  # Same for scaled sample
  sample_scaled_aligned_harm <- list()
  for (i in 1:length(sample_scaled_aligned)) {
    sample_scaled_aligned_harm[[i]] <- data.frame(powdharm(x = sample_scaled_aligned[[i]][,1],
                                                                           y = sample_scaled_aligned[[i]][,2],
                                                                           xout = TTH_constant_short))
  }
  names(sample_scaled_aligned_harm) <- names(sample_scaled_aligned)


  ### Finally harmonise the standard standard
  standard_scaled_harm <- list()
  for (i in 1:length(standard_scaled)) {
    standard_scaled_harm[[i]] <- data.frame(powdharm(x = standard_scaled[[i]][,1],
                                                                     y = standard_scaled[[i]][,2],
                                                                     xout = TTH_constant_short))
  }
  names(standard_scaled_harm) <- names(standard_scaled)



  return(sample_aligned_harm)
}
