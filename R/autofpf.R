#' Automated full pattern fitting
#'
#' \code{auto.fpf} returns estimates of soil mineral concentraitons using full pattern
#'  fitting. This automated version attempts to extract the most appropriate phases
#'  from the library.
#'
#' This function applies full pattern fitting to an XRPD sample to quantify mineral concentrations.
#' It requires a library of reference patterns with pre-measured reference intensity ratios.
#'
#' @param smpl A data frame. First column is 2theta, second column is counts
#' @param lib The XRPD library. A list containing three elements. The first (\code{XRPD}) is a dataframe
#' containing all pre-measured reference patterns by column. The second (\code{tth}) is a vector of the
#' 2theta measurement intervals for the reference patterns. Third (\code{MINERALS}) is a data frame
#' containing the unique ID, mineral name, and reference intensity ratio of each pattern in the library.
#' The order of \code{XRPD} (by column) and \code{MINERALS} (by row) must be identical.
#' @param tth A vector defining the minimum and maximum 2theta values to be used during
#' fitting
#' @param phases A string of unique ID's from the \code{lib} used to subset the reference library.
#' @param std The mineral ID (e.g. "Qzt.662070.Strath.12Mins.P") to be used as internal standard. Must match a mineral
#' name in the MINERALS table.
#' @param amorphous Optional. Then name of an amorphous phase to be added to the fitting process. Must
#' match an ID in the MINERALS table.
#' @param coarse The tuning parameter used to adjust coarseness of fit. Must be greater than 0 and
#' less than 100. Low values are more sensitive to minor phases. Default = 0.1.
#' @param align The maximum shift that is allowed during initial 2theta alignment (degrees). Default = 0.1.
#' @param solver The optimisation routine to be used. One of \code{c("BFGS", "Nelder-Mead", "CG")}. Default = \code{"BFGS"}.
#' @param obj The objective function to minimise. One of \code{c("Delta", "R", "Rwp")}. Default = \code{"Rwp"}.
#' @param shift Optional. The maximum shift applied during full pattern fitting. Default = 0.05.
#' @param weighting Optional. A two column dataframe. First column contains 2theta axis on same scale as that
#' of the XRD library. Second column contains the weighting of each 2theta variable. If not provided, all
#'variables are given a weighting of 1.
#' @param lld Optional parameter used to tune the lower limit of dection computation.
#' Must be between 0 and 1. Default = 0.3.
#' @param amorphous_lld Optional parameter used to exclude amorphous phases if they are below the
#' specified \code{amorphous_lld} (percent). Must be between 0 and 100. Default = 0.
#'
#' @return a list with components:
#' \item{tth}{a vector of the 2theta scale of the fitted data}
#' \item{fitted}{a vector of the fitted XRPD pattern}
#' \item{measured}{a vector of the original XRPD measurement}
#' \item{residuals}{a vector of the Residuals of fitted vs measured}
#' \item{minerals}{a dataframe of the minerals used to produce fitted}
#' \item{minerals_summary}{the MINERALS dataframe grouped by minerals and summarised (mean)}
#' \item{rwp}{the Rwp of the fitted vs measured pattern}
#' \item{weighted_pure_patterns}{a dataframe of reference patterns used to produce the fitted
#' pattern. All patterns have been weighted according to the coefficients used in the fit}
#' \item{coefficients}{a named vector of coefficients used to produce the fitted pattern}
#'
#' @examples
#' # Load the D8 NSIS library
#' data(D8_NSIS)
#'
#' # Load the D8 soil data to use in example
#' data(D8_soil)
#' # automated without any amorphous phases
#' \dontrun{
#' fpf_out <- auto.fpf(smpl = D8_soil$mineral,
#'                     lib = D8_NSIS,
#'                     tth = c(5.0, 69.5),
#'                     std = "QUARTZ.STRATH.P.1142250")
#' }
#' #automated with an amorphous phase (organic matter)
#' \dontrun{
#' fpf_out_org <- auto.fpf(smpl = D8_soil$mineral,
#'                     lib = D8_NSIS,
#'                     tth = c(5.0, 69.5),
#'                     std = "QUARTZ.STRATH.P.1142250",
#'                     amorphous = c("ORGANIC.bez.CRAIGLICHT.668085.DEEP",
#'                                   "ALL.11.P.997615",
#'                                   "FER.2LINE.P.1021047",
#' }                                 "OBSID.P.AMOR.1.828441.882233"))
#'
#' # An example of using weighting
#' weighting <- data.frame(tth = D8_NSIS$tth,
#'                         counts = rep(1, length(D8_NSIS$tth)))
#'
#' # Make all values below tth = 5 have a weighting of 0
#' weighting$counts[which(weighting$tth <= 5)] <- 0
#'
#' # Make all values between tth = 26 and 27 have a weighting of 2
#' weighting$counts[which(weighting$tth >= 26 & weighting$tth <= 27)] <- 10
auto.fpf <- function(smpl, lib, tth, std, amorphous, coarse, align,
                     solver, obj,  shift, weighting, lld, amorphous_lld) {

  #Create defaults for values that aren't specified.

  if(missing(coarse)) {
    coarse = 0.1
  }

  if(missing(align)) {
    align = 0.1
  }

  if(missing(solver)) {
    solver = "BFGS"
  }

  if(missing(obj)) {
    obj = "Rwp"
  }

  if(missing(shift)) {
    shift = 0.05
  }

  if(missing(lld)) {
    lld = 0.3
  }

  if(missing(weighting)) {
    weighting <- data.frame(tth = lib$tth,
                            counts = rep(1, length(lib$tth)))
  }

  if (lld < 0 | lld > 1) {
    stop("The lld argument must be between 0 and 1")
  }

  if(!missing(amorphous)) {

    if(missing(amorphous_lld)) {
      amorphous_lld <- 0
    }

    if (amorphous_lld < 0 | amorphous_lld > 100) {
    stop("The amorphous_lld argument must be between 0 and 100")
    }

  }

  #Also warn if coarse is greater than 10 because this would give a strange fit
  if (coarse >= 10) {
    warning("A coarse value greater than 10 may not produce an accurate fit.")
  }

  #Ensure that the align is greater than 0.
  if (align <= 0) {
    stop("The align argument must be greater than 0")
  }

  #Create a warning message if the shift is greater than 1, since this confuse the optimisation
  if (align > 0.5) {
    warning("Be cautious of large 2theta shifts. These can cause issues in sample alignment.")
  }

  if (shift > 0.1) {
    warning("To speed computation and avoid erroneous alignments, the shift argument should be less than 0.1.")
  }

  #Make only "Nelder-Mead", "BFGS", or "CG" optional for the solver
  if (!solver %in% c("Nelder-Mead", "BFGS", "CG")) {
    stop("The solver argument must be one of 'BFGS', 'Nelder Mead' or 'CG'")
  }

  #Make sure that the coarse argument is set to be greater than 0, but less than 100
  if (coarse > 100) {
    stop("The coarse argument must be a value greater than 0 and less than 100")
  }

  if (coarse <= 0) {
    stop("The coarse argument must be a value greater than 0 and less than 100")
  }

  #Make sure that the mineral identified as the internal standard is contained within the reference library
  if (!std %in% lib$minerals$min_id) {
    stop("The mineral you have specified as the internal standard is not in the reference library")
  }

  #Make sure that the defined tth arguments are within the range of the samples and reference library.
  if (tth[1] < min(smpl[,1])) {
    stop("tth[1] must exceed the minimum 2theta value of the sample")
  }
  if (tth[2] > max(smpl[,1])) {
    stop("tth[2] must be lower than the maximum 2theta value of the sample")
  }

  if (tth[1] < min(lib$tth)) {
    stop("tth[1] must be within the 2theta range of the reference library")
  }
  if (tth[2] > max(lib$tth)) {
    stop("tth[2] must be within the 2theta range of the reference library")
  }

  ##############
  #INITIAL SAMPLE ALIGNMENT USING THE xrd.align function
  ##############

  xrd.standard_df <- lib$xrd[, which(lib$minerals$min_id == std)]

  xrd.standard <- data.frame(tth = lib$tth, counts = xrd.standard_df)

  #align the data
  smpl <- xrd.align(xrd.sample = smpl, xrd.standard, xmin = tth[1] + (align*2),
                      xmax = tth[2] - (align*2), xshift = align)

  if (sqrt(smpl[[1]]^2) == align) {
    message("The optimised shift used in alignment is equal to the maximum shift defined
            in the function call. We advise visual inspection of this alignment.")
  }

  smpl <- smpl[[2]]
  #Define a 2TH scale to harmonise all data to
  smpl_tth <- smpl[, 1]

  xrd_ref_names <- lib$minerals$min_id

  #Ensure that sample in the reference library are on the same scale as the sample

  lib$xrd <- data.frame(lapply(names(lib$xrd),
                                       function(n) approx(x = lib$tth,
                                                          y = unname(unlist(lib$xrd[n])),
                                                          xout = smpl_tth)[[2]]))

  names(lib$xrd) <- xrd_ref_names

  #Replace the library tth with that of the sample

  lib$tth <- smpl_tth

  #get the number of patterns in the library
  lib_length <- nrow(lib$minerals)

  #adjust the weighting to the aligned 2theta scale
  weighting <- data.frame(approx(x = weighting$tth, y = weighting$counts, xout = lib$tth))

  #### decrease 2TH scale to the range defined in the function call
  smpl <- smpl[which(smpl[[1]] >= tth[1] & smpl[[1]] <= tth[2]), ]

  weighting <- subset(weighting, weighting$x >= tth[1] & weighting$x <= tth[2])

  #Subset the XRD dataframe to
  lib$xrd <- lib$xrd[which(lib$tth >= tth[1] & lib$tth <= tth[2]), ]

  #Replace the tth in the library with the shortened one
  lib$tth <- smpl[, 1]

  #Extract amorphous phases from the harmonised list to exclude them from analysis
  #also exclude background parameters

  amorphous_index <- which(lib$minerals$amorphous == 1)

  #If amorphous is present in the argument, then extract the patterns to be used

  if(!missing(amorphous)) {
    amorphous_counts <- lib$xrd[amorphous]
    amorphous_tth <- lib$tth
    amorphous_xrd <- data.frame("tth" = amorphous_tth, amorphous_counts)
  }

  if(length(amorphous_index) > 0) {
    lib$xrd <- lib$xrd[, -amorphous_index]
  }

  #Removing library patterns that have a standard deviation of 0

  flat_ref <- which(unlist(lapply(lib$xrd, sd)) == 0)
  if(length(flat_ref) > 0) {
    lib$xrd <- lib$xrd[, -flat_ref]
    lib$minerals <- lib$minerals[-flat_ref, ]
  }

  #Use the autoid function to select the appropriate samples from the library
  autoid <- xrd.autoid(xrd.lib = lib$xrd,
                       xrd.sample = smpl[,2], delta_lim = coarse)

  x <- autoid[["x"]]
  lib$xrd <- autoid[["xrd.lib"]]


  #OPTIMISATION

  #optimise using objective function rather than qr.solve
  o <- optim(par = x, fullpat,
             method = solver, pure.patterns = lib$xrd,
             sample.pattern = smpl[, 2], obj = obj, weighting = weighting)


  #Alignment and then another optimisation ONLY is the fpf.align parameters
  #is included


  if(shift > 0) {

    fpf_aligned <- fpf.align(sample.tth = smpl[,1], sample.counts = smpl[,2],
                             xrd.lib = lib, fpf_shift = shift,
                             pure.weights = o$par, weighting = weighting)

    smpl <- fpf_aligned[["sample"]]
    lib$xrd <- fpf_aligned[["xrdlib_aligned"]]
    lib$tth <- smpl[,1]

    weighting <- fpf_aligned[["weighting"]]

    #Re-optimise after shifts

    o <- optim(par = o$par, fullpat,
               method = solver, pure.patterns = lib$xrd,
               sample.pattern = smpl[, 2], obj = obj, weighting = weighting)
  }

  #Now add amorphous phases and re-optimise

  x <- o$par

  #Add the amorphous phases

  if(!missing(amorphous)) {
    #Add the amorphous phase to the library
    amorphous_counts2 <- list()

    for (i in 1:ncol(amorphous_counts)) {
      amorphous_counts2[[i]] <- approx(x = amorphous_tth, y = amorphous_counts[[i]],
                                       method = "linear", xout = lib$tth)[[2]]
      names(amorphous_counts2)[i] <- names(amorphous_counts)[i]
    }
    amorphous_counts2 <- data.frame(amorphous_counts2)

    lib$xrd <- data.frame(lib$xrd, amorphous_counts2)

    #Add an initial parameter to the library for the optimisation
    xa <- rep(0, ncol(amorphous_counts))
    names(xa) <- names(amorphous_counts)

    x <- c(x, xa)

    o <- optim(par = x, fullpat,
               method = solver, control = list(), pure.patterns = lib$xrd,
               sample.pattern = smpl[,2], obj = obj, weighting = weighting)
  }


  #Removing negative parameters

  #Sometimes amorphous phases can end up removed here when they should actually be included

  #setup an initial negpar that is negative so that the following while loop will
  #run until no negative parameters are found
  negpar <- -0.1

  if (!missing(amorphous)) {

  while (negpar < 0) {
    #use the most recently optimised coefficients
    x <- o$par
    #check for any negative parameters, but ensure that amorphous phases are retained
    remove_index <- which(x < 0 & !names(x) %in% amorphous)

    #remove the column from the library that contains the identified data
    if (length(remove_index) > 0) {
      lib$xrd <- lib$xrd[, -remove_index]
      x <- x[-remove_index]
    }

    o <- optim(par = x, fullpat,
               method = solver, pure.patterns = lib$xrd,
               sample.pattern = smpl[,2], obj = obj, weighting = weighting)
    x <- o$par
    #identify whether any parameters are negative for the next iteration
    negpar <- min(x[-which(names(x) %in% amorphous)])
  }

  } else {

    while (negpar < 0) {
      #use the most recently optimised coefficients
      x <- o$par
      #check for any negative parameters, but ensure that amorphous phases are retained
      remove_index <- which(x < 0)

      #remove the column from the library that contains the identified data
      if (length(remove_index) > 0) {
        lib$xrd <- lib$xrd[, -remove_index]
        x <- x[-remove_index]
      }

      o <- optim(par = x, fullpat,
                 method = solver, pure.patterns = lib$xrd,
                 sample.pattern = smpl[,2], obj = obj, weighting = weighting)
      x <- o$par
      #identify whether any parameters are negative for the next iteration
      negpar <- min(x)
    }

  }

  #Now that some negative parameters have been removed, the detection limits
  #of the remaining phases are estimated.

  # Removing phases based on detection limits

  #Calculate the lld and remove any phases below it
  xrd_detectable <- xrd.lld(x = x, xrd.sample = smpl, xrd.lib = lib,
                            int_std = std, lld = lld)

  #Omit the other phases but make sure amorphous phases are retained in specified


  if (!missing(amorphous)) {

    remove_index <- which(names(xrd_detectable[["x"]]) %in% amorphous)

  if (length(remove_index) > 0) {
  x <- c(xrd_detectable[["x"]][-remove_index],
         x[which(names(x) %in% amorphous)])

  lib$xrd <- data.frame(xrd_detectable[["xrd.lib"]]
                                [, -remove_index],
                                lib$xrd[, which(names(lib$xrd) %in% amorphous)])

  names(lib$xrd) <- names(x)

  } else {
    x <- c(xrd_detectable[["x"]],
           x[which(names(x) %in% amorphous)])

    lib$xrd <- data.frame(xrd_detectable[["xrd.lib"]],
                                  lib$xrd[, which(names(lib$xrd) %in% amorphous)])

    names(lib$xrd) <- names(x)
  }

  } else {

   x <- xrd_detectable[["x"]]
   lib$xrd <- xrd_detectable[["xrd.lib"]]

  }


  #Re-optimise now that phases below detection limit have been removed
  o <- optim(par = x, fullpat,
             method = solver, control = list(), pure.patterns = lib$xrd,
             sample.pattern = smpl[,2], obj = obj, weighting = weighting)

  fitted_pattern <- apply(sweep(as.matrix(lib$xrd), 2, o$par, "*"), 1, sum)

  resid_x <- smpl[, 2] - fitted_pattern

  x <- o$par


  #Calculate mineral concentrations after amorphous phase was added

  min_concs <- min.conc(x = x, xrd.lib = lib)

  df <- min_concs[[1]]
  dfs <- min_concs[[2]]


  #######
  #Remove amorphous phases if below amorphous_lld

  if(!missing(amorphous)) {

    remove_amorphous <- which(names(x) %in% df$min_id[which(df$amorphous == 1 &
                                                              df$min_percent < amorphous_lld)])

    while (length(remove_amorphous) > 0) {
      #Remove amorphous phase from library
      lib$xrd <- lib$xrd[-remove_amorphous]
      x <- x[-remove_amorphous]

      #recompute mineral percentages

      #reoptimise

      o <- optim(par = x, fullpat,
                 method = solver, control = list(), pure.patterns = lib$xrd,
                 sample.pattern = smpl[,2], obj = obj, weighting = weighting)
      x <- o$par

      min_concs <- min.conc(x = x, xrd.lib = lib)
      df <- min_concs[[1]]
      dfs <- min_concs[[2]]

      #Calculate the fitted pattern and resids again

      fitted_pattern <- apply(sweep(as.matrix(lib$xrd), 2, x, "*"), 1, sum)
      resid_x <- smpl[, 2] - fitted_pattern

      remove_amorphous <- which(names(x) %in% df$min_id[which(df$amorphous == 1 &
                                                                df$min_percent < amorphous_lld)])
    }
  }



  #### Compute the R statistic. This could be used to identify samples
  # that require manual interpretation

  obs_minus_calc <- (smpl[,2] - fitted_pattern)^2
  smpl_squared <- smpl[,2]^2


  #R_fit <- sqrt(sum((smpl[,2] - fitted_pattern)^2)/sum(smpl[,2]^2))

  R_fit <- sqrt(sum((1/smpl[,2]) * ((smpl[,2] - fitted_pattern)^2)) / sum((1/smpl[,2]) * (smpl[,2]^2)))

  xrd <- data.frame(lib$xrd)

  for (i in 1:ncol(xrd)) {
    xrd[,i] <- xrd[,i] * x[i]
  }

  #If only 1 pattern is used in the fit, then rename it
  if (ncol(xrd) == 1) {
    names(xrd) <- df$min_id[1]
  }

  #Define a list that becomes the function output
  out <- list(smpl[,1], fitted_pattern, smpl[,2], resid_x, df, dfs, R_fit, xrd, x)
  names(out) <- c("tth", "fitted", "measured", "residuals",
                  "minerals", "minerals_summary", "rwp", "weighted_pure_patterns", "coefficients")

  return(out)

}
