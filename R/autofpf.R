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
#' containing all pre-measured reference patterns by column. The second (\code{TTH}) is a vector of the
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
#' @return a list with components:
#' \item{TTH}{A vector of the 2theta scale of the fitted data}
#' \item{FITTED}{A vector of the fitted XRPD pattern}
#' \item{MEASURED}{A vector of the original XRPD measurement}
#' \item{RESIDUALS}{A vector of the Residuals of FITTED vs MEASURED}
#' \item{MINERALS}{A dataframe of the minerals used to produced FITTED}
#' \item{MINERALS_SUMMARY}{The MINERALS dataframe grouped by minerals and summarised (mean)}
#' \item{R}{The Rwp of the FITTED vs MEASURED pattern}
#' \item{WEIGHTED_PURE_PATTERNS}{A dataframe of reference patterns used to produce FITTED.
#' All patterns have been weighted according to the coefficients used in the fit}
#' \item{COEFFICIENTS}{A named vector of coefficients used to produce FITTED}
#' @examples
#' # Load the Xpert library
#' data(Xpert)
#'
#' # Load the Xpert soil data to use in example
#' data(Xpert_soil)
#' # automated without any amorphous phases
#' # not run
#' # fpf_out <- auto.fpf(smpl = Xpert_soil$mineral,
#' #                    lib = Xpert,
#' #                    tth = c(3.5, 69.5),
#' #                    std = "Qzt.662070.Strath.12Mins.P")
#'
#' #automated with an amorphous phase (organic matter)
#' #not run
#' #fpf_out_org <- auto.fpf(smpl = Xpert_soil$mineral,
#' #                    lib = Xpert,
#' #                    tth = c(3.5, 69.5),
#' #                    std = "Qzt.662070.Strath.12Mins.P",
#' #                    amorphous = "ORGANIC.337666")
#'
#' # An example of using weighting
#' weighting <- data.frame(TTH = Xpert$TTH,
#'                         COUNTS = rep(1, length(Xpert$TTH)))
#'
#' # Make all values below TTH = 5 have a weighting of 0
#' weighting$COUNTS[which(weighting$TTH <= 5)] <- 0
#'
#' # Make all values between TTH = 26 and 27 have a weighting of 2
#' weighting$COUNTS[which(weighting$TTH >= 26 & weighting$TTH <= 27)] <- 10
auto.fpf <- function(smpl, lib, tth, std, amorphous, coarse = 0.1, align = 0.1, solver = "BFGS", obj = "Rwp",  shift = 0.05, weighting) {

  if(missing(weighting)) {
    weighting <- data.frame(TTH = lib$TTH,
                            COUNTS = rep(1, length(lib$TTH)))
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

  #Provide recommendations of which mineral to use as the internal standard
  #if (!std %in% c("Corundum", "Quartz", "Sylvite", "Halite", "Rutile")) {
  #  warning("We recommend internal standards be highly crystalline, strong diffractors e.g. corundum, quartz, sylvite, #halite and rutile.")
  #}

  xrdlib <- lib

  #Make sure that the mineral identified as the internal standard is contained within the reference library
  if (!std %in% xrdlib[["MINERALS"]]$MIN_ID) {
    stop("The mineral you have specified as the internal standard is not in the reference library")
  }

  sample <- smpl

  #Make sure that the defined TTH arguments are within the range of the samples and reference library.
  if (tth[1] < min(sample[,1])) {
    stop("tth[1] must exceed the minimum 2theta value of the sample")
  }
  if (tth[2] > max(sample[,1])) {
    stop("tth[2] must be lower than the maximum 2theta value of the sample")
  }

  if (tth[1] < min(xrdlib[["TTH"]])) {
    stop("tth[1] must be within the 2theta range of the reference library")
  }
  if (tth[2] > max(xrdlib[["TTH"]])) {
    stop("tth[2] must be within the 2theta range of the reference library")
  }
  ##############
  #INITIAL SAMPLE ALIGNMENT USING THE xrd.align function
  ##############

  xrd.standard_df <- xrdlib[["XRD"]][, which(xrdlib[["MINERALS"]]$MIN_ID == std)]

  if (length(which(xrdlib[["MINERALS"]]$MIN_ID == std)) > 1) {
    xrd.standard_df <- rowMeans(xrd.standard_df)
  }

  xrd.standard <- data.frame(TTH = xrdlib[["TTH"]], COUNTS = xrd.standard_df)

  #align the data
  sample <- xrd.align(xrd.sample = sample, xrd.standard, xmin = tth[1] + (align*2),
                      xmax = tth[2] - (align*2), xshift = align)

  if (sqrt(sample[[1]]^2) == align) {
    message("The optimised shift used in alignment is equal to the maximum shift defined in the function call. We advise visual inspection of this alignment.")
  }

  sample <- sample[[2]]
  #Define a 2TH scale to harmonise all data to
  sample_TTH <- sample[, 1]

  xrd_ref_names <- xrdlib[["MINERALS"]]$MIN_ID

  #Ensure that sample in the reference library are on the same scale as the sample

  xrdlib[["XRD"]] <- data.frame(lapply(names(xrdlib[["XRD"]]),
                                       function(n) approx(x = xrdlib[["TTH"]],
                                                          y = unname(unlist(xrdlib[["XRD"]][n])),
                                                          xout = sample_TTH)[[2]]))

  names(xrdlib[["XRD"]]) <- xrd_ref_names

  #Replace the library TTH with that of the sample

  xrdlib[["TTH"]] <- sample_TTH

  #get the number of patterns in the library
  lib_length <- nrow(xrdlib[["MINERALS"]])

  #adjust the weighting to the aligned 2theta scale
  weighting <- data.frame(approx(x = weighting$TTH, y = weighting$COUNTS, xout = xrdlib$TTH))

  #### decrease 2TH scale to the range defined in the function call
  sample <- subset(sample, sample[,1] >= tth[1] & sample[,1] <= tth[2])

  weighting <- subset(weighting, weighting$x >= tth[1] & weighting$x <= tth[2])

  #Subset the XRD dataframe to
  xrdlib[["XRD"]] <- xrdlib[["XRD"]][which(xrdlib[["TTH"]] >= tth[1] & xrdlib[["TTH"]] <= tth[2]), ]

  #Replace the TTH in the library with the shortened one
  xrdlib[["TTH"]] <- sample[, 1]

  #Extract amorphous phases from the harmonised list to exclude them from analysis
  #also exclude background parameters

  amorphous_index <- which(xrdlib[["MINERALS"]]$AMORPHOUS == 1)

  #If amorphous is present in the argument, then extract the patterns to be used

  if(!missing(amorphous)) {
    amorphous_counts <- xrdlib[["XRD"]][, amorphous]
    amorphous_tth <- xrdlib[["TTH"]]
    amorphous_xrd <- data.frame("TTH" = amorphous_tth, "COUNTS" = amorphous_counts)
  }

  if(length(amorphous_index) > 0) {
    xrdlib[["XRD"]] <- xrdlib[["XRD"]][, -amorphous_index]
  }

  #Removing library patterns that have a standard deviation of 0

  flat_ref <- which(unlist(lapply(xrdlib[["XRD"]], sd)) == 0)
  if(length(flat_ref) > 0) {
    xrdlib[["XRD"]] <- xrdlib[["XRD"]][, -flat_ref]
    xrdlib[["MINERALS"]] <- xrdlib[["MINERALS"]][-flat_ref, ]
  }

  #Use the autoID function to select the appropriate samples from the library
  autoID <- xrd.autoID(xrd.lib = xrdlib[["XRD"]],
                       xrd.sample = sample[,2], delta_lim = coarse)

  x <- autoID[["x"]]
  xrdlib[["XRD"]] <- autoID[["xrd.lib"]]


  #OPTIMISATION

  #optimise using objective function rather than qr.solve
  o <- optim(par = x, fullpat,
             method = solver, pure.patterns = xrdlib[["XRD"]],
             sample.pattern = sample[, 2], obj = obj, weighting = weighting)


  #Alignment and then another optimisation ONLY is the fpf.align parameters
  #is included


  if(shift > 0) {

    fpf_aligned <- fpf.align(sample.tth = sample[,1], sample.counts = sample[,2],
                             xrd.lib = xrdlib, fpf_shift = shift,
                             pure.weights = o$par, weighting = weighting)

    sample <- fpf_aligned[["sample"]]
    xrdlib[["XRD"]] <- fpf_aligned[["xrdlib_aligned"]]
    xrdlib[["TTH"]] <- sample[,1]

    weighting <- fpf_aligned[["weighting"]]

    #Re-optimise after shifts

    o <- optim(par = o$par, fullpat,
               method = solver, pure.patterns = xrdlib[["XRD"]],
               sample.pattern = sample[, 2], obj = obj, weighting = weighting)
  }

  #Removing negative parameters

  #setup an initial negpar that is negative so that the following while loop will
  #run until no negative parameters are found
  negpar <- -0.1

  while (negpar < 0) {
    #use the most recently optimised coefficients
    x <- o$par
    #check for any negative parameters
    remove_index <- which(x < 0)

    #remove the column from the library that contains the identified data
    if (length(which(x < 0)) > 0) {
      xrdlib[["XRD"]] <- xrdlib[["XRD"]][, -remove_index]
      x <- x[-remove_index]
    }

    o <- optim(par = x, fullpat,
               method = solver, pure.patterns = xrdlib[["XRD"]],
               sample.pattern = sample[,2], obj = obj, weighting = weighting)
    x <- o$par
    #identify whether any parameters are negative for the next iteration
    negpar <- min(x)
  }

  #Now that some negative parameters have been removed, the detection limits
  #of the remaining phases are estimated.

  # Removing phases based on detection limits (note that the amorphous
  #phase still hasn't been added yet!)

  #Calculate the LLD and remove any phases below it
  xrd_detectable <- xrd.LLD(x = x, xrd.sample = sample, xrd.lib = xrdlib,
                            int_std = std)

  x <- xrd_detectable[["x"]]
  xrdlib[["XRD"]] <- xrd_detectable[["xrd.lib"]]

  #Add the amorphous phase

  if(!missing(amorphous)) {
    #Add the amorphous phase to the library
    amorphous_counts2 <- approx(x = amorphous_tth, y = amorphous_counts,
                                method = "linear", xout = xrdlib[["TTH"]])[[2]]

    xrdlib[["XRD"]][amorphous] <- amorphous_counts2
    #Add an initial parameter to the library for the optimisation
    x[length(x) + 1] <- 0
    names(x)[length(x)] <- amorphous
  }


  #Re-optimise now that phases below detection limit have been removed and
  #the amorphous phase added
  o <- optim(par = x, fullpat,
             method = solver, control = list(), pure.patterns = xrdlib[["XRD"]],
             sample.pattern = sample[,2], obj = obj, weighting = weighting)

  fitted_pattern <- apply(sweep(as.matrix(xrdlib[["XRD"]]), 2, o$par, "*"), 1, sum)

  resid_x <- sample[, 2] - fitted_pattern

  x <- o$par


  #Calculate mineral concentrations after amorphous phase was added

  min_concs <- min.conc(x = x, xrd.lib = xrdlib)

  df <- min_concs[[1]]
  dfs <- min_concs[[2]]


  #######
  #LASTLLLLLY I NEED TO REMOVE THE AMORPHOUS PHASE IF IT IS BELOW A SET THRESHOLD
  #This threshold is set to 1 % for the moment

  if(!missing(amorphous)) {

    if(df$min_percent[which(df$MIN_ID == amorphous)] < 1) {
      #Remove amorphous phase from library
      xrdlib[["XRD"]][amorphous] <- NULL
      x_index <- which(names(x) == amorphous)
      x <- x[-x_index]

      #recompute mineral percentages

      #reoptimise

      o <- optim(par = x, fullpat,
                 method = solver, control = list(), pure.patterns = xrdlib[["XRD"]],
                 sample.pattern = sample[,2], obj = obj, weighting = weighting)
      x <- o$par

      min_concs <- min.conc(x = x, xrd.lib = xrdlib)
      df <- min_concs[[1]]
      dfs <- min_concs[[2]]

      #Calculate the fitted pattern and resids again

      fitted_pattern <- apply(sweep(as.matrix(xrdlib[["XRD"]]), 2, x, "*"), 1, sum)
      resid_x <- sample[, 2] - fitted_pattern
    }
  }



  #### Compute the R statistic. This could be used to identify samples
  # that require manual interpretation

  obs_minus_calc <- (sample[,2] - fitted_pattern)^2
  sample_squared <- sample[,2]^2


  #R_fit <- sqrt(sum((sample[,2] - fitted_pattern)^2)/sum(sample[,2]^2))

  R_fit <- sqrt(sum((1/sample[,2]) * ((sample[,2] - fitted_pattern)^2)) / sum((1/sample[,2]) * (sample[,2]^2)))

  xrd <- data.frame(xrdlib[["XRD"]])

  for (i in 1:ncol(xrd)) {
    xrd[,i] <- xrd[,i] * x[i]
  }

  #If only 1 pattern is used in the fit, then rename it
  if (ncol(xrd) == 1) {
    names(xrd) <- df$MIN_ID[1]
  }

  #Define a list that becomes the function output
  out <- list(sample[,1], fitted_pattern, sample[,2], resid_x, df, dfs, R_fit, xrd, x)
  names(out) <- c("TTH", "FITTED", "MEASURED", "RESIDUALS",
                  "MINERALS", "MINERALS_SUMMARY", "R", "WEIGHTED_PURE_PATTERNS", "COEFFICIENTS")

  return(out)

}
