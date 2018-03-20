#' Full pattern fitting
#'
#' \code{fpf} returns estimates of soil mineral concentraitons using full pattern fitting.
#'
#' This function applies full pattern fitting to an XRPD sample to quantify mineral
#' concentrations.
#' It requires a library of reference patterns with pre-measured reference intensity ratios.
#'
#' @param smpl A data frame. First column is 2theta, second column is counts
#' @param lib The XRPD library. A list containing three elements. The first (\code{XRPD})
#' is a dataframe containing all pre-measured reference patterns by column. The second
#' (\code{tth}) is a vector of the 2theta measurement intervals for the reference patterns.
#' Third (\code{minerals}) is a data frame containing the unique ID, mineral name, and
#' reference intensity ratio of each pattern in the library. The order of \code{XRPD}
#' (by column) and \code{minerals} (by row) must be identical.
#' @param tth A vector defining the minimum and maximum 2theta values to be used during
#' fitting.
#' @param crystalline A string of mineral ID's used to subset crystalline phases from the
#' library. The ID's must match ID's in the \code{lib$minerals$MIND_ID} column.
#' @param std The mineral ID (e.g. "Qzt.662070.Strath.12Mins.P") to be used as internal
#' standard. Must match an ID provided in the \code{phases} parameter.
#' @param amorphous Optional. The ID's of amorphous phases (e.g. "ORGANIC.337666") to be
#' added to the fitting process.
#' @param align The maximum shift that is allowed during initial 2theta alignment (degrees).
#' Default = 0.1.
#' @param solver The optimisation routine to be used. One of \code{c("BFGS", "Nelder-Mead",
#' "CG")}. Default = "BFGS".
#' @param obj The objective function to minimise. One of \code{c("Delta", "R", "Rwp")}.
#' Default = \code{"Rwp"}.
#' @param shift Optional. The maximum shift applied during full pattern fitting.
#' Default = 0.05
#' @param weighting Optional. A two column dataframe. First column contains 2theta axis on
#' same scale as that of the xrd library. Second column contains the weighting of each 2theta
#' variable. If not provided, all variables are given a weighting of 1.
#'
#' @return a list with components:
#' \item{tth}{a vector of the 2theta scale of the fitted data}
#' \item{fitted}{a vector of the fitted XRPD pattern}
#' \item{measured}{a vector of the original XRPD measurement}
#' \item{residuals}{a vector of the Residuals of fitted vs measured}
#' \item{minerals}{a dataframe of the minerals used to produce the fitted pattern}
#' \item{minerals_summary}{the minerals dataframe grouped by minerals and summarised (mean)}
#' \item{rwp}{the Rwp of the fitted vs mesaured pattern}
#' \item{weighted_pure_patterns}{a dataframe of reference patterns used to produce the fitted pattern.
#' All patterns have been weighted according to the coefficients used in the fit}
#' \item{coefficients}{a named vector of coefficients used to produce the fitted pattern}
#'
#' @examples
#' #Load the minerals library
#' data(minerals)
#'
#' # Load the soils data
#' data(soils)
#'
#' # define the phases to include in the fit
#' c_phases <- c("FEL", "KAO",
#' "OLI", "QUA.1", "QUA.2")
#'
#' # without organic
#' \dontrun{
#' fpf_out <-  fpf(smpl = soils$sandstone,
#'                lib = minerals,
#'                tth = c(4.5, 69.5),
#'                crystalline = c_phases,
#'                std = "QUA.1")
#' }
#'
#' # Try fitting the same sample, but including an amorphous phase (organic matter)
#' \dontrun{
#' fpf_out_org <-  fpf(smpl = soils$sandstone,
#'                    lib = minerals,
#'                    tth = c(4.5, 69.5),
#'                    crystalline = c_phases,
#'                    std = "QUA.1",
#'                    amorphous = "ORG")
#' }
#'
#' # An example of using weighting
#' weighting <- data.frame(tth = minerals$tth,
#'                         counts = rep(1, length(minerals$tth)))
#'
#' # Make all values below tth = 5 have a weighting of 0
#' weighting$counts[which(weighting$tth <= 5)] <- 0
#'
#' # Make all values between tth = 26 and 27 have a weighting of 2
#' weighting$counts[which(weighting$tth >= 26 & weighting$tth <= 27)] <- 10
fpf <- function(smpl, lib, crystalline, std, amorphous,
                tth, align, solver, obj, shift, weighting) {

  #Create defaults for values that aren't specified.

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

  #create an empty vector if amorphous is not used
  if(missing(amorphous)) {
    amorphous <- c()
  }

  #Create a weighting vector containing all 1's if no other weighting vector is provided
  if(missing(weighting)) {
    weighting <- data.frame(tth = lib$tth,
                            counts = rep(1, length(lib$tth)))
  }

  #Ensure that the align_shift is greater than 0.
  if (align <= 0) {
    stop("The align argument must be greater than 0")
  }

  #Create a warning message if the shift is greater than 0.5, since this can confuse the optimisation
  if (align > 0.5) {
    warning("Be cautious of large 2theta shifts. These can cause issues in sample alignment.")
  }

  if (shift > 0.1) {
    warning("To speed computation and avoid erroneous alignments, the shift argument should be
            less than 0.1.")
  }

  #Make only "Nelder-Mead", "BFGS", or "CG" optional for the solver
  if (!solver %in% c("Nelder-Mead", "BFGS", "CG")) {
    stop("The solver argument must be one of 'BFGS', 'Nelder Mead' or 'CG'")
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

#subset lib according to the phases vector

keep <- which(lib$minerals$min_id %in% c(crystalline, amorphous))

lib$xrd <- lib$xrd[, keep]
lib$minerals <- lib$minerals[keep, ]


#if only one phase is being used, make sure it's a dataframe and named correctly
if (length(c(crystalline, amorphous)) == 1) {
  lib$xrd <- data.frame("phase" = lib$xrd)
  names(lib$xrd) <- c(crystalline, amorphous)
}


xrd.standard_df <- lib$xrd[, which(lib$minerals$min_id == std)]


xrd.standard <- data.frame(tth = lib$tth, counts = xrd.standard_df)

#align the data
smpl <- xrd.align(xrd.sample = smpl, xrd.standard, xmin = tth[1] + (align*2),
                    xmax = tth[2] - (align*2), xshift = align)

if (sqrt(smpl[[1]]^2) == align) {
  message("The optimised shift used in alignment is equal to the maximum shift defined
          in the function call. We advise visual inspection of this alignment.")
}

#smpl becomes a data frame
smpl <- smpl[[2]]

#Define a 2TH scale to harmonise all data to
smpl_tth <- smpl[[1]]

xrd_ref_names <- lib$minerals$min_id

#Ensure that sample in the reference library are on the same scale as the sample
lib$xrd <- data.frame(lapply(names(lib$xrd),
                                       function(n) stats::approx(x = lib$tth,
                                                          y = unname(unlist(lib$xrd[n])),
                                                          xout = smpl_tth)[[2]]))

names(lib$xrd) <- xrd_ref_names

#Replace the library tth with that of the sample
lib$tth <- smpl_tth

#adjust the weighting to the aligned 2theta scale
weighting <- data.frame(stats::approx(x = weighting[[1]], y = weighting[[2]], xout = lib$tth))

#### decrease 2TH scale to the range defined in the function call
smpl <- smpl[which(smpl$tth >= tth[[1]] & smpl[[1]] <= tth[2]), ]

weighting <- subset(weighting, weighting[[1]] >= tth[1] & weighting[[1]] <= tth[2])

#Subset the xrd dataframe too
lib$xrd <- lib$xrd[which(lib$tth >= tth[1] & lib$tth <= tth[2]), ]

#Replace the tth in the library with the shortened one
lib$tth <- smpl[, 1]

##If amorphous is present in the argument, then extract the pattern to be used

if(length(amorphous) > 0) {
  amorphous_counts <- lib$xrd[amorphous]
  amorphous_tth <- lib$tth
  amorphous_xrd <- data.frame("tth" = amorphous_tth, amorphous_counts)

  #remove the amorphous phase from the xrd library
  lib$xrd <- lib$xrd[, -which(names(lib$xrd) %in% amorphous)]
}

#if only one phase is being used, make sure it's a dataframe and named correctly
if (is.vector(lib$xrd)) {
  lib$xrd <- data.frame("phase" = lib$xrd)
  names(lib$xrd) <- crystalline
}

#--------------------------------------------
#Initial Optimisation
#--------------------------------------------

x <- rep(0, ncol(lib$xrd))
names(x) <- names(lib$xrd)

o <- stats::optim(par = x, fullpat,
           method = solver, pure.patterns = lib$xrd,
           sample.pattern = smpl[, 2], obj = obj, weighting = weighting)

#----------------------------------------------
#Apply shifts
#----------------------------------------------

if(shift > 0) {

fpf_aligned <- fpf.align(sample.tth = smpl[,1], sample.counts = smpl[,2],
                         xrd.lib = lib, fpf_shift = shift,
                         pure.weights = o$par, weighting = weighting)

smpl <- fpf_aligned[["sample"]]
lib$xrd <- fpf_aligned[["xrdlib_aligned"]]
lib$tth <- smpl[,1]

weighting <- fpf_aligned[["weighting"]]

#Re-optimise after shifts

o <- stats::optim(par = o$par, fullpat,
           method = solver, pure.patterns = lib$xrd,
           sample.pattern = smpl[, 2], obj = obj, weighting = weighting)

}

x <- o$par

#Add the amorphous phase

if(length(amorphous) > 0) {
  #Add the amorphous phase to the library
  amorphous_counts2 <- list()

  for (i in 1:ncol(amorphous_counts)) {
  amorphous_counts2[[i]] <- stats::approx(x = amorphous_tth, y = amorphous_counts[[i]],
                              method = "linear", xout = lib$tth)[[2]]
  names(amorphous_counts2)[i] <- names(amorphous_counts)[i]
  }

  if (length(amorphous_counts2) == 1) {
    amorphous_counts2 <- data.frame("amorphous" = amorphous_counts2[[1]])
    names(amorphous_counts2) <- amorphous
  } else {
  amorphous_counts2 <- data.frame(amorphous_counts2)
  }

  lib$xrd <- data.frame(lib$xrd)

  lib$xrd <- data.frame(lib$xrd, amorphous_counts2)
  #Add an initial parameter to the library for the optimisation
  xa <- rep(0, ncol(amorphous_counts))
  names(xa) <- names(amorphous_counts)

  x <- c(x, xa)

  #reoptimise
  o <- stats::optim(par = x, fullpat,
             method = solver, pure.patterns = lib$xrd,
             sample.pattern = smpl[, 2], obj = obj, weighting = weighting)
}

#-----------------------------------------------
# Remove negative parameters
#-----------------------------------------------

#setup an initial negpar that is negative so that the following while loop will
#run until no negative parameters are found
negpar <- -0.1

while (negpar < 0) {
  #use the most recently optimised coefficients
  x <- o$par
  #check for any negative parameters
  omit <- which(x < 0)

  #remove the column from the library that contains the identified data
  if (length(which(x < 0)) > 0) {
    lib$xrd <- lib$xrd[, -omit]
    x <- x[-omit]
  }

  o <- stats::optim(par = x, fullpat,
             method = solver, pure.patterns = lib$xrd,
             sample.pattern = smpl[, 2], obj = obj, weighting = weighting)
  x <- o$par
  #identify whether any parameters are negative for the next iteration
  negpar <- min(x)
}

#compute fitted pattern and residuals
fitted_pattern <- apply(sweep(as.matrix(lib$xrd), 2, x, "*"), 1, sum)

resid_x <- smpl[, 2] - fitted_pattern

#compute grouped mineral concentrations
min_concs <- qminerals(x = x, xrd.lib = lib)

df <- min_concs[[1]]
dfs <- min_concs[[2]]



#### Compute the R statistic. This could be used to identify samples
# that require manual interpretation

obs_minus_calc <- (smpl[,2] - fitted_pattern)^2
sample_squared <- smpl[,2]^2


#R_fit <- sqrt(sum((sample[,2] - fitted_pattern)^2)/sum(sample[,2]^2))

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
