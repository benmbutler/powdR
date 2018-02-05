#' Full pattern fitting
#'
#' \code{fpf} returns estimates of soil mineral concentraitons using full pattern fitting.
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
#' @param align.shift The maximum shift that is allowed during initial 2theta alignment (degrees)
#' @param TTH.min The minimum value of 2theta used during fitting
#' @param TTH.max The maximum value of 2theta used during fitting
#' @param delta_lim The tuning parameter used to adjust sensitivity of fit. Must greater than 0 and
#' less than 100. Lower value = more sensitive. We recommend using 0.01.
#' @param solver The optimisation routine to be used. One of \code{c("BFGS", "Nelder-Mead", "CG")}
#' @param obj.function The objective function to minimise. One of \code{c("Delta", "R", "Rwp")}.
#' We recommend Rwp
#' @param int.std The mineral ID (e.g. "Qzt.662070.Strath.12Mins.P") to be used as internal standard. Must match a mineral
#' name in the MINERALS table.
#' @param fpf.shift Optional. The maximum shift applied during full pattern fitting.
#' @param amorphous Optional. The ID of an amorphous phase (e.g. "ORGANIC.337666")to be added to the fitting process.
#' Must match an ID provided in the \code{phases} parameter. Only 1 amorphous phase can be used.
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
#'
fpf <- function(smpl, lib, phases, int.std, amorphous, TTH.min, TTH.max, align.shift, solver, obj.function, fpf.shift) {

xrdlib <- lib

#subset xrdlib according to the phases vector

keep_index <- which(xrdlib[["MINERALS"]]$MIN_ID %in% phases)

xrdlib[["XRD"]] <- xrdlib[["XRD"]][, keep_index]
xrdlib[["MINERALS"]] <- xrdlib[["MINERALS"]][keep_index, ]

#read the sample
#smpl <- read.csv(file = s.dir, header = FALSE, sep = " ")

xrd.standard_df <- xrdlib[["XRD"]][, which(xrdlib[["MINERALS"]]$MIN_ID == int.std)]

xrd.standard <- data.frame(TTH = xrdlib[["TTH"]], COUNTS = xrd.standard_df)

#align the data
smpl <- xrd.align(xrd.sample = smpl, xrd.standard, xmin = TTH.min + (align.shift*2),
                    xmax = TTH.max - (align.shift*2), xshift = align.shift)

if (sqrt(smpl[[1]]^2) == align.shift) {
  message("The optimised shift used in alignment is equal to the maximum shift defined in the function call. We advise visual inspection of this alignment.")
}

smpl <- smpl[[2]]
#Define a 2TH scale to harmonise all data to
smpl_TTH <- smpl[, 1]

xrd_ref_names <- xrdlib[["MINERALS"]]$MIN_ID

#Ensure that sample in the reference library are on the same scale as the sample

xrdlib[["XRD"]] <- data.frame(lapply(names(xrdlib[["XRD"]]),
                                     function(n) approx(x = xrdlib[["TTH"]],
                                                        y = unname(unlist(xrdlib[["XRD"]][n])),
                                                        xout = smpl_TTH)[[2]]))

names(xrdlib[["XRD"]]) <- xrd_ref_names

#Replace the library TTH with that of the sample

xrdlib[["TTH"]] <- smpl_TTH

#get the number of patterns in the library
lib_length <- nrow(xrdlib[["MINERALS"]])

#### decrease 2TH scale to the range defined in the function call
smpl <- subset(smpl, smpl[,1] >= TTH.min & smpl[,1] <= TTH.max)

#Subset the XRD dataframe to
xrdlib[["XRD"]] <- xrdlib[["XRD"]][which(xrdlib[["TTH"]] >= TTH.min & xrdlib[["TTH"]] <= TTH.max), ]

#Replace the TTH in the library with the shortened one
xrdlib[["TTH"]] <- smpl[, 1]



##If amorphous is present in the argument, then extract the pattern to be used

if(!missing(amorphous)) {
  amorphous_counts <- xrdlib[["XRD"]][, amorphous]
  amorphous_tth <- xrdlib[["TTH"]]
  amorphous_xrd <- data.frame("TTH" = amorphous_tth, "COUNTS" = amorphous_counts)

  #remove the amorphous phase from the XRD library
  xrdlib[["XRD"]] <- xrdlib[["XRD"]][, -which(names(xrdlib[["XRD"]]) == amorphous)]
}

#Make sure that ther aren't any other amorphous phases left in the library

amorphous_index <- which(xrdlib[["MINERALS"]]$AMORPHOUS == 1)

#remove them if they've been identified
if(length(amorphous_index) > 0) {
  xrdlib[["XRD"]] <- xrdlib[["XRD"]][, -amorphous_index]
}


#--------------------------------------------
#Initial Optimisation
#--------------------------------------------

x <- rep(0, ncol(xrdlib[["XRD"]]))
names(x) <- names(xrdlib[["XRD"]])

o <- optim(par = x, fullpat,
           method = solver, pure.patterns = xrdlib[["XRD"]],
           sample.pattern = smpl[, 2], obj = obj.function)

#----------------------------------------------
#Apply shifts
#----------------------------------------------

fpf_aligned <- fpf.align(sample.tth = smpl[,1], sample.counts = smpl[,2],
                         xrd.lib = xrdlib, fpf_shift = fpf.shift,
                         pure.weights = o$par)

smpl <- fpf_aligned[["sample"]]
xrdlib[["XRD"]] <- fpf_aligned[["xrdlib_aligned"]]
xrdlib[["TTH"]] <- smpl[,1]

#Re-optimise after shifts

o <- optim(par = o$par, fullpat,
           method = solver, pure.patterns = xrdlib[["XRD"]],
           sample.pattern = smpl[, 2], obj = obj.function)

x <- o$par

#Add the amorphous phase

if(!missing(amorphous)) {
  #Add the amorphous phase to the library
  amorphous_counts2 <- approx(x = amorphous_tth, y = amorphous_counts,
                              method = "linear", xout = xrdlib[["TTH"]])[[2]]

  xrdlib$XRD <- data.frame(xrdlib$XRD)

  xrdlib[["XRD"]][amorphous] <- amorphous_counts2
  #Add an initial parameter to the library for the optimisation
  x[length(x) + 1] <- 0
  names(x)[length(x)] <- amorphous

  #reoptimise
  o <- optim(par = x, fullpat,
             method = solver, pure.patterns = xrdlib[["XRD"]],
             sample.pattern = smpl[, 2], obj = obj.function)
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
  remove_index <- which(x < 0)

  #remove the column from the library that contains the identified data
  if (length(which(x < 0)) > 0) {
    xrdlib[["XRD"]] <- xrdlib[["XRD"]][, -remove_index]
    x <- x[-remove_index]
  }

  o <- optim(par = x, fullpat,
             method = solver, pure.patterns = xrdlib[["XRD"]],
             sample.pattern = smpl[, 2], obj = obj.function)
  x <- o$par
  #identify whether any parameters are negative for the next iteration
  negpar <- min(x)
}

#compute fitted pattern and residuals
fitted_pattern <- apply(sweep(as.matrix(xrdlib[["XRD"]]), 2, x, "*"), 1, sum)

resid_x <- smpl[, 2] - fitted_pattern

#compute grouped mineral concentrations
min_concs <- min.conc(x = x, xrd.lib = xrdlib)

df <- min_concs[[1]]
dfs <- min_concs[[2]]



#### Compute the R statistic. This could be used to identify samples
# that require manual interpretation

obs_minus_calc <- (smpl[,2] - fitted_pattern)^2
sample_squared <- smpl[,2]^2


#R_fit <- sqrt(sum((sample[,2] - fitted_pattern)^2)/sum(sample[,2]^2))

R_fit <- sqrt(sum((1/smpl[,2]) * ((smpl[,2] - fitted_pattern)^2)) / sum((1/smpl[,2]) * (smpl[,2]^2)))

xrd <- data.frame(xrdlib[["XRD"]])

for (i in 1:ncol(xrd)) {
  xrd[,i] <- xrd[,i] * x[i]
}

#Define a list that becomes the function output
out <- list(smpl[,1], fitted_pattern, smpl[,2], resid_x, df, dfs, R_fit, xrd, x)
names(out) <- c("TTH", "FITTED", "MEASURED", "RESIDUALS",
                "MINERALS", "MINERALS_SUMMARY", "R", "WEIGHTED_PURE_PATTERNS", "COEFFICIENTS")

return(out)

}
