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
#' (\code{TTH}) is a vector of the 2theta measurement intervals for the reference patterns.
#' Third (\code{MINERALS}) is a data frame containing the unique ID, mineral name, and
#' reference intensity ratio of each pattern in the library. The order of \code{XRPD}
#' (by column) and \code{MINERALS} (by row) must be identical.
#' @param tth A vector defining the minimum and maximum 2theta values to be used during
#' fitting
#' @param phases A string of mineral ID's used to subset the library.
#' @param std The mineral ID (e.g. "Qzt.662070.Strath.12Mins.P") to be used as internal
#' standard. Must match a mineral name in the MINERALS table.
#' @param amorphous Optional. The ID of an amorphous phase (e.g. "ORGANIC.337666")to be
#' added to the fitting process. Must match an ID provided in the \code{phases} parameter.
#' Only 1 amorphous phase can be used.
#' @param align The maximum shift that is allowed during initial 2theta alignment (degrees). Default = 0.1
#' @param solver The optimisation routine to be used. One of \code{c("BFGS", "Nelder-Mead",
#' "CG")}. Default = "BFGS".
#' @param obj The objective function to minimise. One of \code{c("Delta", "R", "Rwp")}.
#' Default = "Rwp". Default = Rwp
#' @param shift Optional. The maximum shift applied during full pattern fitting.
#' Default = 0.05
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
#' # Example 1
#' # Load the Xpert library
#' data(Xpert)
#'
#' # Load the Xpert soil data to use in example
#' data(Xpert_soil)
#'
#' # define the phases to include in the fit
#' xrd_phases <- c("Qzt.662070.Strath.12Mins.P", "Qzt.662074.Qua.10.P", "X996730.QUA.11.P")
#'
#' # without organic
#' #not run
#' #fpf_out <-  fpf(smpl = Xpert_soil$mineral,
#' #               lib = Xpert,
#' #               tth = c(3.5, 69.5),
#' #               phases = xrd_phases,
#' #               std = "Qzt.662070.Strath.12Mins.P")
#'
#' # Try fitting the same sample, but including an amorphous phase (organic matter)
#' xrd_phases_org <- c("Qzt.662070.Strath.12Mins.P", "Qzt.662074.Qua.10.P",
#'                    "X996730.QUA.11.P", "ORGANIC.337666")
#' # not run
#' # fpf_out_org <-  fpf(smpl = Xpert_soil$mineral,
#' #                    lib = Xpert,
#' #                    tth = c(3.5, 69.5),
#' #                    phases = xrd_phases_org,
#' #                    std = "Qzt.662070.Strath.12Mins.P",
#' #                    amorphous = "ORGANIC.337666")
fpf <- function(smpl, lib, phases, std, amorphous,
                tth, align = 0.1, solver = "BFGS",
                obj = "Rwp", shift = 0.05) {

  #Ensure that the align_shift is greater than 0.
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

xrdlib <- lib

#Make sure that the mineral identified as the internal standard is contained within the reference library
if (!std %in% xrdlib[["MINERALS"]]$MIN_ID) {
  stop("The mineral you have specified as the internal standard is not in the reference library")
}

#Make sure that the defined TTH arguments are within the range of the samples and reference library.
if (tth[1] < min(smpl[,1])) {
  stop("tth[1] must exceed the minimum 2theta value of the sample")
}
if (tth[2] > max(smpl[,1])) {
  stop("tth[2] must be lower than the maximum 2theta value of the sample")
}

if (tth[1] < min(xrdlib[["TTH"]])) {
  stop("tth[1] must be within the 2theta range of the reference library")
}
if (tth[2] > max(xrdlib[["TTH"]])) {
  stop("tth[2] must be within the 2theta range of the reference library")
}

#subset xrdlib according to the phases vector

keep_index <- which(xrdlib[["MINERALS"]]$MIN_ID %in% phases)

xrdlib[["XRD"]] <- xrdlib[["XRD"]][, keep_index]
xrdlib[["MINERALS"]] <- xrdlib[["MINERALS"]][keep_index, ]

#read the sample
#smpl <- read.csv(file = s.dir, header = FALSE, sep = " ")

xrd.standard_df <- xrdlib[["XRD"]][, which(xrdlib[["MINERALS"]]$MIN_ID == std)]

xrd.standard <- data.frame(TTH = xrdlib[["TTH"]], COUNTS = xrd.standard_df)

#align the data
smpl <- xrd.align(xrd.sample = smpl, xrd.standard, xmin = tth[1] + (align*2),
                    xmax = tth[2] - (align*2), xshift = align)

if (sqrt(smpl[[1]]^2) == align) {
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
smpl <- subset(smpl, smpl[,1] >= tth[1] & smpl[,1] <= tth[2])

#Subset the XRD dataframe to
xrdlib[["XRD"]] <- xrdlib[["XRD"]][which(xrdlib[["TTH"]] >= tth[1] & xrdlib[["TTH"]] <= tth[2]), ]

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
           sample.pattern = smpl[, 2], obj = obj)

#----------------------------------------------
#Apply shifts
#----------------------------------------------

fpf_aligned <- fpf.align(sample.tth = smpl[,1], sample.counts = smpl[,2],
                         xrd.lib = xrdlib, fpf_shift = shift,
                         pure.weights = o$par)

smpl <- fpf_aligned[["sample"]]
xrdlib[["XRD"]] <- fpf_aligned[["xrdlib_aligned"]]
xrdlib[["TTH"]] <- smpl[,1]

#Re-optimise after shifts

o <- optim(par = o$par, fullpat,
           method = solver, pure.patterns = xrdlib[["XRD"]],
           sample.pattern = smpl[, 2], obj = obj)

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
             sample.pattern = smpl[, 2], obj = obj)
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
             sample.pattern = smpl[, 2], obj = obj)
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

#If only 1 pattern is used in the fit, then rename it
if (ncol(xrd) == 1) {
  names(xrd) <- df$MIN_ID[1]
}


#Define a list that becomes the function output
out <- list(smpl[,1], fitted_pattern, smpl[,2], resid_x, df, dfs, R_fit, xrd, x)
names(out) <- c("TTH", "FITTED", "MEASURED", "RESIDUALS",
                "MINERALS", "MINERALS_SUMMARY", "R", "WEIGHTED_PURE_PATTERNS", "COEFFICIENTS")

return(out)

}
