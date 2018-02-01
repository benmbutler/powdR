#This function is specifically for cases where you don't need autoID of the sample. i.e. cases where manual identification has
#been carried out.

fpf.short <- function(s.dir, xrdlib, int.std, TTH.min, TTH.max, align.shift, solver, obj.function, fpf.shift) {

smpl <- read.csv(file = s.dir, header = FALSE, sep = " ")

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


#--------------------------------------------
#Initial Optimisation
#--------------------------------------------

x <- rep(0, nrow(xrdlib[["MINERALS"]]))
names(x) <- xrdlib[["MINERALS"]]$MIN_ID

o <- optim(par = x, fullpat,
           method = solver, pure.patterns = xrdlib[["XRD"]],
           sample.pattern = smpl[, 2], obj = obj.function)

#----------------------------------------------
#Apply shifts
#----------------------------------------------

#Reoptimise

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
