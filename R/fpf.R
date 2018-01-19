#This is the while loop used to pluck phases out of the library

#xrd.lib = an XRD library
#xrd.sample = a vector of an xrd sample
#df = a blank data frame with the same number of rows are the length of the library/sample

xrd.autoID <- function(xrd.lib, xrd.sample, delta_lim) {

  lib_length <- ncol(xrd.lib)

  df <- data.frame(matrix(nrow = nrow(xrd.lib)))
  delta <- 100
  resid_x <- xrd.sample
  initial_error <- sqrt(sum(abs(xrd.sample)^2))
  fit_error <- c()
  fit_error[1] <- initial_error
  names(fit_error) <- c("inital")
  n <- 1

  #This is a while loop that will continue until the conditions is met
  while (delta > delta_lim) {

    cor_v <- as.numeric(lapply(names(xrd.lib),
                               function(x) cor(xrd.lib[x], resid_x)))

    #identify the phase with maximum correlation
    cor_max <- which.max(cor_v)
    #extract its name
    v_name <- names(xrd.lib)[cor_max]

    #add this phase to the library to be used in fitting
    df[n] <- xrd.lib[v_name]

    #add its name
    names(df)[n] <- v_name

    #remove the identified phase from the original library
    xrd.lib[v_name] <- NULL

    ## fit pattern mixtures with qr.solve
    mat <- as.matrix(df)
    x <- qr.solve(mat, xrd.sample)

    #calculate fitted pattern and residuals
    fitted_pattern <- apply(sweep(mat, 2, x, "*"), 1, sum)
    resid_x <- xrd.sample - fitted_pattern

    #Overall error squared
    err <- sqrt(sum(abs(xrd.sample - fitted_pattern)^2))

    #add 1 to n so that the correct indexes are accessed
    n <- n + 1

    #add the error to the vector in order to deduce how much improvement there is in the fit
    fit_error[n] <- err
    names(fit_error)[n] <- v_name

    #get the current length of the error vector (dependent on the number of iterations of this while loop)
    l <- length(fit_error)

    #If this conditions is met, it means that all phases from the initial library have been used,
    #which means the loop has to stop.
    if (ncol(df) == lib_length) {
      delta <- 0
    }  else{
      #Calculate the percentage difference between the nth and nth-1 iteration
      delta <- 100 - ((fit_error[l]/fit_error[l - 1])* 100)
    }
  }

  # The while loop is always one step behind,

  #calculate the percentage improvment from adding sequential phases to the fit
  min_v <- c()
  for (i in 1:(length(fit_error)-1)) {
    min_v[i] <- 100 - ((fit_error[i+1]/fit_error[i])* 100)
  }

  #Remove the phase the is below the limit set in the function call
  remove_index <- which(min_v < delta_lim)

  #remove the column from the library that contains the identified data
  if (length(which(min_v < delta_lim)) > 0) {
    mat <- df[,-remove_index]
  }

  #re-solve
  x <- qr.solve(mat, xrd.sample)

  out <- list("x" = x, "xrd.lib" = data.frame(mat), "fit.error" = fit_error)
  return(out)

}

# FULLPAT FUNCTION --------------------------------------

fullpat <- function (par, pure.patterns, sample.pattern, obj)
{

  if (length(par) == 1) {
    pure.weights <- par
    s.mix <- par * pure.patterns
    d <- sum(abs(sample.pattern - s.mix))
    return(d)
  }

  if (length(par) > 1) {
    #These will be the pure weights already estimated using qr.solve
    pure.weights <- par

    #This calculates the fitted pattern
    s.mix <- apply(sweep(pure.patterns, 2, pure.weights, "*"),
                   1, sum)
    #This is the objective function that is minimised

    if(obj == "Delta") {
      d <- sum(abs(sample.pattern - s.mix))
    }

    if(obj == "R") {
      d <- sqrt(sum((sample.pattern - s.mix)^2)/sum(sample.pattern^2))
    }

    if(obj == "Rwp") {
      d <-  sqrt(sum((1/sample.pattern) * ((sample.pattern - s.mix)^2)) / sum((1/sample.pattern) * (sample.pattern^2)))
    }

    return(d)
  }
}


#fpf Alignment ---------------------------------------------------------------

fpf.align <- function(sample.tth, sample.counts, xrd.lib, fpf_shift, pure.weights, amorphous) {

  #create a blank list
  pure.patterns <- list()

  #Create a new 2theta scale with 4 times the resolution of the original sample
  sample.pattern <- data.frame(approx(x = sample.tth, y = sample.counts,
                                      method = "linear", n = length(sample.tth) * 4))

  TTH_long <- sample.pattern[,1]
  sample_long <- sample.pattern[,2]

  #Do the same for all data in the selected reference library
  for (i in 1:ncol(xrd.lib[["XRD"]])) {
    pure.patterns[[i]] <- approx(x = xrd.lib[["TTH"]], y = xrd.lib[["XRD"]][, i],
                                 method = "linear", n = nrow(xrd.lib[["XRD"]]) * 4)[[2]]
  }

  #convert from list to data frame
  pure.patterns <- data.frame(pure.patterns)
  names(pure.patterns) <- names(data.frame(xrd.lib[["XRD"]]))
  pure.patterns <- as.matrix(pure.patterns)


  #Define a value that will be used to shift the data.
  TTH_res <- (TTH_long[length(TTH_long)] - TTH_long[1])/(length(TTH_long)-1)

  shift_value <- round(fpf_shift/TTH_res, 0)

  #Shorten the sample pattern and 2theta to account for
  #the maximum/minimum shifts that might be applied
  sample_long <- sample_long[((shift_value + 1):(length(sample_long)-shift_value))]
  TTH_long <- TTH_long[((shift_value + 1):(length(TTH_long) - shift_value))]




  #define an integer vector of positive and negative shifts
  initial.shift <- c((0 - shift_value):shift_value)

  shifting.length <- nrow(pure.patterns)-shift_value

  #Create a matrix of the shortened length that will be used during alignment
  shift.mat <- pure.patterns[((shift_value + 1):(shifting.length)), ]

  #define blank lists to be populated during alignment
  v <- list()
  vm <- list()
  vf <- list()
  d <- list()

  #This vector will be used to identify the optimum shift
  dmin <- c()

  #This vs matrix will be populated with the aligned patterns
  vs <- shift.mat

  for (i in 1:ncol(shift.mat)) {
    for (j in 1:length(initial.shift)) {

      v[[j]] <- pure.patterns[c(((shift_value + 1) + (initial.shift[j])):(shifting.length + (initial.shift[j]))), i]

      #adjusted matrix for each shift

      vm[[j]] <- shift.mat
      # #add the shifted data
      vm[[j]][,i] <- v[[j]]
      #
      # #compute the fitted pattern
      #
      vf[[j]] <- apply(sweep(vm[[j]], 2, pure.weights, "*"), 1, sum)
      #
      #compute the error
      #d[[j]] <- sqrt(sum(abs(sample_long - vf[[j]])^2))

      #Compute the Rwp
      d[[j]] <- sqrt(sum((1/sample_long) * ((sample_long - vf[[j]])^2)) / sum((1/sample_long) * (sample_long^2)))

      #identify which shifted pattern results in minimum Rwp
      dmin[[i]] <- which.min(d)

      #Populate a library with the optimumly shifted references
      vs[, i] <- vm[[which.min(d)]][, i]
    }
  }

  #re-approximate the data to the old TTH scale (i.e. reduce by 4 times)

  vs_short <- list()

  #re-approximate the reference library
  for (i in 1:ncol(vs)) {
    vs_short[[i]] <- approx(x = 1:nrow(vs), y = vs[ , i], method = "linear", n = (nrow(vs) / 4))[[2]]
  }
  #Convert from list to data frame
  vs_short <- data.frame(vs_short)
  names(vs_short) <- names(data.frame(vs))
  #convert to matrix
  vs_short <- as.matrix(vs_short)

  #reapproximate the sample
  sample.pattern <- approx(x = TTH_long, y = sample_long, method = "linear", n = (nrow(vs) / 4))[[2]]

  #reapproximate the 2theta
  TTH_short <- approx(x = TTH_long, y = vs[, 1], method = "linear", n = (nrow(vs) / 4))[[1]]

  vs <- vs_short
  TTH <- TTH_short

  out <- list("sample" = data.frame("TTH" = TTH, "COUNTS" = sample.pattern),
              "xrdlib_aligned" = vs)

}


min.conc <- function(x, xrd.lib) {
  #send the coefficients to a dataframe
  fpf_pc <- data.frame(t(data.frame(x)))

  #Make sure it's ordered
  fpf_pc <- fpf_pc[,order(names(fpf_pc))]

  #compute which of the minerals are within the RIR named vector

  minerals <- xrd.lib[["MINERALS"]]

  minerals <- minerals[which(minerals$MIN_ID %in% names(fpf_pc)),]

  minerals <- minerals[order(minerals$MIN_ID),]

  fpf_pc_v <- as.numeric(fpf_pc[1, ])
  names(fpf_pc_v) <- names(fpf_pc)

  min_percent <- (fpf_pc_v/minerals$RIR)/sum(fpf_pc_v/minerals$RIR)*100

  names(min_percent) <- minerals$MIN_ID

  df <- data.frame(minerals, "min_percent" = min_percent)

  dfg <- group_by(df, MIN_NAME)

  #The final mineral proportions
  dfs <- summarise(dfg, total_min = round(sum(min_percent),2), mean_RIR = mean(RIR))

  out <- list("df" = df, "dfs" = dfs)

  return(out)
}


### LLD estimation

xrd.LLD <- function(x, xrd.sample, xrd.lib, int_std) {

  fpf_pc <- data.frame(t(data.frame(x)))

  #compute which of the minerals are within the RIR vector loaded at the start

  RIR <- xrd.lib[["MINERALS"]]$RIR
  names(RIR) <- xrd.lib[["MINERALS"]]$MIN_ID

  RIR <- RIR[which(names(RIR) %in% names(fpf_pc))]

  #order them alphabetically
  RIR <- RIR[order(names(RIR), decreasing = FALSE)]

  #Extract the mineral names of the selected phases and order them
  #alphabetically too

  min_names <- xrd.lib[["MINERALS"]]$MIN_NAME
  names(min_names) <- xrd.lib[["MINERALS"]]$MIN_ID

  min_names <- min_names[which(names(min_names) %in% names(fpf_pc))]
  min_names <- min_names[order(names(min_names), decreasing = FALSE)]

  #order the coefficients so that they match the order of RIR's and
  #minerals names
  fpf_pc <- fpf_pc[, order(names(fpf_pc), decreasing = FALSE)]

  #create a numeric vector of coefficients
  fpf_pc_v <- as.numeric(fpf_pc[1, ])

  #calculate the mineral percentages based on the RIR's
  min_percent <- (fpf_pc_v/RIR)/sum(fpf_pc_v/RIR)*100

  #create a data frame containing the name, ID, percentage and RIR of the data
  df <- data.frame("min_name" = as.character(min_names),
                   "min_ID" = names(RIR),
                   "min_pc" = as.numeric(min_percent),
                   "RIR" = as.numeric(RIR))

  #group the data by mineral name (for cases that different library patterns for a single
  #mineral are included)
  dfg <- group_by(df, min_name)

  #summarise the grouped data
  dfs <- summarise(dfg, total_min = round(sum(min_pc),2), mean_RIR = round(mean(RIR),2))

  #To compute LDD using full patterns, the background signal has to be estimated,
  #this is done using the bkg function I've written

  #apply the bkg function
  bkg <- xrd.bkg(tth = xrd.sample[,1],
                 counts = xrd.sample[,2],
                 width = 50,
                 res = 0.1)

  #sum of the background
  bkg <- sum(bkg)
  sqrt.bkg <- sqrt(bkg)

  #Compute the total signal from the internal standard

  #Get the index's of the internal standard
  int_std_index <- which(min_names == int_std)
  #Order the optimised (because the min_names vector is already ordered)
  x_ordered <- x[order(names(x), decreasing = FALSE)]
  #extract the halite coefficients and RIR's
  int_std_coefficients <- x_ordered[int_std_index]
  int_std_RIR <- RIR[int_std_index]

  #Get the library of aligned patterns and extract the internal standard patterns from it
  xrd.lib_df <- data.frame(xrd.lib[["XRD"]][, order(names(data.frame(xrd.lib[["XRD"]])), decreasing = FALSE)])
  int_std_patterns <- as.matrix(xrd.lib_df[, int_std_index])

  #Calculate the fitted internal standard pattern
  int_std_fit <- apply(sweep(int_std_patterns, 2, int_std_coefficients, "*"), 1, sum)

  #sum the total counts
  int_std_counts <- sum(int_std_fit)

  #Get the total internal standard percentage currently optimised
  int_std_pc <- sum(dfg$min_pc[which(dfg$min_name == int_std)])

  if (int_std_pc < 5) {
    warning("The internal standard is estimated to be lower than 5 wt.% within the sample, which may hinder the accuracy in estimating the lower limit of detection. Consider using an alternative internal standard.")
  }

  #Estimate the halite LLD (check this equation!!!)
  int_std_LLD <- (4*sqrt(2*bkg))/(int_std_counts/int_std_pc)

  #Now estimate the LLD for all phases of the fit

  #calculate the weighted RIR which accounts for potentially
  #different RIR's of the same mineral

  int_std_RIR <- dfs$mean_RIR[which(dfs$min_name == int_std)]

  int_std_min_pc <- df$min_pc[which(dfs$min_name == int_std)]

  int_std_min_weight <- int_std_min_pc/sum(int_std_min_pc)

  int_std_RIR <- sum(int_std_RIR * int_std_min_weight)

  #Then calculate the LLD
  mineral_LLD <- int_std_LLD * (dfg$RIR
                                /int_std_RIR)^-1

  names(mineral_LLD) <- names(dfg$min_name)

  dfg$LLD <- as.numeric(mineral_LLD)

  #Get the index values of phases that are below 0.75 * LLD or named "Background"
  remove_index <- which(dfg$min_pc < (dfg$LLD*0.8))


  #This only runs when there are cases to remove
  if(length(remove_index) > 0) {
    x_ordered <- x_ordered[-remove_index]
    xrd.lib_df <- xrd.lib_df[ ,-remove_index]
  }

  out <- list("x" = x_ordered, "xrd.lib" = xrd.lib_df)

}


#####################################

##### This is the fpf function that depends upon ALL the functions defined above


#' Full pattern fitting
#'
#' \code{fpf} returns estimates of soil mineral concentraitons using full pattern fitting.
#'
#' This function applies full pattern fitting to an XRPD sample to quantify mineral concentrations.
#' It requires a library of reference patterns with pre-measured reference intensity ratios.
#'
#' @param s.dir Path to the sample to be fitted. The file must be in .xy format (space separated)
#' @param lib The XRPD library. A list containing three elements. The first (\code{XRPD}) is a dataframe
#' containing all pre-measured reference patterns by column. The second (\code{TTH}) is a vector of the
#' 2theta measurement intervals for the reference patterns. Third (\code{MINERALS}) is a data frame
#' containing the unique ID, mineral name, and reference intensity ratio of each pattern in the library.
#' The order of \code{XRPD} (by column) and \code{MINERALS} (by row) must be identical.
#' @param instr The specific instrument library to be used. One of \code{c("D8", "D5000", "Xpert")}
#' @param align_shift The maximum shift that is allowed during initial 2theta alignment (degrees)
#' @param TTH_min The minimum value of 2theta used during fitting
#' @param TTH_max The maximum value of 2theta used during fitting
#' @param delta_lim The tuning parameter used to adjust sensitivity of fit. Must greater than 0 and
#' less than 100. Lower value = more sensitive. We recommend using 0.01.
#' @param solver The optimisation routine to be used. One of \code{c("BFGS", "Nelder-Mead", "CG")}
#' @param obj.function The objective function to minimise. One of \code{c("Delta", "R", "Rwp")}.
#' We recommend Rwp
#' @param int_std The mineral name (e.g. "Quartz") to be used as internal standard. Must match a mineral
#' name in the MINERALS table.
#' @param fpf_shift Optional. The maximum shift applied during full pattern fitting.
#' @param amorphous Optional. Then name of an amorphous phase to be added to the fitting process. Must
#' match a name in the MINERALS table.
fpf <- function(s.dir, lib, instr, align_shift, TTH_min, TTH_max, delta_lim, solver, obj.function, int_std, fpf_shift, amorphous) {

  #library(dplyr)

  #Ensure that the delta_lim argument is more than 0
  if (delta_lim <= 0) {
    stop("The delta_lim value must be greater than 0.")
  }

  #Also warn if delta_lim is greater than 10 because this would give a strange fit
  if (delta_lim >= 10) {
    warning("A delta_lim value greater than 10 may not produce an accurate fit.")
  }

  #Ensure that the align_shift is greater than 0.
  if (align_shift <= 0) {
    stop("The align_shift argument must be greater than 0")
  }

  #Create a warning message if the shift is greater than 1, since this confuse the optimisation
  if (align_shift > 0.5) {
    warning("Be cautious of large 2theta shifts. These can cause issues in sample alignment.")
  }

  if (fpf_shift > 0.1) {
    warning("To speed computation and avoid erroneous alignments, the fpf_shift argument should be less than 0.1.")
  }

  #Make only "Nelder-Mead", "BFGS", or "CG" optional for the solver
  if (!solver %in% c("Nelder-Mead", "BFGS", "CG")) {
    stop("The solver argument must be one of 'BFGS', 'Nelder Mead' or 'CG'")
  }

  #Make sure that the delta_lim argument is set to be greater than 0, but less than 100
  if (delta_lim > 100) {
    stop("The delta_lim argument must be a value greater than 0 and less than 100")
  }

  if (delta_lim <= 0) {
    stop("The delta_lim argument must be a value greater than 0 and less than 100")
  }

  #Provide recommendations of which mineral to use as the internal standard
  if (!int_std %in% c("Corundum", "Quartz", "Sylvite", "Halite", "Rutile")) {
    warning("We recommend internal standards be highly crystalline, strong diffractors e.g. corundum, quartz, sylvite, halite and rutile.")
  }

  xrdlib <- lib[[instr]]

  #Make sure that the mineral identified as the internal standard is contained within the reference library
  if (!int_std %in% xrdlib[["MINERALS"]]$MIN_NAME) {
    stop("The mineral you have specified as the internal standard is not in the reference library")
  }

  sample <- read.csv(file = s.dir, header = FALSE, sep = " ")

  #Make sure that the defined TTH arguments are within the range of the samples and reference library.
  if (TTH_min < min(sample[,1])) {
    stop("The TTH_min argument must exceed the minimum 2theta value of the sample")
  }
  if (TTH_max > max(sample[,1])) {
    stop("The TTH_max argument must be lower than the maximum 2theta value of the sample")
  }

  if (TTH_min < min(xrdlib[["TTH"]])) {
    stop("The TTH_min argument must be within the 2theta range of the reference library")
  }
  if (TTH_max > max(xrdlib[["TTH"]])) {
    stop("The TTH_max argument must be within the 2theta range of the reference library")
  }
  ##############
  #INITIAL SAMPLES ALIGNMENT USING THE xrd.align function
  ##############

  xrd.standard_df <- xrdlib[["XRD"]][, which(xrdlib[["MINERALS"]]$MIN_NAME == int_std)]

  if (length(which(xrdlib[["MINERALS"]]$MIN_NAME == int_std)) > 1) {
    xrd.standard_df <- rowMeans(xrd.standard_df)
  }

  xrd.standard <- data.frame(TTH = xrdlib[["TTH"]], COUNTS = xrd.standard_df)

  #align the data
  sample <- xrd.align(xrd.sample = sample, xrd.standard, xmin = TTH_min + (align_shift*2),
                      xmax = TTH_max - (align_shift*2), xshift = align_shift)

  if (sqrt(sample[[1]]^2) == align_shift) {
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

  #### decrease 2TH scale to the range defined in the function call
  sample <- subset(sample, sample[,1] >= TTH_min & sample[,1] <= TTH_max)

  #Subset the XRD dataframe to
  xrdlib[["XRD"]] <- xrdlib[["XRD"]][which(xrdlib[["TTH"]] >= TTH_min & xrdlib[["TTH"]] <= TTH_max), ]

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
                       xrd.sample = sample[,2], delta_lim = delta_lim)

  x <- autoID[["x"]]
  xrdlib[["XRD"]] <- autoID[["xrd.lib"]]


  #OPTIMISATION

  #optimise using objective function rather than qr.solve
  o <- optim(par = x, fullpat,
             method = solver, pure.patterns = xrdlib[["XRD"]],
             sample.pattern = sample[, 2], obj = obj.function)


  #Alignment and then another optimisation ONLY is the fpf.align parameters
  #is included


  if(!missing(fpf_shift)) {

    fpf_aligned <- fpf.align(sample.tth = sample[,1], sample.counts = sample[,2],
                             xrd.lib = xrdlib, fpf_shift = fpf_shift,
                             pure.weights = o$par)

    sample <- fpf_aligned[["sample"]]
    xrdlib[["XRD"]] <- fpf_aligned[["xrdlib_aligned"]]
    xrdlib[["TTH"]] <- sample[,1]

    #Re-optimise after shifts

    o <- optim(par = o$par, fullpat,
               method = solver, pure.patterns = xrdlib[["XRD"]],
               sample.pattern = sample[, 2], obj = obj.function)
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
               sample.pattern = sample[,2], obj = obj.function)
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
                            int_std = int_std)

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
             sample.pattern = sample[,2], obj = obj.function)

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
                 sample.pattern = sample[,2], obj = obj.function)
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

  #Define a list that becomes the function output
  out <- list(sample[,1], fitted_pattern, sample[,2], resid_x, df, dfs, R_fit, xrd, x)
  names(out) <- c("TTH", "FITTED", "MEASURED", "RESIDUALS",
                  "MINERALS", "MINERALS_SUMMARY", "R", "WEIGHTED_PURE_PATTERNS", "COEFFICIENTS")

  return(out)

}
