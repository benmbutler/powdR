### LLD estimation
xrd.LLD <- function(x, xrd.sample, xrd.lib, int_std, lld) {

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
  dfg <- dplyr::group_by(df, min_name)

  #summarise the grouped data
  dfs <- dplyr::summarise(dfg, total_min = round(sum(min_pc),2), mean_RIR = round(mean(RIR),2))

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
  int_std_name = xrd.lib[["MINERALS"]]$MIN_NAME[which(xrd.lib[["MINERALS"]]$MIN_ID == int_std)]

  int_std_index <- which(min_names == int_std_name)
  #Order the optimised (because the min_names vector is already ordered)
  x_ordered <- x[order(names(x), decreasing = FALSE)]

  #extract the coefficients and RIR's
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
  int_std_pc <- sum(dfg$min_pc[which(dfg$min_name == int_std_name)])

  if (int_std_pc < 5) {
    warning("The internal standard is estimated to be lower than 5 wt.% within the sample, which may hinder the accuracy in estimating the lower limit of detection. Consider using an alternative internal standard.")
  }

  #Estimate the LLD (check this equation!!!)
  int_std_LLD <- (4*sqrt(2*bkg))/(int_std_counts/int_std_pc)

  #Now estimate the LLD for all phases of the fit

  #calculate the weighted RIR which accounts for potentially
  #different RIR's of the same mineral

  int_std_RIR <- dfs$mean_RIR[which(dfs$min_name == int_std_name)]

  int_std_min_pc <- dfs$total_min[which(dfs$min_name == int_std_name)]

  int_std_min_weight <- int_std_min_pc/sum(int_std_min_pc)

  int_std_RIR <- sum(int_std_RIR * int_std_min_weight)

  #Then calculate the LLD
  mineral_LLD <- int_std_LLD * (dfg$RIR
                                /int_std_RIR)^-1

  names(mineral_LLD) <- as.character(dfg$min_name)

  dfg$LLD <- as.numeric(mineral_LLD)

  #Get the index values of phases that are below 0.75 * LLD or named "Background"
  remove_index <- which(dfg$min_pc < (dfg$LLD*lld))


  #This only runs when there are cases to remove
  if(length(remove_index) > 0) {
    x_ordered <- x_ordered[-remove_index]
    xrd.lib_df <- xrd.lib_df[ ,-remove_index]
  }

  out <- list("x" = x_ordered, "xrd.lib" = xrd.lib_df)

}
