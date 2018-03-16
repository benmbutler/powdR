#' Estimation of lower limits of detection in XRPD data
#'
#' \code{xrd.lld} uses full patterns and their reference intensity ratios to esimate
#' the lower limit of detection for crystalline phases.
#'
#' @param x a named vector of parameters from \code{fullpat}
#' @param xrd.sample an XRPD data frame of the sample (2theta, counts)
#' @param xrd.lib a library of XRPD reference patterns
#' @param int_std the ID of a reference pattern to use as the internal standard
#' @param lld a positive tuning parameter used to adjust the lower limit of detection.
#'
#' @return a list with components:
#' \item{x}{named vector of coefficients derived from full pattern fitting}
#' \item{xrd.lib}{data frame of reference patterns that are computed to be present in
#' detectable concentrations}
xrd.lld <- function(x, xrd.sample, xrd.lib, int_std, lld) {

  #compute which of the minerals are within the rir vector loaded at the start
  rir <- xrd.lib$minerals$rir
  names(rir) <- xrd.lib$minerals$min_id

  rir <- rir[which(names(rir) %in% names(x))]

  #order them alphabetically
  rir <- rir[order(names(rir), decreasing = FALSE)]

  #Extract the mineral names of the selected phases and order them
  #alphabetically too
  min_names <- xrd.lib$minerals$min_name
  names(min_names) <- xrd.lib$minerals$min_id

  min_names <- min_names[which(names(min_names) %in% names(x))]
  min_names <- min_names[order(names(min_names), decreasing = FALSE)]

  #order the coefficients so that they match the order of rir's and
  #minerals names
  x <- x[order(names(x), decreasing = FALSE)]

  #create a numeric vector of coefficients
  x_v <- unname(x)

  #calculate the mineral percentages based on the rir's
  min_percent <- (x_v/rir)/sum(x_v/rir)*100

  #create a data frame containing the name, ID, percentage and rir of the data
  df <- data.frame("min_name" = as.character(min_names),
                   "min_id" = names(rir),
                   "min_pc" = as.numeric(min_percent),
                   "rir" = as.numeric(rir))

  #group the data by mineral name (for cases that different library patterns for a single
  #mineral are included)
  dfg <- dplyr::group_by(df, min_name)

  #summarise the grouped data
  dfs <- dplyr::summarise(dfg, total_min = round(sum(min_pc),2), mean_rir = round(mean(rir),2))

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
  int_std_name = xrd.lib$minerals$min_name[which(xrd.lib$minerals$min_id == int_std)]

  int_std_index <- which(min_names == int_std_name)
  #Order the optimised (because the min_names vector is already ordered)
  x_ordered <- x[order(names(x), decreasing = FALSE)]

  #extract the coefficients and rir's
  int_std_coefficients <- x_ordered[int_std_index]
  int_std_rir <- rir[int_std_index]

  #Get the library of aligned patterns and extract the internal standard patterns from it
  xrd.lib_df <- data.frame(xrd.lib$xrd[, order(names(data.frame(xrd.lib$xrd)), decreasing = FALSE)])
  int_std_patterns <- as.matrix(xrd.lib_df[, int_std_index])

  #Calculate the fitted internal standard pattern
  int_std_fit <- apply(sweep(int_std_patterns, 2, int_std_coefficients, "*"), 1, sum)

  #sum the total counts
  int_std_counts <- sum(int_std_fit)

  #Get the total internal standard percentage currently optimised
  int_std_pc <- sum(dfg$min_pc[which(dfg$min_name == int_std_name)])

  if (int_std_pc < 5) {
    warning("The internal standard is estimated to be lower than 5 wt.% within the sample, which may hinder
            the accuracy in estimating the lower limit of detection. Consider using an alternative internal
            standard.")
  }

  #Estimate the lld (check this equation!!!)
  int_std_lld <- (4*sqrt(2*bkg))/(int_std_counts/int_std_pc)

  #Now estimate the lld for all phases of the fit

  #calculate the weighted rir which accounts for potentially
  #different rir's of the same mineral

  int_std_rir <- dfs$mean_rir[which(dfs$min_name == int_std_name)]

  int_std_min_pc <- dfs$total_min[which(dfs$min_name == int_std_name)]

  int_std_min_weight <- int_std_min_pc/sum(int_std_min_pc)

  int_std_rir <- sum(int_std_rir * int_std_min_weight)

  #Then calculate the lld
  mineral_lld <- int_std_lld * (dfg$rir
                                /int_std_rir)^-1

  names(mineral_lld) <- as.character(dfg$min_name)

  dfg$lld <- as.numeric(mineral_lld)

  #Get the index values of phases that are below lld or named "Background"
  remove_index <- which(dfg$min_pc < (dfg$lld*lld))

  #This only runs when there are cases to remove
  if(length(remove_index) > 0) {
    x_ordered <- x_ordered[-remove_index]
    xrd.lib_df <- xrd.lib_df[ ,-remove_index]
  }

  out <- list("x" = x_ordered, "xrd.lib" = xrd.lib_df)

}
