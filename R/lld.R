.lld <- function(x, smpl, lib, std, amorphous, background, lld) {

  #compute which of the minerals are within the rir vector loaded at the start
  rir <- lib$phases$rir
  names(rir) <- lib$phases$phase_id

  rir <- rir[which(names(rir) %in% names(x))]

  #order them alphabetically
  rir <- rir[order(names(rir), decreasing = FALSE)]

  #Extract the mineral names of the selected phases and order them
  #alphabetically too
  phase_names <- lib$phases$phase_name
  names(phase_names) <- lib$phases$phase_id

  phase_names <- phase_names[which(names(phase_names) %in% names(x))]
  phase_names <- phase_names[order(names(phase_names), decreasing = FALSE)]

  #order the coefficients so that they match the order of rir's and
  #minerals names
  x <- x[order(names(x), decreasing = FALSE)]

  #calculate the mineral percentages based on the rir's
  phase_percent <- (unname(x)/rir)/sum(unname(x)/rir)*100

  #create a data frame containing the name, ID, percentage and rir of the data
  df <- data.frame("phase_name" = as.character(phase_names),
                   "phase_id" = names(rir),
                   "phase_percent" = as.numeric(phase_percent),
                   "rir" = as.numeric(rir))

  #create an x_amorphous vector that will be used later in a condition
  x_amorphous <- c()

  #Remove any amorphous phases if a vector is specified and if any phases in
  #that vector have made it to this point!
  if (length(amorphous) > 0 & length(which(df$phase_id %in% amorphous)) > 0) {
  #Extract these so that they can be added at the end
  df_amorphous <- df[which(df$phase_id %in% amorphous), ]
  x_amorphous <- x[which(names(x) %in% amorphous)]
  lib_amorphous <- lib$xrd[which(names(lib$xrd) %in% amorphous)]

  #Remove amorphous components from data that will be used to compute detection limits
  df <- df[-which(df$phase_id %in% amorphous), ]
  x <- x[-which(names(x) %in% amorphous)]
  lib$xrd <- lib$xrd[-which(names(lib$xrd) %in% amorphous)]

  #Close the data
  df$phase_percent <- df$phase_percent/sum(df$phase_percent) * 100

  phase_names <- df$phase_name
  names(phase_names) <- df$phase_id


  }

  dfs_mean_rir <- stats::aggregate(rir ~ phase_name, data = df, FUN = mean)
  dfs_total_phase <- stats::aggregate(phase_percent ~ phase_name, data = df, FUN = sum)

  dfs <- data.frame("phase_name" = dfs_mean_rir$phase_name,
                    "total_phase" = round(dfs_total_phase$phase_percent, 2),
                    "mean_rir" = round(dfs_mean_rir$rir, 2))

  #To compute LDD using full patterns, the background signal has to be estimated,
  #this is done using the bkg function I've written

  #apply the bkg function
  bkg_fit <- bkg(smpl,
                 lambda = background$lambda,
                 hwi = background$hwi,
                 it = background$it,
                 int = background$int)$background

  #sum of the background
  bkg_sum <- sum(bkg_fit)

  #Compute the total signal from the internal standard

  #Get the mineral name of the internal standard
  std_name <- lib$phases$phase_name[which(lib$phases$phase_id == std)]

  std_index <- which(phase_names == std_name)
  #Order the optimised (because the phase_names vector is already ordered)
  x_ordered <- x[order(names(x), decreasing = FALSE)]

  #extract the coefficients and rir's
  std_coefficients <- x_ordered[std_index]
  std_rir <- rir[std_index]

  #Get the library of aligned patterns and extract the internal standard patterns from it
  lib_df <- data.frame(lib$xrd[, order(names(data.frame(lib$xrd)), decreasing = FALSE)])

  std_patterns <- as.matrix(lib_df[, std_index])

  #Calculate the fitted internal standard pattern
  std_fit <- apply(sweep(std_patterns, 2, std_coefficients, "*"), 1, sum)

  #sum the total counts
  std_counts <- sum(std_fit)

  #Get the total internal standard percentage currently optimised
  std_percent <- sum(df$phase_percent[which(df$phase_name == std_name)])

  if (std_percent < 5) {
    warning("The internal standard is estimated to be lower than 5 wt.% within the sample,
            which may hinder the accuracy in estimating the lower limit of detection. Consider
            using an alternative internal standard.")
  }

  #Estimate the lld (check this equation!!!)
  std_lld <- (4*sqrt(2*bkg_sum))/(std_counts/std_percent)

  #Now estimate the lld for all phases of the fit

  #calculate the weighted rir which accounts for potentially
  #different rir's of the same mineral

  std_rir <- dfs$mean_rir[which(dfs$phase_name == std_name)]

  #Then calculate the lld
  mineral_lld <- std_lld * (df$rir/std_rir)^-1

  names(mineral_lld) <- as.character(df$phase_name)

  df$lld <- as.numeric(mineral_lld)

  #Get the index values of phases that are below lld
  remove_index <- which(df$phase_percent < (df$lld*lld))

  #This only runs when there are cases to remove
  if(length(remove_index) > 0) {
    x_ordered <- x_ordered[-remove_index]
    lib_df <- lib_df[ ,-remove_index]
  }

  #Lastly add on the amorphous phases that were held behind
  if(length(amorphous) > 0 & length(x_amorphous > 0)) {
  x_ordered <- c(x_ordered, x_amorphous)
  x_ordered <- x_ordered[order(names(x_ordered))]

  lib_df <- data.frame(lib_df, lib_amorphous)
  lib_df <- lib_df[, order(names(lib_df))]
  }

  out <- list("x" = x_ordered, "lib" = lib_df, "background" = bkg_fit)

  }
