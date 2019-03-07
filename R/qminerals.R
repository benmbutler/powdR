.qminerals <- function(x, xrd_lib) {

  #Make sure x is ordered if there are more than 1 phases in the library
  if (length(x) > 1) {
    x <- x[order(names(x))]
  }

  #Restrict the xrd library to phases within the names of fpf_pc
  minerals <- xrd_lib$phases

  minerals <- minerals[which(minerals$phase_id %in% names(x)),]

  #Order to the same as fpf_pc
  if (length(x) > 1) {
    minerals <- minerals[order(minerals$phase_id),]
  }

  min_percent <- (x/minerals$rir)/sum(x/minerals$rir)*100

  names(min_percent) <- minerals$phase_id

  df <- data.frame(minerals, "phase_percent" = min_percent)
  row.names(df) = c(1:nrow(df))

  #Summarise by summing the concentrations from each mineral group

  dfs <- data.frame(stats::aggregate(phase_percent ~ phase_name, data = df, FUN = sum),
                    stringsAsFactors = FALSE)

  #Ensure that the phase concentrations are rounded to 4 dp
  df$phase_percent <- round(df$phase_percent, 4)
  dfs$phase_percent <- round(dfs$phase_percent, 4)

  out <- list("df" = df, "dfs" = dfs)

  return(out)
}

.qminerals2 <- function(x, xrd_lib, std, std_conc) {

  #Make sure x is ordered if there are more than 1 phases in the library
  if (length(x) > 1) {
    x <- x[order(names(x))]
  }

  #Get the name of the internal standard
  std_name <- xrd_lib$phases$phase_name[which(xrd_lib$phases$phase_id == std)]

  #Extract all the ids of potential standard patterns
  std_ids <- xrd_lib$phases$phase_id[which(xrd_lib$phases$phase_name == std_name)]

  id_match <- which(names(x) %in% std_ids)

  if (length(id_match) < 1) {

    stop("\n-The phase specified as the std is not present. Cannot compute
         phase concentrations.")

  }

  #Get the scaling parameter of x
  std_x <- sum(x[which(names(x) %in% std_ids)])

  minerals <- xrd_lib$phases
  minerals <- minerals[which(minerals$phase_id %in% names(x)),]

  #Calculate a weighted average rir
  std_rir <- sum((minerals$rir[which(minerals$phase_id %in% std_ids)]/
                    std_x) * x[which(names(x) %in% std_ids)])

  #Remove any internal standard patterns from x
  x <- x[-which(names(x) %in% std_ids)]

  #Restrict the xrd library to phases within the names of fpf_pc
  minerals <- minerals[which(minerals$phase_id %in% names(x)),]

  #Order to the same as fpf_pc
  if (length(x) > 1) {
    minerals <- minerals[order(minerals$phase_id),]
  }

  min_percent <- c()

  for (i in 1:length(x)) {

    min_percent[i] <- (std_conc/(minerals$rir[i]/std_rir)) * (x[i]/std_x) * (1+(std_conc/100))

    #names(min_percent)[i] <- minerals$phase_id[i]

  }

  df <- data.frame(minerals, "phase_percent" = min_percent)
  row.names(df) = c(1:nrow(df))

  #Summarise by summing the concentrations from each mineral group

  dfs <- data.frame(stats::aggregate(phase_percent ~ phase_name, data = df, FUN = sum),
                    stringsAsFactors = FALSE)

  #Ensure that the phase concentrations are rounded to 4 dp
  df$phase_percent <- round(df$phase_percent, 4)
  dfs$phase_percent <- round(dfs$phase_percent, 4)

  out <- list("df" = df, "dfs" = dfs)

  return(out)
}
