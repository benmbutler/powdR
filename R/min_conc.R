min.conc <- function(x, xrd.lib) {

  #Make sure x is ordered if there are more than 1 phases in the library
  if (length(x) > 1) {
    x <- x[order(names(x))]
  }

  #Restrict the xrd library to minerals within the names of fpf_pc
  minerals <- xrd.lib[["MINERALS"]]

  minerals <- minerals[which(minerals$MIN_ID %in% names(x)),]

  #Order to the same as fpf_pc
  if (length(x) > 1) {
    minerals <- minerals[order(minerals$MIN_ID),]
  }

  min_percent <- (x/minerals$RIR)/sum(x/minerals$RIR)*100

  names(min_percent) <- minerals$MIN_ID

  df <- data.frame(minerals, "min_percent" = min_percent)

  #group the data by mineral name (e.g. quartz or chlorite)
  dfg <- dplyr::group_by(df, MIN_NAME)

  #Summarise to get the total mineral concentration of each mineral
  dfs <- dplyr::summarise(dfg, total_min = round(sum(min_percent),2), mean_RIR = mean(RIR))

  out <- list("df" = df, "dfs" = dfs)

  return(out)
}
