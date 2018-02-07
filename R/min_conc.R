min.conc <- function(x, xrd.lib) {
  #send the coefficients to a dataframe
  fpf_pc <- data.frame(t(data.frame(x)))

  #Make sure it's ordered

  if (length(x) > 1) {
    fpf_pc <- fpf_pc[,order(names(fpf_pc))]
  }

  #compute which of the minerals are within the RIR named vector

  minerals <- xrd.lib[["MINERALS"]]

  minerals <- minerals[which(minerals$MIN_ID %in% names(fpf_pc)),]

  if (length(x) > 1) {
    minerals <- minerals[order(minerals$MIN_ID),]
  }


  fpf_pc_v <- as.numeric(fpf_pc[1, ])
  names(fpf_pc_v) <- names(fpf_pc)

  min_percent <- (fpf_pc_v/minerals$RIR)/sum(fpf_pc_v/minerals$RIR)*100

  names(min_percent) <- minerals$MIN_ID

  df <- data.frame(minerals, "min_percent" = min_percent)

  dfg <- dplyr::group_by(df, MIN_NAME)

  #The final mineral proportions
  dfs <- dplyr::summarise(dfg, total_min = round(sum(min_percent),2), mean_RIR = mean(RIR))

  out <- list("df" = df, "dfs" = dfs)

  return(out)
}
