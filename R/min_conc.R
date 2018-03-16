#' Calculate mineral concentrations from optimised parameters and reference
#' intensity ratios
#'
#' \code{min.conc} returns a two dataframes detailing the mineral concentrations
#' within a sample analysed by XRPD. \code{min.conc} is used within the \code{fpf}
#' and \code{auto.fpf} functions.
#'
#' @param x a named vector of optimised parameters from \code{fullpat}
#' @param xrd.lib an xrd library containing the reference intensity ratios,
#' mineral id's and mineral names
min.conc <- function(x, xrd.lib) {

  #Make sure x is ordered if there are more than 1 phases in the library
  if (length(x) > 1) {
    x <- x[order(names(x))]
  }

  #Restrict the xrd library to minerals within the names of fpf_pc
  minerals <- xrd.lib$minerals

  minerals <- minerals[which(minerals$min_id %in% names(x)),]

  #Order to the same as fpf_pc
  if (length(x) > 1) {
    minerals <- minerals[order(minerals$min_id),]
  }

  min_percent <- (x/minerals$rir)/sum(x/minerals$rir)*100

  names(min_percent) <- minerals$min_id

  df <- data.frame(minerals, "min_percent" = min_percent)

  #group the data by mineral name (e.g. quartz or chlorite)
  dfg <- dplyr::group_by(df, min_name)

  #Summarise to get the total mineral concentration of each mineral
  dfs <- dplyr::summarise(dfg, total_min = round(sum(min_percent),2), mean_rir = mean(rir))

  out <- list("df" = df, "dfs" = dfs)

  return(out)
}
