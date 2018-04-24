#' Quanity mineral concentrations from optimised parameters and reference
#' intensity ratios
#'
#' \code{qminerals} returns a two dataframes detailing the mineral concentrations
#' within a sample analysed by XRPD. \code{min.conc} is used within the \code{fpf}
#' and \code{auto.fpf} functions.
#'
#' @param x a named vector of optimised parameters from \code{fullpat}
#' @param xrd.lib an xrd library containing the reference intensity ratios,
#' mineral id's and mineral names
qminerals <- function(x, xrd.lib) {

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

  dfs_mean_rir <- stats::aggregate(rir ~ min_name, data = df, FUN = mean)
  dfs_total_min <- stats::aggregate(min_percent ~ min_name, data = df, FUN = sum)

  dfs <- data.frame("min_name" = dfs_mean_rir$min_name,
                    "total_min" = round(dfs_total_min$min_percent, 2),
                    "mean_rir" = round(dfs_mean_rir$rir, 2),
                    stringsAsFactors = FALSE)

  out <- list("df" = df, "dfs" = dfs)

  return(out)
}
