#' Fill zeros in combined quantitative datasets
#'
#' \code{fill.zeros} is designed to be used for datasets where many quantitative
#' outputs are compiled and resulting in accumulation of 0's and/or NA values.
#'
#' @param quant a list of outputs from either \code{fpf} or \code{auto.fpf}
#' functions
#' @param lib a reference library used to extract reference intensity ratios
#' and hence calculate limits of detection
#'
#' @return a list with components:
#' \item{filled}{a vector of the 2theta scale of the fitted data}
#' \item{figures}{a list of figures showing the fitted background and
#' the internal standard. One figure for each sample in the \code{quant}
#' input.}
#' \item{additions}{a vector indicating the total additions for each
#' sample from the 0 replacement.}
fill.zeros <- function(quant, lib) {

  quant_id_list <- list()
  #Create a wide mineral quantification table
  for (i in 1:length(quant)) {

    quant_id_list[[i]] <- quant[[i]]$minerals
    quant_id_list[[i]]$min_name <- NULL
    quant_id_list[[i]]$rir <- NULL
    quant_id_list[[i]]$amorphous <- NULL
    quant_id_list[[i]]$sample_id <- rep(names(quant)[i], nrow(quant_id_list[[i]]))

  }

  quant_id_long <- do.call(rbind, quant_id_list)



  quant_name_list <- list()
  #Create a wide mineral quantification table
  for (i in 1:length(quant)) {

    quant_name_list[[i]] <- quant[[i]]$minerals_summary
    quant_name_list[[i]]$mean_rir <- NULL
    quant_name_list[[i]]$sample_id <- rep(names(quant)[i], nrow(quant_name_list[[i]]))

  }

  quant_name_long <- do.call(rbind, quant_name_list)


  #Convert to wide format

  #Spread the data into columns of minerals concentration
  quant_id_wide <- tidyr::spread(data = quant_id_long,
                                 min_id, min_percent, convert = TRUE)
  #make sure all the column names are correctly formated (i.e. no dashes or spaces)
  quant_id_wide <- data.frame(quant_id_wide)

  #Spread the data into columns of minerals concentration
  quant_name_wide <- tidyr::spread(data = quant_name_long,
                                   min_name, total_min, convert = TRUE)
  #make sure all the column names are correctly formated (i.e. no dashes or spaces)
  quant_name_wide <- data.frame(quant_name_wide)


  #Step 1: fitting a background to each sample:

  #blank list to export to
  xrd <- list()

  #a blank vector to export the lld of the internal standard
  lld_std <- c()
  #a blank vector to export the concentration of the internal standard
  conc_std <- c()

  xrd_plots <- list()

  for (i in 1:length(quant)) {

    #Create a data frame of all the measured and fitted patterns
    xrd[[i]] <- data.frame("tth" = quant[[i]]$tth,
                           "counts" = quant[[i]]$measured,
                           quant[[i]]$weighted_pure_patterns)

    xrd[[i]]$bkg <- xrd.bkg(tth = xrd[[i]]$tth,
                            counts = xrd[[i]]$counts,
                            width = 90,
                            res = 0.3)


    plot(x = xrd[[i]]$tth, y = sqrt(xrd[[i]]$counts), type = "l")
    lines(x = xrd[[i]]$tth, y = sqrt(xrd[[i]]$bkg), col = "red")

    #Extract the fitted patterns associated with quartz for each sample
    quartz_ids <- quant[[i]]$minerals$min_id[which(quant[[i]]$minerals$min_name == "Quartz")]

    #Add the sum of these quartz patterns to produce an internal standard
    xrd[[i]]$int_std <- rowSums(xrd[[i]][quartz_ids])
    #Use these id's to extract the quartz columns, and plot their sum
    #lines(x = xrd[[i]]$tth, y = rowSums(xrd[[i]][quartz_ids[1]]), col = "blue")

    xrd_plots[[i]] <- ggplot2::ggplot(data = data.frame("tth" = xrd[[i]]$tth,
                                                        "counts" = xrd[[i]]$counts,
                                                        "bkg" = xrd[[i]]$bkg,
                                                        "quartz" = (xrd[[i]]$int_std + xrd[[i]]$bkg))) +
      ggplot2::geom_line(ggplot2::aes(x = tth, y = sqrt(counts)), col = "black") +
      ggplot2::geom_line(ggplot2::aes(x = tth, y = sqrt(bkg)), col = "red") +
      ggplot2::geom_line(ggplot2::aes(x = tth, y = sqrt(quartz)), col = "blue")

    names(xrd_plots)[i] <- names(quant)[i]

    #Extract the weight % of the internal standard
    conc_std[i] <- quant[[i]]$minerals_summary$total_min[which(quant[[i]]$minerals_summary$min_name == "Quartz")]

    lld_std[i] <- (4*sqrt(2*sum(xrd[[i]]$bkg)))/(sum(xrd[[i]]$int_std)/conc_std[i])
  }


  #Now estimating the LLD's of all the other phases in the quant_id_wide table
  #This needs to be done for each individual mineral, rather than the groupings


  D8_rirs <- lib$minerals[which(lib$minerals$min_id %in% (names(quant_id_wide)[-1])), ]


  D8_rirs_loop <- list()

  #Create a dataframe that will contain the filled NA values
  quant_id_wide_filled <- quant_id_wide

  #Create an LOD matrix
  lod_mat <- matrix(ncol = (ncol(quant_name_wide)-1), nrow = length(quant),
                    dimnames = list(names(quant),
                                    names(quant_name_wide[-1])))

  for (i in 1:nrow(quant_id_wide)) {

    #Extract the minerals that need the LOD calculated
    D8_rirs_loop[[i]] <- D8_rirs

    names(D8_rirs_loop)[i] <- quant_id_wide$sample_id[i]

    int_std_rir <- quant[[i]]$minerals$rir[which(quant[[i]]$minerals$min_name == "Quartz")]

    int_std_min_pc <- quant[[i]]$minerals$min_percent[which(quant[[i]]$minerals$min_name == "Quartz")]

    int_std_min_weight <- int_std_min_pc/sum(int_std_min_pc)

    int_std_rir <- sum(int_std_rir * int_std_min_weight)

    #Then calculate the lld for all the phases
    D8_rirs_loop[[i]]$LOD <- (lld_std[i] * (D8_rirs_loop[[i]]$rir/int_std_rir)^-1) * 0.3

    #Aggregrate the LOD data
    D8_rirs_loop[[i]] <- aggregate(LOD ~ min_name,
                                   data = D8_rirs_loop[[i]], FUN = sum)

    #spread the data
    D8_rirs_loop[[i]] <- tidyr::spread(data = D8_rirs_loop[[i]],
                                       min_name, LOD, convert = TRUE)
    #Ensure it's a dataframe
    D8_rirs_loop[[i]] <- data.frame(D8_rirs_loop[[i]])

    #Change the amorphous components to 2% detection limit

    #Get the amorphous index
    am_index <- which(names(D8_rirs_loop[[i]]) %in% c("Allophane",
                                                      "Ferrihydrite",
                                                      "Glass",
                                                      "Obsidian",
                                                      "Organic matter"))

    #Replace the amorphous phases with a 2 % instead
    D8_rirs_loop[[i]][1, am_index] <- rep(2, length(am_index))


    #Populate the lod matrix
    lod_mat[i, ] <- as.numeric(as.vector(D8_rirs_loop[[i]][1,]))


  }


  #Replace NA's with detections limits in the quant_name_wide data frame

  filled <- zCompositions::multRepl(as.matrix(quant_name_wide[-1]),
                                    label = NA, dl = lod_mat)

  #Extract the change caused by adding all these LOD's
  change_from_100 <- rowSums(filled)
  names(change_from_100) <- quant_name_wide$sample_id

  #Close the data again
  for (i in 1:nrow(filled)) {

    filled[i, ] <- as.numeric(as.vector(filled[i, ]))/
      sum(as.numeric(as.vector(filled[i, ]))) * 100

  }

  filled <- data.frame("sample_id" = quant_name_wide$sample_id,
                       filled)
  out <- list("filled" = filled, "figures" = xrd_plots,
              "additions" = change_from_100 -100)
  return(out)
}
