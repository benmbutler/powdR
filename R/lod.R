
#-----------------------------------------------------------------------------------------------
#LOD 2, which is simpler and groups phases according to their phase name so that some
#phases with small contributions from mutiple refrence patterns don't get thrown away mistakingly
#------------------------------------------------------------------------------------------------

.weighted_rirs <- function(df) {

  df$weighting <- NA
  df$weighted_rir <- NA

  for (i in 1:length(table(df$phase_name))) {

    current_phase <- names(table(df$phase_name))[i]
    phase_index <- which(df$phase_name == current_phase)

    df$weighting[phase_index] <- df$phase_percent[phase_index]/sum(df$phase_percent[phase_index])
    df$weighted_rir[phase_index] <- df$weighting[phase_index] * df$rir[phase_index]

  }

  dfs <- stats::aggregate(list("phase_percent" = df$phase_percent,
                               "weighted_rir" = df$weighted_rir),
                          by = list("phase_name" = df$phase_name), FUN = sum)

  dfs$weighted_rir <- round(dfs$weighted_rir, 3)

  return(dfs)

}


.lod <- function(x, smpl, lib, std, amorphous, lod) {

  #quantify minerals
  quant <- .qminerals(x = x, xrd_lib = lib)

  #order the data
  quant$df <- quant$df[order(quant$df$phase_id),]

  x <- x[order(names(x))]

  #Same for lib$xrd and lib$phases
  lib$xrd <- lib$xrd[order(names(lib$xrd))]
  lib$phases <- lib$phases[order(lib$phases$phase_id),]

  #Add the amorphous phases as a column in the quant$df table
 # quant$df$amorphous <- FALSE

  #if(length(which(amorphous %in% quant$df$phase_id)) > 0) {
   # quant$df$amorphous[which(quant$df$phase_id %in% amorphous)] <- TRUE
  #}

  #Compute the weighted RIR's
  dfs_weighted <- .weighted_rirs(quant$df)

  #Extract the RIR of the standard
  std_rir <- lib$phases$rir[which(lib$phases$phase_id == std)]

  #Compute a vector of lod's for all phases
  all_lod <- lod * (std_rir/dfs_weighted$weighted_rir)
  names(all_lod) <- dfs_weighted$phase_name

  #Now remove phases that are below detection limit
  remove_these_phases <- dfs_weighted$phase_name[which(dfs_weighted$phase_percent < all_lod)]

  #Need to extract the amorphous names instead of the id's identified in the function call
  amorphous_names <- lib$phases$phase_name[which(lib$phases$phase_id %in% amorphous)]

  #Take amorphous phases out of the remove_these_phases vector if the vector exists and if
  #it contains any amorphous phases
  if (length(remove_these_phases) > 0 & length(which(remove_these_phases %in% amorphous_names)) > 0) {

    #Make sure amorphous phases are retained
    remove_these_phases <- remove_these_phases[-which(remove_these_phases %in% amorphous_names)]

  }

  #if the remove_these_phases vector still exists afer amorphous phases have been removed from it,
  #then use it to remove phase below detection limit
  if (length(remove_these_phases) > 0) {

    remove_these <- which(quant$df$phase_name %in% remove_these_phases)

    cat("\n-Removing phases below detection limit")
    lib_df <- lib$xrd[-remove_these]
    x <- x[-remove_these]

  } else {

      lib_df <- lib$xrd

    }


  out <- list("x" = x, "lib" = lib_df)

}


# .lod <- function(x, smpl, lib, std, amorphous, background, lod, tth_lod) {
#
#   #quantify minerals
#   quant <- .qminerals(x = x, xrd_lib = lib)
#
#   #fit background to fitted pattern
#
#   if (length(x) == 1) {
#     fit <- lib$xrd * x
#   } else {
#     fit <- unname(apply(sweep(lib$xrd[names(x)], 2, x, "*"), 1, sum))
#   }
#
#   fit_bkg <- bkg(xrd = data.frame(tth = lib$tth,
#                                   counts = fit),
#                  lambda = background$lambda,
#                  hwi = background$hwi,
#                  it = background$it,
#                  int = background$int)
#
#   #-----------------------------------------------------------
#   # Catching errors when the internal standard gets dropped
#   #-----------------------------------------------------------
#
#   #Check that the internal standard is present and if it isn't then
#   #use an alternative
#   if (length(which(quant$df$phase_id == std)) == 0) {
#
#      cat("\n-The original internal standard specified is no longer available")
#
#     alt_std_name <- lib$phases$phase_name[which(lib$phases$phase_id == std)]
#     alt_std_options <- quant$df$phase_id[which(quant$df$phase_name == alt_std_name)]
#
#     if (length(alt_std_options) > 0) {
#
#       cat(paste("\n-Selecting an alternative", alt_std_name, "standard to use"))
#
#       std <- alt_std_options[which.max(quant$df$phase_percent[which(quant$df$phase_id %in% alt_std_options)])]
#
#       cat(paste("\n-Now using", std, "as the internal standard for LOD estimation"))
#
#     } else {
#
#       cat(paste("\n-Unable to estimate limits of detection. Retry using a different standard"))
#       lib_df <- lib$xrd
#
#     }
#
#   }
#
#
#
#   #If a suitable standard is available then carry out the lod function
#   if (std %in% names(lib$xrd)) {
#
#   #order quant$df and x so they're guaranteed to be the same order
#   quant$df <- quant$df[order(quant$df$phase_id),]
#
#   x <- x[order(names(x))]
#
#   #Same for lib$xrd and lib$phases
#   lib$xrd <- lib$xrd[order(names(lib$xrd))]
#   lib$phases <- lib$phases[order(lib$phases$phase_id),]
#
#   #Add the amorphous phases as a column in the quant$df table
#   quant$df$amorphous <- FALSE
#
#   if(length(which(amorphous %in% quant$df$phase_id)) > 0) {
#     quant$df$amorphous[which(quant$df$phase_id %in% amorphous)] <- TRUE
#   }
#
#
#   #------------------------------------------------------------------
#   #If more than one phase is available for the given standard mineral
#   #then use it!
#   #------------------------------------------------------------------
#
#   #Create a vector of the summed signal from the identified mineral
#   std_mineral <- quant$df$phase_name[which(quant$df$phase_id == std)]
#
#   #Create a dataframe of scaled counts
#   scaled_xrd <- data.frame(mapply("*", lib$xrd, x))
#
#
#   if (length(which(quant$df$phase_name == std_mineral)) > 1) {
#
#     cat(paste0("\n-Grouping all available ", std_mineral, " standards together to compute LOD (n = ",
#               length(which(quant$df$phase_name == std_mineral)), ")"))
#
#     mineral_ids <- quant$df$phase_id[which(quant$df$phase_name == std_mineral)]
#
#     #Compute the counts as the sum
#     std_counts <- rowSums(scaled_xrd[mineral_ids])
#
#     #Compute the RIR as the weighted average
#     std_props <- quant$df$phase_percent[which(quant$df$phase_name == std_mineral)] /
#                  sum(quant$df$phase_percent[which(quant$df$phase_name == std_mineral)])
#
#     std_rir <- sum(quant$df$rir[which(quant$df$phase_name == std_mineral)] * std_props)
#
#     #Compute the concentration
#     std_conc <- sum(quant$df$phase_percent[which(quant$df$phase_name == std_mineral)])
#
#   } else { #i.e. if only one standard is available
#
#     #Extract the counts
#     std_counts <- scaled_xrd[[std]]
#     std_rir <- quant$df$rir[which(quant$df$phase_id == std)]
#     std_conc <- quant$df$phase_percent[which(quant$df$phase_id == std)]
#
#   }
#
#   #Add a warning if the internal standard is less that 5 %
#   if (std_conc < 5) {
#
#     warning("The estimated concentration of the interal standard is less
#             than 5 %, we recommend selecting another phase to use.")
#
#   }
#
#
#   #fit background to chosen standard
#   std_bkg <- bkg(xrd = data.frame(tth = lib$tth,
#                                   counts = std_counts),
#                  lambda = background$lambda,
#                  hwi = background$hwi,
#                  it = background$it,
#                  int = background$int)
#
#
#   #Take the standard background from the bacground of the fit
#   fit_bkg2 <- fit_bkg$background - std_bkg$background
#
#   #Add the internal standard pattern to the fit_bkg2
#   int_std <- std_counts + fit_bkg2
#
#   #Compute sums of fit_bkg2 and int_std
#   bkg_sum <- sum(fit_bkg2[which(lib$tth >= tth_lod[1] & lib$tth <= tth_lod[2])])
#   std_sum <- sum(int_std[which(lib$tth >= tth_lod[1] & lib$tth <= tth_lod[2])])
#
#   std_lod <- (lod*sqrt(bkg_sum))/(std_sum/std_conc)
#
#   #Compute a vector of lod's for all phases
#   all_lod <- std_lod * (std_rir/quant$df$rir)
#   names(all_lod) <- quant$df$phase_id
#
#   #Now remove phases that are below detection limit
#   remove_these <- which(quant$df$phase_percent < all_lod & quant$df$amorphous == FALSE)
#
#   #Remove phases from x and the library
#   if (length(remove_these > 0)) {
#
#     cat("\n-Removing phases below detection limit")
#     lib_df <- lib$xrd[-remove_these]
#     x <- x[-remove_these]
#
#   } else{
#
#     lib_df <- lib$xrd
#
#   }
#
#   }
#
#   out <- list("x" = x, "lib" = lib_df, "background" = fit_bkg$background)
#
# }


