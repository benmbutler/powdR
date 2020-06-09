
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


.lod <- function(x, lib, std, amorphous, lod, force) {

  if(missing(force)) {

    force <- c()

  }

  #quantify minerals
  quant <- .qminerals(x = x, xrd_lib = lib)


  #Check the order
  if(!all(sapply(list(quant$df$phase_id,
                     names(lib$xrd)), FUN = identical, names(x)))) {

    stop("The orders of the coefficients and library no longer match.")

  }

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

  force_names <- lib$phases$phase_name[which(lib$phases$phase_id %in% force)]

  #Take amorphous phases out of the remove_these_phases vector if the vector exists and if
  #it contains any amorphous phases
  if (length(remove_these_phases) > 0 & length(which(remove_these_phases %in% amorphous_names)) > 0) {

    #Make sure amorphous phases are retained
    remove_these_phases <- remove_these_phases[-which(remove_these_phases %in% amorphous_names)]

  }

  if (length(remove_these_phases) > 0 & length(which(remove_these_phases %in% force_names)) > 0) {

    #Make sure amorphous phases are retained
    remove_these_phases <- remove_these_phases[-which(remove_these_phases %in% force_names)]

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



.lod2 <- function(x, lib, std, std_conc, amorphous, lod, force) {

  if(missing(force)) {

    force <- c()

  }

  #quantify minerals using the qminerals2 function, which automatically
  #excludes any phase associated with the internal standard
  quant <- .qminerals2(x = x, xrd_lib = lib,
                       std = std, std_conc = std_conc)


  #Identify any ID's of phases with the same name as std
  std_name <- lib$phases$phase_name[which(lib$phases$phase_id == std)]

  #Extract all the ids of potential standard patterns
  std_ids <- lib$phases$phase_id[which(lib$phases$phase_name == std_name)]

  id_match <- which(names(x) %in% std_ids)

  #Check the order
  if (length(id_match) < 1) {

    stop("\n-The phase specified as the std is not present. Cannot compute
         limits of detection.")

  }

  if(!all(sapply(list(quant$df$phase_id,
                      names(lib$xrd)), FUN = identical, names(x)))) {

    stop("The orders of the coefficients and library no longer match.")

  }

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

  force_names <- lib$phases$phase_name[which(lib$phases$phase_id %in% force)]

  #Take amorphous phases out of the remove_these_phases vector if the vector exists and if
  #it contains any amorphous phases
  if (length(remove_these_phases) > 0 & length(which(remove_these_phases %in% amorphous_names)) > 0) {

    #Make sure amorphous phases are retained
    remove_these_phases <- remove_these_phases[-which(remove_these_phases %in% amorphous_names)]

  }

  if (length(remove_these_phases) > 0 & length(which(remove_these_phases %in% force_names)) > 0) {

    #Make sure amorphous phases are retained
    remove_these_phases <- remove_these_phases[-which(remove_these_phases %in% force_names)]

  }

  #if the remove_these_phases vector still exists afer amorphous phases have been removed from it,
  #then use it to remove phase below detection limit
  if (length(remove_these_phases) > 0) {

    remove_these <- quant$df$phase_id[which(quant$df$phase_name %in% remove_these_phases)]

    cat("\n-Removing phases below detection limit")
    lib_df <- lib$xrd[-which(names(lib$xrd) %in% remove_these)]
    x <- x[-which(names(x) %in% remove_these)]

  } else {

    lib_df <- lib$xrd

  }


  out <- list("x" = x, "lib" = lib_df)

}


