.remove_amorphous <- function(x, amorphous, amorphous_lod, df, lib, solver, smpl,
                             obj, weighting, tth_fps) {

  if(length(amorphous) > 0) {

    remove_amorphous <- which(names(x) %in% df$phase_id[which(df$phase_id %in% amorphous &
                                                                df$phase_percent < amorphous_lod)])

    while (length(remove_amorphous) > 0) {

      cat("\n-Some amorphous phases below the amorphous_lod limit. Removing them and reoptimising")
      #Remove amorphous phase from library
      lib$xrd <- lib$xrd[-remove_amorphous]
      x <- x[-remove_amorphous]

      #reoptimise
      if (solver %in% c("Nelder-Mead", "BFGS", "CG")) {

        cat("\n-Reoptimising after removing amorphous phases below amorphous_lod")
        o <- stats::optim(par = x, .fullpat,
                        method = solver, pure_patterns = lib$xrd,
                        sample_pattern = smpl[, 2], obj = obj,
                        tth = lib$tth, tth_fps = tth_fps,
                        weighting = weighting[[2]])

      } else {

        cat("\n-Reoptimising after removing amorphous phases below amorphous_lod. Using
            L-BFGS-B constrained to a lower limit of zero")
        o <- stats::optim(par = x, .fullpat,
                          method = solver, lower = 0, pure_patterns = lib$xrd,
                          sample_pattern = smpl[, 2], obj = obj,
                          tth = lib$tth, tth_fps = tth_fps,
                          weighting = weighting[[2]])


      }

      x <- o$par

      #Recompute percentages
      min_concs <- .qminerals(x = x, xrd_lib = lib)
      df <- min_concs[[1]]
      dfs <- min_concs[[2]]

      remove_amorphous <- which(names(x) %in% df$phase_id[which(df$phase_id %in% amorphous &
                                                                  df$phase_percent < amorphous_lod)])
    }
  }

  return(list("x" = x, "lib" = lib))


}



.remove_amorphous2 <- function(x, amorphous, amorphous_lod, df, lib, solver, smpl,
                              obj, std, std_conc, weighting, tth_fps) {

  if(length(amorphous) > 0) {

    remove_amorphous <- which(names(x) %in% df$phase_id[which(df$phase_id %in% amorphous &
                                                                df$phase_percent < amorphous_lod)])

    while (length(remove_amorphous) > 0) {

      cat("\n-Some amorphous phases below the amorphous_lod limit. Removing them and reoptimising")
      #Remove amorphous phase from library
      lib$xrd <- lib$xrd[-remove_amorphous]
      x <- x[-remove_amorphous]

      #reoptimise
      if (solver %in% c("Nelder-Mead", "BFGS", "CG")) {

        cat("\n-Reoptimising after removing amorphous phases below amorphous_lod")
        o <- stats::optim(par = x, .fullpat,
                          method = solver, pure_patterns = lib$xrd,
                          sample_pattern = smpl[, 2], obj = obj,
                          tth = lib$tth, tth_fps = tth_fps,
                          weighting = weighting[[2]])

      } else {

        cat("\n-Reoptimising after removing amorphous phases below amorphous_lod. Using
            L-BFGS-B constrained to a lower limit of zero")
        o <- stats::optim(par = x, .fullpat,
                          method = solver, lower = 0, pure_patterns = lib$xrd,
                          sample_pattern = smpl[, 2], obj = obj,
                          tth = lib$tth, tth_fps = tth_fps,
                          weighting = weighting[[2]])


      }

      x <- o$par

      #Recompute percentages

      min_concs <- .qminerals2(x = x, xrd_lib = lib, std = std, std_conc = std_conc)

      df <- min_concs[[1]]
      dfs <- min_concs[[2]]

      remove_amorphous <- which(names(x) %in% df$phase_id[which(df$phase_id %in% amorphous &
                                                                  df$phase_percent < amorphous_lod)])
    }
  }

  return(list("x" = x, "lib" = lib))


}
