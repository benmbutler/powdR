.remove_amorphous <- function(x, amorphous, amorphous_lld, df, lib, solver, smpl,
                             obj) {

  if(length(amorphous) > 0) {

    remove_amorphous <- which(names(x) %in% df$phase_id[which(df$phase_id %in% amorphous &
                                                                df$phase_percent < amorphous_lld)])

    while (length(remove_amorphous) > 0) {

      cat("\n-Some amorphous phases below the amorphous_lld limit. Removing them and reoptimising")
      #Remove amorphous phase from library
      lib$xrd <- lib$xrd[-remove_amorphous]
      x <- x[-remove_amorphous]

      #reoptimise
      o <- stats::optim(par = x, .fullpat,
                        method = solver, pure_patterns = lib$xrd,
                        sample_pattern = smpl[, 2], obj = obj)
      x <- o$par

      #Recompute percentages
      min_concs <- .qminerals(x = x, xrd_lib = lib)
      df <- min_concs[[1]]
      dfs <- min_concs[[2]]

      remove_amorphous <- which(names(x) %in% df$phase_id[which(df$phase_id %in% amorphous &
                                                                  df$phase_percent < amorphous_lld)])
    }
  }

  return(list("x" = x, "lib" = lib))


}
