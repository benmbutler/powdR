.harmoniser <- function(lib, smpl) {

  #Extract 2theta of lib
  tth_lib <- lib$tth

  #Extract 2theta of smpl
  tth_smpl <- smpl[[1]]

  #Calculate the range of the overlapping two theta
  tth_range <- c(max(unlist(lapply(list(tth_lib, tth_smpl), min))),
                 min(unlist(lapply(list(tth_lib, tth_smpl), max))))

  if (tth_range[1] > tth_range[2]) {

    stop("The 2theta ranges of the sample and library do not overlap at all.")

  }

  #subset the library if required
  keep_tth_lib <- which(lib$tth >= tth_range[1] & lib$tth <= tth_range[2])

  if (!length(keep_tth_lib) == length(tth_lib)) {

    lib$tth <- lib$tth[keep_tth_lib]

    if(ncol(lib$xrd) > 1) {

    lib$xrd <- lib$xrd[keep_tth_lib,]

    } else {

    phase_id_keep <- names(lib$xrd)
    lib$xrd <- data.frame(lib$xrd[keep_tth_lib,])
    names(lib$xrd) <- phase_id_keep

    }

  }

  #subset the sample
  keep_tth_smpl <- which(smpl[[1]] >= tth_range[1] & smpl[[1]] <= tth_range[2])

  if (!length(keep_tth_smpl) == length(tth_smpl)) {

    smpl <- smpl[keep_tth_smpl,]

  }

  #Select whether to harmonise to the smpl or lib
  l <- list("smpl_tth" = smpl[[1]],
            "lib_tth" = lib$tth)

  l_max <- which.max(lapply(l, function (x) mean(diff(x))))

  if (l_max == 1) {

    cat("\n-Harmonising library to the same 2theta resolution as the sample")

    lib$xrd <- data.frame(lapply(lib$xrd,
                                 function(n) stats::spline(x = lib$tth,
                                                           y = n,
                                                           method = "natural",
                                                           xout = smpl[[1]])[[2]]))


    lib$tth <- smpl[[1]]

  } else {

    cat("\n-Harmonising sample to the same 2theta resolution as the library")

    smpl <- data.frame(stats::spline(x = smpl[[1]],
                              y = smpl[[2]],
                              method = "natural",
                              xout = lib$tth))

  }

  #Name the columns for consistency
  names(smpl) <- c("tth", "counts")

  #Remove NA's that can creep in
  df <- data.frame("tth_smpl" = smpl[[1]],
                   "counts_smpl" = smpl[[2]],
                   "tth_lib" = lib$tth,
                   lib$xrd)

  df2 <- stats::na.omit(df)

  smpl <- data.frame("tth" = df2$tth_smpl,
                     "counts" = df2$counts_smpl)

  lib$tth <- df2$tth_lib
  lib$xrd <- df2[-c(1:3)]

  out <- list("lib" = lib,
              "smpl" = smpl)

  return(out)


}
