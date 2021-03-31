.remove_neg <- function(x, lib, smpl, solver, obj, force, tth_fps, weighting) {

  if (missing(force)) {

    force <- c()

  }

  if (length(which(names(x) %in% force)) > 0) {

    negpar <- min(x[-which(names(x) %in% force)])

  } else {

    negpar <- min(x)

  }

while (negpar <= 0) {
  #check for any negative parameters

  if (length(which(names(x) %in% force)) > 0) {

    omit <- which(x <= 0 & !names(x) %in% force)

  } else {

    omit <- which(x <= 0)

  }

  #remove the column from the library that contains the identified data
  if (length(omit) > 0) {
    lib$xrd <- lib$xrd[, -omit]
    x <- x[-omit]
  }


  cat("\n-Removing negative coefficients and reoptimising...")
  o <- stats::optim(par = x, .fullpat,
                    method = solver, pure_patterns = lib$xrd,
                    sample_pattern = smpl, obj = obj,
                    tth = lib$tth, tth_fps = tth_fps,
                    weighting = weighting)

  x <- o$par

  #identify whether any parameters are negative for the next iteration
  if (length(which(names(x) %in% force)) > 0) {

    negpar <- min(x[-which(names(x) %in% force)])

  } else {

    negpar <- min(x)

  }


}

return(list("x" = x, "lib" = lib))

}
