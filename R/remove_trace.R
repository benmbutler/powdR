.remove_trace <- function(x, lib, smpl, solver, obj, remove_trace) {

  quant <- .qminerals(x = x, xrd_lib = lib)

  minpar <- min(quant$df$phase_percent)

  while (minpar <= remove_trace) {

    #check for any negative parameters
    omit <- which(quant$df$phase_percent <= remove_trace)

    #remove the column from the library that contains the identified data
    if (length(which(x <= remove_trace)) > 0) {
      lib$xrd <- lib$xrd[, -omit]
      x <- x[-omit]
    }


    #If L-BFGS-B is being used:
    if (solver == "L-BFGS-B") {

      cat("\n-Removing phases with concentration less than remove_trace and reoptimising using L-BFGS-S constrained to a lower limit of zero...")
      o <- stats::optim(par = x, .fullpat,
                        method = solver, lower = 0, pure_patterns = lib$xrd,
                        sample_pattern = smpl[, 2], obj = obj)
      x <- o$par

    }

    #If other optimisation routines are being used:
    if (solver %in% c("BFGS", "Nelder-Mead", "CG")) {

      cat("\n-Removing phases with concentration less than remove_trace and reoptimising...")
      o <- stats::optim(par = x, .fullpat,
                        method = solver, pure_patterns = lib$xrd,
                        sample_pattern = smpl[, 2], obj = obj)
      x <- o$par

    }

    #If NNLS is being used:
    if (solver == "NNLS") {

      cat("\n-Removing phases with concentration less than remove_trace and reapplying NNLS...")
      nnls_out <- .xrd_nnls(xrd.lib = lib, xrd.sample = smpl[, 2])

      lib$xrd <- nnls_out$xrd.lib
      x <- nnls_out$x

    }

    quant <- .qminerals(x = x, xrd_lib = lib)

    minpar <- min(quant$df$phase_percent)

  }

  return(list("x" = x, "lib" = lib))

}
