.remove_neg <- function(x, lib, smpl, solver, obj) {

negpar <- min(x)

while (negpar < 0) {
  #check for any negative parameters
  omit <- which(x < 0)

  #remove the column from the library that contains the identified data
  if (length(which(x < 0)) > 0) {
    lib$xrd <- lib$xrd[, -omit]
    x <- x[-omit]
  }

  cat("\n-Removing negative coefficients and reoptimising...")
  o <- stats::optim(par = x, .fullpat,
                    method = solver, pure_patterns = lib$xrd,
                    sample_pattern = smpl[, 2], obj = obj)
  x <- o$par
  #identify whether any parameters are negative for the next iteration
  negpar <- min(x)
}

return(list("x" = x, "lib" = lib))

}
