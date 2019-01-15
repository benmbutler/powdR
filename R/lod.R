.lod <- function(x, smpl, lib, std, amorphous, background, lod, tth_lod) {

  #quantify minerals
  quant <- .qminerals(x = x, xrd_lib = lib)

  #fit background to fitted pattern

  if (length(x) == 1) {
    fit <- lib$xrd * x
  } else {
    fit <- unname(apply(sweep(lib$xrd[names(x)], 2, x, "*"), 1, sum))
  }

  fit_bkg <- bkg(xrd = data.frame(tth = lib$tth,
                                  counts = fit),
                 lambda = background$lambda,
                 hwi = background$hwi,
                 it = background$it,
                 int = background$int)

  #-----------------------------------------------------------
  # Catching errors when the internal standard gets dropped
  #-----------------------------------------------------------

  #Check that the internal standard is present and if it isn't then
  #use an alternative
  if (length(which(quant$df$phase_id == std)) == 0) {

     cat("\n-The original internal standard specified is no longer available")

    alt_std_name <- lib$phases$phase_name[which(lib$phases$phase_id == std)]
    alt_std_options <- quant$df$phase_id[which(quant$df$phase_name == alt_std_name)]

    if (length(alt_std_options) > 0) {

      cat(paste("\n-Selecting an alternative", alt_std_name, "standard to use"))

      std <- alt_std_options[which.max(quant$df$phase_percent[which(quant$df$phase_id %in% alt_std_options)])]

      cat(paste("\n-Now using", std, "as the internal standard for LOD estimation"))

    } else {

      cat(paste("\n-Unable to estimate limits of detection. Retry using a different standard"))
      lib_df <- lib$xrd

    }

  }

  #If a suitable standard is available then carry out the lod function
  if (std %in% names(lib$xrd)) {

  #Add a warning if the internal standard is less that 5 %
  if (quant$df$phase_percent[which(quant$df$phase_id == std)] < 5) {

    warning("The estimated concentration of the interal standard is less
            than 5 %, we recommend selecting another phase to use.")

  }

  #order quant$df and x so they're guaranteed to be the same order
  quant$df <- quant$df[order(quant$df$phase_id),]

  x <- x[order(names(x))]

  #Same for lib$xrd and lib$phases
  lib$xrd <- lib$xrd[order(names(lib$xrd))]
  lib$phases <- lib$phases[order(lib$phases$phase_id),]

  #Add the amorphous phases as a column in the quant$df table
  quant$df$amorphous <- FALSE

  if(length(which(amorphous %in% quant$df$phase_id)) > 0) {
    quant$df$amorphous[which(quant$df$phase_id %in% amorphous)] <- TRUE
  }

  #fit background to chosen standard
  std_bkg <- bkg(xrd = data.frame(tth = lib$tth,
                                  counts = lib$xrd[[std]]),
                 lambda = background$lambda,
                 hwi = background$hwi,
                 it = background$it,
                 int = background$int)


  #Take the standard background from the bacground of the fit
  fit_bkg2 <- fit_bkg$background - std_bkg$background

  #Add the internal standard pattern to the fit_bkg2
  int_std <- lib$xrd[[std]] + fit_bkg2

  #Compute sums of fit_bkg2 and int_std
  bkg_sum <- sum(fit_bkg2[which(lib$tth >= tth_lod[1] & lib$tth <= tth_lod[2])])
  std_sum <- sum(int_std[which(lib$tth >= tth_lod[1] & lib$tth <= tth_lod[2])])

  std_lod <- (lod*sqrt(bkg_sum))/(std_sum/quant$df$phase_percent[which(quant$df$phase_id == std)])

  #Compute a vector of lod's for all phases
  all_lod <- std_lod * (quant$df$rir[which(quant$df$phase_id == std)]/quant$df$rir)
  names(all_lod) <- quant$df$phase_id

  #Now remove phases that are below detection limit
  remove_these <- which(quant$df$phase_percent < all_lod & quant$df$amorphous == FALSE)

  #Remove phases from x and the library
  if (length(remove_these > 0)) {

    cat("\n-Removing phases below detection limit")
    lib_df <- lib$xrd[-remove_these]
    x <- x[-remove_these]

  } else{

    lib_df <- lib$xrd

  }

  }

  out <- list("x" = x, "lib" = lib_df, "background" = fit_bkg$background)

}
