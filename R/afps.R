#' Automated full pattern summation
#'
#' \code{afps} returns estimates of phase concentrations using automated full pattern
#' summation of X-ray powder diffraction data. It is designed for high-throughput cases
#' involving mineral quantification from large reference libraries. For more details
#' see \code{?afps.powdRlib}.
#'
#' Applies automated full pattern summation to an XRPD
#' measurement to quantify phase concentrations. Requires a \code{powdRlib} library of
#' reference patterns with pre-measured reference intensity ratios in order to derive
#' mineral concentrations.
#'
#' @param lib A \code{powdRlib} object representing the reference library. Created using the
#' \code{powdRlib} constructor function.
#' @param ... Other parameters passed to methods e.g. \code{afps.powdRlib}
#'
#' @return a list with components:
#' \item{tth}{a vector of the 2theta scale of the fitted data}
#' \item{fitted}{a vector of the fitted XRPD pattern}
#' \item{measured}{a vector of the original XRPD measurement (aligned)}
#' \item{residuals}{a vector of the residuals (fitted vs measured)}
#' \item{phases}{a dataframe of the phases used to produce the fitted pattern}
#' \item{phases_summary}{the phases dataframe grouped by phase_name and summarised (sum)}
#' \item{rwp}{the Rwp of the fitted vs measured pattern}
#' \item{weighted_pure_patterns}{a dataframe of reference patterns used to produce the fitted pattern.
#' All patterns have been weighted according to the coefficients used in the fit}
#' \item{coefficients}{a named vector of coefficients used to produce the fitted pattern}
#'
#' @examples
#' #Load the minerals library
#' data(minerals)
#'
#' # Load the soils data
#' data(soils)
#'
#' #Since the reference library is relatively small,
#' #the whole library can be used at once to get an
#' #estimate of the phases within each sample.
#' \dontrun{
#' afps_sand <-  afps(lib = minerals,
#'                  smpl = soils$sandstone,
#'                  std = "QUA.2",
#'                  align = 0.2,
#'                  lod = 0.2,
#'                  amorphous = "ORG",
#'                  amorphous_lod = 1)
#'
#' afps_lime <- afps(lib = minerals,
#'                 smpl = soils$limestone,
#'                 std = "QUA.2",
#'                 align = 0.2,
#'                 lod = 0.2,
#'                 amorphous = "ORG",
#'                 amorphous_lod = 1)
#'
#' afps_granite <- afps(lib = minerals,
#'                    smpl = soils$granite,
#'                    std = "QUA.2",
#'                    align = 0.2,
#'                    lod = 0.2,
#'                    amorphous = "ORG",
#'                    amorphous_lod = 1)
#'
#' #Alternatively run all 3 at once using lapply
#'
#' afps_soils <- lapply(soils, afps,
#'                      lib = minerals,
#'                      std = "QUA.2",
#'                      align = 0.2,
#'                      lod = 0.2,
#'                      amorphous = "ORG",
#'                      amorphous_lod = 1)
#' }
#' @references
#' Chipera, S.J., Bish, D.L., 2013. Fitting Full X-Ray Diffraction Patterns for Quantitative Analysis:
#' A Method for Readily Quantifying Crystalline and Disordered Phases. Adv. Mater. Phys. Chem. 03, 47-53.
#' doi:10.4236/ampc.2013.31A007
#'
#' Chipera, S.J., Bish, D.L., 2002. FULLPAT: A full-pattern quantitative analysis program for X-ray powder
#' diffraction using measured and calculated patterns. J. Appl. Crystallogr. 35, 744-749.
#' doi:10.1107/S0021889802017405
#'
#' Eberl, D.D., 2003. User's guide to ROCKJOCK - A program for determining quantitative mineralogy from
#' powder X-ray diffraction data. Boulder, CA.
#' @export
afps <- function(lib, ...) {
  UseMethod("afps")
}

#' Automated full pattern summation
#'
#' \code{afps.powdRlib} returns estimates of phase concentrations using automated full pattern
#' summation of X-ray powder diffraction data. It is designed for high-throughput cases
#' involving mineral quantification from large reference libraries.
#'
#' Applies automated full pattern summation to an XRPD
#' sample to quantify phase concentrations. Requires a \code{powdRlib} library of reference
#' patterns with pre-measured reference intensity ratios in order to derive mineral
#' concentrations.
#'
#' @param lib A \code{powdRlib} object representing the reference library. Created using the
#' \code{powdRlib} constructor function.
#' @param smpl A data frame. First column is 2theta, second column is counts
#' @param solver The optimisation routine to be used. One of \code{c("BFGS", "Nelder-Mead",
#' "CG" or "L-BFGS-B")}. Default = \code{"BFGS"}.
#' @param obj The objective function to minimise. One of \code{c("Delta", "R", "Rwp")}.
#' Default = \code{"Rwp"}. See Chipera and Bish (2002) and page 247 of Bish and Post (1989)
#' for definitions of these functions.
#' @param std The phase ID (e.g. "QUA.1") to be used as internal
#' standard. Must match an ID provided in the \code{phases} parameter.
#' @param tth_align A vector defining the minimum and maximum 2theta values to be used during
#' alignment. If not defined, then the full range is used.
#' @param align The maximum shift that is allowed during initial 2theta
#' alignment (degrees). Default = 0.1.
#' @param shift The maximum shift (degrees 2theta) that is allowed during the grid search phases selected
#' from the non-negative least squares. Default = 0.05).
#' @param shift_res A single integer defining the increase in resolution used during grid search shifting. Higher
#' values facilitate finer shifts at the expense of longer computation. Default = 4.
#' @param tth_fps A vector defining the minimum and maximum 2theta values to be used during
#' automated full pattern summation. If not defined, then the full range is used.
#' @param lod Optional parameter used to define the limit of detection (in weight percent) of the internal standard
#' (i.e. the phase provided in the \code{std} argument). The \code{lod} value is used to estimate the lod of other
#' phases during the fitting process and hence remove reference patterns that are considered below detection limit.
#' Default = 0.1. If \code{lod = 0} then limits of detection are not computed.
#' @param amorphous A character string of any phase id's that should be treated as amorphous. Each must
#' match a phase_id in the phases table of `lib`.
#' @param amorphous_lod Optional parameter used to exclude amorphous phases if they are below this
#' specified limit (percent). Must be between 0 and 100. Default = 0.
#' @param ... other arguments
#'
#' @return a list with components:
#' \item{tth}{a vector of the 2theta scale of the fitted data}
#' \item{fitted}{a vector of the fitted XRPD pattern}
#' \item{measured}{a vector of the original XRPD measurement (aligned)}
#' \item{residuals}{a vector of the residuals (fitted vs measured)}
#' \item{phases}{a dataframe of the phases used to produce the fitted pattern}
#' \item{phases_summary}{the phases dataframe grouped by phase_name and summarised (sum)}
#' \item{rwp}{the Rwp of the fitted vs measured pattern}
#' \item{weighted_pure_patterns}{a dataframe of reference patterns used to produce the fitted pattern.
#' All patterns have been weighted according to the coefficients used in the fit}
#' \item{coefficients}{a named vector of coefficients used to produce the fitted pattern}
#'
#' @examples
#' #Load the minerals library
#' data(minerals)
#'
#' # Load the soils data
#' data(soils)
#'
#' #Since the reference library is relatively small,
#' #the whole library can be used at once to get an
#' #estimate of the phases within each sample.
#' \dontrun{
#' afps_sand <-  afps(lib = minerals,
#'                  smpl = soils$sandstone,
#'                  std = "QUA.2",
#'                  align = 0.2,
#'                  lod = 0.2,
#'                  amorphous = "ORG",
#'                  amorphous_lod = 1)
#'
#' afps_lime <- afps(lib = minerals,
#'                 smpl = soils$limestone,
#'                 std = "QUA.2",
#'                 align = 0.2,
#'                 lod = 0.2,
#'                 amorphous = "ORG",
#'                 amorphous_lod = 1)
#'
#' afps_granite <- afps(lib = minerals,
#'                    smpl = soils$granite,
#'                    std = "QUA.2",
#'                    align = 0.2,
#'                    lod = 0.2,
#'                    amorphous = "ORG",
#'                    amorphous_lod = 1)
#'
#' #Alternatively run all 3 at once using lapply
#'
#' afps_soils <- lapply(soils, afps,
#'                      lib = minerals,
#'                      std = "QUA.2",
#'                      align = 0.2,
#'                      lod = 0.2,
#'                      amorphous = "ORG",
#'                      amorphous_lod = 1)
#' }
#' @references
#' Bish, D.L., Post, J.E., 1989. Modern powder diffraction. Mineralogical Society of America.
#'
#' Chipera, S.J., Bish, D.L., 2013. Fitting Full X-Ray Diffraction Patterns for Quantitative Analysis:
#' A Method for Readily Quantifying Crystalline and Disordered Phases. Adv. Mater. Phys. Chem. 03, 47-53.
#' doi:10.4236/ampc.2013.31A007
#'
#' Chipera, S.J., Bish, D.L., 2002. FULLPAT: A full-pattern quantitative analysis program for X-ray powder
#' diffraction using measured and calculated patterns. J. Appl. Crystallogr. 35, 744-749.
#' doi:10.1107/S0021889802017405
#'
#' Eberl, D.D., 2003. User's guide to ROCKJOCK - A program for determining quantitative mineralogy from
#' powder X-ray diffraction data. Boulder, CA.
#' @export
afps.powdRlib <- function(lib, smpl, solver, obj, std,
                         tth_align, align, shift, shift_res, tth_fps, lod,
                         amorphous, amorphous_lod, ...) {

  #If amorphous is misssing then set it to an empty vector
  if(missing(amorphous)) {
    cat("\n-No amorphous phases identified")
    amorphous = c()
  }

  #If tth_align is missing then use the maximum tth range of the sample
  if(missing(tth_align)) {
    cat("\n-Using maximum tth range")
    tth_align = c(min(smpl[[1]]), max(smpl[[1]]))
  }

  #If align is missing then set it to default
  if(missing(align)) {
    cat("\n-Using default alignment of 0.1")
    align = 0.1
  }

  #If solver is missing then set it to BFGS
  if(missing(solver)) {
    cat("\n-Using default solver of 'BFGS'")
    solver = "BFGS"
  }

  #If a solver other than NNLS is being used but the objective function
  #not defined, then used Rwp
  if(missing(obj) & solver %in% c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B")) {
    cat("\n-Using default objective function of 'Rwp'")
    obj = "Rwp"
  }

  #If shift is missing then set it to default
  if(missing(shift)) {
    cat("\n-Using default shift of 0.05")
    shift = 0.05
  }

  #If shift_res is missing then set it to default
  if(missing(shift_res)) {
    cat("\n-Using default shift_res of 4")
    shift_res = 4
  }

  #If lod is missing then set it to a default of 1
  if(missing(lod)) {
    cat("\n-Using default lod of 0.1")
    lod = 0.1
  }

  #If amorphous_lod is missing, set it to 0
  if(missing(amorphous_lod)) {
    cat("\n-Using default amorphous_lod of 0")
    amorphous_lod = 0
  }

  #Ensure that the align is greater than 0.
  if (align < 0) {
    stop("The align argument must be greater than or equal to 0")
  }

  #Ensure that the lod is greater than 0.
  if (lod <= 0) {
    stop("The lod argument must be greater than 0")
  }

  #Create a warning message if the shift is greater than 0.5, since this can confuse the optimisation
  if (align > 0.5) {
    warning("Be cautious of large 2theta shifts. These can cause issues in sample alignment.")
  }

  #Make only "Nelder-Mead", "BFGS", "CG" or "L-BFGS-B" optional for the solver
  if (!solver %in% c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B")) {
    stop("The solver argument must be one of 'BFGS', 'Nelder Mead', 'CG' or 'L-BFGS-B'")
  }

  #Make sure that the phase identified as the internal standard is contained within the reference library
  if (!std %in% lib$phases$phase_id) {
    stop("The phase you have specified as the internal standard is not in the reference library")
  }

  #Make sure that the amorphous phases identified are contained within the reference library
  if (!length(amorphous) == length(which(amorphous %in% lib$phases$phase_id))) {
    stop("One or more of the phases you have specified as amorphous are not in the reference library")
  }

  #if only one phase is being used, make sure it's a dataframe and named correctly
  if (nrow(lib$phases) == 1) {
    lib$xrd <- data.frame("phase" = lib$xrd)
    names(lib$xrd) <- lib$phases$phase_id
  }


  #align the data
  if (align > 0) {
  cat("\n-Aligning sample to the internal standard")
  smpl <- .xrd_align(smpl = smpl,
                     standard = data.frame(tth = lib$tth,
                                           counts = lib$xrd[, which(lib$phases$phase_id == std)]),
                     xmin = tth_align[1],
                     xmax = tth_align[2], xshift = align)

  #If the alignment is close to the limit, provide a warning
  if (sqrt(smpl[[1]]^2) > (align*0.95)) {
    warning("The optimised shift used in alignment is equal to the maximum shift defined
            in the function call. We advise visual inspection of this alignment.")
  }

  #smpl becomes a data frame by extracting only the aligned data
  smpl <- smpl[[2]]
  #Extract the aligned sample
  smpl <- smpl[which(smpl[[1]] >= min(lib$tth) & smpl[[1]] <= max(lib$tth)), ]

  #Define a 2TH scale to harmonise all data to
  smpl_tth <- smpl[[1]]

  } else {

  names(smpl) <- c("tth", "counts")
  smpl_tth <- smpl[[1]]

  }

  #If tth_fps isn't defined, then define it here
  if(missing(tth_fps)) {
    tth_fps <- c(min(smpl_tth), max(smpl_tth))
  }

  if (align > 0) {

  xrd_ref_names <- lib$phases$phase_id

  #Ensure that samples in the reference library are on the same scale as the sample
  cat("\n-Interpolating library to same 2theta scale as aligned sample")
  lib$xrd <- data.frame(lapply(names(lib$xrd),
                               function(n) stats::approx(x = lib$tth,
                                                         y = unname(unlist(lib$xrd[n])),
                                                         xout = smpl_tth)[[2]]))

  names(lib$xrd) <- xrd_ref_names

  #Replace the library tth with that of the sample
  lib$tth <- smpl_tth

  }

  #### decrease 2TH scale to the range defined in the function call
  smpl <- smpl[which(smpl$tth >= tth_fps[1] & smpl$tth <= tth_fps[2]), ]

  #Subset the xrd dataframe too
  lib$xrd <- lib$xrd[which(lib$tth >= tth_fps[1] & lib$tth <= tth_fps[2]), ]

  #Replace the tth in the library with the shortened one
  lib$tth <- smpl[, 1]

  #if only one phase is being used, make sure it's a dataframe and named correctly
  if (is.vector(lib$xrd)) {
    lib$xrd <- data.frame("phase" = lib$xrd)
    names(lib$xrd) <- lib$phases$phase_id
  }



  #--------------------------------------------
  #Initial NNLS to remove some samples
  #--------------------------------------------

  cat("\n-Applying non-negative least squares")
  nnls_out <- .xrd_nnls(xrd.lib = lib, xrd.sample = smpl[, 2])

  lib$xrd <- nnls_out$xrd.lib
  x <- nnls_out$x

  #--------------------------------------------
  #Initial Optimisation
  #--------------------------------------------

    x <- rep(0, ncol(lib$xrd))
    names(x) <- names(lib$xrd)

    if (solver %in% c("Nelder-Mead", "BFGS", "CG")) {

    cat("\n-Optimising...")
    o <- stats::optim(par = x, .fullpat,
                      method = solver, pure_patterns = lib$xrd,
                      sample_pattern = smpl[, 2], obj = obj)

    } else {

    cat("\n-Optimising using L-BFGS-B constrained to a lower limit of zero...")
    o <- stats::optim(par = x, .fullpat,
                      method = solver, lower = 0, pure_patterns = lib$xrd,
                      sample_pattern = smpl[, 2], obj = obj)

    }

    x <- o$par



  #--------------------------------------------------------------------------------------------
  #Remove negative/zero parameters
  #--------------------------------------------------------------------------------------------

  remove_neg_out <- .remove_neg(x = x, lib = lib, smpl = smpl,
                                solver = solver, obj = obj)

  x <- remove_neg_out[[1]]
  lib <- remove_neg_out[[2]]

  #--------------------------------------------
  #Grid-search shifting
  #--------------------------------------------

  #Alignment and then another optimisation ONLY if the shift parameter
  #is included

  if(shift > 0) {

    fpf_aligned <- .shift(smpl = smpl,
                          lib = lib,
                          max_shift = shift,
                          x = x,
                          res = shift_res,
                          obj = obj)

    smpl <- fpf_aligned[["smpl"]]
    lib$xrd <- data.frame(fpf_aligned[["lib"]])
    lib$tth <- smpl[,1]

  }


  #----------------------------------------------
  #Re-optimise after shifting
  #----------------------------------------------

  if(shift > 0) {

    if (solver %in% c("Nelder-Mead", "BFGS", "CG")) {

      cat("\n-Reoptimising after shifting data")

      o <- stats::optim(par = x, .fullpat,
                        method = solver, pure_patterns = lib$xrd,
                        sample_pattern = smpl[, 2], obj = obj)

    } else {

      cat("\n-Reoptimising after shifting data. Using L-BFGS-B constrained
        to a lower limit of zero")

      o <- stats::optim(par = x, .fullpat,
                        method = solver, lower = 0, pure_patterns = lib$xrd,
                        sample_pattern = smpl[, 2], obj = obj)

    }

    x <- o$par

  }


  #Now that some negative parameters have been removed, the detection limits
  #of the remaining phases are estimated.

  # Removing phases based on detection limits

  #Calculate the lod and remove any phases below it if lod > 0

  if (lod > 0) {

  cat("\n-Calculating detection limits")

  xrd_detectable <- .lod(x = x, lib = lib,
                          std = std, amorphous = amorphous,
                          lod = lod)

  #Reoptimise if things have changed
  logical_reoptimise <- identical(x[order(names(x))], xrd_detectable[["x"]])

  x <- xrd_detectable[["x"]]
  lib$xrd <- xrd_detectable[["lib"]]

  if (logical_reoptimise == FALSE) {

  if (solver %in% c("Nelder-Mead", "BFGS", "CG")) {

  cat("\n-Reoptimising after removing crystalline phases below the limit of detection")

  o <- stats::optim(par = x, .fullpat,
                    method = solver, pure_patterns = lib$xrd,
                    sample_pattern = smpl[, 2], obj = obj)

  } else {

  cat("\n--Reoptimising after removing crystalline phases below the limit of detection. Using L-BFGS-B
      constrained to a lower limit of zero")

  o <- stats::optim(par = x, .fullpat,
                    method = solver, lower = 0, pure_patterns = lib$xrd,
                    sample_pattern = smpl[, 2], obj = obj)

  }

  x <- o$par

  }

  }

  #Calculate mineral concentrations so that I can throw away any amorphous
  #phases below detection limit

  min_concs <- .qminerals(x = x, xrd_lib = lib)

  df <- min_concs[[1]]
  dfs <- min_concs[[2]]

  #Remove amorphous phases
  remove_amorphous_out <- .remove_amorphous(x = x,
                                            amorphous = amorphous,
                                            amorphous_lod = amorphous_lod,
                                            df = df,
                                            lib = lib,
                                            solver = solver,
                                            smpl = smpl,
                                            obj = obj)

  x <- remove_amorphous_out[[1]]
  lib <- remove_amorphous_out[[2]]


  #Remove negative parameters again because some can creep in late-on
  remove_neg_out <- .remove_neg(x = x, lib = lib, smpl = smpl,
                                  solver = solver, obj = obj)

  x <- remove_neg_out[[1]]
  lib <- remove_neg_out[[2]]

  #compute fitted pattern and residuals
  fitted_pattern <- apply(sweep(as.matrix(lib$xrd), 2, x, "*"), 1, sum)

  resid_x <- smpl[, 2] - fitted_pattern

  #compute grouped phase concentrations
  cat("\n-Computing phase concentrations")
  min_concs <- .qminerals(x = x, xrd_lib = lib)

  #Extract mineral concentrations (df) and summarised mineral concentrations (dfs)
  df <- min_concs[[1]]
  dfs <- min_concs[[2]]

  #Rwp
  R_fit <- sqrt(sum((1/smpl[,2]) * ((smpl[,2] - fitted_pattern)^2)) / sum((1/smpl[,2]) * (smpl[,2]^2)))

  #Extract the xrd data
  xrd <- data.frame(lib$xrd)

  #Scale them by the optimised weightings
  for (i in 1:ncol(xrd)) {
    xrd[,i] <- xrd[,i] * x[i]
  }

  #If only 1 pattern is used in the fit, then rename it
  if (ncol(xrd) == 1) {
    names(xrd) <- df$phase_id[1]
  }


  #Define a list that becomes the function output
  out <- list("tth" = smpl[,1],
              "fitted" = fitted_pattern,
              "measured" = smpl[,2],
              "residuals" = resid_x,
              "phases" = df,
              "phases_summary" = dfs,
              "rwp" = R_fit,
              "weighted_pure_patterns" = xrd,
              "coefficients" = x)

  #Define the class
  class(out) <- "powdRafps"
  cat("\n***Automated full pattern summation complete***\n")

  return(out)

}



