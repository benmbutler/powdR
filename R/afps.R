#' Automated full pattern summation
#'
#' \code{afps} returns estimates of phase concentrations using automated full pattern
#' summation of X-ray powder diffraction data. For more details see \code{?afps.powdRlib}.
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
#'                  std = "QUA.1",
#'                  amorphous = "ORG",
#'                  align = 0.2)
#'
#' afps_lime <- afps(lib = minerals,
#'                 smpl = soils$limestone,
#'                 std = "QUA.1",
#'                 amorphous = "ORG",
#'                 align = 0.2)
#'
#' afps_granite <- afps(lib = minerals,
#'                    smpl = soils$granite,
#'                    std = "QUA.1",
#'                    amorphous = "ORG",
#'                    align = 0.2)
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
#' summation of X-ray powder diffraction data.
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
#' "CG")}. Default = \code{"BFGS"}.
#' @param obj The objective function to minimise. One of \code{c("Delta", "R", "Rwp")}.
#' Default = \code{"Rwp"}. See Chipera and Bish (2002) and page 247 of Bish and Post (1989)
#' for definitions of these functions.
#' @param std The phase ID (e.g. "QUA.1") to be used as internal
#' standard. Must match an ID provided in the \code{phases} parameter.
#' @param amorphous A character string of any phase id's that should be treated as amorphous. Each must
#' match a phase_id in the phases table of `lib`.
#' @param tth_align A vector defining the minimum and maximum 2theta values to be used during
#' alignment. If not defined, then the full range is used.
#' @param align The maximum shift that is allowed during initial 2theta
#' alignment (degrees). Default = 0.1.
#' @param shift The maximum shift (degrees 2theta) that is allowed during the grid search phases selected
#' from the non-negative least squares. Default = 0.05).
#' @param tth_fps A vector defining the minimum and maximum 2theta values to be used during
#' automated full pattern summation. If not defined, then the full range is used.
#' @param background a list of parameters used to fit a background to the data. Takes the form
#' \code{list(lambda, hwi, it , int)}. If missing, the default used is
#' \code{list(lambda = 0.5, hwi = 25, it = 50, int = round(nrow(smpl)/4, 0)).} To tune these parameters
#' please see the \code{function}, or the background fitting tab of the \code{run_powdR} shiny app.
#' @param lld Optional parameter used to tune the lower limit of dection computation.
#' Must be greater than 0. Default = 0.3.
#' @param amorphous_lld Optional parameter used to exclude amorphous phases if they are below this
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
#'                  std = "QUA.1",
#'                  amorphous = "ORG",
#'                  align = 0.2)
#'
#' afps_lime <- afps(lib = minerals,
#'                 smpl = soils$limestone,
#'                 std = "QUA.1",
#'                 amorphous = "ORG",
#'                 align = 0.2)
#'
#' afps_granite <- afps(lib = minerals,
#'                    smpl = soils$granite,
#'                    std = "QUA.1",
#'                    amorphous = "ORG",
#'                    align = 0.2)
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
afps.powdRlib <- function(lib, smpl, solver, obj, std, amorphous,
                         tth_align, align, shift, tth_fps, background, lld,
                         amorphous_lld, ...) {

  if(missing(amorphous)) {
    cat("\n-No amorphous phases identified")
    amorphous = c()
  }

  if(missing(tth_align)) {
    cat("\n-Using maximum tth range")
    tth_align = c(min(smpl[[1]]), max(smpl[[1]]))
  }

  if(missing(align)) {
    cat("\n-Using default alignment of 0.1")
    align = 0.1
  }

  if(missing(shift)) {
    cat("\n-Using default shift of 0.05")
    shift = 0.05
  }

  if(missing(solver)) {
    cat("\n-Using default solver of 'BFGS'")
    solver = "BFGS"
  }

  if(missing(obj) & solver %in% c("Nelder-Mead", "BFGS", "CG")) {
    cat("\n-Using default objective function of 'Rwp'")
    obj = "Rwp"
  }

  if(missing(lld)) {
    cat("\n-Using default lld of 0.3")
    lld = 0.3
  }

  if(missing(amorphous_lld)) {
    cat("\n-Using default amorphous_lld of 0")
    amorphous_lld = 0
  }

  if(missing(background)) {

    background <- list(lambda = 0.5,
                       hwi = 25,
                       it = 50,
                       int = round(nrow(smpl)/4, 0))

  }

  if(!identical(names(background), c("lambda", "hwi", "it", "int"))) {

    stop("Please provide the correct coefficient names in the background argument (lambda, hwi, it, int)")

  }

  #Ensure that the align is greater than 0.
  if (align <= 0) {
    stop("The align argument must be greater than 0")
  }

  #Create a warning message if the shift is greater than 0.5, since this can confuse the optimisation
  if (align > 0.5) {
    warning("Be cautious of large 2theta shifts. These can cause issues in sample alignment.")
  }

  #Make only "Nelder-Mead", "BFGS", or "CG" optional for the solver
  if (!solver %in% c("Nelder-Mead", "BFGS", "CG")) {
    stop("The solver argument must be one of 'BFGS', 'Nelder Mead' or 'CG'")
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

  #Extract the standard as an xy dataframe
  xrd_standard <- data.frame(tth = lib$tth, counts = lib$xrd[, which(lib$phases$phase_id == std)])

  #align the data
  cat("\n-Aligning sample to the internal standard")
  smpl <- .xrd_align(smpl = smpl, xrd_standard, xmin = tth_align[1],
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

  #If tth_fps isn't defined, then define it here
  if(missing(tth_fps)) {
    tth_fps <- c(min(smpl_tth), max(smpl_tth))
  }

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


  #------------------------------------------------------------------------------
  #Extracting amorphous phases
  #------------------------------------------------------------------------------

  #Extract amorphous phases from the harmonised list to exclude them from analysis
  #also exclude background parameters

  amorphous_index <- which(lib$phases$phase_id %in% amorphous)

  #If amorphous is present in the argument, then extract the patterns to be used

  if(!missing(amorphous) & length(amorphous_index) > 0) {
    amorphous_counts <- lib$xrd[amorphous]
    amorphous_tth <- lib$tth
    amorphous_xrd <- data.frame("tth" = amorphous_tth, amorphous_counts)
    lib$xrd <- lib$xrd[, -amorphous_index]
  }

  #if(length(amorphous_index) > 0) {
  #  lib$xrd <- lib$xrd[, -amorphous_index]
  #}

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

    cat("\n-Optimising...")
    o <- stats::optim(par = x, .fullpat,
                      method = solver, pure_patterns = lib$xrd,
                      sample_pattern = smpl[, 2], obj = obj)

    x <- o$par


  #--------------------------------------------
  #Grid-search shifting
  #--------------------------------------------

  #Alignment and then another optimisation ONLY if the shift parameter
  #is included

  if(shift > 0) {

   fpf_aligned <- .shift(smpl = smpl,
                         lib = lib,
                         max_shift = shift,
                         x = x)

    smpl <- fpf_aligned[["smpl"]]
    lib$xrd <- data.frame(fpf_aligned[["lib"]])
    lib$tth <- smpl[,1]

    #Re-optimise after shifts
    #cat("\n-Reoptimising after shifting data")
    #o <- stats::optim(par = x, .fullpat,
    #                  method = solver, pure_patterns = lib$xrd,
    #                  sample_pattern = smpl[, 2], obj = obj)
    #x <- o$par

    #return(list("x" = x, "smpl" = smpl, "lib" = lib))
  }

  #-----------------------------------------------------------
  #Re-adding amorphous phases and re-optimise
  #-----------------------------------------------------------

    x <- o$par

    #Add the amorphous phases

    if(length(amorphous_index) > 0) {
      #Add the amorphous phase to the library
      amorphous_counts2 <- list()

      for (i in 1:ncol(amorphous_counts)) {
        amorphous_counts2[[i]] <- stats::approx(x = amorphous_tth, y = amorphous_counts[[i]],
                                                method = "linear", xout = lib$tth)[[2]]
        names(amorphous_counts2)[i] <- names(amorphous_counts)[i]
      }
      amorphous_counts2 <- data.frame(amorphous_counts2)

      lib$xrd <- data.frame(lib$xrd, amorphous_counts2)

      #Add an initial parameter to the library for the optimisation
      xa <- rep(0, ncol(amorphous_counts))
      names(xa) <- names(amorphous_counts)

      x <- c(x, xa)

    }

  if(shift > 0 | length(amorphous_index) > 0) {

    cat("\n-Reoptimising after shifting data/adding amorphous phases")

    o <- stats::optim(par = x, .fullpat,
                      method = solver, pure_patterns = lib$xrd,
                      sample_pattern = smpl[, 2], obj = obj)

    x <- o$par

  }


  #--------------------------------------------------------------------------------------------
  #Remove negative parameters
  #--------------------------------------------------------------------------------------------

  remove_neg_out <- .remove_neg(x = x, lib = lib, smpl = smpl,
                                solver = solver, obj = obj)

  x <- remove_neg_out[[1]]
  lib <- remove_neg_out[[2]]


  #Now that some negative parameters have been removed, the detection limits
  #of the remaining phases are estimated.

  # Removing phases based on detection limits

  #Calculate the lld and remove any phases below it if lld > 0

  if (lld > 0) {

  cat("\n-Calculating detection limits")
  xrd_detectable <- .lld(x = x, smpl = smpl, lib = lib,
                        std = std, amorphous = amorphous,
                        background = background, lld = lld)
  cat("\n-Removing phases below detection limit")

  x <- xrd_detectable[["x"]]
  lib$xrd <- xrd_detectable[["lib"]]

  cat("\n-Reoptimising")
  o <- stats::optim(par = x, .fullpat,
                    method = solver, pure_patterns = lib$xrd,
                    sample_pattern = smpl[, 2], obj = obj)
  x <- o$par

  }

  #Calculate mineral concentrations so that I can throw away any amorphous
  #phases below detection limit

  min_concs <- .qminerals(x = x, xrd_lib = lib)

  df <- min_concs[[1]]
  dfs <- min_concs[[2]]

  #Remove amorphous phases
  remove_amorphous_out <- .remove_amorphous(x = x,
                                            amorphous = amorphous,
                                            amorphous_lld = amorphous_lld,
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
              "background" = xrd_detectable$background,
              "residuals" = resid_x,
              "phases" = df,
              "phases_summary" = dfs,
              "rwp" = R_fit,
              "weighted_pure_patterns" = xrd,
              "coefficients" = x)

  #Define the class
  class(out) <- "powdRafps"
  cat("\n-Automated full pattern summation complete")

  return(out)

}






# afps2 <- function(lib, ...) {
#   UseMethod("afps2")
# }
#
#
# afps2.powdRlib <- function(smpl, lib, tth, std, amorphous, align,
#                            solver, obj,  shift, background, lld, amorphous_lld, ...) {
#
#   #Create defaults for values that aren't specified.
#
#   if(missing(background)) {
#
#     background <- list(lambda = 0.5,
#                        hwi = 25,
#                        it = 50,
#                        int = round(nrow(smpl)/4, 0))
#
#   }
#
#   if(missing(align)) {
#     align = 0.1
#   }
#
#   if(missing(solver)) {
#     solver = "BFGS"
#   }
#
#   if(missing(obj)) {
#     obj = "Rwp"
#   }
#
#   if(missing(shift)) {
#     shift = 0.05
#   }
#
#   if(missing(lld)) {
#     lld = 0.3
#   }
#
#   if (lld < 0 | lld > 2) {
#     stop("The lld argument must be between 0 and 2")
#   }
#
#   if(!missing(amorphous)) {
#
#     if(missing(amorphous_lld)) {
#       amorphous_lld <- 0
#     }
#
#     if (amorphous_lld < 0 | amorphous_lld > 100) {
#       stop("The amorphous_lld argument must be between 0 and 100")
#     }
#
#   }
#
#   #Ensure that the align is greater than 0.
#   if (align <= 0) {
#     stop("The align argument must be greater than 0")
#   }
#
#   #Create a warning message if the shift is greater than 1, since this confuse the optimisation
#   if (align > 0.5) {
#     warning("Be cautious of large 2theta shifts. These can cause issues in sample alignment.")
#   }
#
#   if (shift > 0.1) {
#     warning("To speed computation and avoid erroneous alignments, the shift argument should be less than 0.1.")
#   }
#
#   #Make only "Nelder-Mead", "BFGS", or "CG" optional for the solver
#   if (!solver %in% c("Nelder-Mead", "BFGS", "CG")) {
#     stop("The solver argument must be one of 'BFGS', 'Nelder Mead' or 'CG'")
#   }
#
#   #Make sure that the mineral identified as the internal standard is contained within the reference library
#   if (!std %in% lib$phases$phase_id) {
#     stop("The phase you have specified as the internal standard is not in the reference library")
#   }
#
#   #Make sure that the defined tth arguments are within the range of the samples and reference library.
#   if (tth[1] < min(smpl[,1])) {
#     stop("tth[1] must exceed the minimum 2theta value of the sample")
#   }
#   if (tth[2] > max(smpl[,1])) {
#     stop("tth[2] must be lower than the maximum 2theta value of the sample")
#   }
#
#   if (tth[1] < min(lib$tth)) {
#     stop("tth[1] must be within the 2theta range of the reference library")
#   }
#   if (tth[2] > max(lib$tth)) {
#     stop("tth[2] must be within the 2theta range of the reference library")
#   }
#
#   ##############
#   #INITIAL SAMPLE ALIGNMENT USING THE xrd.align function
#   ##############
#
#   xrd_standard <- data.frame(tth = lib$tth,
#                              counts = lib$xrd[, which(lib$phases$phase_id == std)])
#
#   #align the data
#   cat("\n-Aligning data")
#   smpl <- .xrd_align(smpl = smpl, xrd_standard, xmin = tth[1]+ (align*2),
#                      xmax = tth[2]-(align*2), xshift = align)
#
#   if (sqrt(smpl[[1]]^2) == align) {
#     message("The optimised shift used in alignment is equal to the maximum shift defined
#             in the function call. We advise visual inspection of this alignment.")
#   }
#
#   smpl <- smpl[[2]]
#   #Define a 2TH scale to harmonise all data to
#   smpl_tth <- smpl[, 1]
#
#   xrd_ref_names <- lib$phases$phase_id
#
#   #Ensure that sample in the reference library are on the same scale as the sample
#
#   cat("\n-Converting library to aligned 2theta scale")
#   lib$xrd <- data.frame(lapply(names(lib$xrd),
#                                function(n) stats::approx(x = lib$tth,
#                                                          y = unname(unlist(lib$xrd[n])),
#                                                          xout = smpl_tth)[[2]]))
#
#   names(lib$xrd) <- xrd_ref_names
#
#   #Replace the library tth with that of the sample
#
#   lib$tth <- smpl_tth
#
#   #get the number of patterns in the library
#   lib_length <- nrow(lib$phases)
#
#   #### decrease 2TH scale to the range defined in the function call
#   smpl <- smpl[which(smpl[[1]] >= tth[1] & smpl[[1]] <= tth[2]), ]
#
#   #Subset the XRD dataframe to
#   lib$xrd <- lib$xrd[which(lib$tth >= tth[1] & lib$tth <= tth[2]), ]
#
#   #Replace the tth in the library with the shortened one
#   lib$tth <- smpl[, 1]
#
#   #Extract amorphous phases from the harmonised list to exclude them from analysis
#   #also exclude background parameters
#
#   amorphous_index <- which(lib$phases$phase_id %in% amorphous)
#
#   #If amorphous is present in the argument, then extract the patterns to be used
#
#   if(!missing(amorphous)) {
#     cat("\n-Extracting amorphous phases")
#     amorphous_counts <- lib$xrd[amorphous]
#     amorphous_tth <- lib$tth
#     amorphous_xrd <- data.frame("tth" = amorphous_tth, amorphous_counts)
#   }
#
#   if(length(amorphous_index) > 0) {
#     lib$xrd <- lib$xrd[, -amorphous_index]
#   }
#
#   #This is where the new nnls function needs to come in
#   cat("\n-Applying NNLS")
#   autoid <- .xrd_nnls(xrd.lib = lib, xrd.sample = smpl[, 2])
#
#   x <- autoid[["x"]]
#   lib$xrd <- autoid[["xrd.lib"]]
#
#
#   #OPTIMISATION
#
#   #optimise using objective function rather than qr.solve
#   cat("\n-Optimising...")
#   o <- stats::optim(par = x, .fullpat,
#                     method = solver, pure_patterns = lib$xrd,
#                     sample_pattern = smpl[, 2], obj = obj)
#
#   x <- o$par
#
#   #Alignment and then another optimisation ONLY if the shift parameter
#   #is included
#
#   if(shift > 0) {
#     fpf_aligned <- .shift(smpl = smpl,
#                           lib = lib,
#                           max_shift = shift,
#                           x = x)
#
#     smpl <- fpf_aligned[["smpl"]]
#     lib$xrd <- data.frame(fpf_aligned[["lib"]])
#     lib$tth <- smpl[,1]
#
#     #Re-optimise after shifts
#     #cat("\n-Reoptimising")
#     #o <- stats::optim(par = x, .fullpat,
#     #                  method = solver, pure_patterns = lib$xrd,
#     #                  sample_pattern = smpl[, 2], obj = obj)
#
#     #x <- o$par
#   }
#
#
#   #Add the amorphous phases
#
#   if(!missing(amorphous)) {
#     cat("\n-Adding amorphous phases")
#     #Add the amorphous phase to the library
#     amorphous_counts2 <- list()
#
#     for (i in 1:ncol(amorphous_counts)) {
#       amorphous_counts2[[i]] <- stats::approx(x = amorphous_tth, y = amorphous_counts[[i]],
#                                               method = "linear", xout = lib$tth)[[2]]
#       names(amorphous_counts2)[i] <- names(amorphous_counts)[i]
#     }
#     amorphous_counts2 <- data.frame(amorphous_counts2)
#
#     lib$xrd <- data.frame(lib$xrd, amorphous_counts2)
#
#     #Add an initial parameter to the library for the optimisation
#     xa <- rep(0, ncol(amorphous_counts))
#     names(xa) <- names(amorphous_counts)
#
#     x <- c(x, xa)
#
#     #cat("\n-Re-optimising...")
#     #o <- stats::optim(par = x, .fullpat,
#     #                  method = solver, pure_patterns = lib$xrd,
#     #                  sample_pattern = smpl[, 2], obj = obj)
#   }
#
#
#   if (shift > 0 | !missing(amorphous)) {
#
#     cat("\n-Re-optimising...")
#     o <- stats::optim(par = x, .fullpat,
#                       method = solver, pure_patterns = lib$xrd,
#                       sample_pattern = smpl[, 2], obj = obj)
#     x <- o$par
#   }
#
#   #Removing negative parameters
#
#   #Sometimes amorphous phases can end up removed here when they should actually be included
#
#   if (!missing(amorphous)) {
#
#     negpar <- min(x[-which(names(x) %in% amorphous)])
#
#     while (negpar < 0) {
#       cat("\n-Removing negative coefficients and reoptimising...")
#       #use the most recently optimised coefficients
#       x <- o$par
#       #check for any negative parameters, but ensure that amorphous phases are retained
#       remove_index <- which(x < 0 & !names(x) %in% amorphous)
#
#       #remove the column from the library that contains the identified data
#       if (length(remove_index) > 0) {
#         lib$xrd <- lib$xrd[, -remove_index]
#         x <- x[-remove_index]
#       }
#
#       o <- stats::optim(par = x, .fullpat,
#                         method = solver, pure_patterns = lib$xrd,
#                         sample_pattern = smpl[, 2], obj = obj)
#       x <- o$par
#       #identify whether any parameters are negative for the next iteration
#       negpar <- min(x[-which(names(x) %in% amorphous)])
#     }
#
#   } else {
#
#     negpar <- min(x)
#
#     while (negpar < 0) {
#       cat("\n-Removing negative coefficients and reoptimising...")
#       #use the most recently optimised coefficients
#       #x <- o$par
#       #check for any negative parameters, but ensure that amorphous phases are retained
#       remove_index <- which(x < 0)
#
#       #remove the column from the library that contains the identified data
#       if (length(remove_index) > 0) {
#         lib$xrd <- lib$xrd[, -remove_index]
#         x <- x[-remove_index]
#       }
#
#       o <- stats::optim(par = x, .fullpat,
#                         method = solver, pure_patterns = lib$xrd,
#                         sample_pattern = smpl[, 2], obj = obj)
#       x <- o$par
#       #identify whether any parameters are negative for the next iteration
#       negpar <- min(x)
#     }
#
#   }
#
#   #Now that some negative parameters have been removed, the detection limits
#   #of the remaining phases are estimated.
#
#   # Removing phases based on detection limits
#
#   #Calculate the lld and remove any phases below it
#   #xrd_detectable <- xrd.lld(x = x, xrd.sample = smpl, xrd.lib = lib,
#   #                         int_std = std, lld = lld)
#
#   if (lld > 0) {
#
#     cat("\n-Calculating detection limits")
#     xrd_detectable <- .lld2(x = x, smpl = smpl, lib = lib, std = std,
#                             lld = lld, background = background)
#     cat("\n-Removing phases below detection limit")
#
#     x <- xrd_detectable[["x"]]
#     lib$xrd <- xrd_detectable[["lib"]]
#
#     cat("\n-Reoptimising")
#     o <- stats::optim(par = x, .fullpat,
#                       method = solver, pure_patterns = lib$xrd,
#                       sample_pattern = smpl[, 2], obj = obj)
#     x <- o$par
#
#    }
#
#   #Omit the other phases but make sure amorphous phases are retained in specified
#
#   # if (!missing(amorphous)) {
#   #
#   #   remove_index <- which(names(xrd_detectable[["x"]]) %in% amorphous)
#   #
#   #   if (length(remove_index) > 0) {
#   #     x <- c(xrd_detectable[["x"]][-remove_index],
#   #            x[which(names(x) %in% amorphous)])
#   #
#   #     lib$xrd <- data.frame(xrd_detectable[["xrd.lib"]]
#   #                           [, -remove_index],
#   #                           lib$xrd[, which(names(lib$xrd) %in% amorphous)])
#   #
#   #     names(lib$xrd) <- names(x)
#   #
#   #   } else {
#   #     x <- c(xrd_detectable[["x"]],
#   #            x[which(names(x) %in% amorphous)])
#   #
#   #     lib$xrd <- data.frame(xrd_detectable[["xrd.lib"]],
#   #                           lib$xrd[, which(names(lib$xrd) %in% amorphous)])
#   #
#   #     names(lib$xrd) <- names(x)
#   #   }
#   #
#   # } else {
#   #
#   #   x <- xrd_detectable[["x"]]
#   #   lib$xrd <- xrd_detectable[["xrd.lib"]]
#   #
#   # }
#
#
#   #Re-optimise now that phases below detection limit have been removed
#   # o <- stats::optim(par = x, fullpat,
#   #                   method = solver, control = list(), pure.patterns = lib$xrd,
#   #                   sample.pattern = smpl[,2], obj = obj)
#
#   fitted_pattern <- apply(sweep(as.matrix(lib$xrd), 2, o$par, "*"), 1, sum)
#
#   resid_x <- smpl[, 2] - fitted_pattern
#
#   x <- o$par
#
#
#   #Calculate mineral concentrations after amorphous phase was added
#
#   min_concs <- .qminerals(x = x, xrd_lib = lib)
#
#   #Extract mineral concentrations (df) and summarised mineral concentrations (dfs)
#   df <- min_concs[[1]]
#   dfs <- min_concs[[2]]
#
#
#   #######
#   #Remove amorphous phases
#   remove_amorphous_out <- .remove_amorphous(x = x,
#                                             amorphous = amorphous,
#                                             amorphous_lld = amorphous_lld,
#                                             df = df,
#                                             lib = lib,
#                                             solver = solver,
#                                             smpl = smpl,
#                                             obj = obj)
#
#   x <- remove_amorphous_out[[1]]
#   lib <- remove_amorphous_out[[2]]
#
#
#   #Remove negative parameters again because some can creep in late-on
#   remove_neg_out <- .remove_neg(x = x, lib = lib, smpl = smpl,
#                                 solver = solver, obj = obj)
#
#   x <- remove_neg_out[[1]]
#   lib <- remove_neg_out[[2]]
#
#   #Recompute mineral concentrations
#   min_concs <- .qminerals(x = x, xrd_lib = lib)
#
#   #Extract mineral concentrations (df) and summarised mineral concentrations (dfs)
#   df <- min_concs[[1]]
#   dfs <- min_concs[[2]]
#
#   #### Compute the R statistic. This could be used to identify samples
#   # that require manual interpretation
#
#   R_fit <- sqrt(sum((1/smpl[,2]) * ((smpl[,2] - fitted_pattern)^2)) / sum((1/smpl[,2]) * (smpl[,2]^2)))
#
#   xrd <- data.frame(lib$xrd)
#
#   for (i in 1:ncol(xrd)) {
#     xrd[,i] <- xrd[,i] * x[i]
#   }
#
#   #If only 1 pattern is used in the fit, then rename it
#   if (ncol(xrd) == 1) {
#     names(xrd) <- df$phase_id[1]
#   }
#
#   #Define a list that becomes the function output
#   out <- list("tth" = smpl[,1],
#               "fitted" = fitted_pattern,
#               "measured" = smpl[,2],
#               "background" = xrd_detectable$background,
#               "residuals" = resid_x,
#               "phases" = df,
#               "phases_summary" = dfs,
#               "rwp" = R_fit,
#               "weighted_pure_patterns" = xrd,
#               "coefficients" = x)
#
#   #Define the class
#   class(out) <- "powdRafps"
#   cat("\n-Automated full pattern summation complete")
#
#   return(out)
#
#   }
#




