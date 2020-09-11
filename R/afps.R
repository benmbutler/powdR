#' Automated full pattern summation
#'
#' \code{afps} returns estimates of phase concentrations using automated full pattern
#' summation of X-ray powder diffraction data. It is designed for high-throughput cases
#' involving mineral quantification from large reference libraries. For more details
#' see \code{?afps.powdRlib}.
#'
#' Applies automated full pattern summation to an XRPD
#' measurement to quantify phase concentrations. Requires a \code{powdRlib} library of
#' reference patterns with reference intensity ratios in order to derive
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
#' \item{phases_grouped}{the phases dataframe grouped by phase_name and summed}
#' \item{rwp}{the Rwp of the fitted vs measured pattern}
#' \item{weighted_pure_patterns}{a dataframe of reference patterns used to produce the fitted pattern.
#' All patterns have been weighted according to the coefficients used in the fit}
#' \item{coefficients}{a named vector of coefficients used to produce the fitted pattern}
#' \item{inputs}{a list of input arguments used in the function call}
#'
#' @examples
#' #Load the minerals library
#' data(minerals)
#'
#' # Load the soils data
#' data(soils)
#'
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
#'
#' #Automated quantification using the rockjock library
#'
#' data(rockjock)
#' data(rockjock_mixtures)
#'
#' #This takes a few minutes to run
#' rockjock_a1 <- afps(lib = rockjock,
#'                     smpl = rockjock_mixtures$Mix1,
#'                     std = "CORUNDUM",
#'                     align = 0.3,
#'                     lod = 1)
#'
#' #Quantifying the same sample but defining the internal standard
#' #concentration (also takes a few minutes to run):
#' rockjock_a1s <- afps(lib = rockjock,
#'                      smpl = rockjock_mixtures$Mix1,
#'                      std = "CORUNDUM",
#'                      std_conc = 20,
#'                      align = 0.3,
#'                      lod = 1)
#'
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
#' Eberl, D.D., 2003. User's guide to RockJock - A program for determining quantitative mineralogy from
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
#' patterns with reference intensity ratios in order to derive mineral
#' concentrations.
#'
#' @param lib A \code{powdRlib} object representing the reference library. Created using the
#' \code{powdRlib} constructor function.
#' @param smpl A data frame. First column is 2theta, second column is counts
#' @param harmonise logical parameter defining whether to harmonise the \code{lib} and \code{smpl}.
#' Default = \code{TRUE}. Harmonises to the intersecting 2theta range at the coarsest resolution
#' available using natural splines.
#' @param solver The optimisation routine to be used. One of \code{c("BFGS", "Nelder-Mead",
#' or "CG")}. Default = \code{"BFGS"}.
#' @param obj The objective function to minimise. One of \code{c("Delta", "R", "Rwp")}.
#' Default = \code{"Rwp"}. See Chipera and Bish (2002) and page 247 of Bish and Post (1989)
#' for definitions of these functions.
#' @param refs A character string of reference pattern ID's or names from the specified library.
#' The ID's or names supplied must be present within the \code{lib$phases$phase_id} or
#' \code{lib$phases$phase_name} columns. If missing from the function call then all phases in
#' the reference library will be used.
#' @param std The phase ID (e.g. "QUA.1") to be used as internal
#' standard. Must match an ID provided in the \code{refs} parameter.
#' @param force An optional string of phase ID's or names specifying which phases should be forced to
#' remain throughout the automated full pattern summation. The ID's or names supplied must be present
#' within the \code{lib$phases$phase_id} or \code{lib$phases$phase_name} columns.
#' @param std_conc The concentration of the internal standard (if known) in weight percent. If
#' unknown then use \code{std_conc = NA}, in which case it will be assumed that all phases sum
#' to 100 percent (default).
#' @param normalise A logical parameter to be used when the \code{std_conc} argument is defined. When
#' \code{normalise = TRUE} the internal standard concentration is removed and the remaining phase
#' concentrations normalised to sum to 100 percent.
#' @param tth_align A vector defining the minimum and maximum 2theta values to be used during
#' alignment (e.g. \code{c(5,65)}). If not defined, then the full range is used.
#' @param align The maximum shift that is allowed during initial 2theta
#' alignment (degrees). Default = 0.1.
#' @param manual_align A logical operator denoting whether to optimise the alignment within the
#' negative/position 2theta range defined in the \code{align} argument, or to use the specified
#' value of the \code{align} argument for alignment of the sample to the standards. Default
#' = \code{FALSE}, i.e. alignment is optimised.
#' @param shift A single numeric value denoting the maximum (positive or negative) shift,
#' in degrees 2theta, that is allowed during the shifting of selected phases. Default = 0.
#' @param tth_fps A vector defining the minimum and maximum 2theta values to be used during
#' automated full pattern summation (e.g. \code{c(5,65)}). If not defined, then the full range is used.
#' @param lod Optional parameter used to define the limit of detection (in weight percent) of the internal standard
#' (i.e. the phase provided in the \code{std} argument). The \code{lod} value is used to estimate the lod of other
#' phases during the fitting process and hence remove reference patterns that are considered below detection limit.
#' Default = 0.1. If \code{lod = 0} then limits of detection are not computed.
#' @param amorphous A character string of any phase ID's that should be treated as amorphous. These must
#' match phases present in \code{lib$phases$phase_id}.
#' @param amorphous_lod Optional parameter used to exclude amorphous phases if they are below this
#' specified limit (percent). Must be between 0 and 100. Default = 0.
#' @param ... other arguments
#'
#' @return a list with components:
#' \item{tth}{a vector of the 2theta scale of the fitted data}
#' \item{fitted}{a vector of the fitted XRPD pattern}
#' \item{measured}{a vector of the original XRPD measurement (aligned and harmonised)}
#' \item{residuals}{a vector of the residuals (fitted vs measured)}
#' \item{phases}{a dataframe of the phases used to produce the fitted pattern and their concentrations}
#' \item{phases_grouped}{the phases dataframe grouped by phase_name and concentrations summed}
#' \item{rwp}{the Rwp of the fitted vs measured pattern}
#' \item{weighted_pure_patterns}{a dataframe of reference patterns used to produce the fitted pattern.
#' All patterns have been weighted according to the coefficients used in the fit}
#' \item{coefficients}{a named vector of coefficients used to produce the fitted pattern}
#' \item{inputs}{a list of input arguments used in the function call}
#'
#' @examples
#' #Load the minerals library
#' data(minerals)
#'
#' # Load the soils data
#' data(soils)
#'
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
#'
#' #Automated quantification using the rockjock library
#'
#' data(rockjock)
#' data(rockjock_mixtures)
#'
#' #This takes a few minutes to run
#' rockjock_a1 <- afps(lib = rockjock,
#'                     smpl = rockjock_mixtures$Mix1,
#'                     std = "CORUNDUM",
#'                     align = 0.3,
#'                     lod = 1)
#'
#' #Quantifying the same sample but defining the internal standard
#' #concentration (also takes a few minutes to run):
#' rockjock_a1s <- afps(lib = rockjock,
#'                      smpl = rockjock_mixtures$Mix1,
#'                      std = "CORUNDUM",
#'                      std_conc = 20,
#'                      align = 0.3,
#'                      lod = 1)
#'
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
#' Eberl, D.D., 2003. User's guide to RockJock - A program for determining quantitative mineralogy from
#' powder X-ray diffraction data. Boulder, CA.
#' @export
afps.powdRlib <- function(lib, smpl, harmonise, solver, obj, refs, std, force, std_conc, normalise,
                         tth_align, align, manual_align, shift,
                         tth_fps, lod, amorphous, amorphous_lod, ...) {

#Make sure there aren't any negative counts
  if (min(smpl[[2]]) < 0) {

    stop("Please make sure that there are no negative count intensities in the sample data",
         call. = FALSE)

  }

#Define force if missing
  if (missing(force)) {

    force <- c()

  }

#Extract phase ID's from force
  if (length(force) > 0) {

    wrong_force <- which(!force %in% lib$phases$phase_id & !force %in% lib$phases$phase_name)

    if (length(wrong_force) > 0) {

      stop(paste(c("\nThe following reference patterns specified in the force argument are not in the library:\n",
                   paste(c(force[wrong_force]), collapse = ", "))),
           call. = FALSE)

    }

    force <- lib$phases$phase_id[which(lib$phases$phase_id %in% force | lib$phases$phase_name %in% force)]

  }

#Set harmonise to true if missing
  if (missing(harmonise)) {

    harmonise <- TRUE

  }

#Make sure harmonise is logical is defined
  if (!is.logical(harmonise)) {

    stop("The harmonise argument must be logical",
         call. = FALSE)

  }

#Make sure harmonise is used if the sample and library are not identical
  if (harmonise == FALSE & !identical(lib$tth, smpl[[1]])) {

    stop("The 2theta scale of the library and sample do not match. Try
         setting the harmonise argument to TRUE",
         call. = FALSE)

  }

#If amorphous is misssing then set it to an empty vector
  if(missing(amorphous)) {

    amorphous = c()
  }

#Set std_conc to NA if missing
  if (missing(std_conc)) {

    std_conc <- NA

  }

#If std_conc is either NA or numeric then stop
  if (!(is.numeric(std_conc) | is.na(std_conc))) {

    stop("\n-The std_conc argument must either be NA or a numeric value greater than 0 and less than 100.",
         call. = FALSE)

  }

#Make sure the std is defined if std_conc is numeric
  if (is.numeric(std_conc)) {

    if (missing(std)) {

      stop("\n-Please define the std argument",
           call. = FALSE)

    }

#Make sure the std_conc is between 0 and 100
    if(std_conc <= 0 | std_conc >= 100) {

      stop("\n-The std_conc argument must either be NA or a numeric value greater than 0 and less than 100.",
           call. = FALSE)

    }

  }

#If tth_align is missing then use the maximum tth range of the sample
  if(missing(tth_align)) {

    tth_align = c(min(smpl[[1]]), max(smpl[[1]]))
  }

#If align is missing then set it to default
  if(missing(align)) {

    align = 0.1
  }

#If manual_align is not defined then set it to FALSE
  if(missing(manual_align)) {

    manual_align <- FALSE

  }

#Ensure manual_align is logical
  if(!is.logical(manual_align)) {

    stop("The manual_align argument must be logical",
         call. = FALSE)

  }

#If solver is missing then set it to BFGS
  if(missing(solver)) {

    solver = "BFGS"
  }

#If obj is not defined then set to Rwp
  if(missing(obj)) {

    obj = "Rwp"

  }

#If shift is missing then set it to default
  if(missing(shift)) {

    shift = 0

  }

#If refs are not defined then use all of them
  if(missing(refs)) {

    cat("\n-Using all reference patterns in the library")
    refs = lib$phases$phase_id

  }

#If lod is missing then set it to a default of 0.1
  if(missing(lod)) {
    cat("\n-lod argument not defined. Setting to 0.1")
    lod = 0.1
  }

#If amorphous_lod is missing, set it to 0
  if(missing(amorphous_lod)) {

    amorphous_lod = 0

  }


#Ensure that the lod is greater than 0.
  if (lod < 0) {
    stop("The lod argument must be equal to or greater than 0",
         call. = FALSE)
  }

#Create a warning message if the shift is greater than 0.5, since this can confuse the optimisation
  if (align > 0.5 & manual_align == FALSE) {
    warning("Be cautious of large 2theta shifts. These can cause issues in sample alignment.",
            call. = FALSE)
  }

#Make only "Nelder-Mead", "BFGS", "CG" or "L-BFGS-B" optional for the solver
  if (!solver %in% c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B")) {
    stop("The solver argument must be one of 'BFGS', 'Nelder Mead' or 'CG'",
         call. = FALSE)
  }

  if (solver == "L-BFGS-B") {

    cat("\n-The 'L-BFGS-B' option for solver has deprecated.
        Using 'BFGS' instead.")

    solver <- "BFGS"

  }

#No need for a standard if these conditions are met
  if (is.na(std_conc)) {

  #If align is 0 and lod isn't being used then the standard can be set to 'none'
  if (manual_align == TRUE & lod == 0) {

    std <- "none"

  }

  if (align == 0 & lod == 0) {

    std <- "none"

  }

  }

  #Check that none of the refs are spelt wrong
  wrong_spellings <- which(!refs %in% lib$phases$phase_id & !refs %in% lib$phases$phase_name)

  if (length(wrong_spellings) > 0) {

    stop(paste(c("\nThe following reference patterns specified in the refs argument are not in the library:\n",
                 paste(c(refs[wrong_spellings]), collapse = ", "))),
         call. = FALSE)

  }

  #subset lib according to the refs and force vector vectors
  lib <- subset(lib, refs = c(refs, force), mode = "keep")

  #Make sure that the phase identified as the internal standard is contained within the reference library
  if (!std == "none" & !std %in% lib$phases$phase_id) {
    stop("The phase you have specified as the internal standard is not in the reference library",
         call. = FALSE)
  }

  #Make sure that the amorphous phases identified are contained within the reference library
  if (!length(amorphous) == length(which(amorphous %in% lib$phases$phase_id))) {
    stop("One or more of the phases you have specified as amorphous are not in the reference library",
         call. = FALSE)
  }

  #if only one phase is being used, make sure it's a dataframe and named correctly
  if (nrow(lib$phases) == 1) {
    lib$xrd <- data.frame("phase" = lib$xrd)
    names(lib$xrd) <- lib$phases$phase_id
  }

  #Harmonise libraries
  if (harmonise == TRUE & !identical(lib$tth, smpl[[1]])) {

    harmonised <- .harmoniser(lib = lib, smpl = smpl)

    smpl <- harmonised$smpl
    lib <- harmonised$lib

  }

#--------------------------------------------------------------------
#Alignment
#--------------------------------------------------------------------

  #align the data
  if (!align == 0) {
  cat("\n-Aligning sample to the internal standard")
  smpl <- .xrd_align(smpl = smpl,
                     standard = data.frame(tth = lib$tth,
                                           counts = lib$xrd[, which(lib$phases$phase_id == std)]),
                     xmin = tth_align[1],
                     xmax = tth_align[2], xshift = align,
                     manual = manual_align)

  #If the alignment is close to the limit, provide a warning
  if (sqrt(smpl[[1]]^2) > (align*0.95) & manual_align == FALSE) {
    warning("The optimised shift used in alignment is equal to the maximum shift defined
            in the function call. We advise visual inspection of this alignment.",
            call. = FALSE)
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

  #Ensure that samples in the reference library are on the same scale as the sample
  cat("\n-Interpolating library to same 2theta scale as aligned sample")
  lib$xrd <- data.frame(lapply(lib$xrd,
                               function(n) stats::spline(x = lib$tth,
                                                         y = n,
                                                         method = "natural",
                                                         xout = smpl_tth)[[2]]))

  #Replace the library tth with that of the sample
  lib$tth <- smpl_tth

  }

  #----------------------------------------------------------------
  #Cropping data to 2theta range defined in tth_fps
  #----------------------------------------------------------------

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

  #The optimisation can fail if negative have creeped in during interpolation
  if(length(which(smpl[[2]] < 0) > 0)) {

    delete_negs <- which(smpl[[2]] < 0)
    smpl <- smpl[-delete_negs,]
    lib$tth <- lib$tth[-delete_negs]
    lib$xrd <- lib$xrd[-delete_negs, ]

  }

  cat("\n-Applying non-negative least squares")
  nnls_out <- .xrd_nnls(xrd.lib = lib, xrd.sample = smpl[, 2], force = force)

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


#-------------------------------------------------------
  #Remove negative/zero parameters
#-------------------------------------------------------

  if (min(x) < 0) {

  remove_neg_out <- .remove_neg(x = x, lib = lib, smpl = smpl,
                                solver = solver, obj = obj, force = force)

  x <- remove_neg_out[[1]]
  lib <- remove_neg_out[[2]]

  }

  #--------------------------------------------
  #Shifting
  #--------------------------------------------

  #Alignment and then another optimisation ONLY if the shift parameter
  #is included

  if(shift > 0 & length(x) > 1) {

    #This will replace the grid search shifting
    cat("\n-Optimising shifting coefficients...")
    x_s <- rep(0, length(x))
    names(x_s) <- names(x)

    o <- stats::optim(par = x_s, .fullpat_shift_seq,
                      weightings = x,
                      method = solver, lib = lib,
                      smpl = smpl, obj = obj)

    x_s <- o$par

    #Make sure any large shifts are avoided
    if (length(which(x_s > abs(shift) | x_s < -abs(shift))) > 0) {

      x_s[which(x_s > abs(shift) | x_s < -abs(shift))] <- 0

    }

    #Extract the shifted data
    cat("\n-Harmonising library and sample to same 2theta axis")
    shifted <- .fullpat_shift(smpl = smpl, lib = lib,
                              par_shift = x_s)

    lib <- shifted$lib
    smpl <- shifted$smpl

  #----------------------------------------------
  #Re-optimise after shifting
  #----------------------------------------------

  cat("\n-Reoptimising after shifting data")

    #The optimisation can fail if negative have creeped in during interpolation
    if(length(which(smpl[[2]] < 0) > 0)) {

      delete_negs <- which(smpl[[2]] < 0)
      smpl <- smpl[-delete_negs,]
      lib$tth <- lib$tth[-delete_negs]
      lib$xrd <- lib$xrd[-delete_negs, ]

    }

  o <- stats::optim(par = x, .fullpat,
                    method = solver, pure_patterns = lib$xrd,
                    sample_pattern = smpl[, 2], obj = obj)

  x <- o$par


  #Now remove negative parameters if they exist
  if (min(x) < 0) {

        remove_neg_out <- .remove_neg(x = x, lib = lib, smpl = smpl,
                                      solver = solver, obj = obj,
                                      force = force)

        x <- remove_neg_out[[1]]
        lib <- remove_neg_out[[2]]

  }

  }


  #Now that some negative parameters have been removed, the detection limits
  #of the remaining phases are estimated.
#---------------------------------------------------------------
#Removing phases based on detection limits
#---------------------------------------------------------------

#Calculate the lod and remove any phases below it if lod > 0

  if (lod > 0) {

  cat("\n-Calculating detection limits")

  if (is.na(std_conc)) {

  xrd_detectable <- .lod(x = x, lib = lib,
                          std = std, amorphous = amorphous,
                          lod = lod,
                         force = force)

  } else {

  xrd_detectable <- .lod2(x = x, lib = lib,
                          std = std, std_conc = std_conc,
                          amorphous = amorphous,
                          lod = lod,
                          force = force)

  }

  #Reoptimise if things have changed
  logical_reoptimise <- identical(x, xrd_detectable[["x"]])

  x <- xrd_detectable[["x"]]
  lib$xrd <- xrd_detectable[["lib"]]

  if (logical_reoptimise == FALSE) {

  cat("\n-Reoptimising after removing crystalline phases below the limit of detection")

  o <- stats::optim(par = x, .fullpat,
                    method = solver, pure_patterns = lib$xrd,
                    sample_pattern = smpl[, 2], obj = obj)

  x <- o$par

  }

  }

#-------------------------------------------------------------------------
#Recalculating concentrations
#-------------------------------------------------------------------------

  #Calculate mineral concentrations so that I can throw away any amorphous
  #phases below detection limit

  if (is.na(std_conc)) {

  min_concs <- .qminerals(x = x, xrd_lib = lib)

  } else {

  min_concs <- .qminerals2(x = x, xrd_lib = lib,
                           std = std, std_conc = std_conc)

  }

  df <- min_concs[[1]]
  dfs <- min_concs[[2]]

  #Remove amorphous phases
  if (is.na(std_conc)) {

  remove_amorphous_out <- .remove_amorphous(x = x,
                                            amorphous = amorphous,
                                            amorphous_lod = amorphous_lod,
                                            df = df,
                                            lib = lib,
                                            solver = solver,
                                            smpl = smpl,
                                            obj = obj)

  } else {

  remove_amorphous_out <- .remove_amorphous2(x = x,
                                             amorphous = amorphous,
                                             amorphous_lod = amorphous_lod,
                                             df = df,
                                             lib = lib,
                                             solver = solver,
                                             smpl = smpl,
                                             obj = obj,
                                             std = std,
                                             std_conc = std_conc)

  }

  x <- remove_amorphous_out[[1]]
  lib <- remove_amorphous_out[[2]]


  #Remove negative parameters again because some can creep in late-on
  if (min(x) < 0) {
  remove_neg_out <- .remove_neg(x = x, lib = lib, smpl = smpl,
                                solver = solver, obj = obj,
                                force = force)

  x <- remove_neg_out[[1]]
  lib <- remove_neg_out[[2]]
  }


  #-----------------------------------------------------------------------
#Computing fitted pattern and residuals
#-----------------------------------------------------------------------

  #compute fitted pattern and residuals
  fitted_pattern <- apply(sweep(as.matrix(lib$xrd), 2, x, "*"), 1, sum)

  resid_x <- smpl[, 2] - fitted_pattern

#-----------------------------------------------------------------------
#Computing final phase concentrations
#-----------------------------------------------------------------------

  #compute phase concentrations
  cat("\n-Computing phase concentrations")

  if (is.na(std_conc) | identical(names(x), std)) {

    if (!identical(names(x), std)) {
      cat("\n-Internal standard concentration unknown. Assuming phases sum to 100 %")
    }

    min_concs <- .qminerals(x = x, xrd_lib = lib)

    if (identical(names(x), std)) {
      cat("\n-Internal standard is the only phase present, defining its concentration as", std_conc, "%")
      min_concs$df$phase_percent <- std_conc
      min_concs$df$phase_percent <- std_conc

    }

  } else {

    cat("\n-Using internal standard concentration of", std_conc, "% to compute phase concentrations")
    min_concs <- .qminerals2(x = x, xrd_lib = lib, std = std, std_conc = std_conc,
                             normalise = normalise)

  }

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


  #Create a list of the input arguments
  inputs <- list("harmonise" = harmonise,
                 "solver" = solver,
                 "obj" = obj,
                 "std" = std,
                 "force" = force,
                 "std_conc" = std_conc,
                 "tth_align" = tth_align,
                 "align" = align,
                 "manual_align" = manual_align,
                 "shift" = shift,
                 "tth_fps" = tth_fps,
                 "lod" = lod,
                 "amorphous" = amorphous,
                 "amorphous_lod" = amorphous_lod)


  #Define a list that becomes the function output
  out <- list("tth" = smpl[,1],
              "fitted" = fitted_pattern,
              "measured" = smpl[,2],
              "residuals" = resid_x,
              "phases" = df,
              "phases_grouped" = dfs,
              "rwp" = R_fit,
              "weighted_pure_patterns" = xrd,
              "coefficients" = x,
              "inputs" = inputs)

  #Define the class
  class(out) <- "powdRafps"
  cat("\n***Automated full pattern summation complete***\n")

  return(out)

}



