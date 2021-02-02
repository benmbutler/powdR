#' Full pattern summation using linear regression
#'
#' \code{fps_lm} returns a simple fit of a given pattern using linear regression,
#' where coefficients may be either positive or negative. For more details see
#' \code{?fps_lm.powdRlib}.
#'
#' Requires a \code{powdRlib} library of reference patterns. Mineral concentrations
#' are not quantified and therefore reference intensity ratios are not required.
#'
#' @param lib A \code{powdRlib} object representing the reference library. Created using the
#' \code{powdRlib} constructor function.
#' @param ... Other parameters passed to methods e.g. \code{fps_lm.powdRlib}
#'
#' @return a list with components:
#' \item{tth}{a vector of the 2theta scale of the fitted data}
#' \item{fitted}{a vector of the fitted XRPD pattern}
#' \item{measured}{a vector of the original XRPD measurement (aligned)}
#' \item{residuals}{a vector of the residuals (fitted vs measured)}
#' \item{phases}{a dataframe of the phases used to produce the fitted pattern}
#' \item{phases_grouped}{the phases dataframe grouped by phase_name and summed}
#' \item{weighted_pure_patterns}{a dataframe of reference patterns used to produce the fitted pattern.
#' All patterns have been weighted according to the coefficients used in the fit}
#' \item{coefficients}{a named vector of coefficients used to produce the fitted pattern}
#' \item{inputs}{a list of input arguments used in the function call}
#'
#' @examples
#' #Load the rockjock library
#' data(rockjock)
#'
#' # Load the rockjock loadings data
#' data(rockjock_loadings)
#'
#' \dontrun{
#' fps_lm_out <- fps_lm(rockjock,
#'                      smpl = rockjock_loadings$Dim.1,
#'                      refs = rockjock$phases$phase_id,
#'                      std = "QUARTZ",
#'                      align = 0.3,
#'                      p = 0.01)
#'
#' }
#'
#' @export
fps_lm <- function(lib, ...) {
  UseMethod("fps_lm")
}


#' Full pattern summation using linear regression
#'
#' \code{fps_lm.powdRlib} returns a simple fit of a given pattern using linear regression,
#' where coefficients may be either positive or negative.
#'
#' Requires a \code{powdRlib} library of reference patterns. Mineral concentrations
#' are not quantified and therefore reference intensity ratios are not required.
#'
#' @param lib A \code{powdRlib} object representing the reference library. Created using the
#' \code{powdRlib} constructor function.
#' @param smpl A data frame. First column is 2theta, second column is counts
#' @param harmonise logical parameter defining whether to harmonise the \code{lib} and \code{smpl}.
#' Default = \code{TRUE}. Harmonises to the intersecting 2theta range at the coarsest resolution
#' available using natural splines.
#' @param refs A character string of reference pattern ID's or names from the specified library.
#' The ID's or names supplied must be present within the \code{lib$phases$phase_id} or
#' \code{lib$phases$phase_name} columns. If missing from the function call then all phases in
#' the reference library will be used.
#' @param std The phase ID (e.g. "QUA.1") to be used as internal
#' standard. Must match an ID provided in the \code{refs} parameter.
#' @param tth_align A vector defining the minimum and maximum 2theta values to be used during
#' alignment (e.g. \code{c(5,65)}). If not defined, then the full range is used.
#' @param align The maximum shift that is allowed during initial 2theta
#' alignment (degrees). Default = 0.1.
#' @param manual_align A logical operator denoting whether to optimise the alignment within the
#' negative/position 2theta range defined in the \code{align} argument, or to use the specified
#' value of the \code{align} argument for alignment of the sample to the standards. Default
#' = \code{FALSE}, i.e. alignment is optimised.
#' @param tth_fps A vector defining the minimum and maximum 2theta values to be used during
#' full pattern summation (e.g. \code{c(5,65)}). If not defined, then the full range is used.
#' @param shift A single numeric value denoting the maximum (positive or negative) shift,
#' in degrees 2theta, that is allowed during the shifting of selected phases. Default = 0.
#' @param p a numeric parameter between 0 and 1 specifying the p-value limit for coefficients.
#' Any reference patterns with a p-value greater than this value will be omitted from the
#' linear regression and results recomputed. Must be greater than 0.000001 but no greater than 1.
#' @param ... other arguments
#'
#' @return a list with components:
#' \item{tth}{a vector of the 2theta scale of the fitted data}
#' \item{fitted}{a vector of the fitted XRPD pattern}
#' \item{measured}{a vector of the original XRPD measurement (aligned)}
#' \item{residuals}{a vector of the residuals (fitted vs measured)}
#' \item{phases}{a dataframe of the phases used to produce the fitted pattern and their concentrations}
#' \item{phases_grouped}{the phases dataframe grouped by phase_name and concentrations summed}
#' \item{weighted_pure_patterns}{a dataframe of reference patterns used to produce the fitted pattern.
#' All patterns have been weighted according to the coefficients used in the fit}
#' \item{coefficients}{a named vector of coefficients used to produce the fitted pattern}
#' \item{inputs}{a list of input arguments used in the function call}
#'
#' @examples
#' #Load the rockjock library
#' data(rockjock)
#'
#' # Load the rockjock loadings data
#' data(rockjock_loadings)
#'
#' \dontrun{
#' fps_lm_out <- fps_lm(rockjock,
#'                      smpl = rockjock_loadings$Dim.1,
#'                      refs = rockjock$phases$phase_id,
#'                      std = "QUARTZ",
#'                      align = 0.3,
#'                      p = 0.01)
#'
#' }
#'
#' @export
fps_lm.powdRlib <- function(lib, smpl, harmonise, refs, std,
                         tth_align, align, manual_align,
                         tth_fps, shift, p, ...) {

  #---------------------------------------------------
  #Conditions
  #---------------------------------------------------

  sd0 <- which(unlist(lapply(lib$xrd, stats::sd)) == 0)

  if (length(sd0) > 0) {

    cat("\n-Removing", length(sd0), "phases from the library and refs argument
        that have a standard deviation of zero")

    lib <- subset(lib, refs = names(sd0), mode = "remove")

    #remove those reference patterns from the refs string
    refs <- refs[-which(refs %in% names(sd0))]

  }

  if (missing(p)) {

    p <- 1

  }

  if (p > 1 | p < 0.000001) {

    stop("\n-The value specified in the p argument but be greater than 0.000001 and less
         than or equal to 1.")

  }

  #Make sure the reference library is formatted correctly:
  if (!identical(names(lib$xrd), lib$phases$phase_id)) {

    stop("The names of the lib$xrd do not match the phase IDs in lib$phases$phase_id")

  }

  #Make sure the reference library is formatted correctly:
  if (!length(names(lib$xrd)) == length(unique(names(lib$xrd)))) {

    stop("The reference library contains duplicate phase IDs. Make sure that they
         are all unique.")

  }

  #Set harmonise = TRUE as the default if missing
  if (missing(harmonise)) {

    harmonise <- TRUE

  }

  #Make sure harmonise is logical
  if (!is.logical(harmonise)) {

    stop("The harmonise argument must be logical.",
         call. = FALSE)

  }

  #Make sure that the user knows to use harmonise if needed
  if (harmonise == FALSE & !identical(lib$tth, smpl[[1]])) {

    stop("The 2theta scale of the library and sample do not match. Try
         setting the harmonise argument to TRUE.",
         call. = FALSE)

  }


  #If tth_align is missing then use the maximum tth range
  if(missing(tth_align)) {

    tth_align <- c(min(smpl[[1]]), max(smpl[[1]]))
  }

  #If align is missing then set it to default
  if(missing(align)) {

    align = 0.1

  }

  #Set the default alignment type to automated
  if(missing(manual_align)) {

    manual_align <- FALSE

  }

  #Make sure manual_align is logical
  if(!is.logical(manual_align)) {

    stop("The manual_align argument must be logical",
         call. = FALSE)

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

  #Create a warning message if the shift is greater than 0.5, since this can confuse the optimisation
  if (abs(align) > 0.5 & manual_align == FALSE) {
    warning("Be cautious of large 2theta shifts. These can cause issues in sample alignment.",
            call. = FALSE)
  }


  #Check that none of the refs are spelt wrong
  wrong_spellings <- which(!refs %in% lib$phases$phase_id & !refs %in% lib$phases$phase_name)

  if (length(wrong_spellings) > 0) {

    stop(paste(c("\nThe following reference patterns specified in the refs argument are not in the library:\n",
                 paste(c(refs[wrong_spellings]), collapse = ", "))),
         call. = FALSE)

  }

  #-------------------------------------------------------------
  #END OF CONDITIONS, NOW SUBSET LIBRARY
  #-------------------------------------------------------------

  #subset lib according to the refs and force vector
  lib <- subset(lib, refs = refs, mode = "keep")

  #Make sure that the phase identified as the internal standard is contained within the reference library
  if (!std == "none" & !std %in% lib$phases$phase_id) {
    stop("The phase you have specified as the internal standard is not in the subset reference library",
         call. = FALSE)
  }

  #Harmonise libraries
  if (harmonise == TRUE & !identical(lib$tth, smpl[[1]])) {

    harmonised <- .harmoniser(lib = lib, smpl = smpl)

    smpl <- harmonised$smpl
    lib <- harmonised$lib

  }

  #--------------------------------------------------------
  #Alignment
  #--------------------------------------------------------

  if (!align == 0) {

    #align the data
    cat("\n-Aligning sample to the internal standard")
    smpl <- .xrd_align(smpl = smpl,
                       standard = data.frame(tth = lib$tth,
                                             counts = lib$xrd[, which(lib$phases$phase_id == std)],
                                             check.names = FALSE),
                       xmin = tth_align[1],
                       xmax = tth_align[2], xshift = align,
                       manual = manual_align)

    #If the alignment is close to the limit, provide a warning
    if (sqrt(smpl[[1]]^2) > (align*0.95) & manual_align == FALSE) {
      warning("The optimised shift used in alignment is equal to the maximum shift defined
          in the function call. We advise visual inspection of this alignment.",
              call. = FALSE)
    }

    #smpl becomes a data frame
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
                                                           xout = smpl_tth)[[2]]),
                          check.names = FALSE)

  }

  #Replace the library tth with that of the sample
  lib$tth <- smpl_tth

  #### decrease 2TH scale to the range defined in the function call
  smpl <- smpl[which(smpl$tth >= tth_fps[1] & smpl$tth <= tth_fps[2]), ]

  #Subset the xrd dataframe too
  lib$xrd <- lib$xrd[which(lib$tth >= tth_fps[1] & lib$tth <= tth_fps[2]), , drop = FALSE]

  #Replace the tth in the library with the shortened one
  lib$tth <- smpl[, 1]


  #-----------------------------------------------------------
  #Initial linear regression
  #-----------------------------------------------------------


  cat("\n-Applying linear regression")

  lm_out <- stats::lm(x~., data= data.frame("x" = smpl[[2]],
                                            lib$xrd,
                                            check.names = FALSE))

  x <- lm_out$coefficients[-1]

  #Extract the p-values
  x_p <- summary(lm_out)$coefficients[-1, 4]

  if(!identical(names(x), names(lib$xrd))) {

    stop("The names of the coefficients do not match those in the library")

  }

  if(!identical(names(x), names(x_p))) {

    stop("The names of the coefficients do not match the names of the p-values")

  }

  while (length(which(x_p > p)) > 0) {

  #Remove the p-values > p
  remove_p <- which(x_p > p)

  cat("\n-Removing coefficients with p-value less than", p)

  x <- x[-remove_p]

  lib$xrd <- lib$xrd[-remove_p]

  x_p <- x_p[-remove_p]

  #Recompute linear regression
  lm_out <- stats::lm(x~., data= data.frame("x" = smpl[[2]],
                                            lib$xrd,
                                            check.names = FALSE))

  x <- lm_out$coefficients[-1]

  #Extract the p-values
  x_p <- summary(lm_out)$coefficients[-1, 4]


  }


  #----------------------------------------------------
  # Shifting
  #----------------------------------------------------

  #Shift and then another optimisation ONLY if the shift parameter
  #is greater than zero and the correct solver arguments are used

  if(shift > 0 & length(x) > 1) {

    #This will replace the grid search shifting
    cat("\n-Optimising shifting coefficients...")
    x_s <- rep(0, length(x))
    names(x_s) <- names(x)

    o <- stats::optim(par = x_s, .fullpat_shift_seq,
                      weightings = x,
                      method = "BFGS", lib = lib,
                      smpl = smpl, obj = "R")

    x_s <- o$par

    #Make sure any large shifts are avoided
    if (length(which(x_s > shift | x_s < -shift)) > 0) {

      x_s[which(x_s > shift | x_s < -shift)] <- 0

    }

    #Extract the shifted data
    cat("\n-Harmonising library and sample to same 2theta axis")
    shifted <- .fullpat_shift(smpl = smpl, lib = lib,
                              par_shift = x_s)

    lib <- shifted$lib
    smpl <- shifted$smpl


    #-----------------------------------------------------------
    #Recalculate linear regression
    #-----------------------------------------------------------

    cat("\n-Re-applying linear regression after shifting")

    lm_out <- stats::lm(x~., data= data.frame("x" = smpl[[2]],
                                              lib$xrd,
                                              check.names = FALSE))

    x <- lm_out$coefficients[-1]

    #Extract the p-values
    x_p <- summary(lm_out)$coefficients[-1, 4]

    if(!identical(names(x), names(lib$xrd))) {

      stop("The names of the coefficients do not match those in the library")

    }

    if(!identical(names(x), names(x_p))) {

      stop("The names of the coefficients do not match the names of the p-values")

    }

    while (length(which(x_p > p)) > 0) {

      #Remove the p-values > p
      remove_p <- which(x_p > p)

      cat("\n-Removing coefficients with p-value less than", p)

      x <- x[-remove_p]

      lib$xrd <- lib$xrd[-remove_p]

      x_p <- x_p[-remove_p]

      #Recompute linear regression
      lm_out <- stats::lm(x~., data= data.frame("x" = smpl[[2]],
                                                lib$xrd,
                                                check.names = FALSE))

      x <- lm_out$coefficients[-1]

      #Extract the p-values
      x_p <- summary(lm_out)$coefficients[-1, 4]


    }

  }


  #-----------------------------
  #Summarise the data
  #-----------------------------

  df <- data.frame("phase_id" = names(x),
                   "coefficient" = unname(x),
                   "p" = unname(x_p))

  df <- plyr::join(df,
                   lib$phases[c(1:2)],
                   by = "phase_id")

  df <- df[c("phase_id", "phase_name", "coefficient", "p")]

  dfs <- data.frame(stats::aggregate(coefficient ~ phase_name, data = df, FUN = sum),
                    stringsAsFactors = FALSE)

  #--------------------------------------------------------
  #Compute fitted pattern and quantify
  #--------------------------------------------------------

  #compute fitted pattern and residuals
  fitted_pattern <- apply(sweep(as.matrix(lib$xrd), 2, x, "*"), 1, sum)

  resid_x <- smpl[, 2] - fitted_pattern

  #Extract the xrd data
  xrd <- data.frame(lib$xrd,
                    check.names = FALSE)

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
                 "refs" = refs,
                 "std" = std,
                 "force" = force,
                 "tth_align" = tth_align,
                 "align" = align,
                 "manual_align" = manual_align,
                 "tth_fps" = tth_fps,
                 "shift" = shift)


  #Define a list that becomes the function output
  out <- list("tth" = smpl[,1],
              "fitted" = unname(fitted_pattern),
              "measured" = smpl[,2],
              "residuals" = unname(resid_x),
              "phases" = df,
              "phases_grouped" = dfs,
              "weighted_pure_patterns" = xrd,
              "coefficients" = x,
              "inputs" = inputs)

  #Define the class
  class(out) <- "powdRlm"
  cat("\n***Full pattern summation complete***\n")

  return(out)

}
