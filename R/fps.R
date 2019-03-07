.xrd_nnls <- function(xrd.lib, xrd.sample) {

  mat <- xrd.lib$xrd

  x <- nnls::nnls(as.matrix(mat), xrd.sample)
  x <- x$x
  names(x) <- names(data.frame(mat))

  remove_these <- which(x == 0)

  if(length(remove_these) > 0) {
  x <- x[-remove_these]
  xrd.lib[[1]] <- xrd.lib[[1]][-remove_these]
  }

  out <- list("x" = x, "xrd.lib" = xrd.lib[[1]])
  return(out)

}

.fullpat <- function (par, pure_patterns, sample_pattern, obj) {

  #if only 1 pattern is being fitted:
  if (length(par) == 1) {

    #calculate fitted pattern
    s_mix <- par * pure_patterns

    #objective functions
    if(obj == "Delta") {
      d <- sum(abs(sample_pattern - s_mix))
    }

    if(obj == "R") {
      d <- sqrt(sum((sample_pattern - s_mix)^2)/sum(sample_pattern^2))
    }

    if(obj == "Rwp") {
      d <-  sqrt(sum((1/sample_pattern) * ((sample_pattern - s_mix)^2)) / sum((1/sample_pattern) * (sample_pattern^2)))
    }

    return(d)
  }

  #if more than 1 pattern is being fitted
  if (length(par) > 1) {

    #This calculates the fitted pattern
    s_mix <- apply(sweep(pure_patterns, 2, par, "*"),
                   1, sum)

    #objective functions
    if(obj == "Delta") {
      d <- sum(abs(sample_pattern - s_mix))
    }

    if(obj == "R") {
      d <- sqrt(sum((sample_pattern - s_mix)^2)/sum(sample_pattern^2))
    }

    if(obj == "Rwp") {
      d <-  sqrt(sum((1/sample_pattern) * ((sample_pattern - s_mix)^2)) / sum((1/sample_pattern) * (sample_pattern^2)))
    }

    return(d)
  }
}

.qminerals <- function(x, xrd_lib) {

  #Make sure x is ordered if there are more than 1 phases in the library
  if (length(x) > 1) {
    x <- x[order(names(x))]
  }

  #Restrict the xrd library to phases within the names of fpf_pc
  minerals <- xrd_lib$phases

  minerals <- minerals[which(minerals$phase_id %in% names(x)),]

  #Order to the same as fpf_pc
  if (length(x) > 1) {
    minerals <- minerals[order(minerals$phase_id),]
  }

  min_percent <- (x/minerals$rir)/sum(x/minerals$rir)*100

  names(min_percent) <- minerals$phase_id

  df <- data.frame(minerals, "phase_percent" = min_percent)
  row.names(df) = c(1:nrow(df))

  #Summarise by summing the concentrations from each mineral group

  dfs <- data.frame(stats::aggregate(phase_percent ~ phase_name, data = df, FUN = sum),
                    stringsAsFactors = FALSE)

  #Ensure that the phase concentrations are rounded to 4 dp
  df$phase_percent <- round(df$phase_percent, 4)
  dfs$phase_percent <- round(dfs$phase_percent, 4)

  out <- list("df" = df, "dfs" = dfs)

  return(out)
}

.qminerals2 <- function(x, xrd_lib, std, std_conc) {

  #Make sure x is ordered if there are more than 1 phases in the library
  if (length(x) > 1) {
    x <- x[order(names(x))]
  }

  #Get the name of the internal standard
  std_name <- xrd_lib$phases$phase_name[which(xrd_lib$phases$phase_id == std)]

  #Extract all the ids of potential standard patterns
  std_ids <- xrd_lib$phases$phase_id[which(xrd_lib$phases$phase_name == std_name)]

  id_match <- which(names(x) %in% std_ids)

  if (length(id_match) < 1) {

    stop("\n-The phase specified as the std is not present. Cannot compute
         phase concentrations.")

  }

  #Get the scaling parameter of x
  std_x <- sum(x[which(names(x) %in% std_ids)])

  minerals <- xrd_lib$phases
  minerals <- minerals[which(minerals$phase_id %in% names(x)),]

  #Calculate a weighted average rir
  std_rir <- sum((minerals$rir[which(minerals$phase_id %in% std_ids)]/
             std_x) * x[which(names(x) %in% std_ids)])

  #Remove any internal standard patterns from x
  x <- x[-which(names(x) %in% std_ids)]

  #Restrict the xrd library to phases within the names of fpf_pc
  minerals <- minerals[which(minerals$phase_id %in% names(x)),]

  #Order to the same as fpf_pc
  if (length(x) > 1) {
    minerals <- minerals[order(minerals$phase_id),]
  }

  min_percent <- c()

  for (i in 1:length(x)) {

    min_percent[i] <- (std_conc/(minerals$rir[i]/std_rir)) * (x[i]/std_x) * (1+(std_conc/100))

    #names(min_percent)[i] <- minerals$phase_id[i]

  }

  df <- data.frame(minerals, "phase_percent" = min_percent)
  row.names(df) = c(1:nrow(df))

  #Summarise by summing the concentrations from each mineral group

  dfs <- data.frame(stats::aggregate(phase_percent ~ phase_name, data = df, FUN = sum),
                    stringsAsFactors = FALSE)

  #Ensure that the phase concentrations are rounded to 4 dp
  df$phase_percent <- round(df$phase_percent, 4)
  dfs$phase_percent <- round(dfs$phase_percent, 4)

  out <- list("df" = df, "dfs" = dfs)

  return(out)
}


#' Full pattern summation
#'
#' \code{fps} returns estimates of phase concentrations using full pattern
#' summation of X-ray powder diffraction data. For more details see \code{?fps.powdRlib}.
#'
#' Applies full pattern summation (Chipera & Bish, 2002, 2013; Eberl, 2003) to an XRPD
#' measurement to quantify phase concentrations. Requires a \code{powdRlib} library of
#' reference patterns with pre-measured reference intensity ratios in order to derive
#' mineral concentrations.
#'
#' @param lib A \code{powdRlib} object representing the reference library. Created using the
#' \code{powdRlib} constructor function.
#' @param ... Other parameters passed to methods e.g. \code{fps.powdRlib}
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
#' fps_sand <-  fps(lib = minerals,
#'                  smpl = soils$sandstone,
#'                  refs = minerals$phases$phase_id,
#'                  std = "QUA.1",
#'                  align = 0.2)
#'
#' fps_lime <- fps(lib = minerals,
#'                 smpl = soils$limestone,
#'                 refs = minerals$phases$phase_id,
#'                 std = "QUA.1",
#'                 align = 0.2)
#'
#' fps_granite <- fps(lib = minerals,
#'                    smpl = soils$granite,
#'                    refs = minerals$phases$phase_id,
#'                    std = "QUA.1",
#'                    align = 0.2)
#'
#' #Alternatively run all 3 at once using lapply
#'
#' fps_soils <- lapply(soils, fps,
#'                     lib = minerals,
#'                     std = "QUA.2",
#'                     refs = minerals$phases$phase_id,
#'                     align = 0.2)
#'
#' #Using the rockjock library:
#'
#' data(rockjock)
#' data(rockjock_mixtures)
#'
#' rockjock_1 <- fps(lib = rockjock,
#'                   smpl = rockjock_mixtures$Mix1,
#'                   refs = c("ORDERED_MICROCLINE",
#'                            "LABRADORITE",
#'                            "KAOLINITE_DRY_BRANCH",
#'                            "MONTMORILLONITE_WYO",
#'                            "ILLITE_1M_RM30",
#'                            "CORUNDUM"),
#'                   std = "CORUNDUM",
#'                   align = 0.3)
#'
#' #Alternatively you can specify the internal standard
#' #concentration if known:
#' rockjock_1s <- fps(lib = rockjock,
#'                  smpl = rockjock_mixtures$Mix1,
#'                  refs = c("ORDERED_MICROCLINE",
#'                           "LABRADORITE",
#'                           "KAOLINITE_DRY_BRANCH",
#'                           "MONTMORILLONITE_WYO",
#'                           "ILLITE_1M_RM30",
#'                           "CORUNDUM"),
#'                  std = "CORUNDUM",
#'                  std_conc = 20,
#'                  align = 0.3)
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
#' Eberl, D.D., 2003. User's guide to ROCKJOCK - A program for determining quantitative mineralogy from
#' powder X-ray diffraction data. Boulder, CA.
#' @export
fps <- function(lib, ...) {
  UseMethod("fps")
}

#' Full pattern summation
#'
#' \code{fps.powdRlib} returns estimates of phase concentrations using full pattern
#' summation of X-ray powder diffraction data.
#'
#' Applies full pattern summation (Chipera & Bish, 2002, 2013; Eberl, 2003) to an XRPD
#' sample to quantify phase concentrations. Requires a \code{powdRlib} library of reference
#' patterns with pre-measured reference intensity ratios in order to derive mineral
#' concentrations.
#'
#' @param lib A \code{powdRlib} object representing the reference library. Created using the
#' \code{powdRlib} constructor function.
#' @param smpl A data frame. First column is 2theta, second column is counts
#' @param harmonise logical parameter defining whether to harmonise the \code{lib} and \code{smpl}.
#' Default = \code{TRUE}. Harmonises to the intersecting 2theta range at the coarsest resolution
#' available.
#' @param solver The optimisation routine to be used. One of \code{c("BFGS", "Nelder-Mead",
#' "CG", "L-BFGS-B", or "NNLS")}. Default = \code{"BFGS"}.
#' @param obj The objective function to minimise when "BFGS", "Nelder-Mead",
#' "CG" or "L-BFGS-B" are used as the `solver` argument. One of \code{c("Delta", "R", "Rwp")}.
#' Default = \code{"Rwp"}. See Chipera and Bish (2002) and page 247 of Bish and Post (1989)
#' for definitions of these functions.
#' @param refs A character string of reference pattern ID's from the specified library.
#' The ID's must match ID's in the \code{lib$phases$phase_id} column.
#' @param std The phase ID (e.g. "QUA.1") to be used as internal
#' standard. Must match an ID provided in the \code{phases} parameter.
#' @param std_conc The concentration of the internal standard (if known) in weight percent. If
#' unknown then use \code{std_conc = NA}, in which case it will be assumed that all phases sum
#' to 100 percent (default).
#' @param tth_align A vector defining the minimum and maximum 2theta values to be used during
#' alignment. If not defined, then the full range is used.
#' @param align The maximum shift that is allowed during initial 2theta
#' alignment (degrees). Default = 0.1.
#' @param manual_align A logical opertator denoting whether to optimise the alignment within the
#' negative/position 2theta range defined in the \code{align} arugment, or to use the specified
#' value of the \code{align} argument for alignment of the sample to the standards. Default
#' = \code{FALSE}, i.e. alignment is optimised.
#' @param tth_fps A vector defining the minimum and maximum 2theta values to be used during
#' full pattern summation. If not defined, then the full range is used.
#' @param shift The maximum shift (degrees 2theta) that is allowed during the grid search phases selected
#' from the non-negative least squares. Default = 0.05).
#' @param shift_res A single integer defining the increase in resolution used during grid search shifting. Higher
#' values facilitate finer shifts at the expense of longer computation. Default = 4.
#' @param remove_trace A single numeric value representing the limit for the concentration of trace phases to
#' be retained, i.e. any mineral with an estimated concentration below `remove_trace` will be ommitted. Default = 0.
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
#' fps_sand <-  fps(lib = minerals,
#'                  smpl = soils$sandstone,
#'                  refs = minerals$phases$phase_id,
#'                  std = "QUA.1",
#'                  align = 0.2)
#'
#' fps_lime <- fps(lib = minerals,
#'                 smpl = soils$limestone,
#'                 refs = minerals$phases$phase_id,
#'                 std = "QUA.1",
#'                 align = 0.2)
#'
#' fps_granite <- fps(lib = minerals,
#'                    smpl = soils$granite,
#'                    refs = minerals$phases$phase_id,
#'                    std = "QUA.1",
#'                    align = 0.2)
#'
#' #Alternatively run all 3 at once using lapply
#'
#' fps_soils <- lapply(soils, fps,
#'                     lib = minerals,
#'                     std = "QUA.2",
#'                     refs = minerals$phases$phase_id,
#'                     align = 0.2)
#'
#' #Using the rockjock library:
#'
#' data(rockjock)
#' data(rockjock_mixtures)
#'
#' rockjock_1 <- fps(lib = rockjock,
#'                   smpl = rockjock_mixtures$Mix1,
#'                   refs = c("ORDERED_MICROCLINE",
#'                            "LABRADORITE",
#'                            "KAOLINITE_DRY_BRANCH",
#'                            "MONTMORILLONITE_WYO",
#'                            "ILLITE_1M_RM30",
#'                            "CORUNDUM"),
#'                   std = "CORUNDUM",
#'                   align = 0.3)
#'
#' #Alternatively you can specify the internal standard
#' #concentration if known:
#' rockjock_1s <- fps(lib = rockjock,
#'                  smpl = rockjock_mixtures$Mix1,
#'                  refs = c("ORDERED_MICROCLINE",
#'                           "LABRADORITE",
#'                           "KAOLINITE_DRY_BRANCH",
#'                           "MONTMORILLONITE_WYO",
#'                           "ILLITE_1M_RM30",
#'                           "CORUNDUM"),
#'                  std = "CORUNDUM",
#'                  std_conc = 20,
#'                  align = 0.3)
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
#' Eberl, D.D., 2003. User's guide to ROCKJOCK - A program for determining quantitative mineralogy from
#' powder X-ray diffraction data. Boulder, CA.
#' @export
fps.powdRlib <- function(lib, smpl, harmonise, solver, obj, refs, std, std_conc,
                tth_align, align, manual_align, tth_fps, shift, shift_res, remove_trace, ...) {

  if (missing(harmonise)) {

    cat("\n-Setting harmonise to default of TRUE")
    harmonise <- TRUE

  }



  if (missing(std_conc)) {

    cat("\n-Using default std_conc of NA")
    std_conc <- NA

  }

  if (!(is.numeric(std_conc) | is.na(std_conc))) {

    stop("\n-The std_conc argument must either be NA or a numeric value greater than 0 and less than 100.")

  }

  if (is.numeric(std_conc)) {

    if (missing(std)) {

      stop("\n-Please define the std argument")

    }

    if(std_conc <= 0 | std_conc >= 100) {

      stop("\n-The std_conc argument must either be NA or a numeric value greater than 0 and less than 100.")

    }

  }


  #If tth_align is missing then use the maximum tth range
  if(missing(tth_align)) {
    cat("\n-Using maximum tth range")
    tth_align <- c(min(smpl[[1]]), max(smpl[[1]]))
  }

  #If align is missing then set it to default
  if(missing(align)) {
    cat("\n-Using default alignment of 0.1")
    align = 0.1
  }

  if(missing(manual_align)) {

    manual_align <- FALSE

  }

  if(!is.logical(manual_align)) {

    stop("The manual_align argument must be logical")

  }

  #If solver is missing then set it to BFGS
  if(missing(solver)) {
    cat("\n-Using default solver of BFGS")
    solver = "BFGS"
  }

  #If shift is missing then set it to default
  if(missing(shift)) {
    cat("\n-Using default shift of 0")
    shift = 0
  }

  #If shift_res is missing then set it to default
  if(missing(shift_res)) {
    shift_res = 4
  }

  #If remove_trace is missing then set it to default
  if(missing(remove_trace)) {
    remove_trace = 0
  }

  #If solver is NNLS and refs aren't defined then use all of them
  if(solver == "NNLS" & missing(refs)) {

    refs = lib$phases$phase_id

  }

  #If obj is not defined and needs to be, set it to Rwp
  if(missing(obj) & solver %in% c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B")) {
    cat("\n-Using default objective function of Rwp")
    obj = "Rwp"
  }

  #If obj is missing and NNLS is being used along with some shift
  #then set obj to Rwp
  if(missing(obj) & solver == "NNLS" & shift > 0) {
    cat("\n-Using Rwp to optimise shifts when applying grid search")
    obj = "Rwp"
  }

  #Create a warning message if the shift is greater than 0.5, since this can confuse the optimisation
  if (abs(align) > 0.5 & manual_align == FALSE) {
    warning("Be cautious of large 2theta shifts. These can cause issues in sample alignment.")
  }

  #Create a warning message if the shift is greater than 0.5, since this can confuse the optimisation
  if (remove_trace < 0) {
    stop("The remove_trace argument must be greater than 0.")
  }

  #Make only "Nelder-Mead", "BFGS", or "CG", "L-BFGS-B" or "NNLS" optional for the solver
  if (!solver %in% c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "NNLS")) {
    stop("The solver argument must be one of 'BFGS', 'Nelder Mead', 'CG', 'L-BFGS-B' or 'NNLS'")
  }

  #If align is 0 then the standard can be set to 'none'
  if (align == 0 | manual_align == TRUE) {

    std <- "none"

  }

  #Make sure that the phase identified as the internal standard is contained within the reference library
  if (!std == "none" & !std %in% lib$phases$phase_id) {
  stop("The phase you have specified as the internal standard is not in the reference library")
  }

  #subset lib according to the phases vector
  lib$xrd <- lib$xrd[, which(lib$phases$phase_id %in% refs)]
  lib$phases <- lib$phases[which(lib$phases$phase_id %in% refs), ]


  #if only one phase is being used, make sure it's a dataframe and named correctly
  if (length(refs) == 1) {
    lib$xrd <- data.frame("phase" = lib$xrd)
    names(lib$xrd) <- refs
  }


#Harmonise libraries
  if (harmonise == TRUE & !identical(lib$tth, smpl[[1]])) {

    harmonised <- .harmoniser(lib = lib, smpl = smpl)

    smpl <- harmonised$smpl
    lib <- harmonised$lib

  }


if (!align == 0) {

#align the data
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
          in the function call. We advise visual inspection of this alignment.")
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

xrd_ref_names <- lib$phases$phase_id

#Ensure that samples in the reference library are on the same scale as the sample
cat("\n-Interpolating library to same 2theta scale as aligned sample")
lib$xrd <- data.frame(lapply(names(lib$xrd),
                                       function(n) stats::approx(x = lib$tth,
                                                          y = unname(unlist(lib$xrd[n])),
                                                          xout = smpl_tth)[[2]]))

names(lib$xrd) <- xrd_ref_names

}

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
  names(lib$xrd) <- refs
}

if (solver %in% c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B")) {

#--------------------------------------------
#Initial Optimisation
#--------------------------------------------

x <- rep(0, ncol(lib$xrd))
names(x) <- names(lib$xrd)

if (solver == "L-BFGS-B") {

 cat("\n-Optimising using L-BFGS-B constrained to a lower limit of zero...")

 o <- stats::optim(par = x, .fullpat,
                   method = solver, lower = 0, pure_patterns = lib$xrd,
                   sample_pattern = smpl[, 2], obj = obj)

} else {

cat("\n-Optimising...")

o <- stats::optim(par = x, .fullpat,
           method = solver, pure_patterns = lib$xrd,
           sample_pattern = smpl[, 2], obj = obj)

}

x <- o$par

#-----------------------------------------------
# Remove negative parameters or parameters equal to zero
#-----------------------------------------------

remove_neg_out <- .remove_neg(x = x, lib = lib, smpl = smpl,
                              solver = solver, obj = obj)

x <- remove_neg_out[[1]]
lib <- remove_neg_out[[2]]

} else {

  cat("\n-Applying non-negative least squares")
  nnls_out <- .xrd_nnls(xrd.lib = lib, xrd.sample = smpl[, 2])

  lib$xrd <- nnls_out$xrd.lib
  x <- nnls_out$x

}


#----------------------------------------------------
# Grid search shifting
#----------------------------------------------------

#Shift and then another optimisation ONLY if the shift parameter
#is greater than zero

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




#----------------------------------------------
#Re-optimise after shifting
#----------------------------------------------

  if (solver %in% c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B")) {

    cat("\n-Reoptimising after shifting data")

    o <- stats::optim(par = x, .fullpat,
                      method = solver, pure_patterns = lib$xrd,
                      sample_pattern = smpl[, 2], obj = obj)

    x <- o$par

  } else {

    cat("\n-Applying non-negative least squares")

    nnls_out <- .xrd_nnls(xrd.lib = lib, xrd.sample = smpl[, 2])

    lib$xrd <- nnls_out$xrd.lib

    x <- nnls_out$x

  }

#--------------------------------------------------------------------------------------------
#Remove negative/zero parameters after shifting
#--------------------------------------------------------------------------------------------

if (!solver == 'NNLS') {

remove_neg_out <- .remove_neg(x = x, lib = lib, smpl = smpl,
                              solver = solver, obj = obj)

x <- remove_neg_out[[1]]
lib <- remove_neg_out[[2]]

}

}

#-------------------------------------------------------------------------------------------
#Remove trace patterns
#-------------------------------------------------------------------------------------------

if (remove_trace > 0) {

  remove_trace_out <- .remove_trace(x = x, lib = lib, smpl = smpl,
                                  solver = solver, obj = obj,
                                  remove_trace = remove_trace)
  x <- remove_trace_out[[1]]
  lib <- remove_trace_out[[2]]

}

#--------------------------------------------------------
#Compute fitted pattern and quantify
#--------------------------------------------------------

#compute fitted pattern and residuals
fitted_pattern <- apply(sweep(as.matrix(lib$xrd), 2, x, "*"), 1, sum)

resid_x <- smpl[, 2] - fitted_pattern

#compute phase concentrations
cat("\n-Computing phase concentrations")

if (is.na(std_conc)) {

  cat("\n-Internal standard concentration unknown. Assuming phases sum to 100 %")
  min_concs <- .qminerals(x = x, xrd_lib = lib)

} else {

  cat("\n-Using internal standard concentration of", std_conc, "% to compute phase concentrations")
  min_concs <- .qminerals2(x = x, xrd_lib = lib, std = std, std_conc = std_conc)

}

#Extract mineral concentrations (df) and summarised mineral concentrations (dfs)
df <- min_concs[[1]]
dfs <- min_concs[[2]]

#### Compute the Rwp
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
               "refs" = refs,
               "std" = std,
               "std_conc" = std_conc,
               "tth_align" = tth_align,
               "align" = align,
               "manual_align" = manual_align,
               "tth_fps" = tth_fps,
               "shift" = shift,
               "shift_res" = shift_res,
               "remove_trace" = remove_trace)


#Define a list that becomes the function output
out <- list("tth" = smpl[,1],
            "fitted" = unname(fitted_pattern),
            "measured" = smpl[,2],
            "residuals" = unname(resid_x),
            "phases" = df,
            "phases_summary" = dfs,
            "rwp" = R_fit,
            "weighted_pure_patterns" = xrd,
            "coefficients" = x,
            "inputs" = inputs)

#Define the class
class(out) <- "powdRfps"
cat("\n***Full pattern summation complete***\n")

return(out)

}
