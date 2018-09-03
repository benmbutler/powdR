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
#'                  std = "QUA.1")
#'
#' fps_lime <- fps(lib = minerals,
#'                 smpl = soils$limestone,
#'                 refs = minerals$phases$phase_id,
#'                 std = "QUA.1")
#'
#' fps_granite <- fps(lib = minerals,
#'                    smpl = soils$granite,
#'                    refs = minerals$phases$phase_id,
#'                    std = "QUA.1")
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
#' @param refs A character string of reference pattern ID's from the specified library.
#' The ID's must match ID's in the \code{lib$phases$phase_id} column.
#' @param std The phase ID (e.g. "QUA.1") to be used as internal
#' standard. Must match an ID provided in the \code{phases} parameter.
#' @param tth_align A vector defining the minimum and maximum 2theta values to be used during
#' alignment. If not defined, then the full range is used.
#' @param align The maximum shift that is allowed during initial 2theta
#' alignment (degrees). Default = 0.1.
#' @param tth_fps A vector defining the minimum and maximum 2theta values to be used during
#' full pattern summation. If not defined, then the full range is used.
#' @param solver The optimisation routine to be used. One of \code{c("BFGS", "Nelder-Mead",
#' "CG")}. Default = "BFGS".
#' @param obj The objective function to minimise. One of \code{c("Delta", "R", "Rwp")}.
#' Default = \code{"Rwp"}. See Chipera and Bish (2002) and page 247 of Bish and Post (1989)
#' for definitions of these functions.
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
#'                  std = "QUA.1")
#'
#' fps_lime <- fps(lib = minerals,
#'                 smpl = soils$limestone,
#'                 refs = minerals$phases$phase_id,
#'                 std = "QUA.1")
#'
#' fps_granite <- fps(lib = minerals,
#'                    smpl = soils$granite,
#'                    refs = minerals$phases$phase_id,
#'                    std = "QUA.1")
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
fps.powdRlib <- function(lib, smpl, refs, std,
                tth_align, align, tth_fps, solver, obj, ...) {

  #Create defaults for values that aren't specified.
  if(missing(tth_align)) {
    tth_align <- c(min(smpl[[1]]), max(smpl[[1]]))
  }

  if(missing(align)) {
    align = 0.1
  }

  if(missing(solver)) {
    solver = "BFGS"
  }

  if(missing(obj)) {
    obj = "Rwp"
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

  ##Make sure that the defined tth arguments are within the range of the samples and reference library.
  #if (tth[1] < min(smpl[,1])) {
  #stop("tth[1] must exceed the minimum 2theta value of the sample")
  #}
  #if (tth[2] > max(smpl[,1])) {
  #stop("tth[2] must be lower than the maximum 2theta value of the sample")
  #}

  #if (tth[1] < min(lib$tth)) {
  #stop("tth[1] must be within the 2theta range of the reference library")
  #}
  #if (tth[2] > max(lib$tth)) {
  #stop("tth[2] must be within the 2theta range of the reference library")
  #}

#subset lib according to the phases vector

#keep <- which(lib$phases$phase_id %in% refs)

lib$xrd <- lib$xrd[, which(lib$phases$phase_id %in% refs)]
lib$phases <- lib$phases[which(lib$phases$phase_id %in% refs), ]


#if only one phase is being used, make sure it's a dataframe and named correctly
if (length(refs) == 1) {
  lib$xrd <- data.frame("phase" = lib$xrd)
  names(lib$xrd) <- refs
}

#Extract the standard as an xy dataframe
xrd_standard <- data.frame(tth = lib$tth, counts = lib$xrd[, which(lib$phases$phase_id == std)])

#align the data
smpl <- .xrd_align(smpl = smpl, xrd_standard, xmin = tth_align[1],
                    xmax = tth_align[2], xshift = align)

#If the alignment is close to the limit, provide a warning
if (sqrt(smpl[[1]]^2) > (align*0.95)) {
  warning("The optimised shift used in alignment is equal to the maximum shift defined
          in the function call. We advise visual inspection of this alignment.")
}

#smpl becomes a data frame
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
  names(lib$xrd) <- refs
}

#--------------------------------------------
#Initial Optimisation
#--------------------------------------------

x <- rep(0, ncol(lib$xrd))
names(x) <- names(lib$xrd)

o <- stats::optim(par = x, .fullpat,
           method = solver, pure_patterns = lib$xrd,
           sample_pattern = smpl[, 2], obj = obj)

x <- o$par

#-----------------------------------------------
# Remove negative parameters
#-----------------------------------------------

#setup an initial negpar that is negative so that the following while loop will
#run until no negative parameters are found
negpar <- -0.1

while (negpar < 0) {
  #use the most recently optimised coefficients
  x <- o$par
  #check for any negative parameters
  omit <- which(x < 0)

  #remove the column from the library that contains the identified data
  if (length(which(x < 0)) > 0) {
    lib$xrd <- lib$xrd[, -omit]
    x <- x[-omit]
  }

  o <- stats::optim(par = x, .fullpat,
             method = solver, pure_patterns = lib$xrd,
             sample_pattern = smpl[, 2], obj = obj)
  x <- o$par
  #identify whether any parameters are negative for the next iteration
  negpar <- min(x)
}

#compute fitted pattern and residuals
fitted_pattern <- apply(sweep(as.matrix(lib$xrd), 2, x, "*"), 1, sum)

resid_x <- smpl[, 2] - fitted_pattern

#compute grouped phase concentrations
min_concs <- .qminerals(x = x, xrd_lib = lib)

#Extract mineral concentrations (df) and summarised mineral concentrations (dfs)
df <- min_concs[[1]]
dfs <- min_concs[[2]]

#### Compute the R statistic. This could be used to identify samples
# that require manual interpretation

#obs_minus_calc <- (smpl[,2] - fitted_pattern)^2
#sample_squared <- smpl[,2]^2

#R_fit <- sqrt(sum((sample[,2] - fitted_pattern)^2)/sum(sample[,2]^2))

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
out <- list(smpl[,1], fitted_pattern, smpl[,2], resid_x, df, dfs, R_fit, xrd, x)
names(out) <- c("tth", "fitted", "measured", "residuals",
                "phases", "phases_summary", "rwp", "weighted_pure_patterns", "coefficients")

#Define the class
class(out) <- "powdRfps"

return(out)

}
