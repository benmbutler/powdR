#' Tabulate limit of detection estimates
#'
#' \code{tabulate_lod} takes a powdRfps or powdRafps object and
#' estimates the limit of detection of quantified phases using
#' a hypothetical limit of detection of an internal standard and
#' its reference intensity ratio.
#'
#' @param x a \code{powdRfps} or \code{powdRafps} object.
#' @param type one of \code{'all'} or \code{'grouped'}. Specifies whether results
#' are grouped by phase names. Default = \code{grouped}.
#' @param std_rir a numeric value specifying the reference intensity ratio
#' of a hypothetical internal standard.
#' @param std_lod a numeric value specifying an estimated limit of detection
#' (weight percent) for the hypothetical internal standard.
#' @param ... other arguments
#'
#' @return a data frame.
#'
#' @examples
#'
#' \dontrun{
#' #Load the minerals library
#' data(minerals)
#'
#' Load the soils data
#' data(soils)
#'
#' fps_sand <-  fps(lib = minerals,
#'                  smpl = soils$sandstone,
#'                  refs = minerals$phases$phase_id,
#'                  std = "QUA.1",
#'                  align = 0.2)
#'
#' #Tabulate lod estimates based on the assumption that
#' #0.1 % quartz can be detected, with a RIR of 5.
#' tabulate_lod(fps_sand,
#'               std_rir = 5,
#'               std_lod = 0.1)
#' }
#'
#' @export
tabulate_lod <- function(x, type, std_rir, std_lod, ...) {
  UseMethod("tabulate_lod")
}


#' Tabulate limit of detection estimates
#'
#' \code{tabulate_lod} takes a powdRfps or powdRafps object and
#' estimates the limit of detection of quantified phases using
#' a hypothetical limit of detection of an internal standard and
#' its reference intensity ratio.
#'
#' @param x a \code{powdRfps} or \code{powdRafps} object.
#' @param type one of \code{'all'} or \code{'grouped'}. Specifies whether results
#' are grouped by phase names. Default = \code{grouped}.
#' @param std_rir a numeric value specifying the reference intensity ratio
#' of a hypothetical internal standard.
#' @param std_lod a numeric value specifying an estimated limit of detection
#' (weight percent) for the hypothetical internal standard.
#' @param ... other arguments
#'
#' @return a data frame.
#'
#' @examples
#'
#' \dontrun{
#' #Load the minerals library
#' data(minerals)
#'
#' Load the soils data
#' data(soils)
#'
#' fps_sand <-  fps(lib = minerals,
#'                  smpl = soils$sandstone,
#'                  refs = minerals$phases$phase_id,
#'                  std = "QUA.1",
#'                  align = 0.2)
#'
#' #Tabulate lod estimates based on the assumption that
#' #0.1 % quartz can be detected, with a RIR of 5.
#' tabulate_lod(fps_sand,
#'               std_rir = 5,
#'               std_lod = 0.1)
#' }
#'
#' @export
tabulate_lod.powdRfps <- function(x, type,
                                  std_rir, std_lod, ...) {

  out <- .lod_fps(x, type, std_rir, std_lod)

  return(out)

}


#' Tabulate limit of detection estimates
#'
#' \code{tabulate_lod} takes a powdRfps or powdRafps object and
#' estimates the limit of detection of quantified phases using
#' a hypothetical limit of detection of an internal standard and
#' its reference intensity ratio.
#'
#' @param x a \code{powdRfps} or \code{powdRafps} object.
#' @param type one of \code{'all'} or \code{'grouped'}. Specifies whether results
#' are grouped by phase names. Default = \code{grouped}.
#' @param std_rir a numeric value specifying the reference intensity ratio
#' of a hypothetical internal standard.
#' @param std_lod a numeric value specifying an estimated limit of detection
#' (weight percent) for the hypothetical internal standard.
#' @param ... other arguments
#'
#' @return a data frame.
#'
#' @examples
#'
#' \dontrun{
#' #Load the minerals library
#' data(minerals)
#'
#' Load the soils data
#' data(soils)
#'
#' fps_sand <-  fps(lib = minerals,
#'                  smpl = soils$sandstone,
#'                  refs = minerals$phases$phase_id,
#'                  std = "QUA.1",
#'                  align = 0.2)
#'
#' #Tabulate lod estimates based on the assumption that
#' #0.1 % quartz can be detected, with a RIR of 5.
#' tabulate_lod(fps_sand,
#'               std_rir = 5,
#'               std_lod = 0.1)
#' }
#'
#' @export
tabulate_lod.powdRafps <- function(x, type,
                                  std_rir, std_lod, ...) {

  out <- .lod_fps(x, type, std_rir, std_lod)

  return(out)

}


.lod_fps <- function(x, type, std_rir, std_lod) {

  if (missing(std_lod)) {

    stop("A numeric value greater than 0 but less than 100 must be
         supplied to the std_lod argument.",
         call. = FALSE)

  }

  if (std_lod <= 0 | std_lod >= 100) {

    stop("The value supplied to the std_lod argument must be greater
         than 0 but less than 100",
         call. = FALSE)

  }

  if (missing(std_rir)) {

    stop("A numeric value greater than 0 must be supplied to the
         std_rir argument.",
         call. = FALSE)

  }

  if (std_rir <= 0) {

    stop("The value supplied to the std_rir argument must be greater
         than 0.",
         call. = FALSE)

  }

  if (missing(type)) {

    type <- "grouped"

  }

  if (!type %in% c("all", "grouped")) {

    stop("The type argument must be one of 'all' or 'grouped'",
         call. = FALSE)

  }

  df <- x$phases

  if (type == "all") {

    df$lod <- std_lod * (std_rir/df$rir)
    df$lod <- round(df$lod, 3)

    return(df)

  } else {

    df$weighting <- NA
    df$weighted_rir <- NA

    for (i in 1:length(table(df$phase_name))) {

      current_phase <- names(table(df$phase_name))[i]
      phase_index <- which(df$phase_name == current_phase)

      df$weighting[phase_index] <- df$phase_percent[phase_index]/sum(df$phase_percent[phase_index])
      df$weighted_rir[phase_index] <- df$weighting[phase_index] * df$rir[phase_index]

    }

    dfs <- stats::aggregate(list("phase_percent" = df$phase_percent,
                                 "weighted_rir" = df$weighted_rir),
                            by = list("phase_name" = df$phase_name), FUN = sum)

    dfs$weighted_rir <- round(dfs$weighted_rir, 3)

    dfs$weighted_lod <- round(std_lod * (std_rir/dfs$weighted_rir), 3)

    return(dfs)

  }

}
