#' Mean centre XRPD data
#'
#' \code{mean.centre} takes XRPD data (2theta and counts) and
#' mean centres it by subtracting the mean count intensity from
#' each measurement interval in the counts vector. This can act
#' as a form of data normalisation that may be useful when
#' attempting cluster analysis on XRPD data.
#'
#' @param smpl an XRPD dataframe (2theta and counts)
#'
#' @examples
#' # Load soil xrd data
#' data(soils)
#'
#' #Use the sandstone soil
#' xrd <- soils$sandstone
#'
#' xrd$counts <- mean.centre(xrd)
mean.centre <- function(smpl) {

  smpl[[2]] <- smpl[[2]] - mean(smpl[[2]])

  return(smpl)
}
