#' Align and harmonise multiple XRPD patterns
#'
#' \code{multi_xrpd_align} takes a list of XRPD data and aligns them
#' relative to a chosen standard. It uses an optimisation routine that
#' computes a suitable linear shift. After all samples have been aligned,
#' the function harmonises the data to a single 2theta scale.
#'
#' @param x a list XRPD dataframes (column 1 = 2theta, column 2 = counts)
#' @param standard a dataframe of the chosen standard that each
#' sample is aligned to (column 1 = 2theta, column 2 = counts)
#' @param xmin the minimum 2theta value used during alignment
#' @param xmax the maximum 2theta value used during alignment
#' @param xshift the maximum (positive and negative) 2theta shift
#' that is allowed during alignment
#'
#' @return a list of aligned and harmonised XRPD data
#'
#' @examples
#' # Load soils xrd data
#' data(soils)
#'
#' #Load minerals library
#' data(minerals)
#'
#' #Create a standard quartz pattern to align to
#' quartz <- data.frame(tth = minerals$tth,
#'                      counts = minerals$xrd$QUA.1)
#'
#' #This function will allow for easier comparison of the peaks
#' range01 <- function(x){(x-min(x))/(max(x)-min(x))}
#'
#' #Plot the main quartz peak prior to alignment (scale the counts using rng.nm)
#' plot(x = soils$sandstone$tth,
#'      y = range01(soils$sandstone$counts),
#'      xlim = c(26, 27), type = "l")
#' lines(x = soils$granite$tth,
#'       y = range01(soils$granite$counts),
#'       col = "red")
#' lines(x = soils$limestone$tth,
#'       y = range01(soils$limestone$counts),
#'       col = "green")
#' lines(x = quartz$tth,
#'       y = range01(quartz$counts),
#'       col = "blue")
#'
#' #align data
#' aligned <- multi_xrpd_align(soils,
#'                             standard = quartz,
#'                             xmin = 10,
#'                             xmax = 60,
#'                             xshift = 0.2)
#' #replot data
#' plot(x = aligned$sandstone$tth,
#'      y = range01(aligned$sandstone$counts),
#'      xlim = c(26, 27), type = "l")
#' lines(x = aligned$granite$tth,
#'       y = range01(aligned$granite$counts),
#'       col = "red")
#' lines(x = aligned$limestone$tth,
#'       y = range01(aligned$limestone$counts),
#'       col = "green")
#' lines(x = quartz$tth,
#'       y = range01(quartz$counts),
#'       col = "blue")
#'
#' @export
multi_xrpd_align <- function(x, standard, xmin, xmax, xshift) {


  #-----------------------------------
  #Conditions
  #-----------------------------------

  #Make sure the xrpd data is present and is a list
  if (missing(x) | !is.list(x)) {

    stop("The x argument must be a list, with each item of the list being
         a 2 column dataframe.",
         call. = FALSE)

  }

  #Make sure the standard data is provided
  if (missing(standard)) {

    stop("Supply a dataframe for the standard comprised of the
         2theta axis and counts",
         call. = FALSE)

  }

  #Make sure the xmin value is defined
  if (missing(xmin) | xmin > max(standard[[1]]) | xmin < min(standard[[1]])) {

    stop("Specify a numeric value for the xmin argument that is within the
         2theta range of your standard",
         call. = FALSE)

  }

  #Make sure the xmin value is defined
  if (missing(xmax) | xmax > max(standard[[1]]) | xmax < min(standard[[1]])) {

    stop("Specify a numeric value for the xmax argument that is within the
         2theta range of your standard",
         call. = FALSE)

  }

  if (xmax <= xmin) {

    stop("The value specified in xmax must exceed that of xmin",
         call. = FALSE)

  }

  if (missing(xshift) | !is.numeric(xshift)) {

    stop("Specify a numeric value for xshift.",
         call. = FALSE)

  }

  #---------------------------------------------
  #Alignment
  #---------------------------------------------

  #Make sure xshift is not negative
  xshift <- abs(xshift)

  #Aligned data will end up in this list
  xrpd_aligned <- list()

  #The minimum and maximum 2theta value from each alignment will
  #end up in these vectors
  min_tth <- c()
  max_tth <- c()

  for (i in 1:length(x)) {

    xa <- .xrd_align(smpl = x[[i]],
                     standard = standard,
                     xmin = xmin,
                     xmax = xmax,
                     xshift = xshift,
                     manual = FALSE)

    xrpd_aligned[[i]] <- xa

    min_tth[i] <- min(xa[[2]][,1])
    max_tth[i] <- max(xa[[2]][,1])

  }

  #Define the 2theta resolution that a new scale will be built upon. Based on
  #the first pattern in the list
  tth_interval <- (max(standard[[1]]) - min(standard[[1]])) / nrow(standard)

  #Create a new 2th scale based on the shifts of the data so that no NA
  #values result from the subsequent linear spline
  tth <- seq(from = max(min_tth), to = min(max_tth), by = tth_interval)

  #Harmonise the aligned data to the new 2TH scale
  xrpd_harm <- list()

  for (i in 1:length(xrpd_aligned)) {

    xrpd_harm[[i]] <- data.frame(stats::spline(x = xrpd_aligned[[i]][[2]][, 1],
                                               y = xrpd_aligned[[i]][[2]][, 2],
                                               method = "natural",
                                               xout = tth))

    names(xrpd_harm[[i]]) <- c("tth", "counts")

  }

  #preserve names
  names(xrpd_harm) <- names(x)

  return(xrpd_harm)

}
