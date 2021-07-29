#' Read ASCII XY data
#'
#' \code{read_xy} is a wrapper for \code{read.csv} that is designed for space separated XRPD
#' data.
#'
#' @param files path of the file(s) to be imported.
#' @param header a logical value indicating whether the file contains the names
#' of the variables as its first line. Default = \code{FALSE}.
#' @param sep the field separator character. Values on each line of the file
#' are separated by this character. Default = \code{" "}, indicating space
#' separated format.
#'
#' @return If only one path is supplied then an XY data frame with 2 columns is returned,
#' the first being the 2theta axis and the second being the count intensities. If more
#' than one path is supplied then a multiXY list is returned, with each item in the list being
#' an XY data frame as already described.
#'
#' @examples
#' #load example XY file
#' file <- system.file("extdata/D5000/xy/D5000_1.xy", package = "powdR")
#' xy <- read_xy(file)
#'
#' #Load multiple XY files
#' files <- dir(system.file("extdata/D5000/xy", package = "powdR"),
#'              full.names = TRUE)
#' xy_list <- read_xy(files)
#'
#' \dontrun{
#' plot(xy_list, wavelength = "Cu")
#' plot(xy_list, wavelength = "Cu", interactive = TRUE)
#' }
#'
#' @export
read_xy <- function(files, header, sep) {

  if (missing(header)) {

    header <- FALSE

  }

  if (!is.logical(header)) {

    stop("The value supplied to the header argument must be logical.",
         call. = FALSE)

  }

  if (missing(sep)) {

    sep = " "

  }

  if (!is.character(sep)) {

    stop("The value supplied to the sep argument must be a character.",
         call. = FALSE)

  }

  if (length(files) > 1) {

    xy_list <- lapply(files, utils::read.csv,
                      header = header,
                      sep = sep)

    names(xy_list) <- gsub("\\..*","", basename(files))

    for (i in 1:length(xy_list)) {

      names(xy_list[[i]]) <- c("tth", "counts")
      class(xy_list[[i]]) <- c("XY", "data.frame")

    }

    #Check that the first two columns in each dataframe are numeric
    check_xy_condition <- which(unlist(lapply(xy_list, .check_xy)) == TRUE)

    if(!length(check_xy_condition) == length(xy_list)) {

      stop("Not all files contain two columns of numeric data.",
           call. = FALSE)

    }

    class(xy_list) <- c("multiXY", "list")

    return(xy_list)

  } else {

    xy <- utils::read.csv(files, header = header, sep = sep)

    names(xy) <- c("tth", "counts")
    class(xy) <- c("XY", "data.frame")

    if(.check_xy(xy) == FALSE) {

      stop("xy files must contain two columns of numeric data.",
           call. = FALSE)

    }

    return(xy)

  }


}
