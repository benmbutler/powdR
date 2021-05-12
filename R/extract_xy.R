#' Import and extract XY data from proprietary files
#'
#' \code{extract_xy} is a wrapper for \code{read_xyData} of the \code{rxylib} package,
#' which extracts the xy data from various proprietary formats of X-ray powder
#' diffraction data using the code{xylib} C++ library. For more information see
#' \code{?rxylib} and \code{?rxylib::read_xyData}.
#'
#' @param files path of the file(s) to be imported.
#'
#' @return If only one path is supplied then a dataframe with 2 columns is returned,
#' the first being the 2theta axis and the second being the count intensities. If more
#' than one path is supplied then a list is returned, with each item in the list being
#' a dataframe as already described.
#'
#' @examples
#' #load example RAW file
#' file <- system.file("extdata/D5000/RAW/D5000_1.RAW", package = "powdR")
#' raw1 <- extract_xy(file)
#'
#' #Load multiple RAW files
#' files <- dir(system.file("extdata/D5000/RAW", package = "powdR"),
#'              full.names = TRUE)
#' raw_list <- extract_xy(files)
#'
#' class(raw_list)
#'
#' \dontrun{
#' plot(raw_list, wavelength = "Cu")
#' plot(raw_list, wavelength = "Cu", interactive = TRUE)
#' }
#'
#' @export
extract_xy <- function(files) {

  if (length(files) > 1) {

    raw_list <- lapply(files, rxylib::read_xyData)

    names(raw_list) <- gsub("\\..*","", basename(files))

    for (i in 1:length(raw_list)) {

      raw_list[[i]] <- data.frame(raw_list[[i]]$dataset[[1]]$data_block)

      names(raw_list[[i]]) <- c("tth", "counts")
      class(raw_list[[i]]) <- c("XY", "data.frame")

    }

    class(raw_list) <- c("multiXY", "list")

    return(raw_list)

  } else {

  raw1 <- rxylib::read_xyData(file = files)

  raw1 <- data.frame(raw1$dataset[[1]]$data_block)

  names(raw1) <- c("tth", "counts")

  class(raw1) <- c("XY", "data.frame")

  return(raw1)

  }

}
