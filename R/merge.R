#' Merge two powdRlib objects
#'
#' \code{merge.powdRlib} allows two \code{powdRlib} objects to be merged
#' into a single powdRlib object.
#'
#' @param x a \code{powdRlib} object.
#' @param y a \code{powdRlib} object
#' @param ... other arguments
#'
#' @return a \code{powdRlib} object.
#'
#' @method merge powdRlib
#'
#' @examples
#' #Load the minerals library
#' data(minerals)
#'
#' #Load the rockjock library
#' data(rockjock)
#'
#' #interpolate minerals library onto same 2theta as rockjock
#' minerals_i <- interpolate(minerals, new_tth = rockjock$tth)
#'
#' #merge the libraries
#' merged_lib <- merge(rockjock, minerals_i)
#' @export
merge.powdRlib <- function(x, y, ...) {

  if (!class(y) == "powdRlib") {

    stop("Data supplied to y must be a powdRlib object.",
         call. = FALSE)

  }

  #Check that the 2theta scales are the same
  if (!all.equal(x$tth, y$tth)) {

    stop("The 2theta scales in x and y must be identical.
         Use the interpolate function to ensure that the 2theta
         scales are identical and then try again.")

  }

  #Check if there are any refs in y that are already in x
  duplicate_check <- which(names(y$xrd) %in% names(x$xrd))

  if(length(duplicate_check) > 0) {

    stop("There are reference patterns in y that are already in the x. Either rename
         or remove these reference patterns in y, and try again.",
         call. = FALSE)

  }

  x$xrd <- data.frame(x$xrd, y$xrd)
  x$phases <- rbind(x$phases, y$phases)

  class(x) <- "powdRlib"

  return(x)

}
