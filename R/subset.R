#' Subset a powdRlib object
#'
#' \code{subset.powdRlib} is designed to provide an easy way of
#' subsetting a \code{powdRlib} object by defining the phase ID's
#' that the user wishes to either keep or remove.
#'
#' The only mandatory argument is \code{x}, which must be a powdRlib object.
#'
#' @param x a powdRlib object
#' @param refs a strong of the phase ID's of reference patterns to be subset
#' @param mode denotes whether the phase ID's defined in the \code{refs} argument are
#' retained (\code{"keep"}) or removed (\code{"remove"}).
#' @param ... other arguments
#'
#' @method subset powdRlib
#' @examples
#' #Load the minerals library
#' data(minerals)
#'
#' minerals_keep <- subset(minerals,
#'                         refs = c("QUA.1", "QUA.2"),
#'                         mode = "keep")
#'
#' minerals_remove <- subset(minerals,
#'                           refs = c("QUA.1", "QUA.2"),
#'                           mode = "remove")
#' @export
subset.powdRlib <- function(x, refs, mode, ...) {

  if(missing(mode)) {

    stop("The mode argument must be defined.")

  }

  if(!mode %in% c("keep", "remove")) {

  stop("The mode argument must be either 'keep' or 'remove'.")

  }

  if(missing(refs)) {

    stop("Phase ID's to keep or remove must be set using the refs argument")

  }

  if(!length(which(refs %in% names(x[[1]]))) == length(refs)) {

    stop("Not all phase ID's defined in the refs argument are present within
         your powdRlib object.")

  }

  if (mode == "keep") {

    keep_index_xrd <- which(names(x[[1]]) %in% refs)
    keep_index_phases <- which(x[[3]][[1]] %in% refs)

    x[[1]] <- x[[1]][keep_index_xrd]
    x[[3]] <- x[[3]][keep_index_phases, ]

    #Ensure they're ordered
    x[[1]] <- x[[1]][order(names(x[[1]]))]
    x[[3]] <- x[[3]][order(x[[3]][[1]]),]

  } else {

    remove_index_xrd <- which(names(x[[1]]) %in% refs)
    remove_index_phases <- which(x[[3]][[1]] %in% refs)

    x[[1]] <- x[[1]][-remove_index_xrd]
    x[[3]] <- x[[3]][-remove_index_phases, ]

    #Ensure they're ordered
    x[[1]] <- x[[1]][order(names(x[[1]]))]
    x[[3]] <- x[[3]][order(x[[3]][[1]]),]

  }

  return(x)

  }
