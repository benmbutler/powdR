#' Subset a powdRlib object
#'
#' \code{subset.powdRlib} is designed to provide an easy way of
#' subsetting a \code{powdRlib} object by defining the phase ID's
#' that the user wishes to either keep or remove.
#'
#' @param x a \code{powdRlib} object.
#' @param refs a string of the phase IDs or names of reference patterns to be subset. The ID's
#' or names supplied must be present within the \code{lib$phases$phase_id} or
#' \code{lib$phases$phase_name} columns.
#' @param mode denotes whether the phase IDs or names defined in the \code{refs} argument are
#' retained (\code{"keep"}) or removed (\code{"remove"}).
#' @param ... other arguments
#'
#' @return A \code{powdRlib} object.
#'
#' @method subset powdRlib
#'
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

  if (!identical(names(x$xrd), x$phases$phase_id)) {

    stop("The order of the powRlib objects xrd dataframe does not match that of the phases dataframe.",
         call. = FALSE)

  }

  if(missing(mode)) {

    stop("The mode argument must be defined.",
         call. = FALSE)

  }

  if(!mode %in% c("keep", "remove")) {

  stop("The mode argument must be either 'keep' or 'remove'.",
       call. = FALSE)

  }

  if(missing(refs)) {

    stop("The ID's or names of phases to keep or remove must be set using the refs argument",
         call. = FALSE)

  }

  #if(!length(which(refs %in% names(x[[1]]))) == length(refs)) {

  #  stop("Not all phase ID's or names defined in the refs argument are present within
  #       your powdRlib object.")

  #}

  if (mode == "keep") {

    keep_index <- which(x$phases$phase_id %in% refs | x$phases$phase_name %in% refs)

    x[[1]] <- x[[1]][keep_index]
    x[[3]] <- x[[3]][keep_index, ]


  } else {

    remove_index <- which(x$phases$phase_id %in% refs | x$phases$phase_name %in% refs)

    x[[1]] <- x[[1]][-remove_index]
    x[[3]] <- x[[3]][-remove_index, ]


  }

  return(x)

  }
