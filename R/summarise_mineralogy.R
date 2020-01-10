#' Summarise the mineralogy from multiple powdRfps and powdRafps outputs
#'
#' \code{summarise_mineralogy} creates a summary table of quantified mineral
#' concentrations across a given dataset using list of multiple \code{powdRfps}
#' or \code{powdRafps} derived from \code{fps()} and \code{afps()}, respectively.
#'
#' @param x a list of \code{powdRfps} or \code{powdRafps} objects.
#' @param type a string specifying whether the table uses all phase ID's, or
#' summarises them according to the phase name. One of \code{"all"} or \code{"grouped"}.
#' @param order a logical operator denoting whether the columns of the resulting summary
#' table are ordered in descending order according to the summed abundance of each phase
#' across the dataset.
#' @param rwp a logical operator denoting whether to include the Rwp as the final column
#' in the output. This provides an objective measure of the difference between the fitted
#' and measured patterns.
#'
#' @return a dataframe
#'
#' @examples
#' data(minerals)
#' data(soils)
#'
#' \dontrun{
#' multiple_afps <- lapply(soils, afps,
#'                         lib = minerals,
#'                         std = "QUA.1",
#'                         align = 0.2,
#'                         lod = 0.1,
#'                         amorphous = "ORG",
#'                         amorphous_lod = 1)
#'
#' sm1 <- summarise_mineralogy(multiple_afps,
#'                             type = "all",
#'                             order = TRUE)
#'
#' sm2 <- summarise_mineralogy(multiple_afps,
#'                             type = "grouped",
#'                             order = TRUE)
#'
#' sm3 <- summarise_mineralogy(multiple_afps,
#'                             type = "grouped",
#'                             order = TRUE,
#'                             rwp = TRUE)
#' }
#' @export
summarise_mineralogy <- function(x, type, order, rwp) {

#Make sure x is a list
if (!class(x) == "list") {

  stop("x must be a list of powdRfps or powdRafps objects.")

}

#Check that each sample in the list is an powdRfps or powdRafps object
if (!all(names(table(unlist(lapply(x, class)))) %in% c("powdRfps", "powdRafps"))) {

  stop("All items in x must be either powdRfps or powdRafps objects.")

}

#Check that all the items are named
if (!length(table(names(x))) == length(x)) {

  stop("Each item in x needs to be named with a unique sample ID")

}

#Make sure more than one sample is being used
if (length(x) == 1) {

  stop("The summarise_mineralogy function is for list containing more than one
       powdRfps or powdRafps object.")

}

#Check that each item in the list has a name.
if (length(stats::na.omit(names(x))) < length(x)) {

    stop("Please ensure that each item in x is provided with a name that corresponds to its sample ID.")

}

if (missing(type)) {

  stop("Please specify the type argument as one of 'all' or 'grouped'.")

}

if (missing(order)) {

  order <- FALSE

}

if (missing(rwp)) {

    rwp <- FALSE

}

if (!is.logical(rwp)) {

  stop("The rwp argument must be logical.")

}

if (!is.logical(order)) {

  stop("The order argument must be logical.")

}

if (!type %in% c("all", "grouped", "summary")) {

  stop("The type argument must be one of 'all' or 'grouped'.")

}

if (type == "summary") {

  warning("Use of 'summary' in the type argument has deprecated, please use 'grouped' instead.")

}

if (type == "all")  {

  mineralogy <- lapply(x, function(y) y$phases[c(1,4)])

} else {

  mineralogy <- lapply(x, function(y) y[[which(names(y) %in% c("phases_summary", "phases_grouped"))]])

}

if (rwp == TRUE) {

  rwp_v <- lapply(x, function(y) y$rwp)
  rwp_df <- data.frame("sample_id" = names(rwp_v),
                       "rwp" = unname(unlist(rwp_v)),
                       stringsAsFactors = FALSE)

}

#Rename columns and add sample ID as a column
for (i in 1:length(mineralogy)) {

names(mineralogy[[i]]) <- c("phase", "percent")
mineralogy[[i]]$sample_id <- names(mineralogy)[i]

}

mineralogy_long <- do.call(rbind, mineralogy)

#phase_name needs to become the column names, and phase_percent fills the cells
mineralogy_wide <- tidyr::spread(mineralogy_long, "phase", "percent")

#The columns can be ordered based on the sum of the percentages so that the most abundant phases in the dataset
#appear first

if (order == TRUE) {

mineralogy_wide <- mineralogy_wide[, c(1, (order(sapply(mineralogy_wide[-1], sum, na.rm = TRUE),
                                                           decreasing = TRUE)+1))]

}

if (rwp == TRUE) {

  mineralogy_wide <- plyr::join(mineralogy_wide, rwp_df, by = "sample_id")

}

return(mineralogy_wide)

}
