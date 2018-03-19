#' Principal component analysis and pre-treatment of XRPD data
#'
#' \code{xrd.pca} allows for principal components analysis to be applied to
#' XRPD subjected to various pre-treatment options.
#'
#' Four pre-treament options are available. These include peak alignment using
#' \code{multi.xrd.align}, binning using the \code{xrd.bin}, square root transform,
#' and normalisation using \code{mc}.
#'
#' @param xrd a list of XRPD dataframe (2theta and counts)
#' @param align logical. If TRUE then alignment will be applied
#' @param align.standard required if align = TRUE. Specifies the XRPD
#' dataframe to be used as the standard for alignment
#' @param align.xmin required if align = TRUE. Specifies the minimum 2theta
#' value used during alignment
#' @param align.xmax required if align = TRUE. Specifies the maximum 2theta
#' value used during alignment
#' @param align.xshift required if align = TRUE. Specifies the xmaximum (positive
#' and negative) 2theta shift that can be used during alignment
#' @param bin logical. If TRUE then binning will be applied
#' @param bin.width required if bin = TRUE. Specifies the bin width to be used
#' @param square.root logical. If TRUE then the count intensities of all XRPD data
#' will be square root transformed
#' @param normalise logical. If TRUE then all XRPD data will be mean centred using
#' \code{mc}
#'
#' @return a list with components:
#' \item{pca}{a dataframe of PCA scores}
#' \item{pca_variance}{a vector of the cumulative proportion of variance explained
#' by each principal component}
#' \item{xrd}{a list of the treated XRPD data}
xrd.pca <- function(xrd, align, align.standard, align.xmin, align.xmax, align.xshift,
                    bin, bin.width, square.root, normalise) {

  if(missing(align)) {
    align <- FALSE
  }

  if(missing(bin)) {
    bin <- FALSE
  }

  if(missing(square.root)) {
    square.root <- FALSE
  }

  if(missing(normalise)) {
    normalise <- FALSE
  }

  if (align == TRUE) {

    if(missing(align.standard)) {
      stop("The align.standard argument must be provided if align = TRUE")
    }

    if(missing(align.xmin)) {
      stop("The align.xmin argument must be provided if align = TRUE")
    }

    if(missing(align.xmax)) {
      stop("The align.xmax argument must be provided if align = TRUE")
    }

    if(missing(align.xshift)) {
      stop("The align.xshift argument must be provided if align = TRUE")
    }

    xrd <- multi.xrd.align(xrd, xrd.standard = align.standard, xmin = align.xmin,
                           xmax = align.xmax, xshift = align.xshift)

  }

  if (bin == TRUE) {

    if(missing(bin.width)) {
      stop("The bin.width argument must be provided if align = TRUE")
    }

    xrd <- lapply(xrd, xrd.bin, bin.size = bin.width)

  }

  if (square.root == TRUE) {

    for (i in 1:length(xrd)) {
      xrd[[i]][[2]] <- sqrt(xrd[[i]][[2]])
    }

  }

  if (normalise == TRUE) {
    xrd <- lapply(xrd, mc)
  }

  xrpd_m <- matrix(nrow = length(xrd), ncol = nrow(xrd[[1]]))

  #Extract the counts only from the treated data
  for (i in 1:length(xrd)) {
    xrpd_m[i,] <- xrd[[i]][,2]
  }

  #Convert to data frame
  xrpd_m <- data.frame(xrpd_m)

  #Extract sample ID's
  sample_id <- names(xrd)

  #PCA of this data
  xrpd_pca <- stats::prcomp(xrpd_m[,c(1:ncol(xrpd_m))])

  #Extract the cumulative variance explained by all principal components
  pca_var <- apply(xrpd_pca$x, 2, stats::var)
  props <- pca_var / sum(pca_var)
  xrpd_cum_prop <- cumsum(props)

  #I want to have a data frame with full names that I can export
  xrpd_pca_out <- data.frame(sample_id, xrpd_pca$x)

  out <- list("pca" = xrpd_pca_out, "pca_variance" = xrpd_cum_prop, "xrd" = xrd)

  return(out)
}
