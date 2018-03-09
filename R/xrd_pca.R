#This function carries out PCA on a list of treated xrd patterns
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

    xrd <- lapply(xrd, mean.centre)

  }


  xrpd_m <- matrix(nrow = length(xrd), ncol = nrow(xrd[[1]]))

  for (i in 1:length(xrd)) {
    xrpd_m[i,] <- xrd[[i]][,2]
  }

  #Convert to data frame
  xrpd_m <- data.frame(xrpd_m)

  #Extract sample ID's
  sample_id <- names(xrd)

  #PCA of this data
  xrpd_pca <- prcomp(xrpd_m[,c(1:ncol(xrpd_m))])

  #Extract the cumulative variance explained by all principal components
  pca_var <- apply(xrpd_pca$x, 2, var)
  props <- pca_var / sum(pca_var)
  xrpd_cum_prop <- cumsum(props)

  #I want to have a data frame with full names that I can export
  xrpd_pca_out <- data.frame(sample_id, xrpd_pca$x)

  out <- list("pca" = xrpd_pca_out, "pca_variance" = xrpd_cum_prop, "xrd" = xrd)

  return(out)
}
