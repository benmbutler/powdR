#This function carries out PCA on a list of treated xrd patterns
xrd.pca <- function(xrd) {

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

  out <- list("pca" = xrpd_pca_out, "pca_variance" = xrpd_cum_prop)

  return(out)
}
