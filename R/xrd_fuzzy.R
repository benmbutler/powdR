#' Fuzzy clustering of XRPD data based on first three principal components
#'
#' \code{xrd.fuzzy} Applies the Fuzzy c-means clustering algorith from package
#' e1071 (\code{cmeans}) to the first three principal components of an XRPD
#' data set.
#'
#' @param pca a dataframe containing PC1, PC2 and PC3 scores
#' @param sample.id the sample ID's to be assigned to each row of the pca dataframe
#' @param nclust the number of clusters to compute
#' @param it the maximum number of iterations
#'
#' @return a dataframe containing cluster assignments and membership coefficients
xrd.fuzzy <- function(pca, sample.id, nclust, it) {

  PC1 <- pca$PC1

  PC2 <- pca$PC2

  PC3 <- pca$PC3

  PC.Matrix <- cbind(PC1, PC2, PC3)

  cluster_list <- list()
  cluster_pc <- list()

  cluster.1 <- e1071::cmeans(PC.Matrix, nclust, iter.max = it, verbose = FALSE,
                             method = "cmeans", m = 2)

  out <- data.frame(PC1, PC2, PC3)

  out["sample"] <- as.character(sample.id)

  out["cluster"] <- as.character(cluster.1$cluster)

  Membership.coefficients <- data.frame(cluster.1$membership)

  out <- data.frame(out, Membership.coefficients)

  return(out)

}
