xrd.fuzzy <- function(xrpd.pca, sample.id, nclust, it) {

  PC1 <- xrpd.pca$PC1

  PC2 <- xrpd.pca$PC2

  PC3 <- xrpd.pca$PC3

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
