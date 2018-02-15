xrd.fuzzy <- function(xrpd.pca, sample.id, nclust, it) {

  library(e1071)

  PC1 <- xrpd.pca$PC1

  PC2 <- xrpd.pca$PC2

  PC3 <- xrpd.pca$PC3

  PC.Matrix <- cbind(PC1, PC2, PC3)

  cluster_list <- list()
  cluster_pc <- list()


  cluster.1 <- e1071::cmeans(PC.Matrix, nclust, iter.max = it, verbose = FALSE, method = "cmeans", m = 2)


  ####### Now onto plotting the data ###########

  #I'm now going to create a data frame that will be used for plotting data

  plotting_data <- data.frame(PC1, PC2, PC3)

  plotting_data["sample"] <- as.character(sample.id)

  plotting_data["cluster"] <- as.character(cluster.1$cluster)

  Membership.coefficients <- data.frame(cluster.1$membership)

  plotting_data <- data.frame(plotting_data, Membership.coefficients)

  return(plotting_data)

}
