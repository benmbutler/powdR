.mean_center <- function(x) {

  x[[2]] <- x[[2]] - mean(x[[2]])

  return(x)

}

.xrpd_bin <- function(smpl, bin_size) {

  #Split the x-axis into chunks defined by bin_size
  x <- split(smpl[[1]], ceiling(seq_along(smpl[[1]])/bin_size))

  #calculate the mean of each chunk
  x_mean <- c()
  for(i in 1:length(x)) {
    x_mean[i] <- mean(x[[i]])
  }

  #Split the y-axis into chunks defined by bin_size
  y <- split(smpl[[2]], ceiling(seq_along(smpl[[2]])/bin_size))

  #calculate the mean of each chunk
  y_mean <- c()
  for(i in 1:length(y)) {
    y_mean[i] <- mean(y[[i]])
  }

  #create a dataframe of binned data
  out <- data.frame("tth" = x_mean,
                    "counts" = y_mean)

  return(out)

}


.extract_coords <- function(x) {

  coords <- data.frame(x$ind$coord)

  return(coords)

}

.extract_loadings <- function(x) {

  loadings <- data.frame(sweep(x$var$coord,
                               2,
                               sqrt(x$eig[1:ncol(x$var$coord),1]),
                               FUN="/"))

  return(loadings)

}


#' PCA of XRPD data
#'
#' \code{xrpd_pca} is used to apply principal component analysis to X-ray powder
#' diffraction data.
#'
#' Applies data pre-treatment and principal components analysis to XRPD data based
#' based on the protocols detailed in Butler et al. (2020).
#'
#' @param x A multiXY list containing the XRPD data, where each item in the
#' list is a 2 column XY dataframe defining the x (2theta) and y (counts)
#' axes of each measurement. Each item in the list must have a name corresponding
#' to a unique sample ID.
#' @param mean_center A logical argument defining whether mean centering
#' is applied to the XRPD data (default \code{= TRUE}).
#' @param bin_size An integer between 1 and 10 defining whether to bin the
#' XRPD data to a lower resolution. This \code{bin_size} defines the number of
#' data points used in each bin.
#' @param root_transform An integer between 1 and 8 defining the root transform
#' to apply to the XRPD data
#' @param components An integer defining the number of principal components to
#' include in the output. Must be at least 1 less than the
#' number of XRPD patterns in the dataset (the default).
#'
#' @return a list with components:
#' \item{coords}{a dataframe containing the sample ID's for each sample and the
#' PCA coordinates for each dimension}
#' \item{loadings}{a dataframe containing the 2theta axis and the loading of
#' each dimension}
#' \item{eig}{a dataframe summarising the variance explained by each dimension}
#'
#' @examples
#' data(rockjock_mixtures)
#'
#' x1 <- xrpd_pca(rockjock_mixtures,
#'                mean_center = TRUE,
#'                bin_size = 1,
#'                root_transform = 1)
#'
#' #Plot the loading of dimension 1
#'
#' plot(x = x1$loadings$tth,
#'      y = x1$loadings$Dim.1,
#'      type = "l")
#'
#' \dontrun{
#' #Fit loading 1 to the rockjock library
#' f1 <- fps_lm(rockjock,
#'              smpl = data.frame("tth" = x1$loadings$tth,
#'                                "counts" = x1$loadings$Dim.1),
#'              refs = rockjock$phases$phase_id,
#'              std = "QUARTZ",
#'              align = 0,
#'              p = 0.05)
#'
#' plot(f1, wavelength = "Cu", interactive = TRUE)
#' }
#'
#' @references
#' Butler, B.M., Sila, A.M., Shepherd, K.D., Nyambura, M., Gilmore, C.J., Kourkoumelis,
#' N., Hillier, S., 2019. Pre-treatment of soil X-ray powder diffraction data for
#' cluster analysis. Geoderma 337, 413-424. doi:10.4236/ampc.2013.31A007
#'
#' @export
xrpd_pca <- function(x, mean_center, bin_size,
                     root_transform, components) {

  #-----------------------------------
  #CONDITIONS FOR FUNCTION ARGUMENTS
  #-----------------------------------

  #Make sure the xrpd data is present and is a list
  if (missing(x) | !is.list(x)) {

    stop("The x argument must be a list, with each item of the list being
         a 2 column dataframe.",
         call. = FALSE)

  }

  #If mean_center is missing, make it TRUE
  if (missing(mean_center)) {

    mean_center <- TRUE

  }

  if (!is.logical(mean_center)) {

    stop("The mean_center argument must be logical",
         call. = FALSE)

  }

  if (missing(bin_size)) {

    bin_size <- 1

  }

  if (!bin_size %in% c(1:10)) {

    stop("The bin_size argument must be an integer between 1 and 10",
         call. = FALSE)

  }

  #Make sure bin size is a whole number
  bin_size <- round(bin_size)

  #If root_transform is missing, set it to 1
  if (missing(root_transform)) {

    root_transform <- 1

  }

  if (!root_transform %in% c(1:8)) {

    stop("The root_transform argument must be an integer between 1 and 8",
         call. = FALSE)

  }

  root_transform <- round(root_transform)

  if (missing(components)) {

    components <- length(x) - 1

  }

  if (components >= length(x)) {

    stop("The number of components must be less than the number of XRPD patterns",
         call. = FALSE)

  }

  #------------------------------
  #DATA TRANSFORMS
  #------------------------------

  #Root transform
  if (root_transform > 1) {

    x <- lapply(x, function (x) data.frame("tth" = x[[1]],
                                                 "counts" = x[[2]]^(1/root_transform)))

  }

  #Binning
  if (bin_size > 1) {

    x <- lapply(x, .xrpd_bin, bin_size = bin_size)

  }

  #Mean centering
  if (mean_center == TRUE) {

    x <- lapply(x, .mean_center)

  }

  #Now extract the 2theta axis from the first x item (they will all
  #be the same at this point)
  tth <- x[[1]][[1]]

  #Convert the xrpd data to a dataframe comprised purely of the counts
  x_df <- x
  #Extract the sample ID
  sample_id <- names(x_df)

  x_df <- data.frame(lapply(x_df, function (x) x[[2]]))

  #Convert to row-wise dataframe. The sample ID's will be the rownames
  x_df <- data.frame(t(x_df))

  #-------------------------------
  #Principal component analysis
  #-------------------------------

  pca <- FactoMineR::PCA(x_df,
                         scale.unit = FALSE,
                         ncp = components,
                         graph=F)


  #Extract coordinates
  coords <- .extract_coords(pca)
  #reset rownames
  rownames(coords) <- NULL

  #Extract loadings
  loadings <- .extract_loadings(pca)


  out <- list("coords" = data.frame("sample_id" = sample_id,
                                    coords),
              "loadings" = data.frame("tth" = tth,
                                      loadings),
              "eig" = data.frame(factoextra::get_eigenvalue(pca)))


  return(out)

}
