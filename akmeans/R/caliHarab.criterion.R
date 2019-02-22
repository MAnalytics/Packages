#' @title Calinski and Harabatz quality criterion
#' @description Function to calculate the Calinski-Harabatz score for a given clustering solution
#' @param clustr A data.frame of cluster trajectories (e.g. from \code{\link{akmeans.clust}}), in which the last column represents alphabetical cluster ids (labels)
#' @param id_field Whether the first column is a unique (id) field. Default: \code{TRUE}.
#' @references \code{Calinski T, Harabasz J (1974) A dendrite method for cluster analysis. Commun Stat 3(1):1-27}

caliHarab.criterion <- function(clustr, id_field = FALSE){
  print(clustr)
}
