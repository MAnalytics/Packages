#' @title Calinski and Harabatz quality criterion
#' @description Function to calculate the Calinski-Harabatz score for a given clustering solution
#' @param clustr A data.frame of cluster trajectories (e.g. from \code{\link{akmeans.clust}}), in which the last column represents alphabetical cluster ids (labels)
#' @param id_field Whether the first column is a unique (id) field. Default: \code{TRUE}.
#' @param y.scaling To set the vertical scales of the cluster panels. Options are: \code{"fixed"}: uses uniform scale for all panels, \code{"free"}: uses variable scales for panels.
#' @examples
#'
#' @references \code{Calinski T, Harabasz J (1974) A dendrite method for cluster analysis. Commun Stat 3(1):1-27}
sam
