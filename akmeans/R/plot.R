

#Function to plot trajectory clusters
#' @title plot clusters
#' @param data_clusters_list A matrix or data.frame of clusters usually from \code{\link{akmeans_clust}}. The last column represent the cluster ids (labels)
#' @param id_field Whether the first column is a unique (id) field. [default: FALSE]
#' @param init_centroids initialisation method [default: "lpm" - linear partitioning medoids @seealso \code{\link{lpm_centroids}}]
#' @param n_clusters number of clusters to generate [default (minimum value): 3]
#' @return data_clusters_list
#' @rawNamespace import(kml)
#' @export
