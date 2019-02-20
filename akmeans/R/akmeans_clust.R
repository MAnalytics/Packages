
#' @title akmeans_clust
#' @description This function group trajectories based on a given list of initial centroids
#' @param traj A matrix or data.frame with each row representing the trajectory of observations of a unique location. The columns show the observations at consecutive time steps.
#' @param id_field Whether the first column is a unique (\code{id}) field. Default: \code{FALSE}
#' @param init_method initialisation method. Specifying a method to determine the initial centroids for clustering. Default: \code{"lpm"} - linear partitioning medoids @seealso \code{\link{lpm_centroids}}]
#' @param n_clusters number of clusters to generate. Default: \code{3}: (minimum value)
#' @usage akmeans_clust(traj, id_field = FALSE, init_method = "lpm", n_clusters = 3)
#' @details Given a list of trajectories represented in a matrix or data.frame, and a method for choosing initial cluster centroids (e.g. \code{\link{lpm_centroids}}), a list of clusters is generated after a limited number of iterations.
#' traj <- assault_data
#' print(traj)
#' result <- akmeans_clust(traj, id_field = TRUE, init_method = "lpm", n_clusters = 3)
#' plot_clust(result)
#' @return The original (\code{traj}) data with cluster label appended
#' @rawNamespace importFrom(kml, affectIndivC)
#' @export

akmeans_clust <- function(traj, id_field = FALSE, init_method = "lpm", n_clusters = 3){

  dat <- traj
  #check if there is id_field
  #check if there is unique(id) field
  if(id_field==TRUE){
    n_CL <- colnames(dat)[1]
    col_names <- as.vector(dat[,1])
    dat <- dat[,2:ncol(dat)]
    #check if the 'id_field' is a unique field
    if(!length(col_names)==length(unique(col_names))){
      stop("(: The 'id_field' is not a unique field. Function terminated!!! :)")
    }
  }

  #specify initialisation method
  if(init_method=="lpm"){
  #create centroids based on "lpm" initialisation method
    centroids <- lpm_centroids(dat, id_field2 = FALSE, n_centroids=n_clusters)
  }

  #generate clusters and append to data
  clusters <- affectIndivC(dat,  centroids)  #head(crime_Prop)
  #clusters <- 1

  if(id_field==TRUE){
    col_names <- matrix(col_names,,1)
    colnames(col_names) <- n_CL
    dat <- as.data.frame(cbind(col_names, dat))
  }

  #combine the data and clusters
  clusters <- alphaLabel(clusters)
  #colnames(clusters) <- "clusters"
  data_clusters_list <- cbind(dat, clusters)

  return(data_clusters_list)
}

#akmeans_clust(dat, id_field = TRUE, init_centroids = "lpm", n_clusters = 4)
