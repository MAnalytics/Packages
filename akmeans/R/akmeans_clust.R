
#Function to cluster trajectories given a list of initial centroids
#' @title akmeans_clust
#' @description GGGGGGGGG
#' @param dat A matrix or data.frame with each row representing the trajectory of observations of a unique location. The columns show the observation at consecutive time steps.
#' @param id_field Whether the first column is a unique (id) field. [default: FALSE]
#' @param init_centroids initialisation method [default: "lpm" - linear partitioning medoids @seealso \code{\link{lpm_centroids}}]
#' @param n_clusters number of clusters to generate [default (minimum value): 3]
#' @return data_clusters_list
#' @rawNamespace import(kml)
#' @export

akmeans_clust <- function(dat, id_field = FALSE, init_centroids = "lpm", n_clusters = 3){

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

  #create centroids based on "lpm" initialisation method
  lpm_centroid <- lpm_centroids(dat, id_field = FALSE, n_centroids=n_clusters)

  #generate clusters and append to data
  clusters <- affectIndivC(dat,  lpm_centroid)  #head(crime_Prop)

  if(id_field==TRUE){
    dat <- as.data.frame(n_CL=col_names, dat)
  }

  #combine the data and clusters
  data_clusters_list <- cbind(dat, clusters)

  return(data_clusters_list)
}

