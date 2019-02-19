

#Function to plot trajectory clusters
#' @title To plot the clusters
#' @param data_clusters_list A data.frame of clusters from \code{\link{akmeans_clust}}, in which the last column represents alphabetical cluster ids (labels)
#' @param id_field Whether the first column is a unique (id) field. [default: TRUE]
#' @return data_clusters_list
#' @rawNamespace import(reshape2)
#' @export


plot_clust <- function(data_clusters_list, id_field=TRUE){

  #check if 'id_field' is a unique field
  if(id_field==TRUE){
    n_CL <- colnames(data_clusters_list)[1]
    col_names <- as.vector(data_clusters_list[,1])
    if(!length(col_names)==length(unique(col_names))){
      stop("(: The 'id_field' is not a unique field. Function terminated!!! :)")
    }
  }

  if(id_field==FALSE){
    data_clusters_list <- cbind(1:nrow(data_clusters_list), data_clusters_list)  #head(data_clusters_list)
  }

  #cluster list
  clusters <- as.vector(data_clusters_list[,ncol(data_clusters_list)])
  #data
  data_subset <- data_clusters_list[,1:(ncol(data_clusters_list)-1)] #head(data_subset)
  colnames(data_subset) <- c("code", 1:(ncol(data_clusters_list)-2))

  data_subset <- as.data.frame(data_subset) #head(data_subset)

  data_subset.melted <- suppressWarnings(melt(data_subset, id="code"))  #head(data_subset.melted) #length(unique(data_subset$code)) #nrow(data_subset)

  #append cluster list
  data_subset.melted <- cbind(data_subset.melted, rep(clusters, ncol(data_subset)-1))#nrow(data.long.melted)

  colnames(data_subset.melted) <- c("id","Year","value", "clusters")

  #plot
   ggplot(data_subset.melted, aes(x=Year, y=value,
                                     group=id, color="grey")) +
        geom_line() +
        #stat_summary(fun.y=mean, geom="line", aes(group=clusters), color="red", size=1) +
        # facet_wrap(~clusters, scales = "free") +
        facet_wrap(~clusters, scales = "fixed") +
        #scale_colour_brewer(palette = "Set1") +
        geom_smooth(method="lm", aes(group=clusters), color="black", size=1) +
        theme_minimal() #clusters

}

#plot_clust(result)
