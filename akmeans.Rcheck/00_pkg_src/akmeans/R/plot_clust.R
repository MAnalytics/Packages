

#Function to plot trajectory clusters
#' @title To plot the clusters
#' @param data_clusters_list A data.frame of clusters from \code{\link{akmeans_clust}}. The last column should represent numeric cluster ids (labels)
#' @param id_field Whether the first column is a unique (id) field. [default: FALSE]
#' @return data_clusters_list
#' @rawNamespace import(ggplot2)
#' @export


plot_clust <- function(data_clusters_list, id_field=FALSE){

  #check if 'id_field' is a unique field
  if(id_field==TRUE){
    n_CL <- colnames(data_clusters_list)[1]
    col_names <- as.vector(data_clusters_list[,1])
    if(!length(col_names)==length(unique(col_names))){
      stop("(: The 'id_field' is not a unique field. Function terminated!!! :)")
    }
  }

  if(id_field==FALSE){
    data_clusters_list <- cbind(1:nrow(data_clusters_list), data_clusters_list)
  }


  ##dat <- dat[,2:ncol(dat)]
  #check if the 'id_field' is a unique field


    ##crime_Prop.append <- cbind(1:nrow(crime_Prop), crime_Prop) #head(crime_Prop.append)

    #data.append <- data  #testing the proportion

    ##colnames(crime_Prop.append) <- c("code", "1", "2", "3", "4", "5", "6", "7", "8",
                                 ##"9", "10", "11", "12", "13", "14", "15") #head(data.append)
    ##crime_Prop.append <- as.data.frame(crime_Prop.append)

    ##crime_Prop.append.melted <- melt(crime_Prop.append, id="code")#head(data., id="code"
    ##head(crime_Prop.append.melted)

    #data.append <- cbind(data, part2)
    #data.long.melted <- melt(data, id="code")#head(d
    ##crime_Prop.append.melted <- cbind(crime_Prop.append.melted, rep(clusters, 15))#nrow(data.long.melted)

    ##colnames(crime_Prop.append.melted) <- c("OAcode","Year","Crime_Count", "clusters")

    #convert numbers to letters
    ##clusters <- letters[data.long.melted$clusters]
    ##clusters <- toupper(clusters) #head(cluster_letters)

    ##clusters <- list_Letters[crime_Prop.append.melted$clusters]
    ##crime_Prop.append.melted$clusters <- clusters
    ##colnames(crime_Prop.append.melted) <- c("OAcode","Year","Crime_Count", "clusters")
    ##head(crime_Prop.append.melted)

    ##ggplot(crime_Prop.append.melted, aes(x=Year, y=Crime_Count,
                                     ##group=OAcode, color="clusters")) +
        ##geom_line() +
        ##stat_summary(fun.y=mean, geom="line", aes(group=clusters), color="black", size=1) +
        # facet_wrap(~clusters, scales = "free") +
        ##facet_wrap(~clusters, scales = "fixed") +
        #scale_colour_brewer(palette = "Set1") +
        ##theme_minimal() #clusters

}
