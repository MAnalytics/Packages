

#Function to create the initial centroids based on based on linear partitioning medoids
#' @title Linear Partition Medoids (LPM) Centroids
#' @param dat A matrix or data.frame with each row representing the trajectory of observations of a unique location. The columns show the observation at consecutive time steps.
#' @param n_centroids Number of initial (linear) centroids to generate based on lpm technique (Adepeju et al. 2019, under review)
#' @param id_field2 Whether the first column is a unique (id) field. [default: FALSE]
#' @return l_centroids
#' @rawNamespace import(reshape2, Hmisc, stats, utils)
#' @export

lpm_centroids <- function(dat, id_field2 = FALSE, n_centroids=3){

  #library(reshape2)
  #n_centroids <- n_centroids

  if(id_field2 ==  TRUE){
    dat  <- dat[,2:ncol(dat)]
  }

  if( n_centroids < 3 | n_centroids > nrow(dat) | n_centroids > 38 ){
    flush.console()
    print("*******Error!********")
    if(n_centroids < 3){
      flush.console()
      print("Number of clusters need to be less than 2")
    }

    if(n_centroids > nrow(dat)){
      flush.console()
      print("Number of clusters cannot be greater than the number of trajectories")
    }

    if(n_centroids > 38){
      flush.console()
      print("Number of clusters is too large! Please, enter a smaller number (< 40)")
    }
  } else {
    #----------------------------------------------------------
  #create time lists for linear regression.
  time_ <- NULL
  for(k in 1:nrow(dat)){  #k<-1
    time_ <- rbind(time_, 1:ncol(dat))
  }
  time_ <- as.data.frame(time_)
  #-----------------------------------------------------------

#Linear partition medoids (LPM)
#-----------------------------------------------------------
#fit linear regression lines to each trajectory and extract the slope and the intersect

sl_List <- NULL
for(i in 1:nrow(dat)){ #i<-1
  b=coefficients(lm(as.numeric(as.character(dat[i,]))~as.numeric(as.character(time_[i,]))))
  sl_List <- rbind(sl_List, cbind(as.numeric(b[1]),as.numeric(b[2]))) #slope and intersect of each trajectory
}

#convert to dataframe
sl_List <- as.data.frame(sl_List)
sl_List <- as.data.frame(cbind(1:nrow(sl_List), sl_List))
colnames(sl_List) <- c("sn", "intersect","slope")

#--------------------------------------
#To partition the slope_intersect into n_centroid groups and select the medoid (centroid)

  n_Clusters <- n_centroids

  #partitioning
  keep_sl_ListGroup <- split(sl_List, cut2(sl_List$slope, g=n_Clusters))
  length(keep_sl_ListGroup)
  #compute the medoid of each partition
  mean_Slopes_Intersect <- NULL
  for(j in 1:n_Clusters){ #
    s_dty <- as.data.frame(keep_sl_ListGroup[j][1],,2)[,3]
    i_dty <- as.data.frame(keep_sl_ListGroup[j][1],,2)[,2]
    mean_Slopes_Intersect <- rbind(mean_Slopes_Intersect, cbind(mean(i_dty), mean(s_dty)))
  }

  #------------------------
  #generate a list of time-dependent linear regression lines,
  #using the slopes and intersects
  centers_List <- NULL
  for(k in 1:nrow(mean_Slopes_Intersect)){ #k<-1
    centers_List <- rbind(centers_List, (mean_Slopes_Intersect[k,1] + (mean_Slopes_Intersect[k,2]*(1:ncol(dat)))))
  }
  centers_List <- as.data.frame(centers_List)
  l_centroids <- centers_List

  flush.console()
  print("*---Linear partitioned medoids---*")
  print(paste(n_centroids,"- lpm centroids generated!"))
  return(l_centroids)
  }

}   #
#visualise result with trajectory...
#result_ <- lpm_centroids(dat, n_centroids=41, id_field = TRUE)



#dat <- read.table(file="samp.csv", sep=",", head=FALSE)
#dat <- Null_Filler(dat, id_field=TRUE, fill_with="Mean_col")




