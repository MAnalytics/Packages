

#Outlier detector
#' @title Outlier detection in longitudinal or repeated observations
#' @description Detect outlier in a longitudinal or repeated data.
#' This function identify the outlier observations according to a specified method.
#' A matrix, 'outlier_mat', is created with entries 'TRUE' or 'FALSE' indicating whether or not an observation is an outlier.
#' The final list of outlier trajectories is determined by the 'hortz_tolerance' parameter i.e. how many observation in a trajectory exceed the 'threshold' value.
#' @param dat A matrix or data.frame with each row representing the trajectory of observations of a unique location. The columns show the observation at consecutive time steps.
#' @param id_field Whether the first column is a unique (id) field. [default: FALSE]
#' @param method Specify the method for identifying the outlier. Available methods: (1) "quantile" (2) "manual" - a user-defined value
#' @param threshold Value in which an observation must exceed in order to be flagged as outlier. Depending on the method specified: (1) for "quantile" method, enter a numeric vector of probabilities with values in [0,1], (2) for "Manual" method: a user-specified value.
#' @param hortz_tolerance Specifying the number of observations of a trajectory that have to exceed the cut-off 'threshold' value in order for the trajectory to be flagged as outlier. [default: 1]
#' @param replace_with Value to replace the outlier observation with. Values to replace with [Values: "Mean_col" or "Mean_row"]. The default is "Mean_row", meaning to imput the average values of the field in which the observation is located.
#' @return dat_
#' @export

outlier_detector <- function(dat, id_field = FALSE, method = "quantile", threshold = 0.95, hortz_tolerance = 1, replace_with = "Mean_row"){

  #dat <- read.table(file="C:/Users/monsu/Documents/GitHub/Packages/samp.csv", sep=",", head=FALSE)
  #dat <- Null_Filler(dat, id_field=TRUE, replace_with="Mean_col")

  #back up data
  b_dat <- dat
  #remove the id field
  if(id_field ==  TRUE){
    dat  <- dat[,2:ncol(dat)]
  }

  #matrix to store the outlier information [TRUE or FALSE]
  outlier_mat <- matrix(FALSE, nrow(dat), ncol(dat))

  #------------------------------------
  #if method: "quantile"
  if(method=="quantile"){
      thres_ <- as.vector(round(quantile(unlist(as.data.frame(dat)), threshold) , digits=0))
      id_ <- which(dat>thres_)
      outlier_mat[id_] <- "TRUE"
  }

  # method: "manual"
  #------------------------------------
  if(method=="manual"){  #threshold=17
    id_ <- which(dat>threshold)
    outlier_mat[id_] <- "TRUE"
  }

  list_traj <- NULL
      for(j in 1:nrow(outlier_mat)){ #j<-1
        w_ <- length(which(outlier_mat[j,]==TRUE))
        if(w_ >= hortz_tolerance){
          list_traj <- rbind(list_traj, cbind(j,w_))
        }
      }
    #to replace the outlier observation
    if(!is.null(list_traj)){
      #replace with
      #replace with mean of col
      if(replace_with == "Mean_col"){
      for(k in 1:nrow(list_traj)){ #k<-2
        idd_ <- which(outlier_mat[list_traj[k,1],]==TRUE)
        if(length(idd_)==1){
        dat[list_traj[k,1],idd_] <-  colMeans(matrix(dat[,idd_],nrow(dat),))
        }
        if(length(idd_)>1){
          dat[list_traj[k,1],idd_] <-  colMeans(dat[,idd_])
        }

      }
      }
      #replace with mean of row
      if(replace_with == "Mean_row"){
      for(k in 1:nrow(list_traj)){ #k<-2
        idd_ <- which(outlier_mat[list_traj[k,1],]==TRUE)
        dat[list_traj[k,1],idd_] <-  mean(as.numeric(as.character(dat[list_traj[k,1],])))
      }
      }
    }

  dat_ <- dat

    if(id_field ==  TRUE){
      b_dat[,2:ncol(dat)] <- dat
      dat_  <- b_dat
    }

flush.console()
print(paste(nrow(list_traj), "trajectories identified as outliers with a total of", sum(list_traj[,2]), "observations (entries) replaced!", sep=" "))
print("Details:")
for(u_ in 1:nrow(list_traj)){ #u_<-1
  flush.console()
  print(paste("*----- trajectory ", list_traj[u_,1], " : ---- : ", list_traj[u_,2], " observation(s) removed/replaced"), sep="")
}

return(dat_)

  }



