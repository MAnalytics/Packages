
#Outlier detector
#' @title Outlier detection and replacements
#' @description This function identifies outlier observations in the trajectories and determine whether to replace them or remove the trajectories accordingly.
#' @param traj A matrix or dataframe with each row representing the trajectory of observations of a unique location. The columns show the observations at consecutive time steps.
#' @param id_field Whether the first column is a unique (id) field. Default: \code{FALSE}
#' @param method Specify the method for identifying the outlier. Available methods: \code{1}: \code{quantile} method, and \code{2}: \code{manual} method".
#' @param threshold The cut off value for an observation to be flagged as outlier. For \code{quantile} method, \code{threshold} options are: a numeric vector of probability between \code{[0,1]}. For \code{manual} method, \code{threshold} options are: \code{a user-specified value}
#' @param hortz_tolerance [Default: \code{1}]. Specifying the count of outlier observations that must exist in a trajectory in order for the trajectory to be considered an \code{outlier}.
#' @param replace_with How to replace the outlier observations. [Default: \code{1}] - mean value of all the observations in the column in which an outlier observation is located. Other options are: \code{2}: Mean value of row in which the observation is found, \code{3}: To remove the outlier trajectory.
#' @usage outlierDetect(traj, id_field = FALSE, method = 1, threshold = 0.95, hortz_tolerance = 1, replace_with = 1)
#' @details Given a matrix or data.frame with some suspected outlier observations, this function identified those observations based on the method chosen and replace all the observations accordingly.
#' @examples
#' traj <- gm.crime.sample1
#' traj <- missingVal(traj, id_field=TRUE, replace_with=1)
#' outlierDetect(traj, id_field = FALSE, method = 1, threshold = 0.95,
#' hortz_tolerance = 1, replace_with = 1)
#' @return A dataframe with outlier observations replaced or trajectories containing outlier observation removed.
#' @export

outlierDetect <- function(traj, id_field = FALSE, method = 1, threshold = 0.95, hortz_tolerance = 1, replace_with = 1){

  dat <- traj

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
  if(method==1){
    thres_ <- as.vector(round(quantile(unlist(as.data.frame(dat)), threshold) , digits=0))
    id_ <- which(dat>thres_)
    outlier_mat[id_] <- "TRUE"
  }

  # method: "manual"
  #------------------------------------
  if(method==2){  #threshold=17
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
    if(replace_with == 1){
      for(k in 1:nrow(list_traj)){ #k<-1
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
    if(replace_with == 2){
      for(k in 1:nrow(list_traj)){ #k<-2
        idd_ <- which(outlier_mat[list_traj[k,1],]==TRUE)
        dat[list_traj[k,1],idd_] <-  mean(as.numeric(as.character(dat[list_traj[k,1],])))
      }
    }

    #to remove the outlier trajectory
    if(replace_with == 3){
      dat <- dat[-list_traj[,1],]
    }

  }

  dat_ <- dat

  if(id_field ==  TRUE){
    b_dat[,2:ncol(dat)] <- dat
    dat_  <- b_dat
  }

  #if 'replace_with' is 1 or 2
  if(replace_with==1|replace_with==2){
    flush.console()
    print(paste(nrow(list_traj), "trajectories identified as outliers with a total of", sum(list_traj[,2]), "observations (entries) replaced!", sep=" "))
    print("Details:")
    for(u_ in 1:nrow(list_traj)){ #u_<-1
      flush.console()
      print(paste("*----- trajectory ", list_traj[u_,1], " : ---- : ", list_traj[u_,2], " observation(s) replaced!"), sep="")
    }
  }

  #if 'replace_with' is 3
  if(replace_with==3){
    flush.console()
    print(paste(nrow(list_traj), "trajectories identified as outliers and removed!", sep=" "))
    print("Details:")
    for(u_ in 1:nrow(list_traj)){ #u_<-1
      flush.console()
      print(paste("*----- trajectory ", list_traj[u_,1], " : ---- : ", list_traj[u_,2], " observation(s)"), sep="")
    }
  }


  return(dat_)

}

#dat <- read.table(file="C:/Users/monsu/Documents/GitHub/Packages/samp.csv", sep=",", head=FALSE)
#dat <- missingValue(dat, id_field=TRUE, replace_with=1)

