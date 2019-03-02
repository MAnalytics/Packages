
#' @title Convert counts or rates to proportion
#' @description This function converts counts or rates data to proportion.
#' @param traj A matrix or data.frame with each row representing the trajectory of observations of a unique location. The columns show the observation at consecutive time steps.
#' @param id_field Whether the first column is a unique (id) field. Default: \code{FALSE}
#' @examples
#' traj <- gm.crime.sample1
#' head(traj) #
#' traj <- props(traj, id_field = TRUE)
#' print(traj)
#' @details Given a matrix of observations (counts or rates), this function convert each observation to a proportion measure. A matrix or data.frame count or rate is converted by dividing each cell entry by the sum of the corresponding column, i.e. \code{prop = [a cell value] / sum[column]}
#' @return A matrix of proportion measures
#' @export

props <- function(traj, id_field = FALSE){
  dat <- traj
  props <- dat
  if(id_field==FALSE){
    for(h in 1:ncol(dat)){ #h<-6
      prop <- (as.numeric(dat[,h])/sum(as.numeric(as.character(dat[,h]))))
      props[,h] <- prop
    }
  }

  if(id_field==TRUE){
    for(h in 2:ncol(dat)){ #h<-2
      prop <- (as.numeric(dat[,h])/sum(as.numeric(as.character(dat[,h]))))
      props[,h] <- prop
    }
  }
  return(props)
}



