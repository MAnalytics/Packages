
#' @title Conversion of counts (or rates) to 'Proportion'
#' @description This function converts counts or rates to proportions.
#' @param traj [matrix (numeric)]: longitudinal data. Each row represents an individual trajectory (of observations). The columns show the observations at consecutive time points.
#' @param id_field [numeric or character] Whether the first column of the \code{traj} is a unique (\code{id}) field. Default: \code{FALSE}. If \code{TRUE} the function recognises the second column as the first time step.
#' @usage props(traj, id_field = TRUE)
#' @examples
#' traj <- traj
#' head(traj) #
#' traj <- dataImputation(traj, id_field = TRUE, method = 2, replace_with = 1,
#' fill_zeros = FALSE) #filling the missing values
#' traj <- props(traj, id_field = TRUE)
#' print(traj)
#' @details Given a matrix of observations (counts or rates), this function converts each observation to a proportion equivalent to the sum of each column. In other words, each observation is divided by the sum of the column where it is located, i.e. \code{prop = [a cell value] / sum[corresponding column]}
#' @return A matrix of proportion measures
#' @export

props <- function(traj, id_field = TRUE){
  dat <- traj
  props <- dat
  if(id_field==FALSE){
    for(h in 1:ncol(dat)){ #h<-6
      prop <- (as.numeric(as.character(dat[,h]))/sum(as.numeric(as.character(dat[,h]))))
      props[,h] <- round(prop, digits=2)
    }
  }

  if(id_field==TRUE){
    for(h in 2:ncol(dat)){ #h<-2
      prop <- (as.numeric(as.character(dat[,h]))/sum(as.numeric(as.character(dat[,h]))))
      props[,h] <- round(prop, digits = 2)
    }
  }
  return(props)
}



