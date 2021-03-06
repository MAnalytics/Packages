
#' @title Data imputation for longitudinal data
#' @description This function fills any missing entries (\code{NA}, \code{Inf}, \code{null}) in a matrix or dataframe, according to a specified method. By default, \code{'0'} is considered a value.
#' @param traj [\code{matrix (numeric)}]: longitudinal data. Each row represents an individual trajectory (of observations). The columns show the observations at consecutive time points.
#' @param id_field [numeric or character] Whether the first column of the \code{traj} is a unique (\code{id}) field. Default: \code{FALSE}. If \code{TRUE} the function recognises the second column as the first time step.
#' @param method [an integer] indicating a method for calculating the missing values. Options are: \code{'1'}: \code{arithmetic} method, and \code{'2'}: \code{regression} method. The default is \code{'1'}: \code{arithmetic} method
#' @param replace_with [an integer from 1 to 6] indicating the technique, based on a specified \code{method}, for calculating the missing entries.
#' \code{'1'}: \code{arithmetic} method, \code{replace_with} options are: \code{'1'}: Mean value of the corresp column;
#' \code{'2'}: Minimum value of corresp column; \code{'3'}: Maximum value of corresp column;
#' \code{'4'}: Mean value of corresp row; \code{'5'}: Minimum value of corresp row,
#' or \code{'6'}: Maximum value of corresp row. For \code{'2'}: regression method:
#' the available option for the \code{replace_with} is: \code{'1'}: \code{linear}.
#' The regression method fits a linear regression line to a trajectory with missing entry(s)
#' and estimates the missing data values from the regression line.
#' Note: only the missing data points derive their new values from the regression line
#' while the rest of the data points retain their original values. The function terminates if there are
#' trajectories with only one observation. The default is \code{'1'}: Mean value of the corresp column
#' @param fill_zeros [TRUE or FALSE] whether to consider zeros \code{0} as missing values when \code{2: regression} method is used. The default is \code{FALSE}.
#' @usage dataImputation(traj, id_field = FALSE, method = 2, replace_with = 1, fill_zeros = FALSE)
#' @details Given a matrix or data.frame with some missing values indicated by (\code{NA}, \code{Inf}, \code{null}), this function impute the missing value by using either an estimation from the corresponding rows or columns, or to use a regression method to estimate the missing values.
#' @examples
#' print(traj)
#' dataImputation(traj, id_field = TRUE, method = 1, replace_with = 1, fill_zeros = FALSE)
#' @rawNamespace import(stats)
#' @return A data.frame with missing values (\code{NA}, \code{Inf}, \code{null}) imputed according to the a specified technique.
#' @export

dataImputation <- function(traj, id_field = FALSE, method = 2, replace_with = 1, fill_zeros = FALSE){

  dat <- as.data.frame(traj)

  coln_ <- colnames(dat)

  #check if there is unique(id) field
  if(id_field==TRUE){
    backup_dat <- dat
    dat <- dat[,2:ncol(dat)]
  }

  #matrix to backup the 'null' information store the outlier information [TRUE or FALSE]
  null_mat <- matrix(FALSE, nrow(dat), ncol(dat))

  for(a in 1:ncol(dat)){ #a<-1
    sub_ <- suppressWarnings(as.numeric(as.character(dat[a,])))
    i_ <- which(is.na(sub_))
    j_ <- which(is.infinite(sub_))
    k_ <- which(is.null(sub_))
    i_m <- unique(c(i_, j_, k_))[order(unique(c(i_, j_, k_)))]

    null_mat[a,i_m] <- TRUE
  }

  # arithmetic method
  if(method==1){
    #Use column values in order to determine the new value.
    if(replace_with==1 | replace_with==2 | replace_with==3){
      fill_count <- 0
      for(g in 1:ncol(null_mat)){ #g<-2
        i_m <- which(null_mat[,g]==TRUE)
        subset_ <- dat[-i_m,g]

        if(length(i_m)!=0){fill_count <- fill_count + length(i_m)}

        if(length(i_m)!=0){
          if(replace_with==1){
            ave_ <- mean(subset_)
          }
          if(replace_with==2){
            ave_ <- min(subset_)
          }
          if(replace_with==3){
            ave_ <- max(subset_)
          }
          dat[i_m,g] <- ave_
        }
      }
    }

    #Use row values in order to determine the new value.
    if(replace_with==4 | replace_with==5 | replace_with==6){
      fill_count <- 0
      for(g in 1:nrow(null_mat)){ #g<-2
        i_m <- which(null_mat[g,]==TRUE)
        subset_ <- dat[g, -i_m]

        if(length(i_m)!=0){fill_count <- fill_count + length(i_m)}

        if(length(i_m)!=0){
          if(replace_with==4){
            ave_ <- round(mean(as.numeric(as.character(subset_))),digits=2)
          }
          if(replace_with==5){
            ave_ <- min(as.numeric(as.character(subset_)))
          }
          if(replace_with==6){
            ave_ <- max(as.numeric(as.character(subset_)))
          }
          dat[g, i_m] <- ave_
        }
      }
    }

  }

  #regression method
  if(method==2){
    #keep count of missing data
    fill_count <- 0
    for(k in 1:nrow(dat)){ #k<-10
      y <- suppressWarnings(as.numeric(as.character(dat[k,])))
      x <- 1:length(y)
      known <- data.frame(x, y)

      #check which data points are missing, then remove them from the data point
      #include zeros
      if(fill_zeros==FALSE){
        known_1 <- data.frame(known[is.na(known[,2])|is.infinite(known[,2]),])  #
        known_2 <- data.frame(known[!is.na(known[,2])&!is.infinite(known[,2]),])
      }

      if(fill_zeros==TRUE){
        known_1 <- data.frame(known[is.na(known[,2])|is.infinite(known[,2])|(known[,2]==0),])  #
        known_2 <- data.frame(known[!is.na(known[,2])&!is.infinite(known[,2])&!(known[,2]==0),])
      }

      fill_count <- fill_count + nrow(known_1)

      #terminate the programe is there is only one data point
      if(nrow(known_2)==1){
        stop("One of the trajectories has only one data point..Unable to interpolate or extrapolate points. Program terminated!!")
      }
      #train the available data using linear regression
      model.lm <- lm(y ~ x, data = known_2)
      # Use predict the y value for the removed data
      newY <- predict(model.lm, newdata = data.frame(x = known_1[,1]))

      #add to the original data.
      dat[k, known_1[,1]] <- round(newY, digits = 2)

    }
  }

  flush.console()
  print(paste(fill_count, "entries were found/filled!", sep=" "))

  #update the main matrix
  if(id_field==TRUE){
    backup_dat[,2:ncol(backup_dat)] <- dat
    datF <- backup_dat
  }

  colnames(datF) <- coln_
  return(datF)
}



