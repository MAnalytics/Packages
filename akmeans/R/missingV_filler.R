
#' @title Data imputing for longitudinal data
#' @description This function fills up any missing entries \code{(NA, Inf, 0)} in a matrix or dataframe using a value derived using a chosen method.
#' @param traj A matrix or data.frame with each row representing the trajectory of a unique location. The columns show the observations at consecutive time steps.
#' @param id_field Whether the first column is a unique (\code{id}) field. default: \code{FALSE}
#' @param method Method for calculating the missing values. Available options: \code{1}: arithmetic, \code{2}: regression. default: \code{1}
#' @param replace_with How to calculate the missing value. For \code{arithmetic} method: \code{replace_with} options are: \code{1}: Mean value of column, \code{2}: Minimum value of column, \code{3}: Maximum value of column, \code{4}: Mean value of row, \code{5}: Minimum value of row, or \code{6}: Maximum value of row. For \code{regression} method: the only available option for \code{replace_with} is: \code{1}: linear. That is, use a linear regression to interpolate or extrapolate the missing data values. Note: only the missing data points derive their new values from the regression line while the rest of the data points retain their original values. Trajectories with only one observation will be removed.
#' @param fill_zeros Whether to consider zeros (\code{0}) as missing values. default: \code{FALSE}. Only available for \code{2}: \code{regression} method.
#' @usage missingV_filler(traj, id_field = FALSE, method = 2, replace_with = 1, fill_zeros = FALSE)
#' @details Given a matrix or data.frame with some missing values represented by \code{(NA, Inf, 0)}, the function \code{missingV_filler} determines the missing values using either the \code{arithmetic} or \code{regression} method.
#' @examples
#' traj <- assault_data
#' print(traj)
#' missingV_filler(traj, id_field = TRUE, method = 2, replace_with = 1, fill_zeros = FALSE)
#' @rawNamespace import(stats)
#' @return A data.frame with missing values \code{(NA, Inf, 0)} filled up
#' @export


missingV_filler <- function(traj, id_field = FALSE, method = 2, replace_with = 1, fill_zeros = FALSE){  #id_field = TRUE; replace_with = "Mean_row"

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
#Use row values in order to determine the new value.
if(replace_with==4 | replace_with==5 | replace_with==6){
  fill_count <- 0
  for(g in 1:nrow(null_mat)){ #g<-2
    i_m <- which(null_mat[g,]==TRUE)
    subset_ <- dat[g, -i_m]

    if(length(i_m)!=0){fill_count <- fill_count + length(i_m)}

    if(length(i_m)!=0){
      if(replace_with==4){
        ave_ <- mean(as.numeric(as.character(subset_)))
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
  #par(mar=c(2,2,2,2)+0.1)
  #par(mfrow=c(5,2))
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
      dat[k, known_1[,1]] <- newY
      #vignette material
      #Add the predicted points to the original data
      #plot (known$x, known$y, type="o")
      #points(known_1[,1], newY, col = "red")

    }
  }

#missingV_filler(traj, id_field = TRUE, fill_zeros = FALSE, method = 1, replace_with = 1)

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

#na_null_inf_Filler(dat, id_field = TRUE)
