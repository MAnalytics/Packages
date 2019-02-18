
#' @title Data Imputing
#' @description This function replaces any cells with the entry 'NA' or 'Inf' in a matrix or data.frame with the 'Mean', 'Minimum' or 'Maximum' value of either the column or row in which the cell is located.
#' @param dat A matrix or data.frame with each row representing the trajectory of a unique location. The columns show the observation at consecutive time steps.
#' @param id_field Whether the first column is a unique (id) field. [default: FALSE]
#' @param replace_with Values to replace with [Values: "Mean_col", "Min_col", "Max_col, "Mean_row", "Min_row" or "Max_row"]. The default is "Mean_col", meaning to imput the average values of the field in which the cell is located.
#' @return datF
#' @export

null_filler <- function(dat, id_field = FALSE, replace_with = "Mean_col"){  #id_field = TRUE; replace_with = "Mean_row"


  #dat <- read.table(file="samp.csv", sep=",", head=FALSE)
  #dat <- null_filler(dat, id_field=TRUE, replace_with="Mean_col")

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

#datF <- NULL

#Use column values in order to determine the new value.
if(replace_with=="Mean_col" | replace_with=="Min_col" | replace_with=="Max_col"){
  fill_count <- 0
  for(g in 1:ncol(null_mat)){ #g<-2
    i_m <- which(null_mat[,g]==TRUE)
    subset_ <- dat[-i_m,g]

    if(length(i_m)!=0){fill_count <- fill_count + length(i_m)}

    if(length(i_m)!=0){
      if(replace_with=="Mean_col"){
      ave_ <- mean(subset_)
      }
      if(replace_with=="Min_col"){
      ave_ <- min(subset_)
      }
      if(replace_with=="Max_col"){
      ave_ <- max(subset_)
      }
      dat[i_m,g] <- ave_
    }
  }
}

  #Use row values in order to determine the new value.
#Use row values in order to determine the new value.
if(replace_with=="Mean_row" | replace_with=="Min_row" | replace_with=="Max_row"){
  fill_count <- 0
  for(g in 1:nrow(null_mat)){ #g<-2
    i_m <- which(null_mat[g,]==TRUE)
    subset_ <- dat[g, -i_m]

    if(length(i_m)!=0){fill_count <- fill_count + length(i_m)}

    if(length(i_m)!=0){
      if(replace_with=="Mean_row"){
        ave_ <- mean(as.numeric(as.character(subset_)))
      }
      if(replace_with=="Min_row"){
        ave_ <- min(as.numeric(as.character(subset_)))
      }
      if(replace_with=="Max_row"){
        ave_ <- max(as.numeric(as.character(subset_)))
      }
      dat[g, i_m] <- ave_
    }
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

#na_null_inf_Filler(dat, id_field = TRUE)
