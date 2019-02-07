
#' @title Data Imputing
#' @param dat A matrix or data.frame
#' @param id_field Whether the first column is a unique (id) field
#' @param fill_with Values to replace with
#' @return datF
#' @export

na_null_inf_Filler <- function(dat, id_field = FALSE, fill_with = "Mean"){

  coln_ <- colnames(dat)

  #check if there is unique(id) field
  if(id_field==TRUE){
    backup_dat <- dat
    dat <- dat[,2:ncol(dat)]
  }

  fill_count <- 0
  datF <- NULL
  for(g in 1:ncol(dat)){ #g<-7
    sub_ <- suppressWarnings(as.numeric(as.character(dat[,g])))
    i_ <- which(is.na(sub_))
    j_ <- which(is.infinite(sub_))
    k_ <- which(is.null(sub_))
    l_ <- which((sub_)=="null")
    m_ <- which((sub_)=="Null")
    i_m <- unique(c(i_, j_, k_, l_, m_))

    if(length(i_m)!=0){fill_count <- fill_count + length(i_m)}

    if(length(i_m)!=0){
      rem_ <- as.numeric(as.character(sub_[-i_m]))
      if(fill_with=="Mean"){
      ave_ <- mean(rem_)
      }
      if(fill_with=="Min"){
      ave_ <- min(rem_)
      }
      if(fill_with=="Max"){
      ave_ <- max(rem_)
      }
      #for(c_ in 1:length(i_m)){ #c_=1
        sub_[i_m] <- ave_
      #}
    }
    datF <- cbind(datF, sub_)
  }

  flush.console()
  print(paste(fill_count, "entries were filled!", sep=" "))

  #update the main matrix
  if(id_field==TRUE){
    backup_dat[,2:ncol(backup_dat)] <- datF
    datF <- backup_dat
  }

  colnames(datF) <- coln_
  return(datF)
}

#na_null_inf_Filler(dat, id_field = TRUE)
