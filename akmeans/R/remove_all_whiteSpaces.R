
##Function to remove whitespace in data entries
#' @title Function to remove whitespace in data entries
#' @param dat A matrix or data.frame
#' @param head If column names exist
#' @rawNamespace import(utils)
#' @return dat_Cleaned
#' @export
remove_all_whiteSpaces <- function(dat, head=TRUE){  #head(dat)
  dat_Cleaned <- dat
  if(head==TRUE){
  coln_ <- colnames(dat_Cleaned)
  }
  count_ <- 0 #keep the count of whitespace removed.
  for(q in 1:ncol(dat_Cleaned)){#q<-1
    vec_Name1 <- trimws(as.vector(dat_Cleaned[,q]), which="right") #trailing whitespace
    count_ <- count_ + length(which(!vec_Name1%in%dat_Cleaned[,q]))
    vec_Name2 <- trimws(vec_Name1, which="left") #leading whitespace
    count_ <- count_ + length(which(!vec_Name2%in%vec_Name1))
    vec_Name2 <- matrix(vec_Name2,,1)
    if(head==TRUE){
    colnames(vec_Name2) <- coln_[q] #head(data)
    dat_Cleaned[,q] <- vec_Name2
    }
    if(head==FALSE){
      dat_Cleaned[,q] <- vec_Name2
    }
  }
  flush.console()
  print(paste(count_, "whitespaces found/removed!"))
  return(dat_Cleaned)
}
#-----------------------------------------------------
