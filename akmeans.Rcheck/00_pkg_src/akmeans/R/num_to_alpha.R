
#Function to assign alphabetic labels to numeric cluster IDs
#' @param x A vector of numeric cluster ids
#' @return clust_num
#' @export
num_to_alpha <- function(x){
  combind_A <- LETTERS
  combind <-  combn(LETTERS, m=2, sep="")# combind[1:2,]
  list_Letters <- NULL
  for(cc in 1:ncol(combind)){#cc=1
    list_Letters <-c(list_Letters,  paste(combind[1,cc],   combind[2,cc], sep=""))
  }
  list_Letters <- c(combind_A, list_Letters) #combine
  if(length(x)<=350){
    clust_num <- list_Letters[x]
    return(clust_num)
  }
  if(length(x)>350){
    print("Labels exhasted! specify a vector with fewer elements")
  }

}


#Function to assign alphabetic labels to numeric cluster IDs
#' @param x A vector of numeric cluster ids
#' @return clust_num
#' @export
#-----------------------------------------------------
#Function to remove whitespace in data entries  #data<-Neighb_ids  #head(data)
remove_all_whiteSpaces <- function(dat){
  coln_ <- colnames(dat)
  if(is.null(coln_)){
    i_ = "Y"
    coln_ <- "col"
    dat <- as.data.frame(matrix(dat,,1))
  }
  for(q in 1:ncol(dat)){#q<-1
    vec_Name <- trimws(as.vector(dat[,q]), which="right") #trailing whitespace
    vec_Name <- trimws(vec_Name, which="left") #leading whitespace
    vec_Name <- matrix(vec_Name,,1)
    colnames(vec_Name) <- paste(coln_[q], q, sep="")  #head(data)
    dat[,q] <- vec_Name
  }
  if(i_=="Y"){
    data <- as.vector(t(dat)) #data[1]
  }
  i_ <- "N"
  #colnames(data) <- coln_
  return(dat)
}
#-----------------------------------------------------
