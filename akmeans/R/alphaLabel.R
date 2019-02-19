
#Function to assign alphabetic labels to numeric cluster IDs
#' @title Function to assign alphabetic labels to numeric cluster IDs
#' @description GGGGGGGGGGG
#' @param x A vector of numeric cluster ids
#' @rawNamespace import(utils)
#' @export

alphaLabel <- function(x=clusters){
  combind_A <- LETTERS
  combind <-  combn(LETTERS, m=2, sep="")# combind[1:2,]
  list_Letters <- NULL
  for(cc in 1:ncol(combind)){#cc=1
    list_Letters <-c(list_Letters,  paste(combind[1,cc],   combind[2,cc], sep=""))
  }
  list_Letters <- c(combind_A, list_Letters) #combine
  if(length(unique(x))<=350){
    clust_num <- list_Letters[x]
    return(clust_num)
  }
  if(length(unique(x))>350){
    print("Labels exhausted! specify a vector with fewer elements")
  }
}
