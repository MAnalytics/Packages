#' @title Numerics ids to alphabetical ids
#' @description Function to transform a list of numeric ids to alphabetic ids
#' @param x A vector of numeric ids
#' @usage alphaLabel(x)
#' @rawNamespace import(utils)

alphaLabel <- function(x){
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
