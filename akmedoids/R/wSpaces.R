
#' @title Removing whitespaces
#' @description This function removes all the leading and the trailing whitespaces in a longitudinal data
#' @param traj [matrix (numeric)]: longitudinal data. Each row represents an individual trajectory (of observations). The columns show the observations at consecutive time steps.
#' @usage wSpaces(traj)
#' @details Given a matrix suspected to contain whitespaces, this function removes all the whitespaces and returns a cleaned data. 'Whitespace' are white characters in introduced into data by typo error or systematically by data recording devices. For example, in R a character such as " abcd" (or "abcd ") is not the same as "abcd". The former contains a leading (or trailing) white-characters.
#' @return A matrix with all whitespaces (if any) removed.
#' @references \url{https://en.wikipedia.org/wiki/Whitespace_character}
#' @examples
#' traj <- gm.crime.sample1
#' wSpaces(traj)
#' @export

wSpaces <- function(traj){  #head(dat)
  dat <- traj
  dat_Cleaned <- dat
  #if(head==TRUE){
    coln_ <- colnames(dat_Cleaned)
  #}
  count_ <- 0 #keep the count of whitespace removed.
  for(q in 1:ncol(dat_Cleaned)){#q<-1
    vec_Name1 <- trimws(as.vector(dat_Cleaned[,q]), which="right") #trailing whitespace
    count_ <- count_ + length(which(!vec_Name1%in%dat_Cleaned[,q]))
    vec_Name2 <- trimws(vec_Name1, which="left") #leading whitespace
    count_ <- count_ + length(which(!vec_Name2%in%vec_Name1))
    vec_Name2 <- matrix(vec_Name2,,1)
    #if(head==TRUE){
      colnames(vec_Name2) <- coln_[q] #head(data)
      dat_Cleaned[,q] <- vec_Name2
    #}
    #if(head==FALSE){
      #dat_Cleaned[,q] <- vec_Name2
    #}
  }
  flush.console()
  print(paste(count_, "whitespaces found/removed!"))
  return(dat_Cleaned)
}
#-----------------------------------------------------
