
#Function to convert counts or rates to proportion
#' @title Function to convert counts or rates to proportion
#' @param rates A matrix or data.frame with each row representing the trajectory of observations of a unique location. The columns show the observation at consecutive time steps.
#' @param id_field Whether the first column is a unique (id) field. [default: FALSE]
#' @return props
#' @export

props <- function(rates, id_field = FALSE){
  props <- rates
  if(id_field==FALSE){
    for(h in 1:ncol(rates)){ #h<-6
      prop <- (as.numeric(rates[,h])/sum(as.numeric(as.character(rates[,h]))))
      props[,h] <- prop
    }
  }

  if(id_field==TRUE){
    for(h in 2:ncol(rates)){ #h<-2
      prop <- (as.numeric(rates[,h])/sum(as.numeric(as.character(rates[,h]))))
      props[,h] <- prop
    }
  }
return(props)
}



