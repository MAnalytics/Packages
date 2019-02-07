
#Function to convert counts or rates to proportion
#' @title Function to convert counts or rates to proportion
#' @param rates A matrix or data.frame with rows and columns representing unique trajectory and time steps, respectively
#' @param id_field Whether the first column is a unique (id) field
#' @return props
#' @export

rates_to_props <- function(rates, id_field = FALSE){
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



