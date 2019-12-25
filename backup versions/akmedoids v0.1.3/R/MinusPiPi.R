
#' @title Tests
#' @description This function
#' @param sample [matrix (numeric)]:
#' @details This function
#' @return A matrix
#' @references \code{1}. Fisher N. I. (1993). Statistical Analysis of Circular Data. Cambridge University Press, Cambridge.
#' @references \code{2}. Pewsey A. et al. (2013). Circular Statistics in R. Oxford University Press (1st Edition).

#------------------------------------------------------
#Define the function 'MinusPiPi'
MinusPiPi <- function(sample){
  n <- length(sample)
  for (j in 1:n) {
    if (sample[j] < -pi) {sample[j] <- sample[j]+(2*pi)}
    else
      if (sample[j] > pi) {sample[j] <- sample[j]-(2*pi)} }
  return(sample)
}

