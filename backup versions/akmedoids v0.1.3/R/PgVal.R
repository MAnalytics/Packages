
#' @title Tests
#' @description This function
#' @param cdat [matrix (numeric)]:
#' @param ndat [numeric or character]
#' @param g [an integer (numeric)]
#' @details This function
#' @return A matrix
#' @references \code{1}. Fisher N. I. (1993). Statistical Analysis of Circular Data. Cambridge University Press, Cambridge.
#' @references \code{2}. Pewsey A. et al. (2013). Circular Statistics in R. Oxford University Press (1st Edition).
#' @rawNamespace importFrom(circular, median.circular, circular)

#Test statistics:
  PgVal <- function(cdat, ndat, g) {
    N <- length(cdat)
    sumterms <- 0
    M <- 0
    ndatcsum <- cumsum(ndat)
    gmedian <- median.circular(cdat)

    for (k in 1:g){
      if (k==1){
        low <- 0
      }else
        if (k > 1){
          low <- ndatcsum[k-1]
        }

      sample <- circular(0)

      for (j in 1:ndat[k]){
        sample[j] <- cdat[j+low]
      }

      shiftdat <- MinusPiPi(sample-gmedian)

      m <- length(shiftdat[shiftdat<0])
      M <- M + m
      sumterms <- sumterms + m*m/ndat[k]
    }
    term1 <- ((N*N)/(M*(N-M)))
    term2 <- (N*M)/(N-M)
    Pg <- term1 * sumterms - term2
    return(Pg)
  }



