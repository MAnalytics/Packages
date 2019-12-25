
#' @title Tests
#' @description This function
#' @param cdat [matrix (numeric)]:
#' @param ndat [numeric or character]
#' @param g [an integer (numeric)]
#' @param Nsample [an integer (numeric)] Number
#' @details This function
#' @return A matrix
#' @references \code{1}. Fisher N. I. (1993). Statistical Analysis of Circular Data. Cambridge University Press, Cambridge.
#' @references \code{2}. Pewsey A. et al. (2013). Circular Statistics in R. Oxford University Press (1st Edition).

  #Non-parametric function
  PgRandTest <- function(cdat, ndat, g, Nsample){
    ndatcsum <- cumsum(ndat)
    #pg Obs
    PgObs <- PgVal(cdat, ndat, g)
    nxtrm <- 1

    #randomisation section
    for (r in 1:Nsample){
      randsamp <- sample(cdat)

      #-------------------------------
      #pgVal section
      PgRand <- PgVal(randsamp, ndat, g)

      #-------------------------------

      if (PgRand >= PgObs){
        nxtrm <- nxtrm+1
      }
      #flush.console()
      #print(r)
    }
    pval <- nxtrm/(Nsample+1)
    return (c(PgObs, pval))
  }


