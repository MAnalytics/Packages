
#' @title Tests for a Common Median Direction between groups
#' @description This function tests the null hypothesis of a common median direction between pairs of groups resulting from the \code{akmedoids.clust} function. The aim is to provide statistical significance values that could be used to further categorise group into three large classes, denoted as the 'Decreasing', 'Stable' & 'Increasing' classes.
#' @param clustr [vector (charater)] A vector of cluster membership (labels). For instance, the result extracted from the \code{\link{akmedoids.clust}} function.
#' @param traj [matrix (numeric)]: corresponding longitudinal data used to generate \code{clustr} (with rows corresponding to each label of \code{clustr}). For example, the first label of \code{clustr} is the group label of the first row of \code{traj} matrix, and so on.
#' @param id_field [numeric or character] Whether the first column of the \code{traj} is a unique (\code{id}) field. Default: \code{FALSE}. If \code{TRUE} the function recognises the second column as the first time step.
#' @param Nsample [an integer (numeric)] Number of random permutation of the original combined sample
#' @examples
#' print(traj)
#' traj <- dataImputation(traj, id_field = TRUE, method = 1, replace_with = 1,
#' fill_zeros = FALSE)
#' print(traj)
#' traj <- props(traj, id_field = TRUE)
#' clustr <- akmedoids.clust(traj, id_field = TRUE, method = "linear", k = c(3,6))
#' clustr <- as.vector(clustr$optimSolution)
#' p.values <- Cmedians.direction(clustr, traj, id_field=TRUE, Nsample=999)
#' @details This function implements the Fisher's Nonparametric Test (Fisher, 1993) to determine whether g independent samples were drawn from distributions with a common median direction.
#' @return A matrix showing the pvalues indicating whether the medians direction of any two groups are drawn from the same distribution.
#' @references \code{1}. Fisher N. I. (1993). Statistical Analysis of Circular Data. Cambridge University Press, Cambridge.
#' @references \code{2}. Pewsey A. et al. (2013). Circular Statistics in R. Oxford University Press (1st Edition).
#' @export

Cmedians.direction <- function(clustr, traj, id_field=TRUE, Nsample=999){

  #group membership
  clusters <- clustr
  group <- unique(clusters[order(clusters)])

  #joining the data with the clusters
  clustr <- data.frame(cbind(traj, clusters=clustr))

  dat <- traj #backing up the data

  #check if 'id_field' is a unique field
  if(id_field==TRUE){
    dat <- dat[,2:ncol(dat)]
    n_CL <- colnames(clustr)[1]
    col_names <- as.vector(clustr[,1])
    if(!length(col_names)==length(unique(col_names))){
      stop("(: The 'id_field' is not a unique field. Function terminated!!! :)")
    }
  }

  if(id_field==FALSE){
    clustr <- cbind(1:nrow(clustr), clustr)  #head(clustr)
  }

  #----------------------------------------------------
  #To generate angles measured in radians
  sl_List <- NULL
  time <- as.numeric(1:ncol(dat))
  for(i in 1:nrow(dat)){ #i<-1
    b=coefficients(lm(as.numeric(as.character(dat[i,]))~as.numeric(as.character(time))))
    sl_List <- rbind(sl_List, cbind(as.numeric(b[1]), as.numeric(b[2])))
  }

  sl_List <- as.data.frame(cbind(1:nrow(sl_List), sl_List))
  colnames(sl_List) <- c("sn", "intersect","slope")  #head(sl_List)

  #convert the slopes to angles in radian delta x = 12; delta y = 12; atan(1) = 0.7853982
  #0.7854Rad × 180/pi = 45Deg
  sl_List_rad <-  atan(sl_List$slope)
  #---------------------------------------------------------------------

  #get the circular data
  all_circular <- list()
  for(n in 1:length(group)){ #n<-1
    #Calculating the number of trajectories
    a1 <- sl_List_rad[which(clusters%in%group[n])]
    all_circular[n] <- list(a1)
  }

  #------------------------------------------------------
  #loop though pairs of groups and calculate the test statistics
  p.values <- matrix(0, length(all_circular), length(all_circular))
  row.names(p.values) <- group
  colnames(p.values) <- group

  for(t in 1:(length(all_circular)-1)){ #t<-1
    cdat1 <- all_circular[[t]]
    for(s in (t+1):length(all_circular)){ #s<-4
      cdat2 <- all_circular[[s]]
      n1 <- length(cdat1)
      n2 <- length(cdat2)
      ndat <- c(n1, n2)
      g <- 2
      #print(paste(t,s, sep="-"))
      #convert to circular data
      cdat1 <- circular(cdat1)
      cdat2 <- circular(cdat2)
      cdat <- c(cdat1, cdat2)

      #Run
      p_ <- PgRandTest(cdat, ndat, g, Nsample)

      p.values[t,s] <- p_[2]
    }
  }
  return(p.values)
}


