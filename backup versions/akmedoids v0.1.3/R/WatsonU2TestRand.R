
#' @title Tests for a Common Median Direction between groups
#' @description This function tests the null hypothesis of a common median direction between pairs of groups resulting from the \code{akmedoids.clust} function. The aim is to provide statistical significance values that could be used to further categorise group into three large classes, denoted as the 'Decreasing', 'Stable' & 'Increasing' classes.
#' @param clustr [matrix (numeric)]: corresponding longitudinal data used to generate \code{clustr} (with rows corresponding to each label of \code{clustr}). For example, the first label of \code{clustr} is the group label of the first row of \code{traj} matrix, and so on.
#' @param traj [matrix (numeric)]: corresponding longitudinal data used to generate \code{clustr} (with rows corresponding to each label of \code{clustr}). For example, the first label of \code{clustr} is the group label of the first row of \code{traj} matrix, and so on.
#' @param id_field [ss]
#' @param grp [sss]
#' @param Nsample [an integer (numeric)] Number of random permutation of the original combined sample
#' @examples
#' print(traj)
#' traj <- dataImputation(traj, id_field = TRUE, method = 1, replace_with = 1,
#' fill_zeros = FALSE)
#' print(traj)
#' traj <- props(traj, id_field = TRUE)
#' clustr <- akmedoids.clust(traj, id_field = TRUE, method = "linear", k = c(3,6))
#' clustr <- as.vector(clustr$optimSolution)
#' p.values <- WatsonU2TestRand(clustr, traj, id_field=TRUE, grp=c(1:2), Nsample=999)
#' @details This function implements the Fisher's Nonparametric Test (Fisher, 1993) to determine whether g independent samples were drawn from distributions with a common median direction.
#' @return A matrix showing the pvalues indicating whether the medians direction of any two groups are drawn from the same distribution.
#' @references \code{1}. Fisher N. I. (1993). Statistical Analysis of Circular Data. Cambridge University Press, Cambridge.
#' @references \code{2}. Pewsey A. et al. (2013). Circular Statistics in R. Oxford University Press (1st Edition).
#' @rawNamespace importFrom(circular, watson.two.test)
#' @export

#traj <- prop_crime_per000_people
WatsonU2TestRand <- function(clustr, traj, id_field=TRUE, grp=c(1,2), Nsample=999) {

  #group membership
  clusters <- clustr
  group <- unique(clusters[order(clusters)])

  flush.console()
  print("You have specified this test to be carried out between the following groups:")
  print(group[grp])
  x <- readline("Is this correct (Y or N)?")

  if(x=="N"|x=="n"){
    stop("*-------Please, re-specify group membership to test!!!-------*")
  }

  print("**Computing-----**")

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
  #select the groups to work with
  all_circular <- all_circular[grp]
  group <- group[grp]

  #loop though pairs of groups and calculate the test statistics
  p.values <- matrix(0, length(all_circular), length(all_circular))
  row.names(p.values) <- group
  colnames(p.values) <- group

  #loop though pairs of groups and calculate the test statistics
  t.stat <- matrix(0, length(all_circular), length(all_circular))
  row.names(t.stat) <- group
  colnames(t.stat) <- group

  result_ <- list()

  for(t in 1:(length(all_circular)-1)){ #t<-2
    cdat1 <- all_circular[[t]]
    for(s in (t+1):length(all_circular)){ #s<-3
      cdat2 <- all_circular[[s]]

      cdat1 <- circular(cdat1)
      cdat2 <- circular(cdat2)

      n1 <- length(cdat1)
      n2 <- length(cdat2)

      #ndat <- c(n1, n2)
      #g <- 2
      N <- n1 + n2
      combsample <- c(cdat1, cdat2)
      #p_ <- PgRandTest(cdat, ndat, g, Nsample)
      U2Obs <- watson.two.test(cdat1, cdat2)$statistic

      nxtrm <- 1
      for (r in 1:Nsample) {
        randsamp <- sample(combsample, replace=TRUE)
        randsamp1 <- randsamp[1:n1]
        randsamp2 <- randsamp[(n1+1):N]
        U2Rand <- watson.two.test(randsamp1, randsamp2)$statistic
        if (U2Rand >= U2Obs) {
          nxtrm <- nxtrm+1
        }
      }
      pval <- nxtrm/(Nsample+1)
      p.values[t,s] <- pval
      t.stat[t,s] <- U2Obs
    }
    flush.console()
    print(paste("Step" , t, "of", (length(all_circular)-1), "completed!", sep=" "))
  }

  flush.console()
  print("Execution complete!")

  result_[1] <- list(p.values)
  result_[2] <- list(t.stat)

  return(result_)

}
