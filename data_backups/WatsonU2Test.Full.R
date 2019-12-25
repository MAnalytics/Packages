
#' @title Tests for a Common Median Direction between groups - compare categorical classes across cases.
#' @description This function tests the null hypothesis of a common median direction between pairs of groups resulting from the \code{akmedoids.clust} function. The aim is to provide statistical significance values that could be used to further categorise group into three large classes, denoted as the 'Decreasing', 'Stable' & 'Increasing' classes.
#' @param clustr [matrix (numeric)]: corresponding longitudinal data used to generate \code{clustr} (with rows corresponding to each label of \code{clustr}). For example, the first label of \code{clustr} is the group label of the first row of \code{traj} matrix, and so on.
#' @param traj1 [matrix (numeric)]: corresp
#' @param traj2 [matrix (numeric)]: corresponding longitudinal data used to generate \code{clustr} (with rows corresponding to each label of \code{clustr}). For example, the first label of \code{clustr} is the group label of the first row of \code{traj} matrix, and so on.
#' @param id_field [ss] All must have id_field.
#' @param s.orient [an integer (numeric)]. The angle by which traj2 object is re-oriented. Options are: \code{1}: No re-orientation, \code{2}: a specified angle value.
#' @param Nsample [an integer (numeric)] Number of random permutation of the original combined sample
#' @examples
#' @details This function implements the Fisher's Nonparametric Test (Fisher, 1993) to determine whether g independent samples were drawn from distributions with a common median direction.
#' @return A matrix showing the pvalues indicating whether the medians direction of any two groups are drawn from the same distribution.
#' @references \code{1}. Fisher N. I. (1993). Statistical Analysis of Circular Data. Cambridge University Press, Cambridge.
#' @references \code{2}. Pewsey A. et al. (2013). Circular Statistics in R. Oxford University Press (1st Edition).
#' @rawNamespace importFrom(circular, watson.two.test)
#' @export

#fi <- WatsonU2Test.Full(bmOA_result_, brs_result_, id_field=TRUE, k = c(3,20), s.orient = 1, Nsample=999)

WatsonU2Test.Full <- function(traj1, traj2, id_field=TRUE, s.orient = 1, clustr = "A", k = c(3, 6), Nsample=999) {

  #traj1 <- bmOA_result_
  #traj2 <- brs_result_
  #k = c(3, 20)


  #generate the clusters
  print("---------------------------------------------------------")
  print("*--      executing the 1st clustering solution      --*")
  print("---------------------------------------------------------")
  clustr1 <- akmedoids.clust(traj1, id_field = id_field, method = "linear", k = k)
  print("---------------------------------------------------------")

  print("---------------------------------------------------------")
  print("*--      executing the 2nd clustering solution      --*")
  print("---------------------------------------------------------")
  clustr2 <- akmedoids.clust(traj2, id_field = id_field, method = "linear", k = k)
  print("---------------------------------------------------------")

  clustr1 <- as.vector(clustr1$optimSolution)
  b_clustr1 <- clustr1
  clustr2 <- as.vector(clustr2$optimSolution)
  b_clustr2 <- clustr2

  #collate the attributes
  changeStat1 <- statPrint(clustr1, traj1, id_field=id_field, show.plot=FALSE, type="lines", y.scaling="fixed")
  changeStat1 <- as.vector(changeStat1$changeStats$Class)

  changeStat2 <- statPrint(clustr2, traj2, id_field=id_field, show.plot=FALSE, type="lines", y.scaling="fixed")
  changeStat2 <- as.vector(changeStat2$changeStats$Class)

    #group membership - solution 1
  clusters1 <- clustr1
  group1 <- unique(clusters1[order(clusters1)])

  #group membership - solution 2
  clusters2 <- clustr2
  group2 <- unique(clusters2[order(clusters2)])

  #joining the data with the clusters
  clustr1 <- data.frame(cbind(traj1, clusters=clustr1))
  clustr2 <- data.frame(cbind(traj2, clusters=clustr2))

  dat1 <- traj1 #backing up the data
  dat2 <- traj2 #backing up the data

  #check if 'id_field' is a unique field
  if(id_field==TRUE){
    dat1 <- dat1[,2:ncol(dat1)]
    n_CL1 <- colnames(clustr1)[1]
    col_names1 <- as.vector(clustr1[,1])

    dat2 <- dat2[,2:ncol(dat2)]
    n_CL2 <- colnames(clustr2)[1]
    col_names2 <- as.vector(clustr2[,1])

    if((length(col_names1)!=length(unique(col_names1))) | (length(col_names2)!=length(unique(col_names2)))){
      stop("(: Elements of one of the 'id_fields' are not unique. Function terminated!!! :)")
    }
  }

  if(id_field==FALSE){
    clustr1 <- cbind(1:nrow(clustr1), clustr1)  #head(clustr)
    clustr2 <- cbind(1:nrow(clustr2), clustr2)  #head(clustr)
  }

  #----------------------------------------------------
  #To generate angles measured in radians of each solution
  sl_List1 <- NULL
  time1 <- as.numeric(1:ncol(dat1))
  for(i in 1:nrow(dat1)){ #i<-1
    b1=coefficients(lm(as.numeric(as.character(dat1[i,]))~as.numeric(as.character(time1))))
    sl_List1 <- rbind(sl_List1, cbind(as.numeric(b1[1]), as.numeric(b1[2])))
  }
  sl_List1 <- as.data.frame(cbind(1:nrow(sl_List1), sl_List1))
  colnames(sl_List1) <- c("sn", "intersect","slope")  #head(sl_List)
  sl_List_rad1 <-  atan(sl_List1$slope)
  #-------------------
  sl_List2 <- NULL
  time2 <- as.numeric(1:ncol(dat2))
  for(i in 1:nrow(dat2)){ #i<-1
    b2=coefficients(lm(as.numeric(as.character(dat2[i,]))~as.numeric(as.character(time2))))
    sl_List2 <- rbind(sl_List2, cbind(as.numeric(b2[1]), as.numeric(b2[2])))
  }
  sl_List2 <- as.data.frame(cbind(1:nrow(sl_List2), sl_List2))
  colnames(sl_List2) <- c("sn", "intersect","slope")  #head(sl_List)
  sl_List_rad2 <-  atan(sl_List2$slope)
  #---------------------------------------------------------------------

  #get the circular data
  all_circular1 <- list()
  for(n in 1:length(group1)){ #n<-1
    #Calculating the number of trajectories
    a1_1 <- sl_List_rad1[which(clusters1%in%group1[n])]
    all_circular1[n] <- list(a1_1)
  }
  #-----------------------
  all_circular2 <- list()
  for(n in 1:length(group2)){ #n<-1
    #Calculating the number of trajectories
    a1_2 <- sl_List_rad2[which(clusters2%in%group2[n])]
    all_circular2[n] <- list(a1_2)
  }

  #combining the groups into classes
  #first check categorical classes that are present in both solution.
  #totest1 <- c("Decreasing", "Stable")
  totest1 <- unique(changeStat1)
  totest2 <- unique(changeStat2)
  #totest <- c("Decreasing", "Increasing")

  absent_1 <- totest1[which(totest1 %in% totest2)]

  if(length(absent_1)!=3){
    print("---------------------------------------------------------")
    print("*------                Warning!!                 -------*")
    print("---------------------------------------------------------")

    print("*------  One or both of the clustering solutions have only", length(absent_), "matching categorical classes!!", sep=" ")
    print("*------  Hence, only the matching class(es) will be compared!")

    x <- readline("Continue(Y or N)?")

    if(x=="N"|x=="n"){
      stop("*-------Execution halted!!!-------*")
    }
  }


  #loop though pairs of groups and calculate the test statistics
  p.values <- matrix(0, length(absent_1), length(absent_1))
  row.names(p.values) <- absent_1
  colnames(p.values) <- absent_1

  #loop though pairs of groups and calculate the test statistics
  t.stat <- matrix(0, length(absent_1), length(absent_1))
  row.names(t.stat) <- absent_1
  colnames(t.stat) <- absent_1

#loop through the matching classes and test accordingly.  #group1

  result_ <- list()

  for(z in 1:length(absent_1)){ #z<-3
    grpp_1 <- which(changeStat1==absent_1[z])
    from1 <- unlist(all_circular1[grpp_1])

    grpp_2 <- which(changeStat2==absent_1[z])
    from2 <- unlist(all_circular2[grpp_2])

    cdat1 <- circular(from1)
    cdat2 <- circular(from2)

    #----------------------------
    #0.7854Rad × 180/pi = 45Deg
    #convert to degree for calculating the mean resultant vector
    #pcdat1_deg <- circular(90 - (from1 * (180/pi)), units="degrees", zero=circular(pi/2), rotation="clock") #used for plottin
    cdat1_deg <- circular(90 - (from1 * (180/pi)), units="degrees", zero=circular(0), rotation="counter") #used for calculating the stats
    cdat2_deg <- circular(90 - (from2 * (180/pi)), units="degrees", zero=circular(0), rotation="counter") #used f
    cdat1 <- cdat1_deg
    cdat2 <- cdat2_deg
    #plot(pcdat1_deg, stack=TRUE, bins=720, cex=1.5) #arrows.circular(pcdat1_deg) #rho.circular(pcdat1_deg)
    #t10 <- trigonometric.moment(cdat1_deg, p=1); tbar <- t10$mu ; Rbar <- t10$rho ; a1 <- t10$cos ; b1 <- t10$sin;
    #Rbar <- rho.circular(cdat1_deg)
    #var(cdat1_deg)#var(cdat2_deg) #angular.variance(cdat1_deg) #angular.variance(cdat2_deg)
    #Mean angular deviation (angular.deviation) may be the final value to use.
    #----------------------------

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
      randsamp <- sample(combsample, replace=FALSE)
      randsamp1 <- randsamp[1:n1]
      randsamp2 <- randsamp[(n1+1):N]
      U2Rand <- watson.two.test(randsamp1, randsamp2)$statistic
      if (U2Rand >= U2Obs) {
        nxtrm <- nxtrm+1
      }
      flush.console()
      print(U2Rand)
      print(r)
    }
    pval <- nxtrm/(Nsample+1)
    p.values[z,z] <- pval
    t.stat[z,z] <- U2Obs
  }

  flush.console()
  print("Execution complete!")

  result_[1] <- list(p.values)
  result_[2] <- list(t.stat)

  return(result_)

}
