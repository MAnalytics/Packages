
#' @title Clustering of longitudinal data
#' @description This function group trajectories based on a functional definition of their long-term trends.
#' @param traj A matrix or data.frame with each row representing the trajectory of observations of a unique location. The columns show the observations at consecutive time steps.
#' @param id_field Whether the first column is a unique (\code{id}) field. Default: \code{FALSE}
#' @param method Initialisation strategy. Available method: \code{linear}
#' @param k either an exact integer number of clusters, or a vector of length two specifying the minimum and maximum numbers of clusters to be examined. The default is \code{c(3,15)}. When k is a range, the actual number of clusters is determined by Calinski-Harabatz criterion. number of clusters to generate. Default: \code{3}: (minimum value)
#' @usage akmedoids.clust(traj, id_field = FALSE, method = "linear", k = c(3,6)) ##' #details Given a list of trajectories represented in a matrix or data.frame, and a method for choosing initial cluster centroids (e.g. \code{\link{lpm.centroids}}), a list of clusters is generated after a limited number of iterations.
#' @examples
#' traj <- gm.crime.sample1
#' print(traj)
#' traj <- missingVal(traj, id_field = TRUE, method = 2, replace_with = 1,
#' fill_zeros = FALSE) #filling the missing values
#' traj <- props(traj, id_field = TRUE)
#' print(traj)
#' output <- akmedoids.clust(traj, id_field = TRUE, method = "linear", k = c(3,6))
#' print(output)
#' @return A list containing cluster solutions for all value of k, including the solution at the optimal value of \code{k}, based on the Calinski-Harabatz criterion \code{(Calinski T, Harabasz J, 1974)}
#' @references \code{Calinski T, Harabasz J (1974) A dendrite method for cluster analysis. Commun Stat 3:1-27}
#' @rawNamespace importFrom(kml, affectIndivC)
#' @rawNamespace import(reshape2, Hmisc, stats, utils, ggplot2, longitudinalData)
#' @export

#akmeans.clust <- function(traj, id_field = FALSE, init_method = "lpm", k = 3){
akmedoids.clust <- function(traj, id_field = FALSE, method = "linear", k = c(3,6)){

CaliHara <- 0

  if(length(k)==1){
    k <- rep(k, 2)
  }


if(method=="linear"){

  #k list
  k_ <- k[1]:k[2]
  #checks
  if(k[1] < 3 | k[1] > nrow(traj) | k[2] > nrow(traj) |
     k[1] > 20 | k[2] > 20 | k[1]>k[2]){
    flush.console()
    print("*******Error!********")
    if(k[1] < 3 | k[1] > nrow(traj) | k[2] > nrow(traj) |
       k[1] > 20 | k[2] > 20){
      flush.console()
      print("Enter a number GREATER than 2 but NOT GREATER 20 or the number of trajectories.")
    }

    if(k[1]>k[2]){
      flush.console()
      print("Initial number of clusters can not be less than subsequent number of clusters")
    }
    stop("(: Program terminated!!! :)")
  } else {

    dat <- traj
    #check if there is id_field
    #check if there is unique(id) field
    if(id_field==TRUE){
      n_CL <- colnames(dat)[1]
      col_names <- as.vector(dat[,1])
      dat <- dat[,2:ncol(dat)]
      #check if the 'id_field' is a unique field
      if(!length(col_names)==length(unique(col_names))){
        stop("(: The 'id_field' is not a unique field. Function terminated!!! :)")
      }
    }


    #create a time list
    time = 1:ncol(dat) #clean.time   #mode(time)
    #-----------------------------------------------------
    #looping through to calculate the coefficients
    sl_List <- NULL
    time <- as.numeric(1:ncol(dat))
    for(i in 1:nrow(dat)){ #i<-1
      b=coefficients(lm(as.numeric(as.character(dat[i,]))~as.numeric(as.character(time))))
      sl_List <- rbind(sl_List, cbind(as.numeric(b[1]), as.numeric(b[2])))
    }

    sl_List <- as.data.frame(cbind(1:nrow(sl_List), sl_List))
    colnames(sl_List) <- c("sn", "intersect","slope")  #head(sl_List)

    #-----------------------------------------------------------
    #split the slopes into 'k' partitions to determine the medioids for different value of k
    all_cluster_center_List <- list()
    i_counter <- 0
    for(s_ in k[1]:k[2]){   #s_<-3
      i_counter <- i_counter + 1
      split_slopes <- split(sl_List, cut2(sl_List$slope, g=s_))

      #determine the medoids
      median_slopes_ <- list()
      for(j in 1:length(split_slopes)){ #j=1
        #s_dty <- as.data.frame(split_slopes[j][1],,2)[,3]
        m_dty<-median(split_slopes[j][[1]]$slope)
        median_slopes_ <- rbind(median_slopes_, m_dty)
      }

      #generate regression lines based on the medoid slopes (to create the initial centroids)
      centers_List <- NULL
      for(m in 1:nrow(median_slopes_)){ #m<-1
        centers_List <- rbind(centers_List, (0 + (median_slopes_[[m,1]]*(1:ncol(dat)))))  #mean_Slopes[k,1]
      }
      centers_List <- as.data.frame(centers_List)

      all_cluster_center_List[[i_counter]] <- centers_List

    }#n_clusters


    #Generate the linear trendlines for all trajectories (dropping all intersects)
    dat_slopp<- NULL
    for(n in 1:nrow(sl_List)){ #k<-1
      dat_slopp <- rbind(dat_slopp, (0 + (sl_List[n,3]*(1:ncol(dat)))))  #head(dat_slopp)
    }

    #looping through list of k and calculate the Calinski and Harabatz criterion
final_result <- list()
    #quality
    calinski <- NULL

    result_ <- list()
    #solution_ <- list()
    for(r_ in 1:length(k_)){ #r_<-1
      #1st iteration
      part2 <- affectIndivC(dat_slopp, all_cluster_center_List[[r_]])  #------------------------------
      #part2

      distF <- list()
      distF_backup <- list()

      c_count <- unique(part2)[order(unique(part2))]

      time_1 <- 1:ncol(traj)
      simil_ <- matrix(0, 100, length(c_count))

      for(z in 1:15){  #z<-2 #number of iterations

        #recalculate the cluster centrure and do the affection
        if(z > 1){

          #pick the last
          #sort the median of the slopes of all the groups
          centers <- NULL
          for(h_ in 1:length(c_count)){#h_<-3
            #dat_slopp_ <- (dat_slopp*100)[which(part2==c_count[h_]),]
            dat_slopp_ <- as.data.frame(dat_slopp)[which(part2==c_count[h_]),] #------------------------------

            #sort the last column to and determine the median trajectory
            indexSort_ <- order(dat_slopp_[,ncol(dat_slopp_)])
            le_ <- indexSort_[ceiling(length(indexSort_)/2)]

            #now pull out the median trajectory
            centers <- rbind(centers, dat_slopp_[le_, ])
          }

          linear_centers <- as.data.frame(centers)
          part2 <- affectIndivC((dat_slopp), linear_centers)
        }

        if(z > 1){
          for(y in 1:length(c_count)){  #y<-1
            #compare
            simil_[z,y] <- (length(distF_backup[[y]]%in%which(part2==c_count[y]))/length(which(part2==c_count[y])))*100
          }
        }
        #initial back of cluster solution
        if(z==1){
          for(y in 1:length(c_count)){
            distF_backup[[y]] <- which(part2==c_count[y])
          }
        }
        #subsequent backing up of cluster solution
        if(z > 1){
          for(y in 1:length(c_count)){  #
            distF_backup[[y]] <- which(part2==c_count[y]) ##---yes, using x and y here is okay.
          }
        }
      }

      #solution
      sol_k <- alphaLabel(part2)
      attr(sol_k,"k") <- k_[r_]

      result_[[r_]] <- sol_k  #convert numberic labels to alphabetical labels r_<-1
      #attr(result_,"title") <- paste("k =",k_[r_], sep=" ")

      #get the linear trends...
      ld <- longData((dat_slopp)) #convert to longitudinal datga
      part3 <- partition(part2)
      #plotTrajMeans(ld, part1)#, colour="red"
      cr1 <- qualityCriterion(ld,part3)
      calinski <- c(calinski, round(cr1$criters[1], digits=4))
      #-----

      flush.console()
      print(paste("solution when k =", k_[r_], "determined!"))

    }#end of k loop

    #if a single value of k is provided
    if(k[1]==k[2]){
      solution_ <- list()
      solution_[[1]] <- result_[[1]]
      return(solution_[[1]])
    }

    #if a range of value is provided
    if(k[1]!=k[2]){
      qualit<- data.frame(k=k[1]:k[2], CaliHara=calinski)
      id_opt <- (which(qualit[,2]==max(qualit))[1] +(k[1]-1))
      #plot
      plt <- ggplot(qualit, aes(x = k, y = CaliHara)) +
        geom_line(linetype = "dotdash") + geom_point(shape=0)+
        ggtitle(paste("Optimal solution based on the Calinski-Harabatz criterion: k = ", id_opt, sep=" ")) +
        geom_vline(xintercept = (which(qualit[,2]==max(qualit))[1] +(k[1]-1)), linetype="dashed", color = "red", size=0.5)

      #all solutions
      #all_solutions <- result_
      #attr(all_solutions, "title") <- "all solution"   #names(all_solutions)

      qualiCriterion=c("Quality criterion: Calinski-Harabatz criterion")
      #determine optimal solution
      optimal_solution <- result_[[(which(qualit[,2]==max(qualit))[1])]] #all_solutions[[4]]

      flush.console()
      dev.new(width=3, height=3)
      print(plt)

      #combining the results
      #final_result <- list(solutions = result_,
                     #qualitycriterion =  qualiCriterion, optimSolution=optimal_solution)   #final_result$solutions[[1]]

      #combining the results
      final_result <- list(plt,
                           qualitycriterion =  qualiCriterion, optimSolution=optimal_solution)   #final_result$solutions[[1]]

      return(final_result)

      #, plt
    }


  } #end of else if
}

} #end of function
