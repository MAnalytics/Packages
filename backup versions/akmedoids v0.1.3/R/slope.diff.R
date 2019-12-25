
#' @title To calculate the difference between the angles of mean slopes of two trajectory objects
#' @description This function extracts the mean slope a set of trajectories
#' @param traj1 [matrix (numeric)]: corresp
#' @param traj2 [matrix (numeric)]: corresp
#' @param id_field [ss] All must have id_field.
#' @examples
#' @details This function implements the Fisher's Nonparametric Test (Fisher, 1993) to determine whether g independent samples were drawn from distributions with a common median direction.
#' @return A matrix showing the pvalues indicating whether the medians direction of any two groups are drawn from the same distribution.
#' @rawNamespace importFrom(circular, watson.two.test)
#' @export

#fi <- WatsonU2Test.Full(bmOA_result_, brs_result_, id_field=TRUE, k = c(3,20), orientation = 1, Nsample=999)

slope.diff <- function(traj1, traj2, id_field=TRUE) {

  dat1 <- traj1
  dat2 <- traj2

  #check if 'id_field' is a unique field
  if(id_field==TRUE){
    dat1 <- dat1[,2:ncol(dat1)]
    dat2 <- dat2[,2:ncol(dat2)]
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

  m.angle1 <- mean(sl_List_rad1)
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

  m.angle2 <- mean(sl_List_rad2)
  #---------------------------------------------------------------------
  diff_ <- m.angle1 - m.angle2
  #---------------------------------------------------------------------
  return(diff_)

}
