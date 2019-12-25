#code to create matrix of crime dataset from GM_police_crime_uk data
alt_data <- read.table(file="C:/Users/monsu/Documents/MMU DOCUMENTS/wm_bm_LSOA_PC_01_12.csv", sep=",", head=TRUE)

imp <- read.csv(file="C:/Users/monsu/Documents/GitHub/Packages/akmeans/inst/GM_police_crime_uk.csv", sep=",", head=TRUE)

head(imp)

uni_LSOA <- as.vector(unique(imp$LSOA.code))
uni_Date <- as.vector(unique(imp$Month))

final_data <- NULL

for(i in 1:length(uni_Date)){ #i<-1
  subset <- imp[which(imp$Month==uni_Date[i]),] #head(subset)
  sub_ <- NULL
  for(j in 1:length(uni_LSOA)){ #j<-1
    count_ <- length(which(subset$LSOA.code==uni_LSOA[j]))
    sub_ <- rbind(sub_, cbind(uni_LSOA[j], count_))
  }
  final_data <- cbind(final_data, sub_[,2])
}

#ensuring that the data is in numeric format
final_data <- apply(final_data, 2, as.numeric)#head(final_data)

final_data <- as.data.frame(cbind(uni_LSOA, final_data))

head(final_data)

getwd()

gm.crime.sample2 <- final_data

save(gm.crime.sample2, file="data/gm.crime.sample2.rda")


#To prepare the 'assault.data.rda'other dataset

traj <- read.table(file="C:/Users/monsu/Documents/GitHub/Packages/samp.csv", sep=",", head=FALSE)

save(traj, file="data/traj.rda")

Calinski and Harabatz criterion code
References
[1] C. Genolini and B. Falissard "KmL: k-means for longitudinal data" Computational Statistics, vol 25(2), pp 317-328, 2010

[2] C. Genolini and B. Falissard "KmL: A package to cluster longitudinal data" Computer Methods and Programs in Biomedicine, 104, pp e112-121, 2011



#' @title Conversion of counts to rates
#' @description This function converts counts to rates.
#' @param traj [matrix (numeric)] longitudinal data. Each row represents an individual trajectory (of observations). The columns show the observations at consecutive time points.
#' @param denom [matrix (numeric)] denominator data containing the same number of columns as the \code{traj} object. The assumption is that columns of both the \code{traj} and \code{denom} corresponds. That is, column2, column3, ... represent time points 2, 3, ..., respectively, in each object.
#' @param id_field [numeric or character] Whether the first column of both the \code{traj} and the \code{denom} are the unique fields. Default: \code{TRUE}. If \code{FALSE} the function will not run. The unique fields do not have to be sorted but the final results will be sorted in the same order as the \code{traj} object. Non-matching rows from both objects will be excluded from the final result.
#' @param per [integer (numeric)] The denominator size to normalise with. Default is \code{100}: indicating, for example, 'incidents per 100 people'.
#' @usage rates(traj, denom, id_field = TRUE, per = 100)
#' @details Given a matrix of observations (counts) and a matrix of denominator information, this function calculate the rates measure (e.g. count per 100 resident).
#' @examples
#' @export
#' traj2 <- traj
#' @return A matrix of rates measures

rates <- function(traj, denom, id_field = TRUE, per = 100){

  dat1 <- traj
  dat2 <- denom
  #compare the number of columns
  if(ncol(traj)!=ncol(denom)){
    stop("*---Number of columns must be the same---*")
  }

  #compare the number of columns
  if(id_field==FALSE){
    stop("*---unique field must be set as 'TRUE'!---*")
  }
  #check uniqueness of the fields
  if(id_field==TRUE){
   # n_CL <- colnames(dat)[1]
    col_names1 <- as.vector(as.character(dat1[,1]))
    col_names2 <- as.vector(as.character(dat2[,1]))

    #dat <- dat[,2:ncol(dat)]
    #check if the 'id_field' is a unique field
    if(!length(col_names1)==length(unique(col_names1))){
      stop("(: The 'id_field' of the 'traj' object is not a unique field. Function terminated!!! :)")
    }
    if(!length(col_names2)==length(unique(col_names2))){
      stop("(: The 'id_field' of the 'denominator' object is not a unique field. Function terminated!!! :)")
    }
  }

  #head(OA_unique)#[1] "E00047772" "E00047775" "E00047768" "E00047681" "E00047703" "E00047687"
  #------------------------------------------------------
  #now aggregated the data by LSOA using the nest code table
  #uni_OA <- as.vector(data$code) #head(data)  #uni_OA[1:4]#[1] "E00045077" "E00045078" "E00045079" "E00045080"#OA_unique[1:4][1] "E00047772" "E00047775" "E00047768" "E00047681"

  #'traj' data
  #----------------------------------------------------------------------
  data1 <- apply(dat1[,2:ncol(dat1)], 2, as.numeric)#head(data1)
  data1 <- cbind(1:nrow(data1), data1)
  colnames(data1) <- c("ID", 1:(ncol(data1)-1))
  #----------------------------------------------------------
  #'denom' data
  #----------------------------------------------------------------------
  data2 <- apply(dat2[,2:ncol(dat2)], 2, as.numeric)#head(data2)
  data2 <- cbind(1:nrow(data2), data2)
  colnames(data2) <- c("ID", 1:(ncol(data2)-1))
  #----------------------------------------------------------

  data_Fresh <- NULL
  keep_names <- NULL
  #now normalise with population
  for(k in 1:length(col_names1)){#k<-1
    pop_cut <- as.numeric(data2[which(col_names2==col_names1[k]), 2:ncol(data2)])
    if(length(pop_cut)!=0){
    data_cut <- as.numeric(data1[k ,2:ncol(data1)])
    data_Pop_per <- (data_cut / pop_cut)*per
    data_Fresh <- rbind(data_Fresh, round(data_Pop_per,digits=5))
    keep_names <- c(keep_names, col_names1[k])
    #data[k,2] <- data_Pop_100[1]
  }
}

data_Fresh <- data.frame(cbind(keep_names, data_Fresh))
colnames(data_Fresh) <- c("id_field", 1:(ncol(data_Fresh)-1))

flush.console()
print(paste("*----Summary:----*"))
print(paste("<---", nrow(data_Fresh), "of", nrow(dat1), "(", round((nrow(data_Fresh)/nrow(dat1))*100, digits=1), "%) -->", sep=""))
print("found a match in the denominator dataset")
return(data_Fresh)
}







