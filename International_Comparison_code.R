#libraries #--------------------------------------------------
#install akmedoids:
rm(list = ls())
' "Build" > "Install and Rebuild" '

#library(rgdal)
#install.packages("raster")
#library(raster)
library(foreign)

#workspace setting #------------------------------------------
study_Area <- "wm_bm_"

crime_type <- "PC_"


#------------------------------------------------------------------------------------------------------------------
#STUDY AREA 1 - BIRMINGHAM
#------------------------------------------------------------------------------------------------------------------
#Import Birmingham study area
data <- read.table(paste("C:/Users/monsu/Documents/GitHub/Packages/v013/data2/",
                         study_Area, "LSOA_", crime_type, "01_12.csv", sep=""), sep=",", head=TRUE)  #head(data) #nrow(data)

#read the attribute table of the ESRI data (for 'study_area1'), including the population data
df <- read.dbf(paste("C:/Users/monsu/Documents/GitHub/Packages/v013/data2/shp1/",
                     study_Area, "LSOA_", "pop_02_12.dbf", sep=""))
#move the first row
df <- as.data.frame(cbind(df$code, df[,1:(ncol(df)-1)]))   #df[which(df[,1]=="E01009510"),]
head(df) #nrow(df) mode(df)

#subset the crime data with LSOA ids
data_sub <- as.data.frame(data[which(data$code %in% as.vector(df[,1])),])  #head(data_sub)   #data[1,] #mode(data_sub)

#Calculate 'rates' ##==========
crime_per_000_people <- rates(data_sub, denomin=df, id_field=TRUE,
                              multiplier = 1000)   #head(crime_per_000_people)  #nrow(crime_per_000_people)


#detecting the outliers 'outlierDetect' ##===========
#Here a manual value is set after careful examination of the overall data distribution  #imp_traj_New <- crime_per_000_people

##imp_traj_New <- outlierDetect(crime_per_000_people, id_field = TRUE, method = 2,
                              ##threshold = 500, count = 1, replace_with = 1)  #warnings()  #crime_per_000_people[50,]

#---------------------------------------------------------------
if(study_Area == "wm_bm_" && crime_type == "PC_"){  #head(crime_per_000_people)
  out_List <- NULL
  for(w in 1:nrow(crime_per_000_people)){#w<-1
    sn_ <- length(which(crime_per_000_people[w,2:ncol(crime_per_000_people)]>5))  #1605
    if(sn_>0){
      out_List <- c(out_List,w)
    }
  }

  #out_List<-c(604, 609)
  #replace each of these with next two records#data[1605:1606,]
  for(w in 1:length(out_List)){#w<-1
    crime_per_000_people[out_List[w],2:ncol(crime_per_000_people)] <- crime_per_000_people[(out_List[w]+2),2:ncol(crime_per_000_people)]
  }
}
#-------#head(data)
#---------------------------------------------------------------

#Calculate 'proportions' ##===========
prop_crime_per000_people <- props(crime_per_000_people, id_field = TRUE,
                                  digits=4, scale = 1)  #head(prop_crime_per000_people) #prop_crime_per000_people[,2]

#clustering: 'akmedoids.clust' ##==========

cluster_output <- akmedoids.clust(prop_crime_per000_people, id_field = TRUE,
                                  method = "linear", k = c(3,20))

#retrieve cluster attributes: 'statPrint' ##========

clustr <- as.vector(cluster_output$optimSolution)

#line plot
print(statPrint(clustr, prop_crime_per000_people, id_field=TRUE, reference = 1,
                N.quant = 8, type="lines", y.scaling="free"))

#areal plot
print(statPrint(clustr, prop_crime_per000_people, id_field=TRUE, reference = 1,
                N.quant = 4, type="stacked"))

p.values <- Cmedians.direction(clustr, traj=prop_crime_per000_people, id_field=TRUE, Nsample=999)
p.values

bm_result_ <- prop_crime_per000_people
bm_traj_ <- clustr
#-------------------------------------------------------------------------------------------------------------------



#------------------------------------------------------------------------------------------------------------------
#STUDY AREA 2 - GLASGOW
#------------------------------------------------------------------------------------------------------------------

study_Area <- "gl2_"

crime_type <- "PC_"

#Import Birmingham study area
data <- read.table(paste("C:/Users/monsu/Documents/GitHub/Packages/v013/data2/",
                         study_Area, "LSOA_", crime_type, "01_12.csv", sep=""), sep=",", head=TRUE)  #head(data) #nrow(data)

#read the attribute table of the ESRI data (for 'study_area1'), including the population data
df <- read.dbf(paste("C:/Users/monsu/Documents/GitHub/Packages/v013/data2/shp2/",
                     study_Area, "LSOA_", "pop_02_12.dbf", sep="")) #nrow(df)

#move the first row
df <- as.data.frame(cbind(df$code, df[,1:(ncol(df)-1)]))   #df[which(df[,1]=="E01009510"),]
head(df) #nrow(df) mode(df)

#subset the crime data with LSOA ids
data_sub <- as.data.frame(data[which(data$code %in% as.vector(df[,1])),])  #head(data_sub)  nrow(data_sub) #data[1,] #mode(data_sub)

#Calculate 'rates' ##==========
crime_per_000_people <- rates(data_sub, denomin=df, id_field=TRUE,
                              multiplier = 1000)   #head(crime_per_000_people)  #nrow(crime_per_000_people)


#detecting the outliers 'outlierDetect' ##===========
#Here a manual value is set after careful examination of the overall data distribution  #imp_traj_New <- crime_per_000_people

##imp_traj_New <- outlierDetect(crime_per_000_people, id_field = TRUE, method = 2,
##threshold = 500, count = 1, replace_with = 1)  #warnings()  #crime_per_000_people[50,]

#---------------------------------------------------------------
if(study_Area == "wm_bm_" && crime_type == "PC_"){  #head(crime_per_000_people)
  out_List <- NULL
  for(w in 1:nrow(crime_per_000_people)){#w<-1
    sn_ <- length(which(crime_per_000_people[w,2:ncol(crime_per_000_people)]>5))  #1605
    if(sn_>0){
      out_List <- c(out_List,w)
    }
  }

  #out_List<-c(349, 386)
  #replace each of these with next two records#data[1605:1606,]
  for(w in 1:length(out_List)){#w<-1
    crime_per_000_people[out_List[w],2:ncol(crime_per_000_people)] <- crime_per_000_people[(out_List[w]+2),2:ncol(crime_per_000_people)]
  }
}
#-------#head(data)
#---------------------------------------------------------------

#Calculate 'proportions' ##===========
prop_crime_per000_people <- props(crime_per_000_people, id_field = TRUE,
                                  digits=4, scale = 1)  #head(prop_crime_per000_people) #prop_crime_per000_people[,2]

#clustering: 'akmedoids.clust' ##==========

cluster_output <- akmedoids.clust(prop_crime_per000_people, id_field = TRUE,
                                  method = "linear", k = c(3,20))

#retrieve cluster attributes: 'statPrint' ##========

clustr <- as.vector(cluster_output$optimSolution)

#line plot
print(statPrint(clustr, prop_crime_per000_people, id_field=TRUE, reference = 1,
                N.quant = 8, type="lines", y.scaling="free"))

#areal plot
print(statPrint(clustr, prop_crime_per000_people, id_field=TRUE, reference = 1,
                N.quant = 4, type="stacked"))

p.values <- Cmedians.direction(clustr, traj=prop_crime_per000_people, id_field=TRUE, Nsample=999)
p.values

gl_result_ <- prop_crime_per000_people
gl_traj_ <- clustr
#-------------------------------------------------------------------------------------------------------------------



#------------------------------------------------------------------------------------------------------------------
#STUDY AREA 2 - BRISBANE
#------------------------------------------------------------------------------------------------------------------
getwd()

dir_ <- "C:/Users/monsu/Documents/GitHub/Packages/v013/"
dir <- "C:/Users/monsu/Documents/GitHub/Packages/v013/data3/"

#-------------------------------------
#spatial unit of analysis----------
#location attributes
#df <- read.dbf(paste(dir, "Brisbane_merge.dbf", sep="")) #
df <- read.dbf(paste(dir, "brsa2_fnl.dbf", sep=""))
head(df)

#this data is originally in meshblock
dat <- read.table(file=paste(dir, "Brisbane_Property.csv", sep=""), sep=",", head=TRUE)
head(dat)  #nrow(dat)

#unique time
uni_time <- unique(dat$Year)[order(unique(dat$Year))]

#location ids
uni_loc <- unique(df$SA1_16PID)  #length(uni_loc)

#skip from here
#----------------------------------------------------
data <- NULL
#loop through each location and collate crime by year
  for(i in 1:length(uni_loc)){#i<-1
    sub_ <- dat[which(as.vector(dat$SA1_16PID)==as.vector(uni_loc[i])),]
    colate_ <- NULL
    for(j in 1:length(uni_time)){#j<-1
      len_ <- length(which(sub_$Year==uni_time[j]))
      colate_ <- c(colate_, len_)
    }
    data <- rbind(data, c(as.vector(uni_loc[i]),as.numeric(colate_)))
  }
#mesh has now been aggregated to SA1 unit
write.table(data, file=paste(dir, "data_exp.csv", sep=""), sep=",", row.names = F)
Sys.sleep(5)
#----------------------------------------------------
#to here..

#reading the aggregated data.
dataimp <- read.table(file=paste(dir, "data_exp.csv", sep=""), sep=",", head=T)
head(dataimp)
#dataimp[1,2]+1

head(dataimp)
colnames(dataimp) <- c("code", uni_time)
rownames(dataimp) <- 1:nrow(dataimp)

#remove the first row (which appear to be spurious) of crime data
#remove 1st year of the data
dataimp2 <- dataimp[ ,-c(2, ncol(dataimp))]

#skip from here
#----------------------------------------------------------
#Import population data
dir2 <- "C:/Users/monsu/Documents/GitHub/Packages/v013/data3/Brisbane Data/Correspondence _Population/"

pop_ <- read.table(file=paste(dir2, "meshblock_Pop.csv", sep=""), sep=",", head=TRUE)
head(pop_)

pop_data <- NULL
#loop through each unique location and sum the population of the mesh..
for(i in 1:length(uni_loc)){ #i<-1
    sub_ <- pop_[which(as.vector(pop_$code_mesh)==as.vector(uni_loc[i])),]
    sub_[1,2:ncol(sub_)] <- colSums(sub_[,2:ncol(sub_)])
    pop_data <- rbind(pop_data, sub_[1,])
}
head(pop_data)
#----------------------------------------------------------

#add two rows carrying "NA" for the '2008', 2009' and '2010'

append_pop <- matrix(NA, nrow(pop_data), 3)

pop_data <- cbind(pop_data[,1], append_pop, pop_data[2:ncol(pop_data)])
pop_data  <- as.data.frame(pop_data )
colnames(pop_data) <- c("code", 2:ncol(pop_data))
rownames(pop_data) <- 1:nrow(pop_data)
head(pop_data)

#data imputation for the population data (using regression option)
pop_imp_result <- dataImputation(pop_data, id_field = TRUE, method = 2, replace_with = 1, fill_zeros = FALSE)
head(pop_imp_result)

#Calculate 'rates' ##==========
crime_per_000_people <- rates(dataimp2, denomin=pop_imp_result, id_field=TRUE,
                              multiplier = 1000)   #head(crime_per_000_people)  #nrow(crime_per_000_people) crime_per_000_people[90,5]=="NaN" +1
#-----------------------------------------------------------------------
#skip up to here

#RUN FROM HERE NOW..
#--------------------------------------------------------------------------
#CONVERT BACK TO PER 100 RATE..

#Data already exported and cleaned in Excel..
###export the data, remove the 'NaN' in excel and re-import
###write.table(crime_per_000_people, file="crime_per_000_people.csv", sep=",", row.names = F) #getwd()
crime_per_000_people_ <- read.table(file=paste(dir_, "crime_per_000_people_.csv", sep = ""), sep=",", head=TRUE)

#imputation
crime_per_000_people_ <- dataImputation(crime_per_000_people_, id_field = TRUE, method = 2, replace_with = 1, fill_zeros = FALSE)
#head(crime_per_000_people_)


#Calculate 'proportions' ##===========
prop_crime_per000_people_ <- props(crime_per_000_people_, id_field = TRUE,
                                  digits=7, scale = 1)  #head(prop_crime_per000_people_) #prop_crime_per000_people[,2]

#clustering: 'akmedoids.clust' ##==========

cluster_output <- akmedoids.clust(prop_crime_per000_people_, id_field = TRUE,
                                  method = "linear", k = c(3,20))

#retrieve cluster attributes: 'statPrint' ##========

clustr <- as.vector(cluster_output$optimSolution)

#line plot
print(statPrint(clustr, prop_crime_per000_people_, id_field=TRUE, reference = 1,
                N.quant = 8, type="lines", y.scaling="free"))

#areal plot
print(statPrint(clustr, prop_crime_per000_people_, id_field=TRUE, reference = 1,
                N.quant = 4, type="stacked"))

#----------------------------------------------------------------------------------------------
brs_result_ <- prop_crime_per000_people_
brs_traj_ <- clustr
#----------------------------------------------------------------------------------------------


#Test for a Common Median Direction
#7.3.1 Fisher's Nonparametric Test (Fisher (1993) to determine whether
#g independent samples were drawn from distributions
#with a common median direction.
#Determine the groups that represent the 'most-significant- difference between median directions of the
#underlying distribution' - in order to identify class demarcation.




install.packages("circular")

library(circular)

x <- rvonmises(n=100, mu=circular(0), kappa=1)
angular.variance(x)

data <- circular(runif(50, 0, pi))
summary(data)

# Generate 100 observations from a von Mises distribution.
# with mean direction 0 and concentration 3.
data.vm <- rvonmises(n=100, mu=circular(0), kappa=3)
# Plot data set. All points do not fit on plot.
plot(data.vm, stack=TRUE, bins=150)

# Shrink the plot so that all points fit.
plot(data.vm, stack=TRUE, bins=150, shrink=1.5)
# Recentering the figure in a different place
plot(data.vm, stack=TRUE, bins=150, xlim=c(-1,1.2), ylim=c(-1,0))

#Analysis of variance for circular data
x <- c(rvonmises(50, circular(0), 1), rvonmises(100, circular(pi/3), 10))
group <- c(rep(0, 50), rep(1, 100))
aov.circular(x, group)
aov.circular(x, group, method="LRT")

data.vm <- rvonmises(n=100, mu=circular(0), kappa=3)
# Plot data set. All points do not fit on plot.
plot(data.vm, stack=TRUE, bins=150)


plot(rvonmises(10, circular(0), kappa=1))

arrows.circular(rvonmises(10, circular(0), kappa=1))
arrows.circular(rvonmises(10, circular(0), kappa=1), y=runif(10), col=2)
arrows.circular(rvonmises(10, circular(0), kappa=1), y=runif(10),
                x0=runif(10, -1, 1), y0=runif(10, -1, 1), col=3)


x <- c(rvonmises(50, circular(0), 1), rvonmises(100, circular(pi/3), 10))
group <- c(rep(0, 50), rep(1, 100))
aov.circular(x, group)
aov.circular(x, group, method="LRT")


x <- rvonmises(n=100, mu=circular(0), kappa=1)
group <- c(rep(1, 50), rep(2, 50))
angular.variance(x, group)



x <- rvonmises(n=100, mu=circular(0), kappa=1)
var(x, group)












#-------------------------------------------------------------------------------------------------------------------
#reading a shapefile from a directory
dir <- "C:/Users/monsu/Documents/GitHub/Packages/akmedoids v0.1.3/data2/shp1"
ff <- list.files(dir, pattern="\\.shp$", full.names=TRUE)
geo_unit_with_POPData <- lapply(ff, shapefile)
head(geo_unit_with_POPData@data) #nrow(geo_unit_with_POPData@data) #geo_unit_with_POPData[[1]][1,] plot(geo_unit_with_POPData)
