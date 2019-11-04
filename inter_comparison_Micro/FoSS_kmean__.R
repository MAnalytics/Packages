library(traj)
library(akmedoids)

#codes for deprivation (poverty) index calculation
#On my personal PC : "R script_for_Deprivation 2011.R"

#population data at output level using 2011 census boundary
#https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/censusoutputareaestimatesinthewestmidlandsregionofengland

getwd()

#CODE 1: setting the workspace
#----------------------------
#setwd("C:/Users/55131065/Desktop/FoSS Slides_/")

#JQC FoSS presentation

#install.packages("kml")

#install akmedoids:
rm(list = ls())
' "Build" > "Install and Rebuild" '

#library(rgdal)
#install.packages("raster")
#library(raster)
library(foreign)
library(reshape2)
library(ggplot2)
library(akmedoids)

#LETTERS
combind_A <- LETTERS
combind <-  combn(LETTERS, m=2, sep="")# combind[1:2,]
list_Letters <- NULL
for(cc in 1:ncol(combind)){#cc=1
  list_Letters <-c(list_Letters,  paste(combind[1,cc],   combind[2,cc], sep=""))
}
list_Letters <- c(combind_A, list_Letters)


#------------------------------------------------
#for understanding inequality presentation
#------------------------------------------------

#workspace setting #------------------------------------------
study_Area <- "wm_bm_"

crime_type <- "PC_"

#------------------------------------------------------------------------------------------------------------------
#STUDY AREA 1 - BIRMINGHAM - USING THE OUTPUT AREA
#------------------------------------------------------------------------------------------------------------------
#Import Birmingham study area

dir3 <- "C:/Users/monsu/Documents/GitHub/Packages/inter_comparison_Micro/data3/"

data <- read.table(paste(dir3, "birmingham_LSOA.csv", sep=""), sep=",", head=TRUE)
#head(data) #nrow(data)#which()

#import the population data
pop_ <- read.table(paste(dir3, "birmingham_LSOA_pop.csv", sep=""), sep=",", head=TRUE)
#head(pop_)

#Calculate 'rates' ##==========
crime_per_00_people <- rates(data, denomin=pop_, id_field=TRUE,
                             multiplier = 1000)   #head(crime_per_00_people)  #nrow(crime_per_000_people)# crime_per_00_people[,2]
#data[which(data$code=="E01009510"),]
#pop_[which(pop_$code=="E01009510"),]
#ss<-data[which(data$code=="E01009510"),]
#ss<-crime_per_00_people[which(crime_per_00_people$code=="E01008881"),]

#ss<-crime_per_00_people[which(crime_per_00_people$code=="E01009510"),]#E01008881
#remove (manually) a single outlier indexed (1103)
#---------------------------------------------------------------
##out_List<-c(604, 609)
##for(w in 1:length(out_List)){#w<-1
  ##crime_per_00_people[out_List[w],2:ncol(crime_per_00_people)] <-
    ##crime_per_00_people[(out_List[w]+2),2:ncol(crime_per_00_people)]
##}

#some miscella
agg_Data <- as.data.frame(cbind(V1=as.vector(crime_per_00_people$code), crime_per_00_people[,2:ncol(crime_per_00_people)]))
head(agg_Data)
#now transfer back to data
#data <- agg_Data

#data <- apply(data[,2:16], 2, as.numeric)#head(data)
#data <- cbind(1:nrow(data), data)
#colnames(data) <- c("ID","X1","X2","X3","X4","X5","X6","X7","X8","X9","X10", "X11","X12","X13","X14","X15")


#Calculate 'proportions' ##===========
prop_crime_per00_people <- props(crime_per_00_people, id_field = TRUE,
                                 digits=10, scale = 100)
head(prop_crime_per00_people)  #write.table(prop_crime_per00_people, file="dd.csv", sep=",", row.names=F)
prop_crime_per00_people[which(prop_crime_per00_people$code=="E01008881"),]

data_backup <- prop_crime_per00_people

#drop id
##prop_crime_per00_people <- prop_crime_per00_people[,2:ncol(prop_crime_per00_people)]

#clustering: 'akmedoids.clust' ##==========
cluster_output <- akmedoids.clust(prop_crime_per00_people, id_field = TRUE,
                                  method = "linear", k = c(3,20))#

#retrieve cluster attributes: 'statPrint' ##========
clustr <- as.vector(cluster_output$optimSolution)

#line plot
print(statPrint(clustr, prop_crime_per00_people, id_field=TRUE, reference = 1, show.plot=FALSE,
                N.quant = 8, type="lines", y.scaling="free"))

#areal plot
print(statPrint(clustr, prop_crime_per00_people, id_field=TRUE, reference = 1, show.plot = TRUE,
                N.quant = 4, type="stacked"))

#p.values <- Cmedians.direction(clustr, traj=prop_crime_per00_people, id_field=TRUE, Nsample=999)
#p.values

library(reshape2)
#grp_ <- list(c(1,2), c(3,4), c(5,6,7,8)) #remove
grp_ <- list(c(1,2), c(3,4), c(5,6,7)) #don't remove
part2 <- clustr
##for (p_ in 7:30){  #p_<- 40
##&&&&&&&& 2 EDITEDDDD!!
if(study_Area == "wm_bm_" && crime_type == "PC_"){

  ## grp_ <- list(c(1), c(2,3,4), c(5,6,7,8,9)) #for burg #7

  #grp_ <- list(c(1), c(2,3,4,5,6), c(7,8,9)) #for burg #7 my criterion #head(prop_crime_per00_people)

  #rownames(data.append) <- 1:nrow(data.append)
  data.append <- cbind(1:nrow(prop_crime_per00_people), prop_crime_per00_people[,2:ncol(prop_crime_per00_people)]) #head(data.append) data.append <- data  #data <-

  #data.append <- data  #testing the proportion

  colnames(data.append) <- c("code", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")
  data.append <- as.data.frame(data.append)  #head(data.append)


  data.long.melted <- melt(data.append, id="code")#head(data., id="code"
  head(data.long.melted)

  #data.append <- cbind(data, part2)
  #data.long.melted <- melt(data, id="code")#head(d
  data.long.melted <- cbind(data.long.melted, rep(part2, 11))#nrow(data.long.melted)

  colnames(data.long.melted) <- c("OAcode","Year","Crime_Count", "clusters")
}
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#DEFINE CLUSTER LABELS HERE
#LETTERS
combind_A <- LETTERS
combind <-  combn(LETTERS, m=2, sep="")# combind[1:2,]
list_Letters <- NULL
for(cc in 1:ncol(combind)){#cc=1
  list_Letters <-c(list_Letters,  paste(combind[1,cc],   combind[2,cc], sep=""))
}
list_Letters <- c(combind_A, list_Letters)
#-------------------------------------------


clusters <- list_Letters[data.long.melted$clusters]
data.long.melted$clusters <- clusters
colnames(data.long.melted) <- c("OAcode","Year","Crime_Count", "clusters")

#library(ggplot2)
ggplot(data.long.melted, aes(x=Year, y=Crime_Count,
                             group=OAcode, color=clusters)) +
  geom_line() +
  stat_summary(fun.y=mean, geom="line", aes(group=clusters), color="black", size=2) +
  facet_wrap(~clusters) +
  theme_minimal()

#------------------------------------------------------------------

head(data.long.melted)##data.long.melted[1:50,]

#use this to calculate percentage (%) change from year 1 to year n
year_uni <- as.vector(unique(data.long.melted$Year))
order_Cluster <- as.vector(unique(data.long.melted$clusters))
clusters_uni <- order_Cluster[order(as.vector(unique(data.long.melted$clusters)))]

change_ave_yr <- NULL

for(p in 1:length(clusters_uni)){#p<-1
  all_clust_list <- data.long.melted[which(data.long.melted$clusters==clusters_uni[p]),]
  ave_yr <- NULL
  for(m in 1:length(year_uni)){ #m<-1
    yr_ <- all_clust_list[which(as.vector(all_clust_list$Year)==year_uni[m]),]#all first year data..
    ave_yr <- c(ave_yr, sum(yr_$Crime_Count))
  }
  change_ave_yr <- c(change_ave_yr, round(((ave_yr[length(ave_yr)]-ave_yr[1])/ave_yr[1])*100, digits=0))
}

change_ave_yr  <- list(change_ave_yr)

flush.console()
print(change_ave_yr)
#re_List[p_-2] <- list(change_ave_yr)

#-------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------


#PLOTTING THE PROPORTION CHART....
#------
#now area plot for crime count and proportin....

#to calculate mean (average) value of trajectories in each group

head(data.long.melted)##data.long.melted[1:50,]

#use this to calculate percentage (%) change from year 1 to year n
year_uni <- as.vector(unique(data.long.melted$Year))
order_Cluster <- as.vector(unique(data.long.melted$clusters))
clusters_uni <- order_Cluster[order(as.vector(unique(data.long.melted$clusters)))]

change_ave_yr_ALL <- NULL

for(p in 1:length(clusters_uni)){#p<-1
  all_clust_list <- data.long.melted[which(data.long.melted$clusters==clusters_uni[p]),]
  ave_yr <- NULL
  for(m in 1:length(year_uni)){ #m<-1
    yr_ <- all_clust_list[which(as.vector(all_clust_list$Year)==year_uni[m]),]#all first year data..
    ave_yr <- c(ave_yr, sum(yr_$Crime_Count))
  }
  change_ave_yr_ALL <- rbind(change_ave_yr_ALL,  ave_yr)
}

change_ave_yr_ALL  #USE THIS FOR THE DESCRIPTIVE COMPUTATION

change_ave_yr_ALL <- t(change_ave_yr_ALL)
#----------------------------------------------

#re-arrange gl2_ violent
change_ave_yr_ALL <-  change_ave_yr_ALL[,unlist(grp_)]

#Generate descriptive stats....(summary_)
#------------------------------------------------------------- grp_
#to calculate the %rise or %drop (BIRMINGHAM)


#Calculating the number of trajectories  grp_
length(which(part2%in%grp_[[3]]))
length(which(part2%in%grp_[[3]]))/length(part2) * 100


#create data frame for use with plot
#grp.dat<-matrix(grp.dat,nrow=length(t.step),ncol=length(grps))
grp.dat<-data.frame(change_ave_yr_ALL,row.names=1:11)
names(grp.dat)<-clusters_uni

#grp.dat<-data.frame(change_ave_yr_ALL,row.names=1:11)
#grp.dat <- grp.dat[,o_]  #grp.dat <- grp.dat[,o__]
#names(grp.dat)<-clusters_uni
#o_

#reshape the data
p.dat<-data.frame(step=row.names(grp.dat),grp.dat,stringsAsFactors=F) #p.dat

#----------------------------------------------

p.dat<-melt(p.dat,id='step')

p.dat$step<-as.numeric(p.dat$step) #head(p.dat)

## p.dat$type <- rep(type_, 11)
#create plots

class(p.dat$step)


#birmingham
colours <- c(rep("#00FF00", 2), rep("yellow", 2), rep("red", 3)) #burglary

#transfer the results_Offender
Year <- c("2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011")


#colours <-


p <- ggplot(p.dat,aes(x=step,y=value)) + theme(legend.position="none")  #grp_

p + geom_area(aes(fill=variable), colour = "gray5", position='fill', size = 0.5) +
  scale_fill_manual(values = colours) +
  scale_x_continuous(breaks=1:11, labels=Year) + theme_light()

#removing the sub-grouplines
p + geom_area(aes(fill=variable), colour = 'NA', position='fill', size = 0.5) +
  scale_fill_manual(values = colours) +
  scale_x_continuous(breaks=1:11, labels=Year) + theme_light()



#-------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------



#plotting (SPATIAL)
#------------------------------------------------------------------------------

#IN ORDER TO PLOT, THE FOLLOWING HAS TO BE RAN


#prepare new data
##data <- prop_crime_per00_people[,2:ncol(prop_crime_per00_people)]

#================================
#THE LIKELIHOOD FUNCTION USING SUM OF INDIVIDUAL TRAJECTORY
# Setup output table
#calculate the cumulative rate of change (first difference) of the mean value ......
clusters <- clusters[1:nrow(prop_crime_per00_people)]

data_reMelt <- dcast(data.long.melted, OAcode ~ Year, value.var='Crime_Count')
data_reMelt <- cbind(data_reMelt, clusters) #head(data_reMelt)

cluster_Group <- as.vector(unique(data_reMelt$clusters)[order(unique(data_reMelt$clusters))])


#---------------------------------------------------------------------

#---------------------------------------------------------------------------
#plot each cluster....

length_Of_Clusters <- NULL

cluster_Units_ALL <- list()


for(i in 1:length(cluster_Group)){ #i<-1

  data_reMelt_Cut <- data_backup[which(data_reMelt$clusters==cluster_Group[i]),]#head(data_reMelt)

  if(is.null(data_reMelt_Cut)){data_reMelt_Cut<-matrix(data_reMelt_Cut,,12)
  colnames(data_reMelt_Cut) <- c("code", "1", "2", "3", "4", "5", "6", "7", "8",
                                 "9", "10", "11")
  }
  #get the corresponding OA label #head(data)# head(agg_Data[1:10,])
  cluster_Units <- as.vector(agg_Data$V1[as.data.frame(data_reMelt_Cut)$code])

  length_Of_Clusters <- rbind(length_Of_Clusters, cbind(i, length(cluster_Units)))

  cluster_Units_ALL[i] <- list(cluster_Units)
}

# par()#getwd()
library(rgdal)

WM_LSOA <- readOGR(dsn=".", paste(study_Area, "LSOA_hex", sep="")) #geo_unit_with_POPData
city_Centre <- readOGR(dsn=".", paste(study_Area, "citycentre2_2", sep="")) #geo_unit_with_POPData

#WM_LSOA <- readOGR(dsn=".", "Birmingham_hexmap_LSOA_osgb")

##WM_LSOA <- readOGR(dsn=".", "BM_Centroids")  #head(WM_LSOA@data)  #plot(WM_LSOA, border="grey", pch=1)

#par(mfrow=c(3,4))
#par(oma=c(0,0,0,0), mar=c(0,0,0,0), mfrow=c(3,1))

par(oma=c(0,0,0,0), mar=c(0,0,0,0), mfrow=c(1,1))




if(study_Area == "wm_bm_" && crime_type == "PC_"){
  grp_ <- list(c(1,2), c(3,4), c(5,6,7)) #for burg
  colour_wm_bm_ <- colours
}


sp::plot(WM_LSOA, col="grey", border="grey", pch=16)

#plotting for gradient colour
#--------------------------------------------
cc_ <- 0
countt_ <- 0
#spatial patterning
for(q in 1:length(grp_)){ #q<-1

  #for(p in 1:length(cluster_Units_ALL)){

  #combine
  colat_<- NULL

  for(f in 1:length(grp_[[q]])){ # f<-1
    cc_ <- cc_ + 1
    ##colat_ <- c(colat_, cluster_Units_ALL[[grp_[[q]][f]]])
    colat_ <- cluster_Units_ALL[[grp_[[q]][f]]]
    print(length(cluster_Units_ALL[[grp_[[q]][f]]]))
    #}

    #sp::plot(WM_LSOA[which(as.vector(WM_LSOA$OAcode)%in%colat_),], add=TRUE, col="darkgrey", border="black", pch=16)
    #sp::plot(WM_LSOA[1,], add=TRUE, col="grey", border="black")
    #sp::plot(WM_LSOA[which(as.vector(WM_LSOA$OAcode)%in%colat_),], add=TRUE, col="black", border="black", pch=16) #check that OA field is correct
    sp::plot(WM_LSOA[which(as.vector(WM_LSOA$code)%in%colat_),], add=TRUE, col=colours[cc_], border="grey50", pch=16) #check that OA field is correct

  }
  #countt_ <- countt_ + length(unlist(grp_[c(q)]))

}
#}

sp::plot(city_Centre, col="NA", border="black", pch=16, add=TRUE, lwd=3)


#-------------------------------------------------------------------------

#plotting stack historigram (bar chart) to show deprivation vs. exposure to crime
library(rgdal)
library(ggplot2)
library(data.table)
library(dplyr)


#---------------------------------------------------------------------------------------
grp_ <- list(c(1), c(2,3,4), c(5,6,7,8,9)) #for burg | Birming
grp_ <- list(c(1), c(2:12), c(13:19)) #for burg | Birming #optimal customise#  #using my optimal solution criteria...


grp_ <- list(c(1,2,3,4,5), c(6,7,8,9), c(10,11,12)) #for burg glasgow | #optimal customise#  #using my optimal solution criteria...
grp_ <- list(c(1:6), c(7:11), c(12:17)) #for viole | glasgow

grp_ <- list(c(1,2,3,4), c(5,6,7,8,9,10,11), c(12,13,14,15,16,17,18)) #
grp_ <- list(c(1,2,3,4), c(5,6,7,8,11), c(9,10,12,13))
#glasgow 2


import_map_ <- readOGR(dsn=".", paste(study_Area, "LSOA_hex", sep="")) #geo_unit_with_POPData
head(import_map_@data)

TSD_Index <- read.table(file = "TSD_birmingham_2001_2011.csv", sep=",", head = TRUE) #bi
TSD_Index <- read.table(file = "TSD_glasgow_2001_2011.csv", sep=",", head = TRUE) #bi
TSD_Index <- read.table(file = "TSD_england_2001_2011.csv", sep=",", head = TRUE) #bi
TSD_Index <- read.table(file = "TSD_scotland_2001_2011.csv", sep=",", head = TRUE) #bi
#TSD_Index <- read.table(file = "birmingham_TSD_2011.csv", sep=",", head = TRUE) #bi
#TSD_Index <- read.table(file = "glasgow_TSD_2011.csv", sep=",", head = TRUE) #bi
head(TSD_Index)  #TOWNSNDRANK2001; TOWNSNDRANK2011 #nrow(TSD_Index)

#import the deprivation data and join to shapefile...
import_map <- merge(import_map_, TSD_Index, by.x="code", by.y="code")
import_map <- import_map@data #head(import_map) #import_map[which(import_map$code==""),]

#now get the cluster solution from above

#to calculate and use the mean value of 2001 and 2011  #getwd()
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
hist_D <- NULL

for(q in 1:length(grp_)){ #q<-1

  colate_All <- NULL

  for(f in 1:length(grp_[[q]])){ # f<-1

    colat_ <- cluster_Units_ALL[[grp_[[q]][f]]]
    #collate from the import map the lsoa that happens in colat_
    colat_2 <- import_map[which(import_map$code %in% colat_),]  #as.vector(colat_2$code)
    colate_All <- rbind(colate_All, colat_2) #nrow(colate_All)
  }

  to_Keep <- NULL
  for(y_ in 1:2){#y_=1
    if(y_==1){
      tab_ <- colate_All$TOWNSNDRANK2001  #CHANGE THIS...?????????????????????????????????? for the year
      #get the frequency
      tab_2 <- matrix(0, 5, 2)
      for(b_ in 1:nrow(tab_2)){ #b_<-1
        tab_2[b_,1]<-b_
        tab_2[b_,2]<-length(which(tab_==b_))
      }
    }
    if(y_==2){
      tab_ <- colate_All$TOWNSNDRANK2011  #CHANGE THIS...?????????????????????????????????? for the year
      #get the frequency
      tab_2 <- matrix(0, 5, 2)
      for(b_ in 1:nrow(tab_2)){ #b_<-1
        tab_2[b_,1]<-b_
        tab_2[b_,2]<-length(which(tab_==b_))
      }
    }
    colnames(tab_2) <- c("Var1", "Freq")
    tab_2 <- as.data.frame(tab_2)
    #flipping the class ----
    tab_2$Freq <- as.numeric(as.character(tab_2$Freq[length(tab_2$Freq):1]))
    #now convert to proportion
    tab_2$Freq <- round(as.numeric(tab_2$Freq)/sum(as.numeric(tab_2$Freq)), digits=3)

    to_Keep <- cbind(to_Keep, tab_2[,2])

  }

  #calculate mean
  m_ <- matrix(0, 5, 1)
  for(m in 1:nrow(to_Keep)){#m<-1
    m_[m,1] <- mean(to_Keep[m,])
  }
  #tab_2$Gr1 <- paste("Group_",q, sep="")
  m_ <- as.data.frame(m_)
  m_$Gr <- nrow(m_):1


  hist_D <- cbind(hist_D, m_[,1])

}


write.table(hist_D, file=paste(study_Area, "ppC_mean_TOWNSNDRANK01_11.csv", sep=""), sep=",", row.names=F)


#-------------------------------------------------------------------------------------
#to plot bar charts of deprivation classes, STACKED with each inequality group (LOCAL deprivation count)
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------

#head(Data_with_Geo)
#head(dep_Areas)


WM_LSOA <- readOGR(dsn=".", paste(study_Area, "LSOA_hex", sep="")) #geo_unit_with_POPData
city_Centre <- readOGR(dsn=".", paste(study_Area, "citycentre", sep="")) #geo_unit_with_POPData

par(oma=c(0,0,0,0), mar=c(0,0,0,0), mfrow=c(1,1))

#---------------------------------------------------------------------------------------
grp_ <- list(c(1), c(2,3,4), c(5,6,7,8,9)) #for burg | Birming
grp_ <- list(c(1), c(2:12), c(13:19)) #for burg | Birming #optimal customise#  #using my optimal solution criteria...


grp_ <- list(c(1,2,3,4,5), c(6,7,8,9), c(10,11,12)) #for burg glasgow | #optimal customise#  #using my optimal solution criteria...
grp_ <- list(c(1:6), c(7:11), c(12:17)) #for viole | glasgow

#---------------------------------------------------------------------------------------
grp_ <- list(c(1,2,3,4), c(5,6,7,8,9,10,11), c(12,13,14,15,16,17,18)) #for burg | Glasgow #optimal customise

grp_ <- list(c(1,2,3,4), c(5,6,7,8,11), c(9,10,12,13)) #for violent


import_map_ <- readOGR(dsn=".", paste(study_Area, "LSOA_hex", sep="")) #geo_unit_with_POPData
head(import_map_@data)

#TSD_Index <- read.table(file = "TSD_birmingham_2001_2011.csv", sep=",", head = TRUE) #bi
TSD_Index <- read.table(file = "TSD_glasgow_2001_2011.csv", sep=",", head = TRUE) #bi

head(TSD_Index)  #TOWNSNDRANK2001; TOWNSNDRANK2011 #nrow(TSD_Index)

#import the deprivation data and join to shapefile...
import_map <- merge(import_map_, TSD_Index, by.x="code", by.y="code")
import_map <- import_map@data #head(import_map) #import_map[which(import_map$code==""),]

#now get the cluster solution from above

#first step:below ensures that we are dealing only with lsoas that are represented in the cluster solution...
#-------------------------------------------------------------------------------------
list_D <- NULL
#collating the cluster solution...
for(q in 1:length(grp_)){ #q<-1

  colate_All <- NULL

  for(f in 1:length(grp_[[q]])){ # f<-1

    colat_ <- cluster_Units_ALL[[grp_[[q]][f]]]
    #collate from the import map the lsoa that happens in colat_
    colat_2 <- import_map[which(import_map$code %in% colat_),]  #as.vector(colat_2$code) #head(import_map)
    colate_All <- rbind(colate_All, colat_2) #nrow(colate_All)
  }
  list_D <- rbind(list_D, colate_All) #nrow(list_D)#head(list_D) #length(unique(list_D$code))
}
#length(which(list_D$TOWNSNDRANK2001=="5"))
#---------------------------------------------------------------------------------------------

#NOTEEEEEEE! please, not that the deprivation class annotated as 1 is actually 5
#the arrangement below takes care of it..... when I call "5", I am calling the most deprived.....
dep_Classes <- c(5, 4, 3, 2, 1)

both_Yr <- list()

for(i in 1:length(dep_Classes)){#i<-1
  #get the total crime rate in the most deprived, the least deprived.
  #get the list of area in this group
  #remember to change the YEAR...
  for(j in 1:2){ #j<-1;  for 2001 and 2011
    #for 2001
    if(j==1){
      dat_dep_1 <- list_D[which(list_D$TOWNSNDRANK2001==dep_Classes[i]),] #head(dat_dep_1)
      #collating the stats of the three major groups
      sub_list_D1 <- NULL
      #collating the cluster solution...
      for(q in 1:length(grp_)){ #q<-1
        colate_All <- NULL
        #collating main groups and not the subgroups
        for(f in 1:length(grp_[[q]])){ # f<-1
          colat_ <- cluster_Units_ALL[[grp_[[q]][f]]]
          #collate from the import map the lsoa that happens in colat_
          colat_2 <- dat_dep_1[which(dat_dep_1$code %in% colat_),]  #as.vector(colat_2$code)
          colate_All <- rbind(colate_All, colat_2) #nrow(colate_All)
        }
        sub_list_D1 <- rbind(sub_list_D1, nrow(colate_All)) #nrow(list_D)
      }
    }

    #for 2011
    if(j==2){
      dat_dep_1 <- list_D[which(list_D$TOWNSNDRANK2011==dep_Classes[i]),] #head(dat_dep_1)
      #collating the stats of the three major groups
      sub_list_D2 <- NULL
      #collating the cluster solution...
      for(q in 1:length(grp_)){ #q<-1
        colate_All <- NULL
        #collating main groups and not the subgroups
        for(f in 1:length(grp_[[q]])){ # f<-1
          colat_ <- cluster_Units_ALL[[grp_[[q]][f]]]
          #collate from the import map the lsoa that happens in colat_
          colat_2 <- dat_dep_1[which(dat_dep_1$code %in% colat_),]  #as.vector(colat_2$code)
          colate_All <- rbind(colate_All, colat_2) #nrow(colate_All)
        }
        sub_list_D2 <- rbind(sub_list_D2, nrow(colate_All)) #nrow(list_D)
      }
    }
    both_Yr[i] <- list(cbind(sub_list_D1, sub_list_D2))
  }
}

#calculate the mean for export for plotting in 'trendline (version 1).xlsx'
store_Mean <- NULL
for(k in 1:length(both_Yr)){#k<-1
  c_ <- both_Yr[[k]]
  ave_ <- NULL
  for(m in 1:nrow(c_)){#m<-1
    ave_ <- c(ave_, mean(c_[m,]))
  }
  store_Mean <- cbind(store_Mean, ave_)
}


write.table(store_Mean, file=paste(study_Area, crime_type,  "new_dp4.csv", sep=""), sep=",", row.names=F)


#-------------------------------------------------------------------------------------
#to plot bar charts of deprivation classes, STACKED with each inequality group (GLOBAL deprivation count)
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------

#head(Data_with_Geo)
#head(dep_Areas)

WM_LSOA <- readOGR(dsn=".", paste(study_Area, "LSOA_hex", sep="")) #geo_unit_with_POPData
city_Centre <- readOGR(dsn=".", paste(study_Area, "citycentre", sep="")) #geo_unit_with_POPData

par(oma=c(0,0,0,0), mar=c(0,0,0,0), mfrow=c(1,1))

#---------------------------------------------------------------------------------------
grp_ <- list(c(1), c(2,3,4), c(5,6,7,8,9)) #for burg | Birming
grp_ <- list(c(1), c(2:12), c(13:19)) #for burg | Birming #optimal customise#  #using my optimal solution criteria...


grp_ <- list(c(1,2,3,4,5), c(6,7,8,9), c(10,11,12)) #for burg glasgow | #optimal customise#  #using my optimal solution criteria...
grp_ <- list(c(1:6), c(7:11), c(12:17)) #for viole | glasgow

#---------------------------------------------------------------------------------------
grp_ <- list(c(1,2,3,4), c(5,6,7,8,9,10,11), c(12,13,14,15,16,17,18)) #for burg | Glasgow #optimal customise

grp_ <- list(c(1,2,3,4), c(5,6,7,8,11), c(9,10,12,13)) #for violent

import_map_ <- readOGR(dsn=".", paste(study_Area, "LSOA_hex", sep="")) #geo_unit_with_POPData
head(import_map_@data)

#TSD_Index <- read.table(file = "TSD_england_2001_2011.csv", sep=",", head = TRUE) #bi
TSD_Index <- read.table(file = "TSD_scotland_2001_2011.csv", sep=",", head = TRUE) #bi

head(TSD_Index)  #TOWNSNDRANK2001; TOWNSNDRANK2011 #nrow(TSD_Index)

#crop out the TSD_Index that exist in the shapefile...
cd_ <- as.vector(import_map_@data$code)
TSD_Index <- TSD_Index[which(TSD_Index$code %in% cd_),]
head(TSD_Index)

#import the deprivation data and join to shapefile...
import_map <- merge(import_map_, TSD_Index, by.x="code", by.y="code")
import_map <- import_map@data #head(import_map) #import_map[which(import_map$code==""),]

#now get the cluster solution from above

#first step:below ensures that we are dealing only with lsoas that are represented in the cluster solution...
#-------------------------------------------------------------------------------------
list_D <- NULL
#collating the cluster solution...
for(q in 1:length(grp_)){ #q<-1

  colate_All <- NULL

  for(f in 1:length(grp_[[q]])){ # f<-1

    colat_ <- cluster_Units_ALL[[grp_[[q]][f]]]
    #collate from the import map the lsoa that happens in colat_
    colat_2 <- import_map[which(import_map$code %in% colat_),]  #as.vector(colat_2$code) #head(import_map)
    colate_All <- rbind(colate_All, colat_2) #nrow(colate_All)
  }
  list_D <- rbind(list_D, colate_All) #nrow(list_D)#head(list_D) #length(unique(list_D$code))
}
#length(which(list_D$TOWNSNDRANK2001=="5"))
#---------------------------------------------------------------------------------------------

#NOTEEEEEEE! please, not that the deprivation class annotated as 1 is actually 5
#the arrangement below takes care of it..... when I call "5", I am calling the most deprived.....
dep_Classes <- c(5, 4, 3, 2, 1)

both_Yr <- list()

for(i in 1:length(dep_Classes)){#i<-1
  #get the total crime rate in the most deprived, the least deprived.
  #get the list of area in this group
  #remember to change the YEAR...
  for(j in 1:2){ #j<-1;  for 2001 and 2011
    #for 2001
    if(j==1){
      dat_dep_1 <- list_D[which(list_D$TOWNSNDRANK2001==dep_Classes[i]),] #head(dat_dep_1)
      #collating the stats of the three major groups
      sub_list_D1 <- NULL
      #collating the cluster solution...
      for(q in 1:length(grp_)){ #q<-1
        colate_All <- NULL
        #collating main groups and not the subgroups
        for(f in 1:length(grp_[[q]])){ # f<-1
          colat_ <- cluster_Units_ALL[[grp_[[q]][f]]]
          #collate from the import map the lsoa that happens in colat_
          colat_2 <- dat_dep_1[which(dat_dep_1$code %in% colat_),]  #as.vector(colat_2$code)
          colate_All <- rbind(colate_All, colat_2) #nrow(colate_All)
        }
        sub_list_D1 <- rbind(sub_list_D1, nrow(colate_All)) #nrow(list_D)
      }
    }

    #for 2011
    if(j==2){
      dat_dep_1 <- list_D[which(list_D$TOWNSNDRANK2011==dep_Classes[i]),] #head(dat_dep_1)
      #collating the stats of the three major groups
      sub_list_D2 <- NULL
      #collating the cluster solution...
      for(q in 1:length(grp_)){ #q<-1
        colate_All <- NULL
        #collating main groups and not the subgroups
        for(f in 1:length(grp_[[q]])){ # f<-1
          colat_ <- cluster_Units_ALL[[grp_[[q]][f]]]
          #collate from the import map the lsoa that happens in colat_
          colat_2 <- dat_dep_1[which(dat_dep_1$code %in% colat_),]  #as.vector(colat_2$code)
          colate_All <- rbind(colate_All, colat_2) #nrow(colate_All)
        }
        sub_list_D2 <- rbind(sub_list_D2, nrow(colate_All)) #nrow(list_D)
      }
    }
    both_Yr[i] <- list(cbind(sub_list_D1, sub_list_D2))
  }
}

#calculate the mean for export for plotting in 'trendline (version 1).xlsx'
store_Mean <- NULL
for(k in 1:length(both_Yr)){#k<-1
  c_ <- both_Yr[[k]]
  ave_ <- NULL
  for(m in 1:nrow(c_)){#m<-1
    ave_ <- c(ave_, mean(c_[m,]))
  }
  store_Mean <- cbind(store_Mean, ave_)
}


write.table(store_Mean, file=paste(study_Area, crime_type,  "new_dp9.csv", sep=""), sep=",", row.names=F)


#creating a 2 x 2 ratio table for least deprived and most deprived : FoSS presentation...i<-3.
#-------------------------------------------------------------------------------------     #getwd()
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
hist_D_2x2 <- matrix(0, 2, 2)

#get all the area together...

dep_Areas <- NULL
for(q in 1:length(grp_)){ #q<-1

  colate_All <- NULL

  #combine all groups together....
  #get the list of areas and their deprivations
  #I could easily just import teh deprivation information without necessarily having to do this below looping...
  #however, this helps to filter out the LSOA that are not part of the analysis (cluster solution)
  for(f in 1:length(grp_[[q]])){ # f<-1

    colat_ <- cluster_Units_ALL[[grp_[[q]][f]]]
    #collate from the import map the lsoa that happens in colat_
    colat_2 <- import_map[which(import_map$code %in% colat_),]  #as.vector(colat_2$code)
    colate_All <- rbind(colate_All, colat_2) #nrow(colate_All)
  }

  dep_Areas <- rbind(dep_Areas, colate_All) #nrow(dep_Areas) #head(dep_Areas)
}


#now bring in the crime rate datasets; remember, not crime proportion....
#------------------------------

study_Area <- "wm_bm_"
#study_Area <- "gl_"

#crime_type <- "PC_"
crime_type <- "VC_"

#-----------------------------------------------------------------------------------------------------------------------------------------
#read crime dataset
data <- read.table(paste(study_Area, "LSOA_", crime_type, "01_12.csv", sep=""), sep=",", head=TRUE)
head(data)#nrow(data)


#read the geographical shapefile and population data
geo_unit_with_POPData <- readOGR(dsn=".", paste(study_Area, "LSOA_", "pop_02_12", sep=""))
head(geo_unit_with_POPData@data) #nrow(geo_unit_with_POPData@data)

#-----------------------------------------------------------------------------------------------------------------------------------------
#subsetted dataset (to work with)

#subset the crime dataset using the geographical shapefile..
data <- data[which(data$code %in% as.vector(geo_unit_with_POPData@data$code)),]
#head(data) #nrow(data)


#geographical units
geo_unique <- as.vector(geo_unit_with_POPData@data$code)
#head(geo_unique)

#geographical units from data
data_OA <- as.vector(data$code)
#head(data_OA)

#----------------------------------------------------------------------
data <- apply(data[,2:12], 2, as.numeric)#head(data)
data <- cbind(1:nrow(data), data)
colnames(data) <- c("ID","X1","X2","X3","X4","X5","X6","X7","X8","X9","X10", "X11") #head(data)
#----------------------------------------------------------

#extracting population data from the geographical shapefile
pop_02_12 <- geo_unit_with_POPData@data #head(pop_02_12)
rownames(pop_02_12) <- 1:nrow(pop_02_12)

data_Fresh <- NULL
#now calculate crime rate (normalise with population)
for(k in 1:length(geo_unique)){#k<-1
  pop_cut <- as.numeric(as.matrix(as.vector(pop_02_12[which(as.vector(pop_02_12$code)==geo_unique[k]),1:11]))) #population
  data_cut <- as.numeric(data[which(data_OA==geo_unique[k]),2:12]) #head(data_cut)
  data_Pop_1000 <- (data_cut / pop_cut)*1000
  data_Fresh <- rbind(data_Fresh, round(data_Pop_1000,digits=5))
  #data[k,2] <- data_Pop_100[1]
}

head(data_Fresh)

agg_Data <- as.data.frame(cbind(V1=geo_unique, data_Fresh))

#now transfer back to data
data <- agg_Data

head(data)
data <- apply(data[,2:12], 2, as.numeric)#head(data)
data <- cbind(1:nrow(data), data)
colnames(data) <- c("ID","X1","X2","X3","X4","X5","X6","X7","X8","X9","X10", "X11")
#-----------------------------------------------------------------------------------------------------------------------------------------
#head(data) #data[1:10,]

#check for infinity or Nan
c_ <- NULL
for(g in 2:ncol(data)){ #g<-6
  i_ <- which(is.na(data[,g]))
  j_ <- which(is.infinite(data[,g]))
  i_j <- unique(c(i_, j_))
  if(length(i_j)!=0){
    for(c_ in 1:length(i_j)){
      data[i_j[c_],2:ncol(data)] <- data[(i_j[c_]+1),2:ncol(data)]
    }
  }

}

#--------------------------------------------------------------------------
#remove that outlier thing....

#burglary -------------- ONLY LSOA
if(study_Area == "wm_bm_" && crime_type == "PC_"){
  out_List<-c(604, 609)
  #replace each of these with next two records#data[1605:1606,]
  for(w in 1:length(out_List)){#w<-1
    data[out_List[w],2:ncol(data)] <- data[(out_List[w]+2),2:ncol(data)]
  }
}
#-------#head(data)

#assault -------------- ONLY LSOA
if(study_Area == "wm_bm_" && crime_type == "VC_"){
  out_List<-c(604, 609)
  #replace each of these with next two records#data[1605:1606,]
  for(w in 1:length(out_List)){#w<-1
    data[out_List[w],2:ncol(data)] <- data[(out_List[w]+2),2:ncol(data)]
  }
}
#-------#head(data)


#burglary -------------- DZ
if(study_Area == "gl_" && crime_type == "PC_"){
  out_List<-c(349, 386)
  #replace each of these with next two records#data[1605:1606,]
  for(w in 1:length(out_List)){#w<-1
    data[out_List[w],2:ncol(data)] <- data[(out_List[w]+2),2:ncol(data)]
  }
}
#-------#head(data)


#violence -------------- DZ
if(study_Area == "gl_" && crime_type == "VC_"){
  out_List<-c(349, 386)
  for(w in 1:length(out_List)){#w<-1
    data[out_List[w],2:ncol(data)] <- data[(out_List[w]+2),2:ncol(data)]
  }
}
#-------#head(data)

data_ <- data[, -1]

#now combine the LSOA unit back to it..
#----------------------------------------------------------------
Data_with_Geo <- as.data.frame(cbind(ID=geo_unique, data_))
#----------------------------------------------------------------
#now collect the most deprived and the least deprived and get the ratio values...

head(Data_with_Geo)
head(dep_Areas)
hist_D_2x2 <- matrix(0, 2, 2)


#NOTEEEEEEE! please, not that the deprivation class annotated as 1 is actually 5

#this is taking care of it..... when I call "5", I am calling I am calling the most deprived.....
dep_Classes <- c(5, 1)

year <- c(1, 2)

for(i in 1:length(dep_Classes)){#i<-2
  #get the total crime rate in the most deprived, the least deprived.
  #get the list of area in this group
  #remember to change the YEAR...
  if(i==1){ #for 2001 and 2011
    dat_1 <- as.vector(dep_Areas[which(dep_Areas$TOWNSNDRANK2001==dep_Classes[1]),c("code")])
    dat_2 <- as.vector(dep_Areas[which(dep_Areas$TOWNSNDRANK2011==dep_Classes[1]),c("code")])
    #to check that I am collecting the right group, use the plot lines below
    #sp::plot(WM_LSOA, col="grey", border="grey", pch=16)
    #sp::plot(WM_LSOA[which(as.vector(WM_LSOA$code)%in%dat_),], add=TRUE, col=colours[cc_], border="grey", pch=16)

    keep_D_1 <- Data_with_Geo[which(as.vector(Data_with_Geo[,1])%in%dat_1),]  #length(dat_1)
    keep_D_2 <- Data_with_Geo[which(as.vector(Data_with_Geo[,1])%in%dat_2),]  #head(keep_D_2)

    hist_D_2x2[1,1] <- mean(as.numeric(as.character(keep_D_1[,2])))  #as.numer
    hist_D_2x2[1,2] <- mean(as.numeric(as.character(keep_D_2[,12]))) #as.numer
    #dat_ <- dat_2
  }

  if(i==2){ #for 2001 and 2011
    dat_1 <- as.vector(dep_Areas[which(dep_Areas$TOWNSNDRANK2001==dep_Classes[2]),c("code")])
    dat_2 <- as.vector(dep_Areas[which(dep_Areas$TOWNSNDRANK2011==dep_Classes[2]),c("code")])
    #to check that I am collecting the right group, use the plot lines below
    #sp::plot(WM_LSOA, col="grey", border="grey", pch=16)
    #sp::plot(WM_LSOA[which(as.vector(WM_LSOA$code)%in%dat_),], add=TRUE, col=colours[cc_], border="grey", pch=16)

    keep_D_1 <- Data_with_Geo[which(as.vector(Data_with_Geo[,1])%in%dat_1),]  #length(dat_1)
    keep_D_2 <- Data_with_Geo[which(as.vector(Data_with_Geo[,1])%in%dat_2),]  #head(keep_D_2)

    hist_D_2x2[i,1] <- mean(as.numeric(as.character(keep_D_1[,2])))  #as.numer
    hist_D_2x2[i,2] <- mean(as.numeric(as.character(keep_D_2[,12]))) #as.numer
    #dat_ <- dat_2
  }

}

colnames(hist_D_2x2) <- c("2001","2011")
rownames(hist_D_2x2) <- c("Most_depr","Least_depr")

x_ <- matrix(0, 1, 2)
for(i in 1:2){
  x_[1, i] <- (hist_D_2x2[1,i]/hist_D_2x2[2,i])
  flush.console()
  print(x_)
}

colnames(x_) <- c("2001", "2011")
hist_D_2x2 <- rbind(hist_D_2x2, x_)

#---------------------------

y_ <- matrix(0, 2, 1)
for(i in 1:2){
  y_[i, 1] <- (hist_D_2x2[i,1]/hist_D_2x2[i,2])
  flush.console()
  print(y_)
}

y_ <- rbind(y_, matrix(0,1,1))
hist_D_2x2 <- cbind(hist_D_2x2, y_)
colnames(hist_D_2x2) <- c("2001","2011", "Ratio")
rownames(hist_D_2x2) <- c("Most_depr","Least_depr", "Ratio")

hist_D_2x2

write.table(hist_D_2x2, file=paste(study_Area,  crime_type, "change.csv",  sep=""), sep=",")



#below code plot the deprivation maps
#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
library(rgdal)
library(ggplot2)

#colfunc <- colorRampPalette(c("green", "yellow", "red"))
#colours <- colfunc(9)

#study_Area <- "wm_bm_"
#study_Area <- "gl_"

import_map_ <- readOGR(dsn=".", paste(study_Area, "LSOA_hex", sep="")) #geo_unit_with_POPData
city_Centre <- readOGR(dsn=".", paste(study_Area, "citycentre", sep="")) #geo_unit_with_POPData
city_bry_1 <- readOGR(dsn=".", paste(study_Area, "LSOA_hex_bry", sep="")) #geo_unit_with_POPData
plot(city_bry_1)

if(study_Area=="gl_"){
  city_bry_2 <- readOGR(dsn=".", paste(study_Area, "LSOA_hex_bry_2", sep="")) #geo_unit_with_POPData
  city_bry_3 <- readOGR(dsn=".", paste(study_Area, "LSOA_hex_bry_3", sep="")) #geo_unit_with_POPData
}

par(oma=c(0,0,0,0), mar=c(0,0,0,0), mfrow=c(1,1))


#DEPRIVATION ANALYSIS - TOWNSEND INDEX

#TSD_Index <- read.table(file = paste(study_Area, "TSD.csv", sep=""), sep=",", head = TRUE) #birmingham_TSD_2011
TSD_Index <- read.table(file = "TSD_birmingham_2001_2011.csv", sep=",", head = TRUE) #bi
head(TSD_Index)  #TOWNSNDRANK2001; TOWNSNDRANK2011

#plot(import_map)
#head(import_map)
#library(sf)
#library(tidyverse)

import_map <- merge(import_map_, TSD_Index, by.x="code", by.y="code")

import_map_2 <- fortify(import_map)
class(import_map_2)

#adding the city centre
city_Centre_2 <- fortify(city_Centre)
city_bry_1_ <- fortify(city_bry_1)

if(study_Area=="gl_"){
  city_bry_2_ <- fortify(city_bry_2)
  city_bry_3_ <- fortify(city_bry_3)
}

import_map@data$id <- 0:(dim(import_map@data)[1]-1) # add id field

import_map_2_join = plyr::join(x = import_map_2,y = import_map@data, by="id") # join by id


#For Birmingham #plot deprivation map
deprivation_map <- ggplot(data=import_map_2_join) + # data layer
  geom_polygon(aes(x=long, y=lat, group=group, fill=TOWNSNDRANK2001)) +
  #coord_map(projection = "mercator",xlim=xrange,ylim=yrange) +
  scale_fill_gradient2(low='dodgerblue4',mid = "lightgray",high='darkred',na.value = "lightgrey",midpoint=mean(import_map_2_join$TOWNSNDRANK2001), name='Depr_Score') +
  geom_path(aes(x=long, y=lat, group=group), color='NA',size=0.2) +
  geom_polygon(data = city_bry_1_, aes(x=long, y=lat), fill='NA', colour="grey", linetype=1, size=0.5) +
  geom_polygon(data = city_Centre_2, aes(x=long, y=lat), fill='NA', colour="black", linetype=1, size=1.5) +
  #scale_fill_brewer(palette = "Spectral", direction = 1)
  #scale_fill_brewer(palette = "Spectral", direction = 1)
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

#run to plot
deprivation_map


#For Glasgow
deprivation_map <- ggplot(data=import_map_2_join) + # data layer
  geom_polygon(aes(x=long, y=lat, group=group, fill=TOWNSNDRANK2011)) +
  #coord_map(projection = "mercator",xlim=xrange,ylim=yrange) +
  scale_fill_gradient2(low='dodgerblue4',mid = "lightgray",high='darkred',na.value = "lightgrey",midpoint=mean(import_map_2_join$TOWNSNDRANK2011), name='Depr_Score') +
  geom_path(aes(x=long, y=lat, group=group), color='NA',size=0.2) +
  geom_polygon(data = city_Centre_2, aes(x=long, y=lat), fill='NA', colour="black", linetype=1, size=1.5) +
  geom_polygon(data = city_bry_1_, aes(x=long, y=lat), fill='NA', colour="grey", linetype=1, size=0.5) +
  geom_polygon(data = city_bry_2_, aes(x=long, y=lat), fill='NA', colour="grey", linetype=1, size=0.5) +
  geom_polygon(data = city_bry_3_, aes(x=long, y=lat), fill='NA', colour="grey", linetype=1, size=0.5) +
  #scale_fill_brewer(palette = "Spectral", direction = 1)
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

#run to plot
deprivation_map

#--------------------------------------------------------------------------



###pvalue statistical significance code
#---------------------------------------------------------------------------
#compute the statistical significance (deviation from the citywide trend)
#---------------------------------------------------------------------------

#create data
#explore result to eliminate outliers


grp_

#result  #head(data) #head(data.long.melted)  #head(data)
data.long.melted

#use 'dcast' function to re-generate the original data (with the cluster IDs)
data_reg <- dcast(data.long.melted, OAcode ~ Year, value.var='Crime_Count')
data_reg <- cbind(data_reg, part2) ##append the clusters list #head(data_reg)

#list_I <- c(1:12)
#collate the row ids of trajectories belonging to the 'I-Class'  #head(data_reg)
list_I <- which(data_reg$part2%in%grp_[[3]])  #list_Letters
#ls_ <- c(grp_[[2]],grp_[[3]]) #for randomising - E-Class & I-Class
#list_I <- which(data_reg$part2%in%ls_)  #list_Letters

#using all classes
#list_I <- which(data_reg$part2%in%c(1:12))  #list_Letters

#Bring in the data...
#--------------------------------------------------------------------------------
#function 1: Given original dataset, generate a randomise replica of the data
#function 2: Given a randomised dataset, generate hypothetical cluster centers, head(data)
#--------------------------

#function 2: Given a randomised dataset, generate hypothetical cluster centers, head(data)

generate_data_replica <- function(data=data, agg_Data = agg_Data, list_I=list_I, seed_= MC){ #, list_I=list_I
  #RANDOMISING THE DATA.... #head(data)

  #variable to hold the randomise ids
  data_with_sort_ids <- list()

  head(data)

  #collect the increasing groups
  set.seed(seed_)  #
  I_gr_ <- data[list_I,]  #head(I_gr_)
  I_gr_ <- matrix(sample(I_gr_),,ncol(data))
  #join back to the main data..
  data[list_I,] <- I_gr_
  data_sim <- data

  #set.seed(seed_)  #
  #data_sim <- sample(data)
  #data_sim <- matrix(data_sim,,ncol(data))

  #data_sim[1:nrow(data_sim),1] <- 1:nrow(data_sim)
  head(data_sim)

  agg_Data_sim <- agg_Data

  data_with_sort_ids[[1]] <- data_sim
  data_with_sort_ids[[2]] <- agg_Data_sim

  #data_sim <- data_sim[,1:ncol(data_sim)]
  return(data_with_sort_ids)
}

#head(data_sim)

###generate_data_replica <- function(data=data, agg_Data = agg_Data, list_I=list_I, seed_= MC){ #, list_I=list_I
  #RANDOMISING THE DATA.... #head(data)

  #variable to hold the randomise ids
  data_with_sort_ids <- list()

  head(data)
  #adding the index
  #data_Crt <- cbind(matrix(1:nrow(data),,1),data)# data=data[,-1])
  #suffle the indices of 'I-class' trajectories
  set.seed(seed_)  #MC=2
  list_I2 <- sample(list_I)

  data_sim <- data #create a copies of the actual data  #head(data_sim)
  agg_Data_sim <- agg_Data
  #data_Crt <- data
  #Shuffling the 'I-Class' trajectories in the original dataset, and keep the rest the same.
  #Looping through each record and randomise the values.
  for(u in 1:length(list_I2)){#u<-1
    #randomise the observations
    ids <- sample(1:ncol(data))
    data_sim[list_I[u], 1:ncol(data_sim)] <- data[list_I2[u],ids] #simulated data
    agg_Data_sim[list_I[u], 1] <- as.vector(agg_Data[list_I2[u],1]) #simulated backup data (with random element in 'I-Class')
    flush.console()
    #print(paste(list_I[u],list_I2[u], sep="||")) #head(data_sim) #data_sim[3219,]    #data[1528,]
    #print(ids)
  }

  #agg_Data_sim[list_I[u],1]

  #data_sim[1:nrow(data_sim),1] <- 1:nrow(data_sim)
  head(data_sim)

  data_with_sort_ids[[1]] <- data_sim
  data_with_sort_ids[[2]] <- agg_Data_sim

  #data_sim <- data_sim[,1:ncol(data_sim)]
  return(data_with_sort_ids)
##}

#head(data_sim)



generate_cluster_centres <- function(data, N_CENTERS=3){
  #--------------------------------------
  data_ <- cbind(1:nrow(data), data)
  #-------------------------------------------------
  #looping through to compute the slope of each trajectory
  sl_List <- NULL
  for(i in 1:nrow(data_)){ #i<-1
    b=coefficients(lm(data_[i,2:ncol(data_)]~time[i,]))
    sl_List <- rbind(sl_List, cbind(as.numeric(b[1]),as.numeric(b[2])))
  }
  sl_List <- as.data.frame(sl_List)
  sl_List <- as.data.frame(cbind(1:nrow(sl_List), sl_List))
  colnames(sl_List) <- c("sn", "intersect","slope")
  #lines with negative slope
  #keep_sl_List <- sl_List[1:20,]
  library(Hmisc) # cut2
  library(data.table)
  #--------------------------------------
  #computing cluster centers for different no-of-clusters
  # cluster_center_List_sim <- list()
  #for(p in 3:36){ #p<-3
  #input the number of clusters go generate
  n_Clusters <- N_CENTERS
  keep_sl_ListGroup <- split(sl_List, cut2(sl_List$slope, g=n_Clusters))
  #keep_sl_ListGroup <- split(keep_sl_List, cut2(keep_sl_List$slope, g=n_Clusters))
  # sl_List_Grouped$groups <- chunk.2(sl_List$slope, 8, force.number.of.groups = TRUE)
  length(keep_sl_ListGroup)
  #keep_sl_ListGroup[2]
  #line with positive slopes

  #COMPUTE THE CENTRE OF EACH GROUP
  mean_Slopes <- NULL
  for(j in 1:n_Clusters){ #j<-9

    s_dty <- as.data.frame(keep_sl_ListGroup[j][1],,2)[,3]
    i_dty <- as.data.frame(keep_sl_ListGroup[j][1],,2)[,2]

    mean_Slopes <- rbind(mean_Slopes, cbind(mean(i_dty), mean(s_dty)))
  }
  #------------------------
  #get the centers
  centers_List <- NULL
  for(k in 1:nrow(mean_Slopes)){ #k<-1
    centers_List <- rbind(centers_List, (mean_Slopes[k,1] + (mean_Slopes[k,2]*(1:11))))
  }
  centers_List <- as.data.frame(centers_List)
  cluster_center_List_sim <- list(centers_List)
  return(cluster_center_List_sim)
}



#---------------------------------------------------------------------------------#head(agg_Backup)
#function to calculate the observed Q

Q_likelihood <- function (data=data, cluster_center_List= cluster_center_List_ALL[[1]], agg_Backup=agg_Data, list_Letters=list_Letters) { #agg_Data: which preserved the OA codes #head(data)

  #data <- data_simulated
  run=1

  result_ALL <- list()

  ##for(rn in 1:run){  #rn=1
  #now to plot a k solution
  #h=10
  #using the cluster centres from the above...
  part2 <- affectIndivC(data, cluster_center_List)   #which(part2=="12") #head(data)

  #data.append <- cbind(data, part2)
  #rownames(data.append) <- 1:ncol(data.append)
  data.append <- cbind(1:nrow(data), data)
  colnames(data.append) <- c("code", "1", "2", "3", "4", "5", "6", "7", "8",
                             "9", "10","11")
  data.append <- as.data.frame(data.append)

  data.long.melted <- melt(data.append, id="code")#head(data., id="code"
  head(data.long.melted)

  #data.append <- cbind(data, part2)
  #data.long.melted <- melt(data, id="code")#head(d
  data.long.melted <- cbind(data.long.melted, rep(part2, 11))#nrow(data.long.melted)

  colnames(data.long.melted) <- c("OAcode","Year","Crime_Count", "clusters")

  #convert numbers to letters
  ##clusters <- letters[data.long.melted$clusters]
  ##clusters <- toupper(clusters) #head(cluster_letters)

  clusters <- list_Letters[data.long.melted$clusters]
  data.long.melted$clusters <- clusters
  colnames(data.long.melted) <- c("OAcode","Year","Crime_Count", "clusters")

  #================================
  #THE LIKELIHOOD FUNCTION USING SUM OF INDIVIDUAL TRAJECTORY
  # Setup output table
  #calculate the cumulative rate of change (first difference) of the mean value ......
  clusters <- clusters[1:nrow(data)]

  data_reMelt <- dcast(data.long.melted, OAcode ~ Year, value.var='Crime_Count')
  data_reMelt <- cbind(data_reMelt, clusters) #head(data_reMelt)

  cluster_Group <- as.vector(unique(data_reMelt$clusters)[order(unique(data_reMelt$clusters))])
  sl_check_ALL <- NULL

  for(i in 1:length(cluster_Group)){ #i<-1
    # Deal with IDs
    data_reMelt_Cut <- data[which(data_reMelt$clusters==cluster_Group[i]),]

    #if have just one row
    if(length(data_reMelt_Cut)==11){data_reMelt_Cut<-matrix(data_reMelt_Cut,,11)}

    # time_reMelt_Cut <- time[which(data_reMelt$clusters==cluster_Group[i]),]
    count_Cluster <- nrow(data_reMelt_Cut)
    #coefficients(lm(data[i,]~time[i,]))
    sl_check <- NULL
    #HERE INSTEAD OF FINDING THE MEAN OF ENTIRE TRAJECTORIES, I WANT TO
    #DO IT FOR INDIVIDUAL TRAJECTORY
    for(s in 1:nrow(data_reMelt_Cut)){   #s<-1
      data_reMelt_Cut_Sub <- data_reMelt_Cut[s,]
      #for(q in 1:ncol(data_reMelt_Cut)){ #q<-1
      #sl_check<-c(sl_check, mean(data_reMelt_Cut[,q]))
      # }
      cum_rate_Change <- NULL
      #for(r in 2:length(sl_check)){#r<-2
      for(r in 2:length(data_reMelt_Cut_Sub)){#r<-2
        #cum_rate_Change <- c(cum_rate_Change, (sl_check[r] - sl_check[r-1]))
        cum_rate_Change <- c(cum_rate_Change, (data_reMelt_Cut_Sub[r] - data_reMelt_Cut_Sub[r-1]))
        #data_reMelt_Cut_Sub
      }
      sl_check <- c(sl_check, sum(cum_rate_Change))

    }
    sl_check <- sum(sl_check)
    #sl_check_ALL <- rbind(sl_check_ALL, cbind(cluster_Group[i], round(cum_rate_Change,digits = 3)))
    sl_check_ALL <- rbind(sl_check_ALL, cbind(cluster_Group[i], round(sl_check,digits = 3)))
  }

  #---------------------------
  head(sl_check_ALL)
  #---------------------------
  sl_check_ALL
  sl_check_ALL_Ordered <- sl_check_ALL[order(-as.numeric(as.character(sl_check_ALL[,2]))),]
  #---------------------------------------------------------------------

  #mmD ----------------------------------------------
  data_reMelt <- dcast(data.long.melted, OAcode ~ Year, value.var='Crime_Count')
  data_reMelt <- cbind(data_reMelt, clusters) #head(data_reMelt)

  cluster_Group <- as.vector(unique(data_reMelt$clusters)[order(unique(data_reMelt$clusters))])
  #---------------------------------------------------------------------------
  #plot each cluster....
  #---------------------------------------------------------------------------
  clusters <- clusters[1:nrow(data)]

  data_reMelt <- dcast(data.long.melted, OAcode ~ Year, value.var='Crime_Count')
  data_reMelt <- cbind(data_reMelt, clusters) #head(data_reMelt)

  cluster_Group <- as.vector(unique(data_reMelt$clusters)[order(unique(data_reMelt$clusters))])

  length_Of_Clusters <- NULL

  cluster_Units_ALL <- list()

  for(i in 1:length(cluster_Group)){ #i<-1

    data_reMelt_Cut <- data_backup[which(data_reMelt$clusters==cluster_Group[i]),]#head(data_reMelt)

    if(is.null(data_reMelt_Cut)){data_reMelt_Cut<-matrix(data_reMelt_Cut,,12)
    colnames(data_reMelt_Cut) <- c("code", "1", "2", "3", "4", "5", "6", "7", "8",
                                   "9", "10","11") #head(data_reMelt_Cut)
    }
    #get the corresponding OA label #head(data)# head(agg_Data[1:10,])
    cluster_Units <- as.vector(agg_Backup$V1[as.data.frame(data_reMelt_Cut)$ID])

    length_Of_Clusters <- rbind(length_Of_Clusters, cbind(i, length(cluster_Units)))

    cluster_Units_ALL[i] <- list(cluster_Units)
  }

  #ranked from the most inequitous area to the least
  rank_Areas <- sl_check_ALL[order(-as.numeric(as.character(sl_check_ALL[,2]))),]
  colnames(rank_Areas) <- c("id","Q")
  length_Of_Clusters <- cbind(list_Letters[length_Of_Clusters[,1]], length_Of_Clusters[,2] ) #letter
  colnames(length_Of_Clusters) <- c("id","count")

  #result_Merge <- merge(length_Of_Clusters, rank_Areas, by="id")

  result_Merge_New <- cbind(length_Of_Clusters, rank_Areas[match(length_Of_Clusters[,1], rank_Areas[,1]),])

  #-----------------------------------------
  #result_Merge_New[order(-as.numeric(result_Merge_New[,4])),]
  #-----------------------------------------

  result_A <- result_Merge_New[order(-as.numeric(result_Merge_New[,4])),]

  #MC_List <-
  result_ALL[[run]] <- result_A

  #flush.console()
  #print(run)
  ##}

  #getting the final Q value, based on the final derivation...  going back the division thing....
  for(vb in 1:length(result_ALL)){ #vb<-1
    Q_ <- round(as.numeric(result_ALL[[vb]][,4])/as.numeric(result_ALL[[vb]][,2]), digits=3)
    sub_Table_ <- cbind(result_ALL[[vb]],Q_)
    result_ALL[[vb]] <- sub_Table_
  }

  result_ALL[[2]] <- cluster_Units_ALL

  return(result_ALL)

}
#end of function
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------
#SECTION ** : FOR MC SIMULATION OF MULTIPLE ...
#to compute the observed Q, expected Q, and pvalue for a given value of k  (for real data) grp_
final_Result <- list()

#for(runs in 3:24){ #runs <- 3

for(runs in 13:13){ #runs <- 9
  #generate cluster centre of the original dataset
  center_Original_dataset <- generate_cluster_centres(data=data, N_CENTERS=runs)

  observed_Q <- Q_likelihood(data=data, cluster_center_List = center_Original_dataset[[1]], agg_Backup=agg_Data, list_Letters=list_Letters)
  observed_Q

  expected_Q1 <- NULL
  expected_Q2 <- NULL
  nsim <- 999 #99
  for(MC in 1:999){#MC=1 nsim
    #MOnte Carlo simulation
    #function generate 999 randomised hypothetical centers
    data_simulated <- generate_data_replica(data=data, agg_Data=agg_Data,list_I=list_I, seed_= MC)
    #simulated cluster center
    cluster_Centers_simulated <- generate_cluster_centres(data=data_simulated[[1]], N_CENTERS=runs)

    #agg_Data_Sim <- as.data.frame(V1=code_, data_simulated[[1]]) #head(agg_Data_Sim)

    simulated_Q <- Q_likelihood(data=data_simulated[[1]], cluster_center_List = cluster_Centers_simulated[[1]], agg_Backup=data_simulated[[2]], list_Letters=list_Letters)

    simulated_Q
    expected_Q1 <- c(expected_Q1, as.numeric(simulated_Q[[1]][1,4]))#to use only the summation  #head(agg_Data)
    expected_Q2 <- c(expected_Q2, as.numeric(simulated_Q[[1]][1,5]))#to use only the summa
    #expected_Q <- c(expected_Q, simulated_Q[1,4])#to use ..divided by n
    flush.console()
    print(MC)
    #print(head(data_simulated))
    #print(data_simulated[22,])
  }
  #--------------------------------------------------------------------------------
  #generate the pvalue for all the observed clusters
  pvalue1 <- NULL
  pvalue2 <- NULL

  for(kt in 1:nrow(observed_Q[[1]])){#kt<-1
    p_v1 <- length(which(expected_Q1>as.numeric(observed_Q[[1]][kt,4])))
    #for_Q_ <- order(-as.numeric(observed_Q[[1]][,5]))#which is larger
    p_v2 <- length(which(expected_Q2>as.numeric(observed_Q[[1]][kt,5])))
    pvalue1 <- c(pvalue1, (p_v1+1)/(nsim+1))
    pvalue2 <- c(pvalue2, (p_v2+1)/(nsim+1))
  }

  just_Q_ <- cbind(as.data.frame(observed_Q[[1]]), matrix(pvalue1,,1), matrix(pvalue2,,1))

  final_Result[[runs-2]] <- list(just_Q_, observed_Q[[2]])
  #final_Result[[2]] <- list(just_Q_, observed_Q[[2]])
  flush.console()
  print(runs)

  flush.console()
  print(final_Result[[runs-2]])

}#runs=4

























#----------------------------------------


#to plot mean trajectories, after "generate_data_replica" function
for(i in 1:999){
  dat_1 <- generate_data_replica(data)  #head(dat_1) # dat_1 <- data_rate[,-1]
  #dat_1 <- data
  dat_1 <- as.matrix(dat_1)
  mean_trend <- NULL
  #average trendline
  for(a in 1:ncol(dat_1)){#111111 a<-1
    mean_trend <- c(mean_trend, mean(dat_1[ ,a]))
  }#
  min_ <- min(mean_trend)
  max_ <- max(mean_trend)

  #plot the citywide trendline.. #head(dat_1)
  par(mar=c(2,2,4,2)+0.2, mgp=c(5,1,1))
  #dev.new()
  plot(c(0,15), c(min_,max_), xlab="x..(%)", ylab="y..(%)", main="...",
       cex=0.001, col="white", cex.lab=1.5, cex.axis=1.5, cex.main=1.5, las=1, xaxt ='n', axes=FALSE)
  #axis(1, at=1:15 , labels=Year, cex.axis=0.5)
  #axis(2, at=c(seq(5, ceiling(max_),1)) , labels=c(seq(1, ceiling(max_),1)), cex.axis=0.5)
  #abline(h=0, col="black")
  #abline(v=0, col="black")
  #----
  lines(mean_trend, col="black", cex=2, lty=16, lwd=4)
  points(mean_trend, col="black", pch=pt_[aaa], cex=2)
  flush.console()
  print(i)

}
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------








crime_type






colMeans(hist_D_2x2)


colSum(hist_D_2x2)

hist_D_2x2

min_ <- min(hist_D_2x2)

hist_D_2x2 <- as.data.frame(hist_D_2x2)

t(apply(hist_D_2x2, 1, function(x)(x-min(x))/(max(x)-min(x))))



#-------------------
#now calculate the average crime rate for this area


dat_1_y_1 <- mean(as.numeric(as.character(keep_D[,2])))  #as.numeric(as.character(keep_D[,2]))[1] + as.numeric(as.character(keep_D[,2]))[2]
dat_1_y_2 <- mean(as.numeric(as.character(keep_D[,12])))

hist_D_2x2[]



}

hist_D_2x2[] <-








































































































to_Keep <- NULL
for(y_ in 1:2){#y_=1
  if(y_==1){
    tab_ <- colate_All$TOWNSNDRANK2001  #CHANGE THIS...?????????????????????????????????? for the year
    #get the frequency
    tab_2 <- matrix(0, 5, 2)
    for(b_ in 1:nrow(tab_2)){ #b_<-1
      tab_2[b_,1]<-b_
      tab_2[b_,2]<-length(which(tab_==b_))
    }
  }
  if(y_==2){
    tab_ <- colate_All$TOWNSNDRANK2011  #CHANGE THIS...?????????????????????????????????? for the year
    #get the frequency
    tab_2 <- matrix(0, 5, 2)
    for(b_ in 1:nrow(tab_2)){ #b_<-1
      tab_2[b_,1]<-b_
      tab_2[b_,2]<-length(which(tab_==b_))
    }
  }
  colnames(tab_2) <- c("Var1", "Freq")
  tab_2 <- as.data.frame(tab_2)
  #flipping the class ----
  tab_2$Freq <- as.numeric(as.character(tab_2$Freq[length(tab_2$Freq):1]))
  #now convert to proportion
  tab_2$Freq <- round(as.numeric(tab_2$Freq)/sum(as.numeric(tab_2$Freq)), digits=3)

  to_Keep <- cbind(to_Keep, tab_2[,2])

}

#calculate mean
m_ <- matrix(0, 5, 1)
for(m in 1:nrow(to_Keep)){#m<-1
  m_[m,1] <- mean(to_Keep[m,])
}
#tab_2$Gr1 <- paste("Group_",q, sep="")
m_ <- as.data.frame(m_)
m_$Gr <- nrow(m_):1


hist_D <- cbind(hist_D, m_[,1])

}



write.table(hist_D, file=paste(study_Area, "VC_mean_TOWNSNDRANK01_11.csv", sep=""), sep=",", row.names=F)
























































hist_D <- NULL
head(import_map)

for(dep in 1:5){#dep<-1
  cut_ <- import_map[which(import_map$TOWNSNDRANK2001==dep),] #head(cut_)


}
#collate all the subgroups to be considered together

all_Grs <- NULL

for(q in 1:length(grp_)){ #q<-1

  colate_All <- NULL

  for(f in 1:length(grp_[[q]])){ # f<-1

    colat_ <- cluster_Units_ALL[[grp_[[q]][f]]]
    #collate from the import map the lsoa that happens in colat_
    #colat_2 <- import_map[which(import_map$code %in% colat_),]  #as.vector(colat_2$code)
    colate_All <- c(colate_All, colat_) #nrow(colate_All)
  }

  #now subset/call the deprivation data



}


nrow(import_map)

for(q in 1:length(grp_)){ #q<-1

  colate_All <- NULL

  for(f in 1:length(grp_[[q]])){ # f<-1

    colat_ <- cluster_Units_ALL[[grp_[[q]][f]]]
    #collate from the import map the lsoa that happens in colat_
    colat_2 <- import_map[which(import_map$code %in% colat_),]  #as.vector(colat_2$code)
    colate_All <- rbind(colate_All, colat_2) #nrow(colate_All)
  }

  to_Keep <- NULL
  for(y_ in 1:2){#y_=1
    if(y_==1){
      tab_ <- colate_All$TOWNSNDRANK2001  #CHANGE THIS...?????????????????????????????????? for the year
      #get the frequency
      tab_2 <- matrix(0, 5, 2)
      for(b_ in 1:nrow(tab_2)){ #b_<-1
        tab_2[b_,1]<-b_
        tab_2[b_,2]<-length(which(tab_==b_))
      }
    }
    if(y_==2){
      tab_ <- colate_All$TOWNSNDRANK2011  #CHANGE THIS...?????????????????????????????????? for the year
      #get the frequency
      tab_2 <- matrix(0, 5, 2)
      for(b_ in 1:nrow(tab_2)){ #b_<-1
        tab_2[b_,1]<-b_
        tab_2[b_,2]<-length(which(tab_==b_))
      }
    }
    colnames(tab_2) <- c("Var1", "Freq")
    tab_2 <- as.data.frame(tab_2)
    #flipping the class ----
    tab_2$Freq <- as.numeric(as.character(tab_2$Freq[length(tab_2$Freq):1]))
    #now convert to proportion
    tab_2$Freq <- round(as.numeric(tab_2$Freq)/sum(as.numeric(tab_2$Freq)), digits=3)

    to_Keep <- cbind(to_Keep, tab_2[,2])

  }

  #calculate mean
  m_ <- matrix(0, 5, 1)
  for(m in 1:nrow(to_Keep)){#m<-1
    m_[m,1] <- mean(to_Keep[m,])
  }
  #tab_2$Gr1 <- paste("Group_",q, sep="")
  m_ <- as.data.frame(m_)
  m_$Gr <- nrow(m_):1


  hist_D <- cbind(hist_D, m_[,1])

}



write.table(hist_D, file=paste(study_Area, "mean_TOWNSNDRANK01_11.csv", sep=""), sep=",", row.names=F)












df2 <- data.frame(supp=rep(c("VC", "OJ"), each=3),
                  dose=rep(c("0.5", "1", "2"),2),
                  len=c(6.8, 15, 33, 4.2, 10, 29.5))
head(df2)

p <- ggplot(data=hist_D, aes(x=Var1, y=Freq, fill=Gr)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal()

+ scale_fill_manual(values=c('#999999','#E69F00','#E69F00'))
# Use custom colors
p + scale_fill_manual(values=c('#999999','#E69F00','#E69F00','#E69F00','#E69F00'))
# Use brewer color palettes
p + scale_fill_brewer(palette="Blues")

p <- ggplot(data=df2, aes(x=dose, y=len, fill=supp)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal()
# Use custom colors
p + scale_fill_manual(values=c('#999999','#E69F00'))
# Use brewer color palettes
p + scale_fill_brewer(palette="Blues")



#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
library(rgdal)
library(ggplot2)

#colfunc <- colorRampPalette(c("green", "yellow", "red"))
#colours <- colfunc(9)

#study_Area <- "wm_bm_"
#study_Area <- "gl_"

import_map_ <- readOGR(dsn=".", paste(study_Area, "LSOA_hex", sep="")) #geo_unit_with_POPData
city_Centre <- readOGR(dsn=".", paste(study_Area, "citycentre", sep="")) #geo_unit_with_POPData
city_bry_1 <- readOGR(dsn=".", paste(study_Area, "LSOA_hex_bry", sep="")) #geo_unit_with_POPData
plot(city_bry_1)

if(study_Area=="gl_"){
  city_bry_2 <- readOGR(dsn=".", paste(study_Area, "LSOA_hex_bry_2", sep="")) #geo_unit_with_POPData
  city_bry_3 <- readOGR(dsn=".", paste(study_Area, "LSOA_hex_bry_3", sep="")) #geo_unit_with_POPData
}

par(oma=c(0,0,0,0), mar=c(0,0,0,0), mfrow=c(1,1))


#DEPRIVATION ANALYSIS - TOWNSEND INDEX

#TSD_Index <- read.table(file = paste(study_Area, "TSD.csv", sep=""), sep=",", head = TRUE) #birmingham_TSD_2011
TSD_Index <- read.table(file = "birmingham_TSD_2011.csv", sep=",", head = TRUE) #bi
head(TSD_Index)  #TOWNSNDRANK2001; TOWNSNDRANK2011

#plot(import_map)
#head(import_map)
#library(sf)
#library(tidyverse)

import_map <- merge(import_map_, TSD_Index, by.x="code", by.y="code")

import_map_2 <- fortify(import_map)
class(import_map_2)

#adding the city centre
city_Centre_2 <- fortify(city_Centre)
city_bry_1_ <- fortify(city_bry_1)

if(study_Area=="gl_"){
  city_bry_2_ <- fortify(city_bry_2)
  city_bry_3_ <- fortify(city_bry_3)
}

import_map@data$id <- 0:(dim(import_map@data)[1]-1) # add id field

import_map_2_join = plyr::join(x = import_map_2,y = import_map@data, by="id") # join by id


#For Birmingham #plot deprivation map
deprivation_map <- ggplot(data=import_map_2_join) + # data layer
  geom_polygon(aes(x=long, y=lat, group=group, fill=TOWNSNDRANK2011)) +
  #coord_map(projection = "mercator",xlim=xrange,ylim=yrange) +
  scale_fill_gradient2(low='dodgerblue4',mid = "lightgray",high='darkred',na.value = "lightgrey",midpoint=mean(import_map_2_join$TOWNSNDRANK2011), name='Depr_Score') +
  geom_path(aes(x=long, y=lat, group=group), color='NA',size=0.2) +
  geom_polygon(data = city_bry_1_, aes(x=long, y=lat), fill='NA', colour="grey", linetype=1, size=0.5) +
  geom_polygon(data = city_Centre_2, aes(x=long, y=lat), fill='NA', colour="black", linetype=1, size=1.5) +
  #scale_fill_brewer(palette = "Spectral", direction = 1)
  #scale_fill_brewer(palette = "Spectral", direction = 1)
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

#run to plot
deprivation_map


#For Glasgow
deprivation_map <- ggplot(data=import_map_2_join) + # data layer
  geom_polygon(aes(x=long, y=lat, group=group, fill=TOWNSNDRANK2011)) +
  #coord_map(projection = "mercator",xlim=xrange,ylim=yrange) +
  scale_fill_gradient2(low='dodgerblue4',mid = "lightgray",high='darkred',na.value = "lightgrey",midpoint=mean(import_map_2_join$TOWNSNDRANK2011), name='Depr_Score') +
  geom_path(aes(x=long, y=lat, group=group), color='NA',size=0.2) +
  geom_polygon(data = city_Centre_2, aes(x=long, y=lat), fill='NA', colour="black", linetype=1, size=1.5) +
  geom_polygon(data = city_bry_1_, aes(x=long, y=lat), fill='NA', colour="grey", linetype=1, size=0.5) +
  geom_polygon(data = city_bry_2_, aes(x=long, y=lat), fill='NA', colour="grey", linetype=1, size=0.5) +
  geom_polygon(data = city_bry_3_, aes(x=long, y=lat), fill='NA', colour="grey", linetype=1, size=0.5) +
  #scale_fill_brewer(palette = "Spectral", direction = 1)
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

#run to plot
deprivation_map

#--------------------------------------------------------------------------




library(sf)
library(ggplot2)

WM_LSOA.sf <- st_as_sf(import_map_)

ggplot(WM_LSOA.sf) + geom_sf(aes(fill = TOWNSNDRANK2011)) + scale_fill_brewer(palette = "Spectral", direction = 1)

sp::plot(city_Centre, col="NA", border="black", pch=16, add=TRUE, lwd=2)


#plotting for only three colours

#---------------------------------------------------------




##indices <- list(1:3, 4:6, 10:12, 14:16, 18:20)

x <- rnorm(100)

x[c(indices[[3]], indices[[5]])]

#}









colours

#}

colours

head(WM_LSOA@data)

WM_LSOA_[1,]@data[1,]
#---------------------------

#set parameters
grp_ <- list(c(1), c(2:6), c(7:9), c(10:12), c(13:16)) #for burg

grp_ <- list(c(1), c(2:9), c(10), c(11:17), c(18:21)) #for violence


#--------------------------------------------------------------------------------------------------------------------------------
#THIS CODE WILL CONTAIN THE ABOVE TWO CODES, WHICH RANK THE GROUPS AND THE ONE THAT LIST ALL THE AREAS BELONGING TO EACH AREA
#--------------------------------------------------------------------------------------------------------------------------------
#having obtained the cluster centres for all the number of clusters, k
#now run the experiment to do the actual clustering
#determine the autocorrelation of each solution using the global moran's I
#Answer the question, which solution produces the best clustering - the best grouping of 'like-values'
#Affectation of each individual

#-----------------------------------------------------------------
#import the shapefile
WM_LSOA <- readOGR(dsn=".", "Birmingham_normal_LSOA_osgb")  #head(WM_LSOA) head(WM_LSOA@data)

pvalue <- NULL

sl_check_ALL_Q <- NULL

#now to plot a k solution
for(h in 2:length(cluster_center_List_ALL)){ # h <- 8 NEW METHOD


  #FIRST: ---> CODE THAT DO THE CLUSTERING


  #METHOD 1: -------------------------------------------------------------
  #using the cluster centres from the above...
  part2 <- affectIndivC(data, cluster_center_List_ALL[[h]])

  #data.append <- cbind(data, part2)
  #rownames(data.append) <- 1:nrow(data.append)
  data.append <- cbind(1:nrow(data), data)
  colnames(data.append) <- c("code", "1", "2", "3", "4", "5", "6", "7", "8",
                             "9", "10", "11", "12", "13", "14", "15")
  data.append <- as.data.frame(data.append)

  data.long.melted <- melt(data.append, id="code")#head(data., id="code"
  head(data.long.melted)

  #data.append <- cbind(data, part2)
  #data.long.melted <- melt(data, id="code")#head(data.long.melted)
  data.long.melted <- cbind(data.long.melted, rep(part2, 15))#nrow(data.long.melted)

  colnames(data.long.melted) <- c("OAcode","Year","Crime_Count", "clusters")

  #convert numbers to letters
  clusters <- letters[data.long.melted$clusters]
  clusters <- toupper(clusters) #head(cluster_letters)
  data.long.melted$clusters <- clusters
  colnames(data.long.melted) <- c("OAcode","Year","Crime_Count", "clusters")
  #METHOD 1: -------------------------------------------------------------
  #}#
  #----------------------------------------------

  #import the shapefile
  WM_LSOA <- readOGR(dsn=".", "Birmingham_normal_LSOA_osgb")  #head(WM_LSOA) head(WM_LSOA@data)

  pvalue <- NULL

  sl_check_ALL_Q <- NULL

  #now to plot a k solution
  for(h in 2:24){ # h <- 2 #RECALL: OLDE METOD# TECHNICALLY TO 26


    #METHOD 2: -------------------------------------------------------------
    #GO DOWN TO generate the appropriate "data" from method 2 (old method)
    ##for_Crime <- OAtrajk_burglary_lsoa ########<- OAtrajk_crime
    for_Crime <- OAtrajk_TMV_lsoa #<- OAtrajk_crime
    #for_Crime <- OAtrajk_assault_lsoa ###<- OAtrajk_crime
    #for_Crime <- OAtrajk_criminalDamage_lsoa #######<- OAtrajk_crime

    #h <- 3 #i<-12
    clusters <- getClusters(for_Crime, c(h+2)) #head(data)
    if(ncol(data)>15){
      data <- data[,-1]
    }
    #rownames(data.append) <- 1:nrow(data.append)
    data.append <- cbind(1:nrow(data), data)
    colnames(data.append) <- c("code", "1", "2", "3", "4", "5", "6", "7", "8",
                               "9", "10", "11", "12", "13", "14", "15")
    data.append <- as.data.frame(data.append)
    #data.append <- cbind(data, clusters) #head(data.append)
    #melt(data.append)
    data.long.melted <- melt(data.append, id="code")#head(data.long.melted)#head(data)
    data.long.melted <- cbind(data.long.melted, rep(clusters, 15))
    head(data.long.melted)
    colnames(data.long.melted) <- c("OAcode","Year","Crime_Count", "Clusters")

    #METHOD 2: -------------------------------------------------------------

    #------------------------------------------------
    #SECOND: --> CODE THAT RANK THE GROUPS
    #------------------------------------------------------------------------------------
    # Setup output table
    #calculate the cumulative rate of change (first difference) of the mean value ......
    clusters <- clusters[1:nrow(data)]

    data_reMelt <- dcast(data.long.melted, OAcode ~ Year, value.var='Crime_Count')
    data_reMelt <- cbind(data_reMelt, clusters) #head(data_reMelt)

    cluster_Group <- as.vector(unique(data_reMelt$clusters)[order(unique(data_reMelt$clusters))])


    sl_check_ALL <- NULL

    for(i in 1:length(cluster_Group)){ #i<-1 univaria

      # Deal with IDs
      #if(length(colnames(data))==16){
      #data2 <- data[,-1]
      #}
      data_reMelt_Cut <- data_backup[which(data_reMelt$clusters==cluster_Group[i]),]
      appender <- matrix(0, 1, 16)
      colnames(appender) <- c("ID","X1", "X2", "X3", "X4", "X5", "X6","X7", "X8", "X9", "X10", "X11", "X12", "X13", "X14", "X15")
      data_reMelt_Cut <- rbind(data_reMelt_Cut, appender)

      #if(dim(data_reMelt_Cut)[1]==1){
      #data_reMelt_Cut <- matrix(data_reMelt_Cut,,15)
      #data_reMelt_Cut <- rbind(data_reMelt_Cut, matrix(0, 1, 15))
      #}
      # time_reMelt_Cut <- time[which(data_reMelt$clusters==cluster_Group[i]),]
      count_Cluster <- nrow(data_reMelt_Cut) - 1
      #coefficients(lm(data[i,]~time[i,]))
      sl_check <- NULL

      for(q in 2:ncol(data_reMelt_Cut)){ #q<-2
        #b=coefficients(lm(data_reMelt_Cut[q,]~time_reMelt_Cut[q,]))
        #sl_check  <- rbind(sl_check , cbind(as.numeric(b[1]),as.numeric(b[2])))
        sl_check<-c(sl_check, mean(data_reMelt_Cut[,q]))
      }

      cum_rate_Change <- NULL
      for(r in 2:length(sl_check)){#r<-2
        cum_rate_Change <- c(cum_rate_Change, (sl_check[r] - sl_check[r-1]))
      }
      cum_rate_Change <- sum(cum_rate_Change)


      sl_check_ALL <- rbind(sl_check_ALL, cbind(cluster_Group[i], round(cum_rate_Change,digits = 3)))
    }

    #---------------------------
    head(sl_check_ALL)
    sl_check_ALL_Q <- rbind(sl_check_ALL_Q, cbind((h+2), sum(as.numeric(sl_check_ALL[,2]))))
    colnames(sl_check_ALL_Q) <- c("N_cluster","Q")
    #---------------------------
    #sl_check_ALL
    #sl_check_ALL[order(-as.numeric(as.character(sl_check_ALL[,2]))),]
    #order of clusters
    order_sl_check_ALL <-  order(-as.numeric(as.character(sl_check_ALL[,2])))
    #---------------------------------------------------------------------

    #------------------------------------------------
    #THIRD: --> CODE THAT LIST ALL THE LSOA OF EACH CLUSTER (GROUP)
    #------------------------------------------------------------------------------------

    data_reMelt <- dcast(data.long.melted, OAcode ~ Year, value.var='Crime_Count')
    data_reMelt <- cbind(data_reMelt, clusters) #head(data_reMelt)

    cluster_Group <- as.vector(unique(data_reMelt$clusters)[order(unique(data_reMelt$clusters))])
    #---------------------------------------------------------------------------
    #plot each cluster....
    #---------------------------------------------------------------------------
    clusters <- clusters[1:nrow(data)]

    data_reMelt <- dcast(data.long.melted, OAcode ~ Year, value.var='Crime_Count')
    data_reMelt <- cbind(data_reMelt, clusters) #head(data_reMelt)

    cluster_Group <- as.vector(unique(data_reMelt$clusters)[order(unique(data_reMelt$clusters))])

    length_Of_Clusters <- NULL

    cluster_Units_ALL <- list()

    for(i in 1:length(cluster_Group)){ #i<-1

      data_reMelt_Cut <- data_backup[which(data_reMelt$clusters==cluster_Group[i]),]
      appender <- matrix(0, 1, 16)
      colnames(appender) <- c("ID","X1", "X2", "X3", "X4", "X5", "X6","X7", "X8", "X9", "X10", "X11", "X12", "X13", "X14", "X15")
      data_reMelt_Cut <- rbind(data_reMelt_Cut, appender)

      #head(data_reMelt_Cut)

      #get the corresponding OA label #head(data)# head(agg_Data[1:10,])
      cluster_Units <- as.vector(agg_Data$V1[as.data.frame(data_reMelt_Cut)$ID])

      length_Of_Clusters <- rbind(length_Of_Clusters, cbind(i, length(cluster_Units)))

      cluster_Units_ALL[i] <- list(cluster_Units)

    }


    #list of all clusters
    cluster_Units_ALL

    #combine the order with the cluster list
    cluster_order_Rank <- NULL

    for(a in 1:length(order_sl_check_ALL)){ #a<-1


      cluster_order_Rank <- rbind(cluster_order_Rank, cbind(cluster_Units_ALL[[a]], order_sl_check_ALL[a]))

    }

    colnames(cluster_order_Rank) <- c("code", "Inequity_Rank")
    cluster_order_Rank <- as.data.frame(cluster_order_Rank)

    #now join the result to the LSOA list using the oacode field
    WM_LSOA_Rank_Data <- merge(WM_LSOA, cluster_order_Rank, by.x = "lsoa11cd", by.y = "code")  #mode(x_y_dat_LL_Data)
    #head(WM_LSOA_Rank_Data)  #mode(plot(WM_LSOA_Rank_Data)

    #utah = readOGR(dsn=".", layer="eco_l3_ut")
    WM_LSOA_Rank_Data@data$id = rownames(WM_LSOA_Rank_Data@data)
    WM_LSOA_Rank_Data.points = fortify(WM_LSOA_Rank_Data, region="id")
    WM_LSOA_Rank_Data.df = join(WM_LSOA_Rank_Data.points, WM_LSOA_Rank_Data@data, by="id")

    #utah@data$id = rownames(WM_LSOA_Rank_Data@data)
    #utah.points = fortify(utah, region="id")
    #utah.df = join(utah.points, utah@data, by="id") #head(WM_LSOA_Rank_Data.df)

    #-----------------
    colourCount = length(unique(WM_LSOA_Rank_Data.df$Inequity_Rank))
    getPalette = colorRampPalette(brewer.pal(7, "Set1"))
    if(h==2){
      #par(oma=c(0,0,0,0), mar=c(0,0,0,0), mfrow=c(ceiling(sqrt(length(cluster_center_List))),ceiling(sqrt(length(cluster_center_List)))))
      par(oma=c(0,0,0,0), mar=c(0,0,0,0), mfrow=c(1,1))
      #par()
    }
    ggplot(WM_LSOA_Rank_Data.df) +
      aes(long,lat,group=group,fill=factor(Inequity_Rank)) +
      #geom_histogram(aes(factor(hp), fill=factor(hp))) +
      geom_polygon() +
      geom_path(color="white") +
      scale_fill_manual(values = getPalette(colourCount)) +
      theme(legend.position="bottom")
    guides(fill=guide_legend(nrow=2)) +
      coord_equal() #+
    #scale_fill_brewer("Clusters")
    #-----------------
    #--------------------------------------------------------------

    .
    .
    .
    .
    .

    #-------------------------------------------

    #---------------------------------------------------------------------------------
    #now to plot a k solution
    h=10

    data_sim2 <- data_sim[, 2:ncol(data_sim)]
    #using the cluster centres from the above...
    part2 <- affectIndivC(data_sim2, cluster_center_List_sim[[h]])

    #data.append <- cbind(data, part2)
    #rownames(data.append) <- 1:nrow(data.append)
    data_sim.append <- cbind(1:nrow(data_sim2), data_sim2)
    colnames(data_sim.append) <- c("code", "1", "2", "3", "4", "5", "6", "7", "8",
                                   "9", "10", "11", "12", "13", "14", "15")
    data_sim.append <- as.data.frame(data_sim.append)

    data_sim.long.melted <- melt(data_sim.append, id="code")#head(data., id="code"
    head(data_sim.long.melted)

    #data.append <- cbind(data, part2)
    #data.long.melted <- melt(data, id="code")#head(d
    data_sim.long.melted <- cbind(data_sim.long.melted, rep(part2, 15))#nrow(data.long.melted)

    colnames(data_sim.long.melted) <- c("OAcode","Year","Crime_Count", "clusters")

    #convert numbers to letters
    clusters <- letters[data_sim.long.melted$clusters]
    clusters <- toupper(clusters) #head(cluster_letters)
    data_sim.long.melted$clusters <- clusters
    colnames(data_sim.long.melted) <- c("OAcode","Year","Crime_Count", "clusters")


    ggplot(data_sim.long.melted, aes(x=Year, y=Crime_Count,
                                     group=OAcode, color=clusters)) +
      geom_line() +
      stat_summary(fun.y=mean, geom="line", aes(group=clusters), color="black", size=2) +
      facet_wrap(~clusters) +
      #scale_colour_brewer(palette = "Set1") +
      theme_minimal()
    #------------------------------------------------------

    #THE LIKELIHOOD FUNCTION USING SUM OF INDIVIDUAL TRAJECTORY
    # Setup output table
    #calculate the cumulative rate of change (first difference) of the mean value ......
    clusters <- clusters[1:nrow(data_sim)]

    data_reMelt <- dcast(data_sim.long.melted, OAcode ~ Year, value.var='Crime_Count')
    data_reMelt <- cbind(data_reMelt, clusters) #head(data_reMelt)

    cluster_Group <- as.vector(unique(data_reMelt$clusters)[order(unique(data_reMelt$clusters))])
    sl_check_ALL <- NULL

    for(i in 1:length(cluster_Group)){ #i<-1
      # Deal with IDs
      data_reMelt_Cut <- data[which(data_reMelt$clusters==cluster_Group[i]),]
      # time_reMelt_Cut <- time[which(data_reMelt$clusters==cluster_Group[i]),]
      count_Cluster <- nrow(data_reMelt_Cut)
      #coefficients(lm(data[i,]~time[i,]))
      sl_check <- NULL
      #HERE INSTEAD OF FINDING THE MEAN OF ENTIRE TRAJECTORIES, I WANT TO
      #DO IT FOR INDIVIDUAL TRAJECTORY
      for(s in 1:nrow(data_reMelt_Cut)){   #s<-1
        data_reMelt_Cut_Sub <- data_reMelt_Cut[s,]
        #for(q in 1:ncol(data_reMelt_Cut)){ #q<-1
        #sl_check<-c(sl_check, mean(data_reMelt_Cut[,q]))
        # }
        cum_rate_Change <- NULL
        #for(r in 2:length(sl_check)){#r<-2
        for(r in 2:length(data_reMelt_Cut_Sub)){#r<-2
          #cum_rate_Change <- c(cum_rate_Change, (sl_check[r] - sl_check[r-1]))
          cum_rate_Change <- c(cum_rate_Change, (data_reMelt_Cut_Sub[r] - data_reMelt_Cut_Sub[r-1]))
          #data_reMelt_Cut_Sub
        }
        sl_check <- c(sl_check, sum(cum_rate_Change))

      }
      sl_check <- sum(sl_check)
      #sl_check_ALL <- rbind(sl_check_ALL, cbind(cluster_Group[i], round(cum_rate_Change,digits = 3)))
      sl_check_ALL <- rbind(sl_check_ALL, cbind(cluster_Group[i], round(sl_check,digits = 3)))
    }
    #---------------------------
    head(sl_check_ALL)
    #---------------------------
    sl_check_ALL
    sl_check_ALL[order(-as.numeric(as.character(sl_check_ALL[,2]))),]
    #-----------
    ###################################################################################
    # STATISTICAL SIGNIFICANCE CALCULATIONS -END
    ###################################################################################

















    #CALCULATE MORAN'S I STAT
    neighbourhood <- poly2nb(WM_LSOA_Rank_Data, queen=TRUE)
    #generate list of weights
    neighbourhood_Weight_list <- nb2listw(neighbourhood, style = "B", zero.policy = TRUE)#W
    #names(neighbourhood_Weight_list)#neighbourhood_Weight_list$weights[1]    #neighbourhood_Weight_list$neighbours[1]


    moran_i_chisqr <- moran.test(as.numeric(as.character(WM_LSOA_Rank_Data@data$Inequity_Rank)), neighbourhood_Weight_list)
    #Using a Monte Carlo based test of significance.#names(moran_i_chisqr)#moran_i_chisqr$p.value
    moran_i_monte_carlo <- moran.mc(as.numeric(as.character(WM_LSOA_Rank_Data@data$Inequity_Rank)),
                                    neighbourhood_Weight_list,
                                    nsim=999)
    pvalue <- rbind(pvalue, cbind((h+2), as.numeric(moran_i_chisqr$estimate[1]), moran_i_chisqr$p.value, moran_i_monte_carlo$p.value))



    flush.console()
    print(h+2)
  }

  colnames(pvalue) <- c("N.Clusters", "I_estimate", "p.value", "MC_p.value")
  result_NN <- cbind(pvalue, sl_check_ALL_Q[,2])
  colnames(result_NN) <- c("N.Clusters", "I_estimate", "p.value", "MC_p.value", "Q")


  write.table(result_NN, file="TMV_New_Q.csv", sep=",")



  sl_check_ALL_Q[,2]
  #pvalue
  #sl_check_ALL_Q
  names(  as.numeric(moran_i_chisqr$estimate[1])  )
  ##------------------------------------
  #join


  #-------------------------------------------------------------------------
  #-------------------------------------------------------------------------

  install.packages("OasisR")
  library(OasisR)
  contig(segdata)
  plot(segdata)
  names(segdata)
  foldername <- system.file('extdata', package = 'OasisR')
  shapename <- 'segdata'
  contig(folder = foldername, shape = shapename)




  plot_List <- list()

  plot_List[[1]] <- WM_LSOA_Rank_Data.df


  plotAllCounts <- function (dt){
    plots <- list();
    for(i in 1:length(dt)) {
      #strX = names(dt)[i]
      #print(sprintf("%i: strX = %s", i, strX))
      plots[[i]] <- ggplot(WM_LSOA_Rank_Data.df) +
        aes(long,lat,group=group,fill=factor(Inequity_Rank)) +
        #geom_histogram(aes(factor(hp), fill=factor(hp))) +
        geom_polygon() +
        geom_path(color="white") +
        scale_fill_manual(values = getPalette(colourCount)) +
        theme(legend.position="bottom") +
        guides(fill=guide_legend(nrow=2)) +
        coord_equal() #+
    }

    columnsToPlot <- floor(sqrt(length(dt)))
    multiplot(plotlist = plots, cols = columnsToPlot)
  }

  dt = ggplot2::diamonds
  plotAllCounts(dt=WM_LSOA_Rank_Data.df)


  #to merge the cluster

  WM_LSOA <- readOGR(dsn=".", "Birmingham_normal_LSOA_osgb")  #head(WM_LSOA)
  neighbourhood <- poly2nb(WM_LSOA, queen=TRUE)
  #generate list of weights
  neighbourhood_Weight_list <- nb2listw(neighbourhood, style = "B", zero.policy = TRUE)#W
  #names(neighbourhood_Weight_list)#neighbourhood_Weight_list$weights[1]    #neighbourhood_Weight_list$neighbours[1]
  moran.test(as.numeric(as.character(WM_LSOA@data$imd_rank)), neighbourhood_Weight_list)



  #-----------------------------
  #ggsave(paste("assaultl_8
  #-----------------------------------------------------------------------------------------
  #-----------------------------------------------------------------------------------------
  AIC_BIC_List <- NULL

  for(q in 3:24){ #q<-3  16
    #comparison of the two methods (automated center selection and pre-determined linear trends)
    old_K <- kmeans(data, q)
    AIC_BIC_old <- BIC2(old_K)

    new_K <- kmeans(data, centers = cluster_center_List[[q-2]]) #(q-2) --> equivalent to three cluster centers
    AIC_BIC_new <- BIC2(new_K)
    #centers <- k10$centers

    AIC_BIC_List <- rbind(AIC_BIC_List, cbind(AIC_BIC_old, AIC_BIC_new))

  }
  #-----------------------------------------------------------------------------------------
  #-----------------------------------------------------------------------------------------

  AIC_BIC_List

  #AIC_BIC_List <- t(AIC_BIC_List)
  AIC_BIC_List.append <- cbind(1:nrow(AIC_BIC_List), AIC_BIC_List)
  colnames(AIC_BIC_List.append) <- c("nos_clusters","1", "2", "3", "4")

  melted = melt(AIC_BIC_List.append, id="nos_clusters")

  Kmean_Model <- c(rep("Standard",44), rep("Modified",44))
  #Metric_Aic_Bic <- c(rep("aic_old",22), rep("aic_new",22), rep("bic_old",22), rep("bic_new",22))
  Metric <- c(rep("AIC",22), rep("BIC",22), rep("AIC",22), rep("BIC",22))

  ggplot() +
    geom_line(data=melted, aes(x=nos_clusters, y=value, group=variable,
                               colour=Kmean_Model, linetype=Metric)) #+


  geom_point(color="blue", size=13)


  #------------------------------------------
  BIC2 <- function(fit){
    m = ncol(fit$centers)
    n = length(fit$cluster)
    k = nrow(fit$centers)
    D = fit$tot.withinss
    return(data.frame(AIC = D + 2*m*k,
                      BIC = D + log(n)*m*k))
  }
  #------------------------------------------

  #-----------------------------------------------------
  #-----------------------------------------------------
  #-----------------------------------------------------
  #-----------------------------------------------------
  #-----------------------------------------------------
  #-----------------------------------------------------
  #REGULAR KML


  data <- read.table("allCrime_agg.csv", sep=",", head=TRUE)
  data <- read.table("burglary_agg.csv", sep=",", head=TRUE)
  data <- read.table("assault_agg.csv", sep=",", head=TRUE)
  data <- read.table("criminalDamage_agg.csv", sep=",", head=TRUE)
  data <- read.table("TMV_agg.csv", sep=",", head=TRUE)

  head(data)

  #------------------------------------------------------
  #to use the LSOA boundary
  #read the nested data
  #extract the Birmingham OA lists#head(data)
  birm_codes <- as.vector(data$code)
  WM_nested_Details <- read.table("west-midlands-lookup-full-2011.csv", sep=",", head=TRUE)#head(nested_Details)
  WM_nested_Details_OAcode <- as.vector(WM_nested_Details$OAcode)
  Birm_nested_Details_OA <- WM_nested_Details[which(WM_nested_Details_OAcode %in% birm_codes),]#head(Birm_nested_Details_OA)#Birm_nested_Details_OA[1:16,]
  #now aggregated the data by LSOA using the nest code table
  uni_LSOA <- as.vector(unique(Birm_nested_Details_OA$LSOAcod))#head(data)

  agg_Data <- NULL

  for(r in 1:length(uni_LSOA)){ #r = 1
    ind_ <- as.vector(Birm_nested_Details_OA$OAcode[which(Birm_nested_Details_OA$LSOAcod == uni_LSOA[r])])
    sub_Data_ <- data[which(data$code %in%ind_),]
    sum_sub_Data_ <- colSums(sub_Data_[,2:ncol(sub_Data_)])
    agg_Data <- rbind(agg_Data, c(uni_LSOA[r], as.vector(sum_sub_Data_)))
  }#head(as.data.frame(agg_Data))
  agg_Data <- as.data.frame(agg_Data)
  data <- agg_Data
  #colnames(agg_Data) <- c("")#head(data) #head(agg_Data)
  #------------------------------------------------------


  data[,2:16] <- apply(data[,2:16], 2, as.numeric)#head(data)
  data <- cbind(1:nrow(data), data[,2:ncol(data)])
  colnames(data) <- c("ID","X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12","X13","X14","X15")


  #change absolute count to proportion
  for(h in 2:ncol(data)){ #h<-2
    prop <- (data[,h]/sum(as.numeric(as.character(data[,h]))))*100
    data[,h] <- prop
  }

  #--------------------------
  data_backup <- data
  #--------------------------

  #DON'T GO BEYOND HERE FOR "data" THE MORAN'S I SECTION ABOVE........


  #colnames(data) <- c("code","yr1","yr2","yr3","yr4","yr5","yr6","yr7","yr8","yr9","yr10","yr11","yr12","yr13","yr14","yr15")
  colnames(data) <- c("code","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")
  #colnames(data) <- c("ID","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")
  #
  #data[,2] + 1

  data[,2:16] <- apply(data[,2:16], 2, as.numeric)

  data$code <- 1:nrow(data)
  #data$ID <- 1:nrow(data)

  ##set.seed(101)#head(data)
  #burglary

  ### Generation of some data
  ##cld1 <- generateArtificialLongData()

  ### Setting two different set of option :
  (option1 <- parALGO())
  ##(option2 <- parALGO(distanceName="euclidean",centerMethod=function(x)median(x,na.rm=TRUE)))

  ### Running kml We suspect 3, 4 or 5 clusters, we want 3 redrawing.
  ##kml(cld1,3:5,3,toPlot="both",parAlgo=option1)
  ##kml(cld1,3:5,3,toPlot="both",parAlgo=option2)

  OAtrajk_crime <- clusterLongData(traj= data)
  #kml(OAtrajk_crime, nbClusters = c(3:16), toPlot = "none", nbRedrawing = 20)
  kml(OAtrajk_crime, nbClusters = c(3:26), toPlot = "none", nbRedrawing = 20, parAlgo=option1)

  #dev.new()
  #plot(OAtrajk_crime, 4)


  #"randomAll"
  #for_Crime <- OAtrajk_all_crime #<- OAtrajk_crime
  ##for_Crime <- OAtrajk_burglary ######<- OAtrajk_crime
  #for_Crime <- OAtrajk_TMV #<- OAtrajk_crime
  #for_Crime <- OAtrajk_assault ###<- OAtrajk_crime
  #for_Crime <- OAtrajk_criminalDamage #######<- OAtrajk_crime

  #LSOA
  #for_Crime <- OAtrajk_all_crime #<- OAtrajk_crime
  for_Crime <- OAtrajk_burglary_lsoa ########<- OAtrajk_crime
  for_Crime <- OAtrajk_TMV_lsoa <- OAtrajk_crime
  for_Crime <- OAtrajk_assault_lsoa <- OAtrajk_crime
  for_Crime <- OAtrajk_criminalDamage_lsoa #######<- OAtrajk_crime


  dev.new()
  choice(for_Crime)

  #plot(for_Crime, 16, toPlot="both", criterion=)

  #temporal plot dev.new()
  #---------------------------------

  dev.new()#head(data.append)

  i<-2 + 6 #change the last number

  clusters <- getClusters(for_Crime, c(i))

  data.append <- cbind(data, clusters)

  #melt(data.append)

  data.long.melted <- melt(data, id="code")#head(data.long.melted)#head(data)

  data.long.melted <- cbind(data.long.melted, rep(clusters, 15))

  head(data.long.melted)

  colnames(data.long.melted) <- c("OAcode","Year","Crime_Count", "Clusters")

  #-----------------------------head(data.long.melted)#head(data)


  #library(tidyverse)
  #to plot
  ggplot(data.long.melted, aes(x=Year, y=Crime_Count,
                               group=OAcode, color=Clusters)) +
    geom_line() +
    stat_summary(fun.y=mean, geom="line", aes(group=Clusters), color="black", size=2) +
    facet_wrap(~Clusters) +
    #scale_colour_brewer(palette = "Set1") +
    theme_minimal()
  #-----------------------------
  #ggsave(paste("assaultl_8.png", sep=""))

  #OLD METHOD
  #---------------------------------------------------------------------------
  # Setup output table
  #calculate the cumulative rate of change of the mean value ......
  #clusters <-

  ##data_reMelt <- dcast(data.long.melted, OAcode ~ Year, value.var='Crime_Count')
  ##data_reMelt <- cbind(data_reMelt, clusters) #head(data_reMelt)

  ##cluster_Group <- as.vector(unique(data_reMelt$clusters)[order(unique(data_reMelt$clusters))])


  ##sl_check_ALL <- NULL

  ##for(i in 1:length(cluster_Group)){ #i<-12

  # Deal with IDs
  ##if(length(colnames(data))==16){
  ##data2 <- data[,-1]
  ##}

  ##data_reMelt_Cut <- data2[which(data_reMelt$clusters==cluster_Group[i]),]
  # time_reMelt_Cut <- time[which(data_reMelt$clusters==cluster_Group[i]),]

  #coefficients(lm(data[i,]~time[i,]))
  ##sl_check <- NULL

  ##for(q in 1:ncol(data_reMelt_Cut)){ #q<-1
  #b=coefficients(lm(data_reMelt_Cut[q,]~time_reMelt_Cut[q,]))
  #sl_check  <- rbind(sl_check , cbind(as.numeric(b[1]),as.numeric(b[2])))
  ##sl_check<-c(sl_check, mean(data_reMelt_Cut[,q]))
  ##}

  ##cum_rate_Change <- NULL
  ##for(r in 2:length(sl_check)){#r<-2
  ##cum_rate_Change <- c(cum_rate_Change, (sl_check[r] - sl_check[r-1]))
  ##}
  ##cum_rate_Change <- sum(cum_rate_Change)

  ##sl_check_ALL <- rbind(sl_check_ALL, cbind(cluster_Group[i], round(cum_rate_Change,digits = 3)))
  ## }

  #---------------------------
  ##head(sl_check_ALL)
  #---------------------------
  ##sl_check_ALL
  ##sl_check_ALL[order(-as.numeric(as.character(sl_check_ALL[,2]))),]
  #---------------------------------------------------------------------------


  #THE LIKELIHOOD FUNCTION USING SUM OF INDIVIDUAL TRAJECTORY
  # Setup output table
  #calculate the cumulative rate of change (first difference) of the mean value ......
  clusters <- clusters[1:nrow(data)]

  data_reMelt <- dcast(data.long.melted, OAcode ~ Year, value.var='Crime_Count')
  data_reMelt <- cbind(data_reMelt, clusters) #head(data_reMelt)

  cluster_Group <- as.vector(unique(data_reMelt$clusters)[order(unique(data_reMelt$clusters))])
  sl_check_ALL <- NULL

  for(i in 1:length(cluster_Group)){ #i<-1
    # Deal with IDs
    data_reMelt_Cut <- data[which(data_reMelt$clusters==cluster_Group[i]),]
    # time_reMelt_Cut <- time[which(data_reMelt$clusters==cluster_Group[i]),]
    count_Cluster <- nrow(data_reMelt_Cut)
    #coefficients(lm(data[i,]~time[i,]))
    sl_check <- NULL
    #HERE INSTEAD OF FINDING THE MEAN OF ENTIRE TRAJECTORIES, I WANT TO
    #DO IT FOR INDIVIDUAL TRAJECTORY
    for(s in 1:nrow(data_reMelt_Cut)){   #s<-1
      data_reMelt_Cut_Sub <- data_reMelt_Cut[s,]
      #for(q in 1:ncol(data_reMelt_Cut)){ #q<-1
      #sl_check<-c(sl_check, mean(data_reMelt_Cut[,q]))
      # }
      cum_rate_Change <- NULL
      #for(r in 2:length(sl_check)){#r<-2
      for(r in 3:length(data_reMelt_Cut_Sub)){#r<-3
        #cum_rate_Change <- c(cum_rate_Change, (sl_check[r] - sl_check[r-1]))
        cum_rate_Change <- c(cum_rate_Change, (data_reMelt_Cut_Sub[1,r] - data_reMelt_Cut_Sub[1,(r-1)]))
        #data_reMelt_Cut_Sub
      }
      sl_check <- c(sl_check, sum(cum_rate_Change))

    }
    sl_check <- sum(sl_check)
    #sl_check_ALL <- rbind(sl_check_ALL, cbind(cluster_Group[i], round(cum_rate_Change,digits = 3)))
    sl_check_ALL <- rbind(sl_check_ALL, cbind(cluster_Group[i], round(sl_check,digits = 3)))
  }


  #---------------------------
  head(sl_check_ALL)
  #---------------------------
  sl_check_ALL
  sl_check_ALL[order(-as.numeric(as.character(sl_check_ALL[,2]))),]
  #---------------------------------------------------------------------------
  #

  #---------------------------------------------------------------------------
  #plot each cluster....
  #---------------------------------------------------------------------------
  data_reMelt <- dcast(data.long.melted, OAcode ~ Year, value.var='Crime_Count')
  data_reMelt <- cbind(data_reMelt, clusters) #head(data_reMelt)

  cluster_Group <- as.vector(unique(data_reMelt$clusters)[order(unique(data_reMelt$clusters))])

  length_Of_Clusters <- NULL

  cluster_Units_ALL <- list()

  for(i in 1:length(cluster_Group)){ #i<-1

    # Deal with IDs
    #if(length(colnames(data))==16){
    #data2 <- data[,-1]
    #}

    data_reMelt_Cut <- data[which(data_reMelt$clusters==cluster_Group[i]),]#head(data_reMelt)
    #head(data_reMelt_Cut)

    #get the corresponding OA label #head(data)# head(agg_Data[1:10,])
    cluster_Units <- as.vector(agg_Data$V1[as.data.frame(data_reMelt_Cut)$ID])

    length_Of_Clusters <- rbind(length_Of_Clusters, cbind(i, length(cluster_Units)))

    cluster_Units_ALL[i] <- list(cluster_Units)

  }

  #--------------------------
  #ranked from the most inequitous area to the least
  #ranked from the most inequitous area to the least
  rank_Areas <- sl_check_ALL[order(-as.numeric(as.character(sl_check_ALL[,2]))),]
  rank_Areas <-cbind(match(rank_Areas[,1], toupper(letters)), rank_Areas[,2])
  colnames(rank_Areas) <- c("id","Q")
  #length_Of_Clusters <- cbind(toupper(letters[length_Of_Clusters[,1]]), length_Of_Clusters[,2] ) #letter
  length_Of_Clusters <- cbind(length_Of_Clusters[,1], length_Of_Clusters[,2] ) #letter

  colnames(length_Of_Clusters) <- c("id","count")

  result_Merge <- merge(length_Of_Clusters, rank_Areas, by="id")

  result_Merge_New <- cbind(length_Of_Clusters, rank_Areas[match(length_Of_Clusters[,1], rank_Areas[,1]),])

  #-----------------------------------------
  result_Merge_New[order(-as.numeric(result_Merge_New[,4])),]

  #plotting
  #------------------------------------------------------------------------------

  # par()#getwd()

  #read-in the polygon shapefiles
  WM_LSOA <- readOGR(dsn=".", "Birmingham_normal_LSOA_osgb")  #head(WM_LSOA)
  #WM_LSOA <- readOGR(dsn=".", "Birmingham_hexmap_LSOA_osgb")

  #par(mfrow=c(3,4))
  par(oma=c(0,0,0,0), mar=c(0,0,0,0), mfrow=c(3,4))
  #
  for(p in 1:length(cluster_Units_ALL)){ #p<-1

    sp::plot(WM_LSOA, border="grey")
    sp::plot(WM_LSOA[which(as.vector(WM_LSOA$LSOA11CD_1)%in%cluster_Units_ALL[[p]]),], add=TRUE, col="grey", border="black")

  }

  #---------------------------
  #head(sl_check_ALL)
  #---------------------------
  #---------------------------------------------------------------------------


  ####SECTION "REGULAR": HERE IT IS! REGULAR KMEAN
  #---------------------------------------------------------------------------
  #---------------------------------------------------------------------------
  #---------------------------------------------------------------------------
  #---------------------------------------------------------------------------

  library(traj)

  getwd()

  #setwd("G:/20180613/Monsuru_WMD_12062018/")
  setwd("C:/Users/monsu/Documents/MMU DOCUMENTS/G/Removable Disk/20180613/Monsuru_WMD_12062018/")

  setwd("//staffhome/staff_home0/55131065/Documents/Monsuru_WMD_12062018/")
  #setwd("G:/new data to upload/")

  #install.packages("kml")
  library(TTR)
  library(kml)
  library(crimCV)
  library(tidyr)
  library(base)
  library(data.table)
  library(sf)
  library(tidyverse)
  library(rgdal)
  library(reshape2)
  library(kml)
  library(geogrid)
  require(sp)
  require(spdep)
  require("rgdal") # requires sp, will use proj.4 if installed
  require("maptools")
  require("plyr")
  library(RColorBrewer)
  library(vegan)
  require(foreign)
  library(MASS)
  library(Hmisc)

  #isolate the outlier



  #--------------------------------------------------------------------------
  #--------------------------------------------------------------------------
  #IMPORTING THE DATA TO INCLUDE RATE (i.e. times 100 population at the Output area level)

  #CRIME DATA
  #data <- read.table("allCrime_agg.csv", sep=",", head=TRUE)
  #data <- read.table("burglary_agg.csv", sep=",", head=TRUE)
  data <- read.table("assault_agg.csv", sep=",", head=TRUE)
  #data <- read.table("criminalDamage_agg.csv", sep=",", head=TRUE)
  #data <- read.table("TMV_agg.csv", sep=",", head=TRUE) #head(data)head(data) data[which(data$code=="E00047775"),]
  data[1:10,]

  #uni_LSOA <- as.vector(unique(data$code))#head(data)

  #read the population data
  pop_02_16 <- read.table(file="population_WM_OA_02_16.csv", sep=",", head=TRUE) #this was generated below
  #head(pop_02_16)  #pop_02_16[which(pop_02_16$OAcode=="E00175716"),]
  #OA list
  OA_unique <- readOGR(dsn=".", "Birmingham_OA_osgb")  #head
  OA_unique <- as.vector(OA_unique@data$OAcode)
  head(OA_unique)#[1] "E00047772" "E00047775" "E00047768" "E00047681" "E00047703" "E00047687"
  #------------------------------------------------------
  #now aggregated the data by LSOA using the nest code table
  uni_OA <- as.vector(data$code) #head(data)  #uni_OA[1:4]#[1] "E00045077" "E00045078" "E00045079" "E00045080"#OA_unique[1:4][1] "E00047772" "E00047775" "E00047768" "E00047681"



  data <- apply(data[,2:16], 2, as.numeric)#head(data)
  data <- cbind(1:nrow(data), data)
  colnames(data) <- c("ID","X1","X2","X3","X4","X5","X6","X7","X8","X9","X10", "X11","X12","X13","X14","X15")
  #----------------------------------------------------------
  #head(data)

  data_Fresh <- NULL
  #now normalise with population
  for(k in 1:length(OA_unique)){#k<-3133
    pop_cut <- as.numeric(pop_02_16[which(as.vector(pop_02_16$OAcode)==OA_unique[k]),2:16])
    data_cut <- as.numeric(data[which(uni_OA==OA_unique[k]),2:16])
    data_Pop_100 <- (data_cut / pop_cut)*100
    data_Fresh <- rbind(data_Fresh, round(data_Pop_100,digits=5))
    #data[k,2] <- data_Pop_100[1]
  }

  agg_Data <- as.data.frame(cbind(V1=OA_unique, data_Fresh))

  #now transfer back to data
  data <- agg_Data

  data <- apply(data[,2:16], 2, as.numeric)#head(data)
  data <- cbind(1:nrow(data), data)
  colnames(data) <- c("ID","X1","X2","X3","X4","X5","X6","X7","X8","X9","X10", "X11","X12","X13","X14","X15")


  #NOW, CONVERTING TO PROPORTION
  #--------------------------------------------------------------------------
  #--------------------------------------------------------------------------
  #data_Sub <- NULL

  for(h in 2:ncol(data)){ #h<-2
    prop <- (data[,h]/sum(as.numeric(as.character(data[,h]))))*100
    data[,h] <- prop
    #data_Sub <- cbind(data_Sub, prop)
  }
  head(data)
  #head(data_Sub)
  #--------------------------
  data_backup <- data #head(data)
  #--------------------------



  #burglary --------------------------------------------------------------------------
  out_List <- NULL
  for(w in 1:nrow(data)){#w<-1
    sn_ <- length(which(data[w,2:ncol(data)]> 0.8))
    if(sn_>0){
      out_List <- c(out_List,w)
    }
  }
  #replace each of these with subsequent records#data[1605:1606,]
  for(w in 1:length(out_List)){#w<-1
    data[out_List[w],2:ncol(data)] <- data[(out_List[w]+1),2:ncol(data)]
  }
  #-------

  #assault------------------------------------------------------------------------------
  out_List <- NULL
  for(w in 1:nrow(data)){#w<-1  data[3137,]
    sn_ <- length(which(as.numeric(data[w,2:ncol(data)])  > 1))
    if(sn_>0){
      out_List <- c(out_List,w)
    }
  }
  #replace each of these with subsequent records#data[1605:1606,]
  for(w in 1:length(out_List)){#w<-1
    data[out_List[w],2:ncol(data)] <- data[1,2:ncol(data)]
  }
  #-------

  #criminal Damage---------------------------------------------------------------------
  out_List <- NULL
  for(w in 1:nrow(data)){#w<-1  data[3137,]
    sn_ <- length(which(as.numeric(data[w,2:ncol(data)])  > 1))
    if(sn_>0){
      out_List <- c(out_List,w)
    }
  }
  #replace each of these with subsequent records#data[1605:1606,]
  for(w in 1:length(out_List)){#w<-1
    data[out_List[w],2:ncol(data)] <- data[1,2:ncol(data)]
  }
  #-------

  #TMV--------------------------------------------------------------------------------
  out_List <- NULL
  for(w in 1:nrow(data)){#w<-1  data[3137,]
    sn_ <- length(which(as.numeric(data[w,2:ncol(data)])  > 1))
    if(sn_>0){
      out_List <- c(out_List,w)
    }
  }
  #replace each of these with subsequent records#data[1605:1606,]
  for(w in 1:length(out_List)){#w<-1
    data[out_List[w],2:ncol(data)] <- data[1,2:ncol(data)]
  }
  #-------
  #data[c(3069, 3076, 3136, 3148),]
  #data[which(data[,12]>5),]  #data[1605,]
  #----------------------------


  #date infor
  data_time <- NULL
  for(k in 1:nrow(data)){
    data_time <- rbind(data_time, 1:15)
  }
  data_time <- as.data.frame(data_time)
  data_time <- cbind(1:nrow(data), data_time)
  colnames(data_time) <- c("ID","time.1", "time.2", "time.3", "time.4", "time.5", "time.6", "time.7", "time.8", "time.9", "time.10", "time.11",
                           "time.12", "time.13", "time.14", "time.15")
  head(data_time)
  #data.time <- data_time[,1:6]

  #use absolute count
  #identify trends

  #data <- ts(data)
  #plot.ts(data)
  #decomposing the (non-seasonal) data into its components: trend component and an irregular component
  #To estimate the trend component of a non-seasonal time series that can be described using an additive model, it is common to use a smoothing method, such as calculating the simple moving average of the time series.
  #http://a-little-book-of-r-for-time-series.readthedocs.io/en/latest/src/timeseries.html

  #install.packages("TTR")
  #dataSMA3 <- SMA(data[1,2:15],n=3)
  #plot.ts(dataSMA3)

  #------------------------------------------------------------------------------

  time = data_time
  sample.size = dim(data)[1]

  # Deal with IDs
  #if(ID){
  IDvector = data[,1]
  data = data[,-1]
  time = time[,-1]
  #}

  max.num.obs  = dim(data)[2]
  # Clean input tables
  clean.data = matrix(ncol= max.num.obs, nrow=sample.size)
  clean.time = matrix(ncol= max.num.obs, nrow=sample.size)

  num.obs = rep(999,sample.size)
  less.than.4.obs = NULL

  # Check for appropriate amount of usable data
  for(i_sample in 1:sample.size)#i_sample <- 1
  {
    real.obs.pos = which(!is.na(data[i_sample,]))
    num.obs[i_sample] = length(real.obs.pos)
    clean.data[i_sample,] = as.vector(c(unlist(data[i_sample,real.obs.pos]), rep(NA, max.num.obs - num.obs[i_sample])))
    clean.time[i_sample,] = as.vector(c(unlist(time[i_sample,real.obs.pos]), rep(NA, max.num.obs - num.obs[i_sample])))

    if(length(real.obs.pos) < 4)
      less.than.4.obs = c(less.than.4.obs, i_sample)

    clean.data.pos = which(!is.na(clean.data[i_sample,]))
    if(any(is.na(clean.time[i_sample,clean.data.pos])))
      stop(paste("There must be a time associated to every observation. Line: ", i_sample, sep = ""))
  }


  # Setup output table
  data = clean.data #head(data)
  time = clean.time

  #-----------------------------------------------------------------------------
  #-----------------------------------------------------------------------------
  #-----------------------------------------------------------------------------


  #set.seed(1)
  #colnames(data) <- c("code","yr1","yr2","yr3","yr4","yr5","yr6","yr7","yr8","yr9","yr10","yr11","yr12","yr13","yr14","yr15")
  ##colnames(data) <- c("code","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")
  #colnames(data) <- c("ID","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")
  #
  #data[,2] + 1

  data <- as.data.frame(cbind(1:nrow(data), data))#head(data)

  colnames(data) <- c("code","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")  #data[1,2] +1

  #data[,2:16] <- apply(data[,2:16], 2, as.numeric)

  #data$code <- 1:nrow(data)
  #data$ID <- 1:nrow(data)

  set.seed(101)#head(data)
  #burglary

  ### Generation of some data
  ##cld1 <- generateArtificialLongData()

  ### Setting two different set of option :
  (option1 <- parALGO())
  ##(option2 <- parALGO(distanceName="euclidean",centerMethod=function(x)median(x,na.rm=TRUE)))

  ### Running kml We suspect 3, 4 or 5 clusters, we want 3 redrawing.
  ##kml(cld1,3:5,3,toPlot="both",parAlgo=option1)
  ##kml(cld1,3:5,3,toPlot="both",parAlgo=option2)

  OAtrajk_crime <- clusterLongData(traj= data)
  #kml(OAtrajk_crime, nbClusters = c(3:16), toPlot = "none", nbRedrawing = 20)
  kml(OAtrajk_crime, nbClusters = c(3:26), toPlot = "none", nbRedrawing = 20, parAlgo=option1)

  #dev.new()
  #plot(OAtrajk_crime, 4)


  #"randomAll"
  #for_Crime <- OAtrajk_all_crime #<- OAtrajk_crime
  ##for_Crime <- OAtrajk_burglary ######<- OAtrajk_crime
  #for_Crime <- OAtrajk_TMV #<- OAtrajk_crime
  #for_Crime <- OAtrajk_assault ###<- OAtrajk_crime
  #for_Crime <- OAtrajk_criminalDamage #######<- OAtrajk_crime

  #LSOA
  #for_Crime <- OAtrajk_all_crime #<- OAtrajk_crime
  #for_Crime <- OAtrajk_burglary_lsoa ########<- OAtrajk_crime
  #for_Crime <- OAtrajk_TMV_lsoa <- OAtrajk_crime
  #for_Crime <- OAtrajk_assault_lsoa <- OAtrajk_crime
  #for_Crime <- OAtrajk_criminalDamage_lsoa #######<- OAtrajk_crime

  #OA Birmingham
  ##for_Crime <- OAtrajk_burglary_OA #<- OAtrajk_crime
  for_Crime <- OAtrajk_assault_OA <- OAtrajk_crime
  ##for_Crime <- OAtrajk_criminalDamage_OA <- OAtrajk_crime
  ##for_Crime <- OAtrajk_TMV_OA <- OAtrajk_crime

  dev.new()
  choice(for_Crime)

  #plot(for_Crime, 16, toPlot="both", criterion=)

  #temporal plot dev.new()
  #---------------------------------

  dev.new()#head(data.append)

  i<-2 + 19 #change the last number

  clusters <- getClusters(for_Crime, c(i))

  ###data.append <- cbind(data, clusters)

  #head(data.append)#head(data)

  ##data.long.melted <- melt(data.append, id="code")#head(data.long.melted)#head(data)
  #data.long.melted <- melt(data, id="code")#head(data.long.melted)#head(data)
  ##colnames(data.append) <- c("code", "1", "2", "3", "4", "5", "6", "7", "8",
  ##"9", "10", "11", "12", "13", "14", "15")
  ##data.append <- as.data.frame(data.append)



  #rownames(data.append) <- 1:nrow(data.append)#head(data)
  ####data.append <- cbind(1:nrow(data), data)
  data.append <- as.data.frame(data)
  colnames(data.append) <- c("code", "1", "2", "3", "4", "5", "6", "7", "8",
                             "9", "10", "11", "12", "13", "14", "15")
  ###data.append <- as.data.frame(data.append)
  #data.append <- cbind(data.append, clusters) #head(data.append)
  #melt(data.append)##nrow(data.long.melted)#
  data.long.melted <- melt(data.append, id="code")#head(data.long.melted)#head(data)

  data.long.melted <- cbind(data.long.melted, as.vector(rep(clusters, 15)))
  head(data.long.melted)
  colnames(data.long.melted) <- c("OAcode","Year","Crime_Count", "Clusters")





  ##data.long.melted <- melt(data.append, id="code")#head(data., id="code"
  ##head(data.long.melted)#nrow(data.long.melted)

  #data.long.melted <- cbind(data.long.melted, rep(clusters, 21))

  head(data.long.melted)

  colnames(data.long.melted) <- c("OAcode","Year","Crime_Count", "Clusters")

  #-----------------------------head(data.long.melted)#head(data)


  #library(tidyverse)
  #to plot
  ggplot(data.long.melted, aes(x=Year, y=Crime_Count,
                               group=OAcode, color=Clusters)) +
    geom_line() +
    stat_summary(fun.y=mean, geom="line", aes(group=Clusters), color="black", size=2) +
    facet_wrap(~Clusters) +
    #scale_colour_brewer(palette = "Set1") +
    theme_minimal()
  #-----------------------------
  #ggsave(paste("assaultl_8.png", sep=""))


  #-----------------------------------------
  #-----------------------------------------
  #Generating the .RData for the comparison
  #GENERATING THE .RData for the comparative study....
  #for_Crime <- OAtrajk_burglary_OA #<- OAtrajk_crime
  #for_Crime <- OAtrajk_criminalDamage_OA #<- OAtrajk_crime
  for_Crime <- OAtrajk_assault_OA #<- OAtrajk_crime
  #for_Crime <- OAtrajk_TMV_OA #<- OAtrajk_crime


  result_List_Regular <- NULL

  for(countn in 1:22){ #countn <- 1

    j <-2 + countn #change the last number

    clusters <- as.vector(getClusters(for_Crime, c(j)))

    data.append <- cbind(data, clusters)  #head(data.append)

    rownames(data.append) <- 1:nrow(data.append)
    data.append <- cbind(1:nrow(data.append), data.append)
    colnames(data.append) <- c("code", "1", "2", "3", "4", "5", "6", "7", "8",
                               "9", "10", "11", "12", "13", "14", "15", "clusters")
    data.append <- as.data.frame(data.append)

    observed_Q_C <- Q_likelihood_Compare(data.append=data.append, agg_Data = agg_Data, clusters=clusters)

    just_Q_ <- as.data.frame(observed_Q_C[[1]])

    result_List_Regular[[countn]] <- list(just_Q_, observed_Q_C[[2]])
    #final_Result[[2]] <- list(just_Q_, observed_Q[[2]])
    flush.console()
    print(countn)

    flush.console()
    print(result_List_Regular[[countn]])

  }


  save(result_List_Regular, file="final_birmigham_OA_prop_rate_Burglary_RegularKMean.RData")

  save(result_List_Regular, file="final_birmigham_OA_prop_rate_CriminalDamage_RegularKMean.RData")

  save(result_List_Regular, file="final_birmigham_OA_prop_rate_Assault_RegularKMean.RData")

  save(result_List_Regular, file="final_birmigham_OA_prop_rate_TMV_RegularKMean.RData")

  result_List_Regular





  #---------------------------------------------------------------------
  #---------------------------------------------------------------------

  #Preparing data to compare regular kmeam & modified kmean
  #COLLECT DATA FROM "Rcode_for_post_process_kmean.R"


  #SELECT FOR THE NEW KMEAN # this


  load("final_birmigham_OA_prop_rate.RData")#final_birmigham_OA_prop_rate #final_Result[[1]] getwd()

  load("final_birmigham_OA_prop_rate_CriminalDamage.RData")

  load("final_birmigham_OA_prop_rate_Assault.RData")

  load("final_birmigham_OA_prop_rate_TMV.RData")


  #setwd("G:/20180613/Monsuru_WMD_12062018/")

  result_List <- NULL

  for(i in 1:length(final_Result)){ #i<-1 #34

    get_Cut_ <- final_Result[[i]][[1]]
    id_Cut_ <- which(is.na(get_Cut_[,3])) #to remove those <NA>

    if(length(id_Cut_)!=0){
      get_Cut_ <- get_Cut_[-id_Cut_,]
    }

    get_Cut_ <- get_Cut_[order(get_Cut_[,7]),]

    id_ <- which(get_Cut_[,7] <= 0.05)
    #final_Result[[i]][[1]][id_,]
    if(length(id_)>0){

      if(length(id_)==1){
        result_List <- rbind(result_List, cbind((i+2),
                                                as.character(get_Cut_[id_,1]),
                                                as.character(get_Cut_[id_,5]),
                                                as.character(get_Cut_[id_,2]),
                                                as.character(get_Cut_[id_,7])))
      }

      if(length(id_)>1){
        result_List <- rbind(result_List, cbind((i+2),
                                                apply(get_Cut_[id_,], 2, paste, collapse=",")[1],
                                                apply(get_Cut_[id_,], 2, paste, collapse=",")[5],
                                                apply(get_Cut_[id_,], 2, paste, collapse=",")[2],
                                                apply(get_Cut_[id_,], 2, paste, collapse=",")[7]))
      }


    }
  }

  #final_Result[[20]][[1]]


  rownames(result_List) <- 1:nrow(result_List)
  colnames(result_List) <- c("k", "Cluster_ID", "Q", "No.of.traj.", "Pseudo.pvalue")
  result_List

  #to calculate the total number of clusters for each value of k
  sub_ <- result_List[,4]
  total_T <- NULL
  for(j in 1:nrow(result_List)){

    lst <- strsplit(sub_[j],',')
    lst <- sum(as.numeric(lst[[1]]))
    total_T <- c(total_T, lst)
  }

  #recall that "total_T" is to sort
  #Now, sort the result
  ##result_List <- result_List[order(-as.numeric(as.character(total_T))),]


  #To change the cluster IDs to the actual number they represent

  list_split <- NULL

  for(k in 1:length(result_List[,2])){#k<-4

    id <- as.vector(strsplit(result_List[,2][k],','))[[1]]
    if(length(id)==1){
      #what number is it in the list "list_Letters"
      id_N <- which(list_Letters==as.vector(id))
      list_split <- rbind(list_split, id_N)
    }

    if(length(id)>1){
      keep_L <- which(list_Letters%in%as.vector(unlist(strsplit(id,','))))[[1]]
      #list_split <- keep_L
      for(m in 2:length(id)){#m<-3
        keep_L <- paste(keep_L,",",which(list_Letters%in%as.vector(unlist(strsplit(id,','))))[[m]], sep = "")
      }
      list_split <- rbind(list_split, keep_L)
    }

  }

  #now join back to the "result_List"
  #--------------------------------------------
  result_List <- cbind(result_List[,1], list_split, result_List[,3:5])
  result_List <- as.data.frame(result_List)
  colnames(result_List) <- c("k","cluster.Idx","Q_index","No.of.traj.","Pseudo.p")
  #--------------------------------------------

  #result for the new Kmean
  new_R <- burg_Kmean_New #<- result_List
  new_R <- crimiDamage_Kmean_New #<- result_List
  new_R <- Assault_Kmean_New #<- result_List
  new_R <- TMV_Kmean_New #<- result_List



  #k cluster.Idx     Q_index No.of.traj.    Pseudo.p
  #1   3           3       0.028         831       0.002
  #2   4           4       0.029         689       0.004
  #3   5           5       0.029         584       0.018
  #4   6           6       0.031         515       0.002




  #SELECT FOR THE OLD KMEAN
  #COPYING FROM "SECTION "REGULAR""  BELOW; and the workspace: "final_Workspace.RData"
  #OA Birmingham

  #GO BACK AND GET THE DATA FOR THE CRIME TYPE TO DEAL WITH BELOW

  #IMPORT THE REGULAR KMEAN RESULT

  load("final_birmigham_OA_prop_rate_Burglary_RegularKMean.RData")#final_birmigham_OA_prop_rate

  load("final_birmigham_OA_prop_rate_CriminalDamage_RegularKMean.RData")

  load("final_birmigham_OA_prop_rate_Assault_RegularKMean.RData")

  load("final_birmigham_OA_prop_rate_TMV_RegularKMean.RData")
  #load("final_birmigham_OA_prop_rate_Assault.RData")

  #load("final_birmigham_OA_prop_rate_TMV.RData")


  #setwd("G:/20180613/Monsuru_WMD_12062018/")#result_List_Regular

  result_List_qq <- NULL

  for(i in 1:length(result_List_Regular)){ #i<-1 #34

    get_Cut_ <- result_List_Regular[[i]][[1]]
    id_Cut_ <- which(is.na(get_Cut_[,3])) #to remove those <NA>

    if(length(id_Cut_)!=0){
      get_Cut_ <- get_Cut_[-id_Cut_,]
    }

    get_Cut_ <- get_Cut_[order(get_Cut_[,5]),]

    ###id_ <- which(as.vector(get_Cut_[,5]) <= 100)
    #final_Result[[i]][[1]][id_,]
    ###if(length(id_)>0){

    ### if(length(id_)==1){
    ###result_List_qq <- rbind(result_List_qq, cbind((i+2),
    ###                                     as.character(get_Cut_[id_,1]),
    ###                                  as.character(get_Cut_[id_,5]),
    ###                               as.character(get_Cut_[id_,2])))
    ###}

    ### if(length(id_)>1){
    result_List_qq <- rbind(result_List_qq, cbind((i+2),
                                                  apply(get_Cut_[nrow(get_Cut_),], 2, paste, collapse=",")[1],
                                                  apply(get_Cut_[nrow(get_Cut_),], 2, paste, collapse=",")[5],
                                                  apply(get_Cut_[nrow(get_Cut_),], 2, paste, collapse=",")[2]))
    ### }


    ###}
  }

  #final_Result[[20]][[1]]


  rownames(result_List_qq) <- 1:nrow(result_List_qq)
  colnames(result_List_qq) <- c("k", "Cluster_ID", "Q", "No.of.traj.")
  result_List_qq <- as.data.frame(result_List_qq)


  #------------------------------------------------------
  #result for the new Kmean
  old_R <- burg_Kmean_Old #<- result_List_qq
  old_R <- crimiDamage_Kmean_Old #<- result_List_qq
  old_R <- Assault_Kmean_Old #<- result_List_qq
  old_R <- TMV_Kmean_Old #<- result_List_qq


  #-----------------------------

  #TO COMPARE THE TWO RESULTS....library(base)

  #---------------------------------------------
  #result for the new Kmean
  new_R <- burg_Kmean_New #<- result_List
  new_R <- crimiDamage_Kmean_New #<- result_List
  new_R <- Assault_Kmean_New #<- result_List
  new_R <- TMV_Kmean_New #<- result_List
  #---------------------------------------------

  #---------------------------------------------
  #result for the new Kmean
  old_R <- burg_Kmean_Old #<- result_List_qq
  old_R <- crimiDamage_Kmean_Old #<- result_List_qq
  old_R <- Assault_Kmean_Old #<- result_List_qq

  <- TMV_Kmean_Old #<- result_List_qq
  #---------------------------------------------

  new_R_N <- as.vector(new_R$Q_index)#burg
  #new_R_N[length(new_R_N)] <- substr(new_R_N[length(new_R_N)],1,5) #burg
  old_R_N <- as.vector(old_R$Q)#

  #--------------------------------------
  #collate number of trajectories
  traj_N_new <- as.vector(new_R$No.of.traj.)#burg
  #traj_N_new[length(traj_N_new)] <- substr(traj_N_new[length(traj_N_new)],1,3)#burg
  traj_N_old <- as.vector(old_R$No.of.traj.)#burg  #traj_N_old+1

  comb_traj_ <- data.frame(c(as.numeric(traj_N_new), as.numeric(traj_N_old))) #comb_traj_ +1
  comb_traj_Per <- round((comb_traj_/3223)*100, digits=1)
  comb_traj_final <- cbind(comb_traj_, comb_traj_Per)
  #colnames(comb_traj_) <- c("new_K", "old_K")
  #--------------------------------------


  #combine them
  combi_R_N <- data.frame(cbind(new_R_N, old_R_N))

  #result_to_Plot_Comp <- data.frame(Burg, Crime_Damage)
  #Assault <- determine_K[2, 2:ncol(determine_K)]
  #TMV <- determine_K[4, 2:ncol(determine_K)]

  #result_to_Plot_Comp <- data.frame(Burg, Crime_Damage)

  combi_R_N <- cbind(combi_R_N, 1:nrow(combi_R_N))
  colnames(combi_R_N) <- c("New_Kmean","Old_Kmean","sn")  #head(result_to_Plot)
  rownames(combi_R_N) <- 1:nrow(combi_R_N)

  combi_R_N_melt <- melt(combi_R_N, id="sn")
  #combine the 'number of trajectory' to plot point size
  combi_R_N_melt_C <- cbind(combi_R_N_melt, comb_traj_final)
  colnames(combi_R_N_melt_C) <- c("K","Type","Inequity_Index","number.of.Traj", "perc_Neighb")


  #-------------------------------------------------------------------
  #-------------------------------------------------------------------
  library(ggrepel)

  par(oma=c(0,0,0,0), mar=c(0,0,0,0), mfrow=c(1,3))

  ##ggplot(data = combi_R_N_melt, aes(x=K, y=Inequity_Index, color=Type)) +
  ##geom_point(alpha=0.5, shape=21, size=2) +
  set.seed(42)
  ggplot(combi_R_N_melt_C, aes(K, Inequity_Index, group=Type)) +
    geom_point(size=combi_R_N_melt_C$number.of.Traj/100) +
    #geom_text(aes(label=paste(combi_R_N_melt_C$perc_Neighb, "%", sep=""), hjust=0, vjust=0)) +
    geom_text_repel(aes(label=paste(combi_R_N_melt_C$perc_Neighb, "%", sep="")), size = 4) +
    geom_line(aes(y = Inequity_Index, group=Type, linetype = Type)) +
    scale_x_discrete(limits = c(1,7,14,22), labels=c(3,10,16,24)) +
    xlab("Number of groups, K") +
    ylab("Inequity Index, Q") +
    theme(legend.position="bottom") +
    theme(axis.text = element_text(size = 50)) + # this will change all text sizes
    theme_minimal()





  outlier <- function(dat, perc=100){ #perc<-50

    outlier_List <- NULL

    for(i in 2:ncol(dat)){ #i<-2
      x <- matrix(as.numeric(as.character(dat[,i])),,1)
      colnames(x) <- c("InCount")
      hold_result <- matrix(0, nrow(x), 1)

      #x<-as.numeric(as.vector(x$InCount))
      ind_hold.na <- which(is.na(x))
      ind_hold.not.na <- which(!is.na(x))
      x_2 <- x[ind_hold.not.na]
      med <- median(x_2)
      MAD <- median(abs(med - x_2))
      dtf <<- data.frame(ID=seq.int(length(x_2)), obs=x_2, outlier=abs(x_2-med)>3.5 * (MAD/0.6745))
      dtf <- as.data.frame(cbind(dtf, ind_hold.not.na))
      colnames(dtf) <- c("id", "obs", "outlier", "ind")
      outlier_ind <- which(dtf$outlier=="TRUE")
      not_outlier_ind <- which(dtf$outlier!="TRUE")
      hold_result[dtf$ind[outlier_ind],1] <- 1 #for outlier
      hold_result[dtf$ind[not_outlier_ind],1] <- 0
      hold_result[ind_hold.na,1] <- 0
      rownames(hold_result) <- 1:nrow(hold_result)
      outlier_List <- cbind(outlier_List, hold_result)
    }

    outlier_Indices <- NULL

    for(j in 1:nrow(outlier_List)){ #j<-1

      how_many <- length(which(outlier_List[j,]==1))
      #if the number of data point in each area is > 50%, for example
      if((how_many/ncol(outlier_List)) >= (perc/100)){
        outlier_Indices <- c(outlier_Indices, j)
      }
    }

    return(outlier_Indices)

  }

  #--------------------------------------
  result_of_outler <- outlier(dat)
  #--------------------------------------


  dat[691,]

  #----------------------------------------------------------------
  #----------------------------------------------------------------#library(lubridate)
  #----------------------------------------------------------------

  #function to plot the 5-day predictions (and also their corresponding previous weeks predictions)
  auc_plot3 <- function(y, y_past=NULL){
    xy_1 <- y
    labs <- data.frame(xy_1Type=c(1, 2),
                       label = fontawesome(c('fa-arrow-circle-up','fa-arrow-circle-down'))  )
    d <- merge(xy_1, labs, by="xy_1Type")[order(merge(xy_1, labs, by="xy_1Type")[,2]),]
    d$Perc =  d$Perc * -1
    dateLabels = seq(Sys.Date()+1, (Sys.Date()+1 +(nrow(d)-1)), by = "day")
    dayLabels =   weekdays(as.Date(dateLabels))
    d <- cbind(d, dateLabels)
    #prepare the previous weeks data
    y_m <- melt(y_past)
    colfunc <- colorRampPalette(c("black", "grey"))
    colfunc <- colfunc(length(unique(y_m$Var2)))
    Ids <- NULL
    Ids_col <- NULL
    for(g in 1:length(unique(y_m$Var2))){   #g<-1
      Ids <- c(Ids, rep(g, length(unique(y_m$Var1))))
      Ids_col <- c(Ids_col, rep(colfunc[g], length(unique(y_m$Var1))))
    }
    y_m <- cbind(y_m, Ids, Ids_col)
    colnames(y_m) <- c("Var1","Var2","value","Var3", "Var4")
    y_m <- as.data.frame(y_m)
    print(ggplot(d, aes(Date, InCount)) + #ylim(-1,max(50)) +
            geom_point(aes(Date, InCount, color=factor(xy_1Type)), size = 1) +
            theme(legend.position=" ") +
            geom_ribbon(aes(ymin=0, ymax=InCount), alpha=0.1, fill="blue") +
            geom_line(color="blue", size = 1.5)+
            geom_point(aes(Date, InCount, color=factor(xy_1Type)), size = 11) +
            geom_text(aes(Date, InCount,label=label),family='fontawesome-webfont', size=c(9)) + #nudge_x=0, nudge_y=0
            scale_x_discrete(limits=d$Date,labels=dateLabels) +
            geom_point(data = y_m, aes(x = Var3, y = value, size = (abs(y_m$Var1-length(unique(y_m$Var1)))+1), group = Var3, color=Var4)) +
            annotate(geom = "text", x = y_m$Var3, y = y_m$value, label = y_m$Var1, size = 2) +
            annotate(geom = "text", x = d$Date, y = (min(d$InCount)-(min(d$InCount)/3)), label = dayLabels, size = 4) +
            coord_cartesian(ylim = c((min(d$InCount)-(min(d$InCount)/3)), (max(d$InCount)+(max(d$InCount)/8)))) +
            geom_text_repel(
              aes(Date, InCount, color=factor(xy_1Type), label=paste(Perc,"%", sep="")),
              size = 5,
              nudge_x = 0, nudge_y = 0.5,
              fontface = 'bold',
              box.padding=0.5, point.padding = 1.6, segment.size = 0)
    )
  }

  #----------------------------------------------------------------
  #----------------------------------------------------------------
  #----------------------------------------------------------------



  outlier <- function(dat, perc=100){ #perc<-50

    outlier_List <- NULL

    for(i in 2:ncol(dat)){ #i<-2
      x <- matrix(as.numeric(as.character(dat[,i])),,1)
      colnames(x) <- c("InCount")
      hold_result <- matrix(0, nrow(x), 1)

      #x<-as.numeric(as.vector(x$InCount))
      ind_hold.na <- which(is.na(x))
      ind_hold.not.na <- which(!is.na(x))
      x_2 <- x[ind_hold.not.na]
      med <- median(x_2)
      MAD <- median(abs(med - x_2))
      dtf <<- data.frame(ID=seq.int(length(x_2)), obs=x_2, outlier=abs(x_2-med)>3.5 * (MAD/0.6745))
      dtf <- as.data.frame(cbind(dtf, ind_hold.not.na))
      colnames(dtf) <- c("id", "obs", "outlier", "ind")
      outlier_ind <- which(dtf$outlier=="TRUE")
      not_outlier_ind <- which(dtf$outlier!="TRUE")
      hold_result[dtf$ind[outlier_ind],1] <- 1 #for outlier
      hold_result[dtf$ind[not_outlier_ind],1] <- 0
      hold_result[ind_hold.na,1] <- 0
      rownames(hold_result) <- 1:nrow(hold_result)
      outlier_List <- cbind(outlier_List, hold_result)
    }

    outlier_Indices <- NULL

    for(j in 1:nrow(outlier_List)){ #j<-1

      how_many <- length(which(outlier_List[j,]==1))
      #if the number of data point in each area is > 50%, for example
      if((how_many/ncol(outlier_List)) >= (perc/100)){
        outlier_Indices <- c(outlier_Indices, j)
      }
    }

    return(outlier_Indices)

  }




  #---------------------------------------------------


  #--------------------------------------------------------------------------
  #--------------------------------------------------------------------------
  Birmingham_OA_osgb

  data <- read.table("allCrime_agg.csv", sep=",", head=TRUE)
  data <- read.table("burglary_agg.csv", sep=",", head=TRUE)
  data <- read.table("assault_agg.csv", sep=",", head=TRUE)
  data <- read.table("criminalDamage_agg.csv", sep=",", head=TRUE)
  data <- read.table("TMV_agg.csv", sep=",", head=TRUE) #head(data)

  #uni_LSOA <- as.vector(unique(data$code))#head(data)

  #------------------------------------------------------
  #to use the LSOA boundary
  #read the nested data
  #extract the Birmingham OA lists#head(data)
  birm_codes <- as.vector(data$code)
  WM_nested_Details <- read.table("west-midlands-lookup-full-2011.csv", sep=",", head=TRUE)#head(nested_Details)
  WM_nested_Details_OAcode <- as.vector(WM_nested_Details$OAcode)
  Birm_nested_Details_OA <- WM_nested_Details[which(WM_nested_Details_OAcode %in% birm_codes),]#head(Birm_nested_Details_OA)#Birm_nested_Details_OA[1:16,]
  #now aggregated the data by LSOA using the nest code table
  uni_LSOA <- as.vector(unique(Birm_nested_Details_OA$LSOAcod))#head(data)

  agg_Data <- NULL

  for(r in 1:length(uni_LSOA)){ #r = 1
    ind_ <- as.vector(Birm_nested_Details_OA$OAcode[which(Birm_nested_Details_OA$LSOAcod == uni_LSOA[r])])
    sub_Data_ <- data[which(data$code %in%ind_),]
    sum_sub_Data_ <- colSums(sub_Data_[,2:ncol(sub_Data_)])
    agg_Data <- rbind(agg_Data, c(uni_LSOA[r], as.vector(sum_sub_Data_)))
  }#head(as.data.frame(agg_Data))
  agg_Data <- as.data.frame(agg_Data)
  data <- agg_Data
  #colnames(agg_Data) <- c("")#head(data) #head(agg_Data)
  #------------------------------------------------------


  data[,2:16] <- apply(data[,2:16], 2, as.numeric)#head(data)
  data <- cbind(1:nrow(data), data[,2:ncol(data)])
  colnames(data) <- c("ID","X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12","X13","X14","X15")
  #--------------------------------------------------------------------------
  #--------------------------------------------------------------------------


  #ADJUSTING Q_likelihood function for comparison

  #---------------------------------------------------------------------------------
  #function to calculate the observed Q
  result_List_Regular


  Q_likelihood_Compare <- function (data.append=data.append, clusters=clusters) { #

    #data <- data_simulated
    run=1

    data.append <- data.append[,1:16]

    result_ALL <- list()

    for(rn in 1:run){  #rn=1
      #now to plot a k solution
      #h=10
      #using the cluster centres from the above...
      ##part2 <- affectIndivC(data, cluster_center_List)

      #data.append <- cbind(data, part2)

      data.long.melted <- melt(data.append, id="code")#head(data., id="code"
      head(data.long.melted)

      ##data.append <- cbind(data, part2)
      ##data.long.melted <- melt(data, id="code")#head(d
      data.long.melted <- cbind(data.long.melted, rep(clusters, 15))#nrow(data.long.melted)#head(data.long.melted)

      colnames(data.long.melted) <- c("OAcode","Year","Crime_Count", "clusters")

      #convert numbers to letters
      ##clusters <- letters[data.long.melted$clusters]
      ##clusters <- toupper(clusters) #head(cluster_letters)

      ##clusters <- list_Letters[data.long.melted$clusters]
      ##data.long.melted$clusters <- clusters
      ##colnames(data.long.melted) <- c("OAcode","Year","Crime_Count", "clusters")

      #================================
      #THE LIKELIHOOD FUNCTION USING SUM OF INDIVIDUAL TRAJECTORY
      # Setup output table
      #calculate the cumulative rate of change (first difference) of the mean value ......
      clusters <- clusters[1:nrow(data.append)]

      data_reMelt <- dcast(data.long.melted, OAcode ~ Year, value.var='Crime_Count')
      data_reMelt <- cbind(data_reMelt, clusters) #head(data_reMelt)

      cluster_Group <- as.vector(unique(data_reMelt$clusters)[order(unique(data_reMelt$clusters))])
      sl_check_ALL <- NULL

      for(i in 1:length(cluster_Group)){ #i<-1
        # Deal with IDs
        data_reMelt_Cut <- data[which(data_reMelt$clusters==cluster_Group[i]),]

        if(length(data_reMelt_Cut)==15){data_reMelt_Cut<-matrix(data_reMelt_Cut,,15)}

        # time_reMelt_Cut <- time[which(data_reMelt$clusters==cluster_Group[i]),]
        count_Cluster <- nrow(data_reMelt_Cut)
        #coefficients(lm(data[i,]~time[i,]))
        sl_check <- NULL
        #HERE INSTEAD OF FINDING THE MEAN OF ENTIRE TRAJECTORIES, I WANT TO
        #DO IT FOR INDIVIDUAL TRAJECTORY
        for(s in 1:nrow(data_reMelt_Cut)){   #s<-1
          data_reMelt_Cut_Sub <- data_reMelt_Cut[s,]
          #for(q in 1:ncol(data_reMelt_Cut)){ #q<-1
          #sl_check<-c(sl_check, mean(data_reMelt_Cut[,q]))
          # }
          cum_rate_Change <- NULL
          #for(r in 2:length(sl_check)){#r<-2
          for(r in 2:length(data_reMelt_Cut_Sub)){#r<-2
            #cum_rate_Change <- c(cum_rate_Change, (sl_check[r] - sl_check[r-1]))
            cum_rate_Change <- c(cum_rate_Change, (data_reMelt_Cut_Sub[r] - data_reMelt_Cut_Sub[r-1]))
            #data_reMelt_Cut_Sub
          }
          sl_check <- c(sl_check, sum(cum_rate_Change))

        }
        sl_check <- sum(sl_check)
        #sl_check_ALL <- rbind(sl_check_ALL, cbind(cluster_Group[i], round(cum_rate_Change,digits = 3)))
        sl_check_ALL <- rbind(sl_check_ALL, cbind(cluster_Group[i], round(sl_check,digits = 3)))
      }

      #---------------------------
      head(sl_check_ALL)
      #---------------------------
      sl_check_ALL
      sl_check_ALL_Ordered <- sl_check_ALL[order(-as.numeric(as.character(sl_check_ALL[,2]))),]
      #---------------------------------------------------------------------

      #mmD ----------------------------------------------
      data_reMelt <- dcast(data.long.melted, OAcode ~ Year, value.var='Crime_Count')
      data_reMelt <- cbind(data_reMelt, clusters) #head(data_reMelt)

      cluster_Group <- as.vector(unique(data_reMelt$clusters)[order(unique(data_reMelt$clusters))])
      #---------------------------------------------------------------------------
      #plot each cluster....
      #---------------------------------------------------------------------------
      clusters <- clusters[1:nrow(data)]

      data_reMelt <- dcast(data.long.melted, OAcode ~ Year, value.var='Crime_Count')
      data_reMelt <- cbind(data_reMelt, clusters) #head(data_reMelt)

      cluster_Group <- as.vector(unique(data_reMelt$clusters)[order(unique(data_reMelt$clusters))])

      length_Of_Clusters <- NULL

      cluster_Units_ALL <- list()

      for(i in 1:length(cluster_Group)){ #i<-1

        data_reMelt_Cut <- data_backup[which(data_reMelt$clusters==cluster_Group[i]),]#head(data_reMelt)

        if(is.null(data_reMelt_Cut)){data_reMelt_Cut<-matrix(data_reMelt_Cut,,16)
        colnames(data_reMelt_Cut) <- c("code", "1", "2", "3", "4", "5", "6", "7", "8",
                                       "9", "10", "11", "12", "13", "14", "15")
        }
        #get the corresponding OA label #head(data)# head(agg_Data[1:10,])
        cluster_Units <- as.vector(agg_Data$V1[as.data.frame(data_reMelt_Cut)$ID])

        length_Of_Clusters <- rbind(length_Of_Clusters, cbind(i, length(cluster_Units)))

        cluster_Units_ALL[i] <- list(cluster_Units)
      }

      #ranked from the most inequitous area to the least
      rank_Areas <- sl_check_ALL[order(-as.numeric(as.character(sl_check_ALL[,2]))),]
      colnames(rank_Areas) <- c("id","Q")
      length_Of_Clusters <- cbind(list_Letters[length_Of_Clusters[,1]], length_Of_Clusters[,2]) #letter
      colnames(length_Of_Clusters) <- c("id","count")

      result_Merge <- merge(length_Of_Clusters, rank_Areas, by="id")

      result_Merge_New <- cbind(length_Of_Clusters, rank_Areas[match(length_Of_Clusters[,1], rank_Areas[,1]),])

      #-----------------------------------------
      result_Merge_New[order(-as.numeric(result_Merge_New[,4])),]
      #-----------------------------------------

      result_A <- result_Merge_New[order(-as.numeric(result_Merge_New[,4])),]

      #MC_List <-
      result_ALL[[run]] <- result_A

      #flush.console()
      #print(run)
    }

    #getting the final Q value, based on the final derivation...  going back the division thing....
    for(vb in 1:length(result_ALL)){ #vb<-1
      Q_ <- round(as.numeric(result_ALL[[vb]][,4])/as.numeric(result_ALL[[vb]][,2]), digits=3)
      sub_Table_ <- cbind(result_ALL[[vb]],Q_)
      result_ALL[[vb]] <- sub_Table_
    }

    result_ALL[[2]] <- cluster_Units_ALL

    return(result_ALL)

  }
  #end of function



  library(Hmisc) # cut2
  split(das, cut2(das$wt, g=3))
  keep_sl_ListGroup <- split(sl_List, cut2(sl_List$slope, g=n_Clusters))
  #
  #-------------------------------------------------------------------------------------
  #-------------------------------------------------------------------------------------
  #-------------------------------------------------------------------------------------
  #UNIVARIATE KMEAN ------- (INEQUALITY INDEX)

  par(oma=c(0,0,0,0), mar=c(0,0,0,0), mfrow=c(1,1))

  install.packages("Ckmeans.1d.dp")
  require(Ckmeans.1d.dp)

  #install.packages("Ckmeans")
  require(Ckmeans.1d.dp)
  require(stats)


  #USING OA
  #--------------------------------------------------------------------------
  #--------------------------------------------------------------------------
  #--------------------------------------------------------------------------
  #--------------------------------------------------------------------------
  #--------------------------------------------------------------------------
  #--------------------------------------------------------------------------
  #--------------------------------------------------------------------------
  #--------------------------------------------------------------------------
  #--------------------------------------------------------------------------
  #--------------------------------------------------------------------------

  determine_K <- NULL
  num_OAs <- NULL

  ## for(uu in 1:4){ #uu<-1

  if(uu==1){
    data <- read.table("burglary_agg.csv", sep=",", head=TRUE)
  }
  if(uu==2){
    data <- read.table("assault_agg.csv", sep=",", head=TRUE)
  }
  if(uu==3){
    data <- read.table("criminalDamage_agg.csv", sep=",", head=TRUE)
  }
  if(uu==4){
    data <- read.table("TMV_agg.csv", sep=",", head=TRUE) #head(data)head(data) data[which(data$code=="E00047775"),]
  }
  #data[1:10,]

  #-----------------------------------------------------------------------------------------------------------------------------------------
  #USING OA
  #read the population data
  pop_02_16 <- read.table(file="population_WM_OA_02_16.csv", sep=",", head=TRUE) #this was generated below
  #head(pop_02_16)  #pop_02_16[which(pop_02_16$OAcode=="E00175716"),]
  #OA list
  OA_unique <- readOGR(dsn=".", "Birmingham_OA_osgb")  #head
  OA_unique <- as.vector(OA_unique@data$OAcode)
  head(OA_unique)#[1] "E00047772" "E00047775" "E00047768" "E00047681" "E00047703" "E00047687"
  #------------------------------------------------------
  #now aggregated the data by LSOA using the nest code table
  uni_OA <- as.vector(data$code) #head(data)  #uni_OA[1:4]#[1] "E00045077" "E00045078" "E00045079" "E00045080"#OA_unique[1:4][1] "E00047772" "E00047775" "E00047768" "E00047681"

  #----------------------------------------------------------------------
  data <- apply(data[,2:16], 2, as.numeric)#head(data)
  data <- cbind(1:nrow(data), data)
  colnames(data) <- c("ID","X1","X2","X3","X4","X5","X6","X7","X8","X9","X10", "X11","X12","X13","X14","X15")
  #----------------------------------------------------------

  data_Fresh <- NULL
  #now normalise with population
  for(k in 1:length(OA_unique)){#k<-3133
    pop_cut <- as.numeric(pop_02_16[which(as.vector(pop_02_16$OAcode)==OA_unique[k]),2:16])
    data_cut <- as.numeric(data[which(uni_OA==OA_unique[k]),2:16])
    data_Pop_100 <- (data_cut / pop_cut)*100
    data_Fresh <- rbind(data_Fresh, round(data_Pop_100,digits=5))
    #data[k,2] <- data_Pop_100[1]
  }

  agg_Data <- as.data.frame(cbind(V1=OA_unique, data_Fresh))

  #now transfer back to data
  data <- agg_Data

  data <- apply(data[,2:16], 2, as.numeric)#head(data)
  data <- cbind(1:nrow(data), data)
  colnames(data) <- c("ID","X1","X2","X3","X4","X5","X6","X7","X8","X9","X10", "X11","X12","X13","X14","X15")
  #-----------------------------------------------------------------------------------------------------------------------------------------
  #head(data)

  #Now, conve3rting to proportion
  for(h in 2:ncol(data)){ #h<-2
    prop <- (data[,h]/sum(as.numeric(as.character(data[,h]))))*100
    data[,h] <- prop
    #data_Sub <- cbind(data_Sub, prop)
  }
  head(data)
  #--------------------------------------------------------------------------

  #-------------------------------------------------
  #NOW CALCULATING THE INEQUITY INDEX FOR EACH OA
  #x(t) - x(t-k)#https://campus.datacamp.com/courses/manipulating-time-series-data-in-r-with-xts-zoo/merging-and-modifying-time-series?ex=10#skiponboarding

  #calculate the first difference
  data_New <- NULL

  for(t in 1:nrow(data)){#t<-1

    row_firstDiff <- NULL
    for(q in 3:ncol(data)){ #q<-5
      d_ <- data[t,q]-data[t,q-1]
      row_firstDiff <- cbind(row_firstDiff, d_)
    }
    data_New <- rbind(data_New, cbind(t, row_firstDiff))
  }
  head(data_New)

  #inequity index of a trajectory
  sum_Traj <- as.vector(rowSums(data_New[,-1]))

  #removing the outliers
  #burglary
  r_ <- which(sum_Traj > 0.5)  #sum_Traj[1605] #max(sum_Traj)
  sum_Traj_ <- sum_Traj[-r_]
  hist(sum_Traj_)


  #also remove the outliers from the OAs
  OA_unique_ <- OA_unique[-r_]

  #also remove the outlier traj from the data
  data_ <- data[-r_,]


  #ids after ordering --> useful for ordering everything else, including the OA code.
  id_ordered <- order(sum_Traj_)

  #order the data   #idddd <- c(3086, 3131, 2563)  #data_[idddd, ]  #sum_Traj_[idddd]
  data_2 <- data_[id_ordered,]

  ##order the inequity indices
  sum_Traj_2 <- as.vector(unlist(sum_Traj_[id_ordered]))

  #order the OAs
  OA_unique_2 <- OA_unique_[id_ordered]
  ## k=8

  #demonstrating that Kmedian is better than kmean.
  #x_ <- c(-2,  -2, -1, 0, 2, 3, 3, 4, 5, 5,  9, 13, 14, 15, 16,17,18)
  #order(x...)........................
  #x_ <- data.frame(id=sum_Traj)  #head(x_)

  #order the inequity index
  #ordered_x_ <- data.frame(id=x_[id_ordered,])  #head(ordered_x_)

  ####
  #spliting the vector into k evenly divided groups
  x_2_groups <- split(ordered_x_, cut2(sum_Traj_2, g=8))

  ####
  #------------------------------------------------checking the number of items in each group
  for(b in 1:length(x_2_groups)){
    print(nrow(x_2_groups[[b]]))
  }
  #nrow(x_2_groups[[2]])
  #------------------------------------------------

  ####
  med_list <- NULL

  c_ <- 0
  for(f in 1:length(x_2_groups)){#f<-1
    v_ <- c_ + round(nrow(x_2_groups[[f]])/2, digits=0)
    med_list <- c(med_list, v_)
    c_ <- c_ + nrow(x_2_groups[[f]])
  }

  ##ordered_x_[1,1]
  ##ordered_x_[nrow(ordered_x_)-8,1]
  # length(ordered_x_),


  #hist(ordered_x_, breaks=200)

  ####
  #now pick the 'center' values for the initialisation, based in the indices.
  x_2_centres <- as.vector(ordered_x_)[med_list,]
  #insert in a clustering algo.

  ##results_ <- kmeans(sum_Traj, k)
  ##results_ <- kmeans(sum_Traj, centers = x_2_centres)
  ##results_ <- Ckmeans.1d.dp(sum_Traj, 8)

  ##https://stats.stackexchange.com/questions/109547/k-means-vs-k-median
  #https://www.coursera.org/lecture/cluster-analysis/3-5-the-k-medians-and-k-modes-clustering-methods-pShI2   #less sensitive to outier than mean...
  ##prepare the algorith.
  #absolute deviation....

  ### results_ <- Ckmedian.1d.dp(sum_Traj, 16)

  results_ <- Ckmedian.1d.dp(sum_Traj_2, estimate.k="BIC", k=c(1,30))
  ###results_ <- Ckmedian.1d.dp(sum_Traj_2, estimate.k="BIC", k=c(1,30))
  ###results_ <- kmeans(x=sum_Traj_2, centers =x_2_centres, algorithm="Hartigan-Wong")


  plot(results_)

  results_$cluster

  for(kk in 1:max(results_$cluster)){
    print(length(which(results_$cluster==kk)))
  }

  k <- unique(results_$cluster)

  plot(sum_Traj_2, col=results_$cluster, pch=results_$cluster, cex=1.5,
       main="Optimal univariate clustering given k",
       #sub=paste("Number of clusters given:", k)
  )

  abline(h=results_$centers, col=1:k, lty="dashed", lwd=2)
  #legend("bottomright", paste("Cluster", 1:k), col=1:k, pch=1:k, cex=1.5, bty="n")  #hist(sum_Traj_)

  #-------------------------------------------------------------------------------------------------------------

  #GIVEN A LIST OF K CENTERS, USE THE BELOW TO PLOT THE CLUSTER SOLUTION FOR A GIVEN VALUE OF K
  #using the cluster centres from the above...
  ###part2 <- affectIndivC(data, cluster_center_List_ALL[[14]])  #length(which(part2=="3"))

  part2 <- results_$cluster
  #
  combined_data.append_Result <- cbind(OA_unique_2, part2)  #head(combined_data.append_Result) #combined_data.append_Result[1:12,]
  #rownames(data.append) <- 1:nrow(data.append)

  #re-order the data based on the cluster solution# id_ordered[1] #data[3086,]
  #data_ <- data[id_ordered,]  #data_[1,]


  data.append_ <- cbind(1:nrow(data_2), data_2[,-1])  #head(data_2)

  #data.append_ <- data.append_[,-1]
  #head(data.append_)

  colnames(data.append_) <- c("code", "1", "2", "3", "4", "5", "6", "7", "8",
                              "9", "10", "11", "12", "13", "14", "15")
  data.append_ <- as.data.frame(data.append_)

  data.long.melted <- melt(data.append_, id="code")#head(data., id="code"
  head(data.long.melted)

  #data.append <- cbind(data, part2)
  #data.long.melted <- melt(data, id="code")#head(d
  data.long.melted <- cbind(data.long.melted, rep(part2, 15))#head(data.long.melted)

  colnames(data.long.melted) <- c("OAcode","Year","Crime_Count", "clusters")

  #convert numbers to letters
  ##clusters <- letters[data.long.melted$clusters]
  ##clusters <- toupper(clusters) #head(cluster_letters)

  clusters <- list_Letters[data.long.melted$clusters]
  data.long.melted$clusters <- clusters
  colnames(data.long.melted) <- c("OAcode","Year","Crime_Count", "clusters")



  #data.long.melted2 <- data.long.melted[which(data.long.melted$clusters!="M"),]

  ggplot(data.long.melted, aes(x=Year, y=Crime_Count,
                               group=OAcode, color=clusters)) +
    geom_line() +
    stat_summary(fun.y=mean, geom="line", aes(group=clusters), color="black", size=2) +
    facet_wrap(~clusters) +
    #scale_colour_brewer(palette = "Set1") +
    theme_minimal()
  #-------------------------------------------------------------------------
  #-------------------------------------------------------------------------
  #-------------------------------------------------------------------------head(data.long.melted) 48330/13

  #print the number of trajectories in each group
  for(kk in 1:max(part2)){
    print(length(which(part2==kk)))
  }





  #SECTION: QJC
  #to calculate mean (average) value of trajectories in each group

  head(data.long.melted)##data.long.melted[1:50,]

  #use this to calculate percentage (%) change from year 1 to year n
  year_uni <- as.vector(unique(data.long.melted$Year))
  order_Cluster <- as.vector(unique(data.long.melted$clusters))
  clusters_uni <- order_Cluster[order(as.vector(unique(data.long.melted$clusters)))]
  change_ave_yr <- NULL

  for(p in 1:length(clusters_uni)){#p<-1
    all_clust_list <- data.long.melted[which(data.long.melted$clusters==clusters_uni[p]),]
    ave_yr <- NULL
    for(m in 1:length(year_uni)){ #m<-1
      yr_ <- all_clust_list[which(as.vector(all_clust_list$Year)==year_uni[m]),]#all first year data..
      ave_yr <- c(ave_yr, mean(yr_$Crime_Count))
    }
    change_ave_yr <- c(change_ave_yr, round(((ave_yr[length(ave_yr)]-ave_yr[1])/ave_yr[1])*100, digits=0))
  }

  change_ave_yr


  #------
  #now area plot for crime count and proportin....

  #to calculate mean (average) value of trajectories in each group

  head(data.long.melted)##data.long.melted[1:50,]

  #use this to calculate percentage (%) change from year 1 to year n
  year_uni <- as.vector(unique(data.long.melted$Year))
  order_Cluster <- as.vector(unique(data.long.melted$clusters))
  clusters_uni <- order_Cluster[order(as.vector(unique(data.long.melted$clusters)))]

  change_ave_yr_ALL <- NULL

  for(p in 1:length(clusters_uni)){#p<-1
    all_clust_list <- data.long.melted[which(data.long.melted$clusters==clusters_uni[p]),]
    ave_yr <- NULL
    for(m in 1:length(year_uni)){ #m<-1
      yr_ <- all_clust_list[which(as.vector(all_clust_list$Year)==year_uni[m]),]#all first year data..
      ave_yr <- c(ave_yr, mean(yr_$Crime_Count))
    }
    change_ave_yr_ALL <- rbind(change_ave_yr_ALL,  ave_yr)
  }

  change_ave_yr_ALL

  change_ave_yr_ALL <- t(change_ave_yr_ALL)


  require(ggplot2)
  require(reshape)

  #create data
  set.seed(3)

  #time steps
  t.step<-seq(0,20)


  #group names
  grps<-letters[1:10]

  #random data for group values across time
  grp.dat<-runif(length(t.step)*length(grps),5,15)

  #create data frame for use with plot
  grp.dat<-matrix(grp.dat,nrow=length(t.step),ncol=length(grps))
  grp.dat<-data.frame(grp.dat,row.names=t.step)
  names(grp.dat)<-grps

  #reshape the data
  p.dat<-data.frame(step=row.names(grp.dat),grp.dat,stringsAsFactors=F)
  p.dat<-melt(p.dat,id='step')
  p.dat$step<-as.numeric(p.dat$step)

  #create plots
  p<-ggplot(p.dat,aes(x=step,y=value)) + theme(legend.position="none")
  p + geom_area(aes(fill=variable))
  p + geom_area(aes(fill=variable),position='fill')







  ## for(kk in 1:max(part2)){
  ##print(length(which(part2==kk)))
  ## }




  require(Ckmeans.1d.dp)
  x <- c(rnorm(50, mean=-1, sd=0.3), rnorm(50, mean=1, sd=1), rnorm(50, mean=5, sd=0.4), rnorm(50, mean=8, sd=1))
  # Divide x into k clusters, k automatically selected (default: 1~9)
  result <- Ckmeans.1d.dp(x)
  plot(result)


  result <- Ckmedian.1d.dp(x)
  plot(result)


























































  require(Ckmeans.1d.dp)


  #1.168440e-03  5.234586e-02 -1.453994e-02  1.968816e-02

  #USING OA
  #------------------------------------------------
  #------------------------------------------------
  #------------------------------------------------
  #------------------------------------------------
  #burglary -------------- ONLY OA
  if(uu==1){
    out_List <- NULL
    for(w in 1:nrow(data)){#w<-1
      sn_ <- length(which(data[w,2:ncol(data)]> 0.8))  #1605
      if(sn_>0){
        out_List <- c(out_List,w)
      }
    }

    #out_List<-1605
    #replace each of these with subsequent records#data[1605:1606,]
    for(w in 1:length(out_List)){#w<-1
      data[out_List[w],2:ncol(data)] <- data[(out_List[w]+1),2:ncol(data)]
    }
  }
  #-------#head(data)


  #assault----------------
  if(uu==2){
    out_List <- NULL
    for(w in 1:nrow(data)){#w<-1  data[3137,]
      sn_ <- length(which(as.numeric(data[w,2:ncol(data)])  > 1)) #change "1" for OA to "3" FOR THE LSOA
      if(sn_>0){
        out_List <- c(out_List,w)
      }
    }
    #out_List <- c(732, 1122, 1605, 3053, 3069, 3072, 3076, 3083, 3101, 3132, 3136, 3137, 3138, 3143, 3148, 3154)
    #replace each of these with subsequent records#data[1605:1606,]
    for(w in 1:length(out_List)){#w<-1
      data[out_List[w],2:ncol(data)] <- data[1,2:ncol(data)]
    }
  }
  #-------

  #criminal Damage
  if(uu==3){
    out_List <- NULL
    for(w in 1:nrow(data)){#w<-1  data[3137,]
      sn_ <- length(which(as.numeric(data[w,2:ncol(data)])  > 1))
      if(sn_>0){
        out_List <- c(out_List,w)
      }
    }
    #replace each of these with subsequent records#data[1605:1606,]
    for(w in 1:length(out_List)){#w<-1
      data[out_List[w],2:ncol(data)] <- data[1,2:ncol(data)]
    }
  }
  #-------


  #TMV
  if(uu==4){
    out_List <- NULL
    for(w in 1:nrow(data)){#w<-1  data[3137,]
      sn_ <- length(which(as.numeric(data[w,2:ncol(data)])  > 1))
      if(sn_>0){
        out_List <- c(out_List,w)
      }
    }
    #replace each of these with subsequent records#data[1605:1606,]
    for(w in 1:length(out_List)){#w<-1
      data[out_List[w],2:ncol(data)] <- data[1,2:ncol(data)]
    }
  }
  #-------
  #data[c(3069, 3076, 3136, 3148),]
  #data[which(data[,12]>5),]  #data[1605,]
  #----------------------------

  #skip if you like
  #-------------------------------------------------------------------------
  #GIVEN A LIST OF K CENTERS, USE THE BELOW TO PLOT THE CLUSTER SOLUTION FOR A GIVEN VALUE OF K
  #using the cluster centres from the above...
  part2 <- affectIndivC(data, cluster_center_List_ALL[[12]])  #length(which(part2=="3"))

  part2

  #
  combined_data.append_Result <- cbind(OA_unique, part2)  #head(combined_data.append_Result)
  #rownames(data.append) <- 1:nrow(data.append)
  data.append <- cbind(1:nrow(data), data)

  colnames(data.append) <- c("code", "1", "2", "3", "4", "5", "6", "7", "8",
                             "9", "10", "11", "12", "13", "14", "15")
  data.append <- as.data.frame(data.append)

  data.long.melted <- melt(data.append, id="code")#head(data., id="code"
  head(data.long.melted)

  #data.append <- cbind(data, part2)
  #data.long.melted <- melt(data, id="code")#head(d
  data.long.melted <- cbind(data.long.melted, rep(part2, 15))#nrow(data.long.melted)

  colnames(data.long.melted) <- c("OAcode","Year","Crime_Count", "clusters")

  #convert numbers to letters
  ##clusters <- letters[data.long.melted$clusters]
  ##clusters <- toupper(clusters) #head(cluster_letters)

  clusters <- list_Letters[data.long.melted$clusters]
  data.long.melted$clusters <- clusters
  colnames(data.long.melted) <- c("OAcode","Year","Crime_Count", "clusters")


  ggplot(data.long.melted, aes(x=Year, y=Crime_Count,
                               group=OAcode, color=clusters)) +
    geom_line() +
    stat_summary(fun.y=mean, geom="line", aes(group=clusters), color="black", size=2) +
    facet_wrap(~clusters) +
    #scale_colour_brewer(palette = "Set1") +
    theme_minimal()
  #-------------------------------------------------------------------------
  #-------------------------------------------------------------------------
  #-------------------------------------------------------------------------
  #SECTION: QJC
  #to calculate mean (average) value of trajectories in each group

  head(data.long.melted)##data.long.melted[1:50,]

  #use this to calculate percentage (%) change from year 1 to year n
  year_uni <- as.vector(unique(data.long.melted$Year))
  order_Cluster <- as.vector(unique(data.long.melted$clusters))
  clusters_uni <- order_Cluster[order(as.vector(unique(data.long.melted$clusters)))]
  change_ave_yr <- NULL

  for(p in 1:length(clusters_uni)){#p<-1
    all_clust_list <- data.long.melted[which(data.long.melted$clusters==clusters_uni[p]),]
    ave_yr <- NULL
    for(m in 1:length(year_uni)){ #m<-1
      yr_ <- all_clust_list[which(as.vector(all_clust_list$Year)==year_uni[m]),]#all first year data..
      ave_yr <- c(ave_yr, mean(yr_$Crime_Count))
    }
    change_ave_yr <- c(change_ave_yr, round(((ave_yr[length(ave_yr)]-ave_yr[1])/ave_yr[1])*100, digits=0))
  }

  change_ave_yr


  #------
  #now area plot for crime count and proportin....

  #to calculate mean (average) value of trajectories in each group

  head(data.long.melted)##data.long.melted[1:50,]

  #use this to calculate percentage (%) change from year 1 to year n
  year_uni <- as.vector(unique(data.long.melted$Year))
  order_Cluster <- as.vector(unique(data.long.melted$clusters))
  clusters_uni <- order_Cluster[order(as.vector(unique(data.long.melted$clusters)))]

  change_ave_yr_ALL <- NULL

  for(p in 1:length(clusters_uni)){#p<-1
    all_clust_list <- data.long.melted[which(data.long.melted$clusters==clusters_uni[p]),]
    ave_yr <- NULL
    for(m in 1:length(year_uni)){ #m<-1
      yr_ <- all_clust_list[which(as.vector(all_clust_list$Year)==year_uni[m]),]#all first year data..
      ave_yr <- c(ave_yr, mean(yr_$Crime_Count))
    }
    change_ave_yr_ALL <- rbind(change_ave_yr_ALL,  ave_yr)
  }

  change_ave_yr_ALL

  change_ave_yr_ALL <- t(change_ave_yr_ALL)

  #to calculate the %rise or %drop
  #for burglary crime
  #(change_ave_yr_ALL[1,1] / sum(change_ave_yr_ALL[,1]))*100
  #(change_ave_yr_ALL[16,1] / sum(change_ave_yr_ALL[,1]))*100
  #(change_ave_yr_ALL[1,15] / sum(change_ave_yr_ALL[,15]))*100
  #(change_ave_yr_ALL[16,15] / sum(change_ave_yr_ALL[,15]))*100

  #for violence crime
  #(change_ave_yr_ALL[1,1] / sum(change_ave_yr_ALL[,1]))*100
  #(change_ave_yr_ALL[21,1] / sum(change_ave_yr_ALL[,1]))*100
  #(change_ave_yr_ALL[1,15] / sum(change_ave_yr_ALL[,15]))*100
  #(change_ave_yr_ALL[21,15] / sum(change_ave_yr_ALL[,15]))*100

  #create data frame for use with plot
  #grp.dat<-matrix(grp.dat,nrow=length(t.step),ncol=length(grps))
  grp.dat<-data.frame(change_ave_yr_ALL,row.names=1:15)
  names(grp.dat)<-clusters_uni


  #reshape the data
  p.dat<-data.frame(step=row.names(grp.dat),grp.dat,stringsAsFactors=F) #p.dat

  p.dat<-melt(p.dat,id='step')

  p.dat$step<-as.numeric(p.dat$step) #head(p.dat)

  #create plots

  class(p.dat$step)

  #p.dat$step <- as.factor(as.character(p.dat$step))

  p<-ggplot(p.dat,aes(x=step,y=value)) + theme(legend.position="none")

  p + geom_area(aes(fill=variable), colour = "gray16") + scale_fill_manual(values = colours) +
    theme

  colours <- c("gray100","gray94","gray88","gray82","gray76","gray70","gray64","gray58","gray52",
               "gray46","gray40","gray34","gray28","gray22","gray16","gray10")

  colours <- c("gray100","gray95","gray90","gray85","gray80","gray75","gray70","gray65","gray60",
               "gray55","gray50","gray45","gray40","gray35","gray30","gray25", "gray20", "gray15", "gray10", "gray3", "gray1")



  p + geom_area(aes(fill=variable), colour = "gray16", position='fill') + scale_fill_manual(values = colours) +
    scale_x_continuous(breaks=1:15, labels=Year)



  #transfer the results_Offender
  Year <- c("2001", "2002", "2003", "2004", "2005", "2006", "2007",
            "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015")



  #---------------------------------------------------------------------------------------
  #---------------------------------------------------------------------------------------
  #PLOTTING THE TRAJECTORIES OF EACH GROUP.


  #PLOT STYLE 2  - PROPERTY
  #alternative plot
  par(mar=c(7,7,4,2)+0.2, mgp=c(5,1,1))
  #dev.new()

  plot(c(0,15), c(0,0.09), xlab="Area coverage(%)", ylab="% crime captured", main="...",
       cex=0.001, col="white", cex.lab=1.5, cex.axis=1.5, cex.main=1.5, las=1, xaxt ='n', axes=FALSE)

  axis(1, at=1:15 , labels=Year, cex.axis=0.5)
  axis(2, at=c(seq(0.01, 0.1, 0.01)) , labels=c(seq(0.01, 0.1, 0.01)), cex.axis=0.5)
  abline(h=0, col="black")
  abline(v=0, col="black")
  #----

  #----------------------------------------------
  pt_ <- c(16, 17, 15, 13, 12, 8, 5, 2, 4, 6)
  ln_ <- c(1, 2, 3, 4, 5, 6, 3, 3, 3)
  gr_2 <- as.matrix(gr_1[,-1])
  for(aaa in 1:nrow(gr_2)){#111111 aaa<-1
    v1 <- gr_2[aaa,1:ncol(gr_2)]
    lines(v1, col="black", cex=2, lty=ln_[aaa], lwd=4)
    points(v1, col="black", pch=pt_[aaa], cex=2)
  }#111111
  #-------------------------------------------------------------------------------
  #-------------------------------------------------------------------------------
  #PLOT STYLE 2  - VIOLENT
  #alternative plot
  par(mar=c(7,7,4,2)+0.2, mgp=c(5,1,1))
  #dev.new()  #c(0,0.18) #c(0,0.18)
  #for the first group
  plot(c(0,15), c(0,0.1), xlab="Area coverage(%)", ylab="% crime captured", main="...",
       cex=0.001, col="white", cex.lab=1.5, cex.axis=1.5, cex.main=1.5, las=1, xaxt ='n', axes=FALSE)

  #for the rest
  plot(c(0,15), c(0,0.08), xlab="Area coverage(%)", ylab="% crime captured", main="...",
       cex=0.001, col="white", cex.lab=1.5, cex.axis=1.5, cex.main=1.5, las=1, xaxt ='n', axes=FALSE)

  axis(1, at=1:15 , labels=Year, cex.axis=0.5)
  axis(2, at=c(seq(0.01, 0.1, 0.01)) , labels=c(seq(0.01, 0.1, 0.01)), cex.axis=0.5)
  abline(h=0, col="black")
  abline(v=0, col="black")
  #----

  #----------------------------------------------
  pt_ <- c(16, 17, 15, 13, 12, 8, 5, 2, 4, 6)
  ln_ <- c(1, 2, 3, 4, 5, 6, 3, 3, 3)
  gr_2 <- as.matrix(gr_1[,-1])
  for(aaa in 1:nrow(gr_2)){#111111 aaa<-1
    v1 <- gr_2[aaa,1:ncol(gr_2)]
    lines(v1, col="black", cex=2, lty=ln_[aaa], lwd=4)
    points(v1, col="black", pch=pt_[aaa], cex=2)
  }#111111
  Year <- c("2001", "2002", "2003", "2004", "2005", "2006", "2007",
            "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015")
  #--------------------------------------------------------------------------------


  library(sf)
  ## Linking to GEOS 3.5.0, GDAL 2.2.2, PROJ 4.8.0
  nc <- st_read(system.file("shape/nc_Project.shp", package="sf"))

  nc <- st_read(system.file("shape/birming_2001_2012_LSOA_pop.shp", package="sf"))

  data <- nc

  trace(utils:::unpackPkgZip, edit=TRUE)

  install.packages("recmap")
  library(recmap)
  install.packages("foreach")
  library(foreach)
  library(rgdal)
  #setwd() #getwd()

  X <- readOGR(dsn=".", "gl_oa_2011_recmap1")  #head #plot(X)
  X_r <- as.recmap(X)

  plot(recmap(X_r))
  # use the GA
  #set.seed(1)
  #plot(recmapGA(X_r))

  X_r_ <- as.SpatialPolygonsDataFrame(X_r)

  plot(X_r)



  SpDf <- as.SpatialPolygonsDataFrame(recmap(checkerboard(8)))
  summary(SpDf)
  spplot(SpDf)
  summary(as.recmap(SpDf))

  library(maptools)
  #https://github.com/sjewo/cartogram

  data(wrld_simpl) #plot(wrld_simpl[wrld_simpl$REGION==2, ])  #mode(wrld_simpl)#head(wrld_simpl@data)
  afr <- as.recmap(wrld_simpl[wrld_simpl$REGION==2, ])#plot(wrld_simpl[wrld_simpl$REGION==2, ])
  is.recmap(afr)
  afr$z <- afr$POP2005
  is.recmap(afr)
  afr <- afr[afr$z > 0, ]
  # make map overlap to generate a connected pseudo dual
  afr$dx <- 2.0 * afr$dx
  afr$dy <- 2.0 * afr$dy
  # overview
  plot(recmap(afr))
  # use the GA
  set.seed(1)
  plot(recmapGA(afr))
}

