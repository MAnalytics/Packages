#libraries #--------------------------------------------------
#install akmedoids:
rm(list = ls())
' "Build" > "Install and Rebuild" '

#library(rgdal)
#install.packages("raster")
#library(raster)
library(foreign)
library(reshape2)
library(ggplot2)

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

dir1 <- "C:/Users/monsu/Documents/GitHub/Packages/inter_comparison_Micro/data1/"

data <- read.table(paste(dir1, "wm_bm_OA_PC_07_16.csv", sep=""), sep=",", head=TRUE)
#head(data) #nrow(data)

#import the population data
pop_ <- read.table(paste(dir1, "population_WM_OA_07_16.csv", sep=""), sep=",", head=TRUE)
#head(pop_)

#Calculate 'rates' ##==========
crime_per_00_people <- rates(data, denomin=pop_, id_field=TRUE,
                              multiplier = 100)   #head(crime_per_00_people)  #nrow(crime_per_000_people)# crime_per_00_people[,2]

#remove (manually) a single outlier indexed (1103)
#---------------------------------------------------------------
  out_List<-c(1103)
  for(w in 1:length(out_List)){#w<-1
    crime_per_00_people[out_List[w],2:ncol(crime_per_00_people)] <-
      crime_per_00_people[(out_List[w]+2),2:ncol(crime_per_00_people)]
  }

#some miscella
agg_Data <- as.data.frame(cbind(V1=as.vector(crime_per_00_people$Code), crime_per_00_people[,2:ncol(crime_per_00_people)]))
head(agg_Data)
#now transfer back to data
#data <- agg_Data

#data <- apply(data[,2:16], 2, as.numeric)#head(data)
#data <- cbind(1:nrow(data), data)
#colnames(data) <- c("ID","X1","X2","X3","X4","X5","X6","X7","X8","X9","X10", "X11","X12","X13","X14","X15")



#Calculate 'proportions' ##===========
prop_crime_per00_people <- props(crime_per_00_people, id_field = TRUE,
                                  digits=6, scale = 1)

data_backup <- prop_crime_per00_people

#clustering: 'akmedoids.clust' ##==========
cluster_output <- akmedoids.clust(prop_crime_per00_people, id_field = TRUE,
                                  method = "linear", k = c(3,20))

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

wat_result <- WatsonU2TestRand(clustr, traj=prop_crime_per00_people,
                             id_field=TRUE, grp=c(1:7), Nsample=999)

wat_result <- WatsonU2Test.Across(clustr, traj=prop_crime_per00_people,
                               id_field=TRUE, grp=c(1:7), Nsample=999)


bmOA_result_ <- prop_crime_per00_people
bmOA_traj_ <- clustr

bmOA_result_dd <- cbind(bmOA_result_, clustr)
#write.table(bmOA_result_dd, file="rrsult.csv", sep=",", row.names=F)
#obtaining citywide trend

#calculate total crime
dat_1 <- data
total_crime <- NULL
#average trendline
for(a in 2:ncol(dat_1)){#111111 a<-1
  total_crime <- c(total_crime, sum(as.numeric(as.character(dat_1[ ,a]))))
}#

#calculate total population
dat_1 <- pop_
total_pop <- NULL
#average trendline
for(a in 2:ncol(dat_1)){#111111 a<-1
  total_pop <- c(total_pop, sum(as.numeric(as.character(dat_1[ ,a]))))
}#

(total_crime/total_pop)*100

mean_trend <- write.table(mean_trend, file="meantrend.csv", sep=",", row.names = F)

#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------nrow(prop_crime_per00_people)
#-------------------------------------------------------------------------------------------------------------------head(prop_crime_per00_people)
#-------------------------------------------------------------------------------------------------------------------
#.
#.
#.

#OPTIMAL VALUE OF K

part2 <- clustr  #
geo_unique <- as.vector(data$Code)

if(study_Area == "wm_bm_" && crime_type == "PC_"){

  grp_ <- list(c(1,2,3), c(4), c(5,6,7)) #
  grp__ <- list(c("A","B","C"), c("D"), c("E","F","G"))

  part2 <- clustr

  data.append <- cbind(1:nrow(prop_crime_per00_people), prop_crime_per00_people[,-1]) #head(data.append) data.append <- data  #data <-

  colnames(data.append) <- c("code", 1:(ncol(data.append)-1))
  data.append <- as.data.frame(data.append)

  data.long.melted <- melt(data.append, id="code")#head(data., id="code"
  head(data.long.melted)

  data.long.melted <- cbind(data.long.melted, rep(part2, (ncol(data.append)-1)))#nrow(data.long.melted)

  colnames(data.long.melted) <- c("OAcode","Year","Crime_Count", "clusters")
}

#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------

head(data.long.melted)#

#use this to calculate percentage (%) change from year 1 to year n
year_uni <- as.vector(unique(data.long.melted$Year))
order_Cluster <- as.vector(unique(data.long.melted$clusters))
clusters_uni <- order_Cluster[order(as.vector(unique(data.long.melted$clusters)))]

change_ave_yr_ALL <- NULL
for(p in 1:length(clusters_uni)){#p<-1
  all_clust_list <- data.long.melted[which(data.long.melted$clusters==clusters_uni[p]),]
  ave_yr <- NULL
  for(m in 1:length(year_uni)){ #m<-1
    yr_ <- all_clust_list[which(as.vector(all_clust_list$Year)==year_uni[m]),]#.
    ave_yr <- c(ave_yr, sum(yr_$Crime_Count))
  }
  change_ave_yr_ALL <- rbind(change_ave_yr_ALL,  ave_yr)
}
change_ave_yr_ALL
change_ave_yr_ALL <- t(change_ave_yr_ALL)

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------

#Calculating the number of trajectories  grp_
length(which(part2%in%grp__[[1]]))
length(which(part2%in%grp__[[1]]))/length(part2) * 100

grp.dat<-data.frame(change_ave_yr_ALL,row.names=1:(ncol(data.append)-1))
names(grp.dat)<-clusters_uni

#reshape the data
p.dat<-data.frame(step=row.names(grp.dat),grp.dat,stringsAsFactors=F) #p.dat
#----------------------------------------------
p.dat<-melt(p.dat,id='step')
p.dat$step<-as.numeric(p.dat$step) #head(p.dat)

class(p.dat$step)
colfunc <- colorRampPalette(c("green", "yellow", "red"))
colours <- c(rep("#00FF00", 3), rep("yellow", 1), rep("red", 3)) #burglary
#transfer the results_Offender
Year <- c("2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016")

dev.new()

p <- ggplot(p.dat,aes(x=step,y=value)) + theme(legend.position="none")  #grp_

p + geom_area(aes(fill=variable), colour = "gray5", position='fill', size = 0.5) +
  scale_fill_manual(values = colours) +
  scale_x_continuous(breaks=1:10, labels=Year) + theme_light()

#removing the sub-grouplines
p + geom_area(aes(fill=variable), colour = 'NA', position='fill', size = 0.5) +
  scale_fill_manual(values = colours) +
  scale_x_continuous(breaks=1:10, labels=Year) + theme_light()

#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------



#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------



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

#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
#plotting (SPATIAL) - Birmingham
#------------------------------------------------------------------------------


part2 <- clustr  #
data <- prop_crime_per00_people
geo_unique <- as.vector(data$Code)

  grp_ <- list(c(1,2,3), c(4), c(5,6,7)) #
  grp__ <- list(c("A","B","C"), c("D"), c("E","F","G"))
  colours <- c(rep("#00FF00", 3), rep("yellow", 1), rep("red", 3)) #burglary
  part2 <- clustr

  data.append <- cbind(1:nrow(prop_crime_per00_people), prop_crime_per00_people[,-1]) #head(data.append) data.append <- data  #data <-

  colnames(data.append) <- c("code", 1:(ncol(data.append)-1))
  data.append <- as.data.frame(data.append)

  data.long.melted <- melt(data.append, id="code")#head(data., id="code"
  head(data.long.melted)

  data.long.melted <- cbind(data.long.melted, rep(part2, (ncol(data.append)-1)))#nrow(data.long.melted)

  colnames(data.long.melted) <- c("OAcode","Year","Crime_Count", "clusters")


  clusters <- list_Letters[data.long.melted$clusters]
  data.long.melted$clusters <- clusters
  colnames(data.long.melted) <- c("OAcode","Year","Crime_Count", "clusters")

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
                                 "9", "10")
  }
  #get the corresponding OA label #head(data)# head(agg_Data[1:10,])
  cluster_Units <- as.vector(agg_Data$V1[as.data.frame(data_reMelt_Cut)$Code])

  length_Of_Clusters <- rbind(length_Of_Clusters, cbind(i, length(cluster_Units)))

  cluster_Units_ALL[i] <- list(cluster_Units)
}

# par()#getwd()
library(rgdal)
dev.new()
WM_LSOA <- readOGR(dsn=".", "Birmingham_hexmap") #geo_unit_with_POPData
plot(WM_LSOA)
#WM_LSOA <- readOGR(dsn=".", "Birmingham_hexmap_LSOA_osgb")

##WM_LSOA <- readOGR(dsn=".", "BM_Centroids")  #head(WM_LSOA@data)  #plot(WM_LSOA, border="grey", pch=1)

#par(mfrow=c(3,4))
#par(oma=c(0,0,0,0), mar=c(0,0,0,0), mfrow=c(3,1))

par(oma=c(0,0,0,0), mar=c(0,0,0,0), mfrow=c(1,1))

colour_wm_bm_ <- colours


sp::plot(WM_LSOA, col="grey", border="grey", pch=16)

#plotting for gradient colour
#--------------------------------------------
cc_ <- 0
countt_ <- 0
#spatial patterning
for(q in 1:length(grp_)){ #q<-2

  #for(p in 1:length(cluster_Units_ALL)){

  #combine
  colat_<- NULL

  for(f in 1:length(grp_[[q]])){ # f<-1
    cc_ <- cc_ + 1
    ##colat_ <- c(colat_, cluster_Units_ALL[[grp_[[q]][f]]])
    colat_ <- cluster_Units_ALL[[grp_[[q]][f]]]
    print(length(cluster_Units_ALL[[grp_[[q]][f]]]))
    #}

    #sp::plot(WM_LSOA[which(as.vector(WM_LSOA$OAcode)%in%colat_),], add=TRUE, col="darkgrey", border="black", pch=16) head(WM_LSOA@data)
    #sp::plot(WM_LSOA[1,], add=TRUE, col="grey", border="black")
    #sp::plot(WM_LSOA[which(as.vector(WM_LSOA$OAcode)%in%colat_),], add=TRUE, col="black", border="black", pch=16) #check that OA field is correct
    sp::plot(WM_LSOA[which(as.vector(WM_LSOA$OAcode)%in%colat_),], add=TRUE, col=colours[cc_], border="grey50", pch=16) #check that OA field is correct

  }
  #countt_ <- countt_ + length(unlist(grp_[c(q)]))

}
#}
sp::plot(city_Centre, col="NA", border="black", pch=16, add=TRUE, lwd=3)

#.
#.


#-------------------------------------------------------------------------------------
#to plot bar charts of deprivation classes, STACKED with each inequality group (LOCAL deprivation count)
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------

#head(Data_with_Geo)
#head(dep_Areas)


#WM_LSOA <- readOGR(dsn=".", paste(study_Area, "LSOA_hex", sep="")) #geo_unit_with_POPData
#city_Centre <- readOGR(dsn=".", paste(study_Area, "citycentre", sep="")) #geo_unit_with_POPData
#par(oma=c(0,0,0,0), mar=c(0,0,0,0), mfrow=c(1,1))

#---------------------------------------------------------------------------------------
dev.new()
import_map_ <- readOGR(dsn=".", "Birmingham_hexmap") #geo_unit_with_POPData
plot(import_map_)
head(import_map_@data)

#TSD_Index <- read.table(file = "TSD_birmingham_2001_2011.csv", sep=",", head = TRUE) #bi
TSD_Index <- read.table(file = paste(dir1, "deprivation_tsd_fnl.csv", sep=""), sep=",", head = TRUE) #bi

head(TSD_Index)  #TOWNSNDRANK2001; TOWNSNDRANK2011 #nrow(TSD_Index)

#import the deprivation data and join to shapefile...
import_map <- merge(import_map_, TSD_Index, by.x="OAcode", by.y="code")
import_map <- import_map@data #
head(import_map) #import_map[which(import_map$code==""),]

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
    colat_2 <- import_map[which(import_map$OAcode %in% colat_),]  #as.vector(colat_2$code) #head(import_map)
    colate_All <- rbind(colate_All, colat_2) #nrow(colate_All)
  }
  list_D <- rbind(list_D, colate_All) #nrow(list_D)#head(list_D) #length(unique(list_D$code))
}
#length(which(list_D$TOWNSNDRANK2001=="5"))
#---------------------------------------------------------------------------------------------

#NOTEEEEEEE! please, not that the deprivation class annotated as 1 is actually 5
#the arrangement below takes care of it..... when I call "5", I am calling the most deprived.....
dep_Classes <- c(5, 4, 3, 2, 1)

both_Yr <- NULL

for(i in 1:length(dep_Classes)){#i<-5
  #get the total crime rate in the most deprived, the least deprived.
  #get the list of area in this group
  #remember to change the YEAR...
  #for(j in 1:2){ #j<-1;  for 2001 and 2011
    #for 2001
  #  if(j==1){
      dat_dep_1 <- list_D[which(list_D$quintile==dep_Classes[i]),] #head(dat_dep_1)#nrow(list_D) #head(list_D) #nrow(dat_dep_1)
      #collating the stats of the three major groups
      sub_list_D1 <- NULL
      #collating the cluster solution...
      for(q in 1:length(grp_)){ #q<-1
        colate_All <- NULL
        #collating main groups and not the subgroups
        for(f in 1:length(grp_[[q]])){ # f<-1
          colat_ <- cluster_Units_ALL[[grp_[[q]][f]]]
          #collate from the import map the lsoa that happens in colat_
          colat_2 <- dat_dep_1[which(dat_dep_1$OAcode %in% colat_),]  #as.vector(colat_2$code) #nrow(colat_2)
          colate_All <- rbind(colate_All, colat_2) #nrow(colate_All)
        }
        sub_list_D1 <- rbind(sub_list_D1, nrow(colate_All)) #nrow(list_D)
      }

      both_Yr <- cbind(both_Yr, sub_list_D1)
    }

    #both_Yr[i] <- list(cbind(sub_list_D1, sub_list_D2))

  #}
#}

#calculate the mean for export for plotting in 'trendline (version 1).xlsx'
##store_Mean <- NULL
##for(k in 1:length(both_Yr)){#k<-1
  ##c_ <- both_Yr[[k]]
  ##ave_ <- NULL
  ##for(m in 1:nrow(c_)){#m<-1
    ##ave_ <- c(ave_, mean(c_[m,]))
  ##}
  ##store_Mean <- cbind(store_Mean, ave_)
##}

#write.table(store_Mean, file=paste(study_Area, crime_type,  "new_dp4.csv", sep=""), sep=",", row.names=F)
write.table(both_Yr, file="bm_dep_dp4.csv", sep=",", row.names = F)


#------------------------------------------------------------------------------------------------------------------
#STUDY AREA 2 - BRISBANE
#------------------------------------------------------------------------------------------------------------------
getwd()

dir2 <- "C:/Users/monsu/Documents/GitHub/Packages/inter_comparison_Micro/data2/"

#library(foreign)
#-------------------------------------
#spatial unit of analysis----------
#location attributes
df <- read.dbf(paste(dir2, "brsa2_fnl.dbf", sep=""))
head(df)  #nrow(df)

#this data is originally in meshblock
dat <- read.table(file=paste(dir2, "Brisbane_Property.csv", sep=""), sep=",", head=TRUE)
head(dat)  #nrow(dat)

#unique time
uni_time <- unique(dat$Year)[order(unique(dat$Year))]

#location ids
uni_loc <- unique(df$SA1_16PID)  #length(uni_loc)

#-----------------------------------------------------#see the exported one
#reading the aggregated data.
dataimp <- read.table(file=paste(dir2, "data_exp.csv", sep=""), sep=",", head=T)
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

#These has already been used, exported and re-imported..
crime_per_00_people_ <- read.table(file=paste(dir2, "crime_per_00_people_.csv", sep = ""), sep=",", head=TRUE)
#head(crime_per_00_people_)

#imputation
crime_per_00_people_ <- dataImputation(crime_per_00_people_, id_field = TRUE, method = 2, replace_with = 1, fill_zeros = FALSE)
#head(crime_per_00_people_)

#some miscella
agg_Data <- as.data.frame(cbind(V1=as.vector(crime_per_00_people_$code), crime_per_00_people_[,2:ncol(crime_per_00_people_)]))
head(agg_Data)

#Calculate 'proportions' ##===========
prop_crime_per00_people_ <- props(crime_per_00_people_, id_field = TRUE,
                                  digits=7, scale = 1)
#nrow(prop_crime_per00_people_) #prop_crime_per00_people[,2]

data_backup <- prop_crime_per00_people_

#clustering: 'akmedoids.clust' ##==========

cluster_output <- akmedoids.clust(prop_crime_per00_people_, id_field = TRUE,
                                  method = "linear", k = c(3,20))

#retrieve cluster attributes: 'statPrint' ##========

clustr <- as.vector(cluster_output$optimSolution)

#line plot
print(statPrint(clustr, prop_crime_per00_people_, id_field=TRUE, reference = 1, show.plot = FALSE,
                N.quant = 8, type="lines", y.scaling="free"))

#areal plot
print(statPrint(clustr, prop_crime_per00_people_, id_field=TRUE, reference = 1,
                N.quant = 4, type="stacked"))

#to export
brsn_exp <- cbind(prop_crime_per00_people_, clustr)
write.table(brsn_exp, file="re_brs.csv", sep=",", row.names = F)
#----------------------------------------------------------------------------------------------
brs_result_ <- prop_crime_per00_people_
brs_traj_ <- clustr

dat_1 <- crime_per_00_people_
#obtaining citywide trend
mean_trend <- NULL
#average trendline
for(a in 2:ncol(dat_1)){#111111 a<-1
  mean_trend <- c(mean_trend, mean(dat_1[ ,a]))
}#

write.table(mean_trend, file="meantrend.csv", sep=",", row.names = F)
#----------------------------------------------------------------------------------------------

##Deprivation data
brs_ineq <- read.table(file=paste(dir2, "deprivation_IndRelSocDis.csv", sep=""), sep=",", head=TRUE)
head(brs_ineq)
#collect for only the qld area
df <- read.dbf(paste(dir2, "brsa2_fnl.dbf", sep=""))
head(df)  #nrow(df)
#subset the deprivation data
brs_subsetted <- brs_ineq[which(brs_ineq$Code %in% df$SA1_16PID), ] #head(brs_subsetted) #nrow(brs_subsetted)

quintile <- cut(brs_subsetted$Score, quantile(brs_subsetted$Score, prob=0:5/5, na.rm=TRUE), include.lowest = TRUE, labels = FALSE)
brs_subsetted <- cbind(brs_subsetted, quintile)
head(brs_subsetted)
write.table(brs_subsetted, file="bris_inequity_Index2.csv", sep=",", row.names = F)

#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------

#OPTIMAL VALUE OF K
data <- prop_crime_per00_people_
part2 <- clustr  #
geo_unique <- as.vector(prop_crime_per00_people_$code)

if(study_Area == "wm_bm_" && crime_type == "PC_"){

  grp_ <- list(c(1,2,3,4,5), c(6), c(7,8,9)) #
  grp__ <- list(c("A","B","C","D","E"), c("F"), c("G","H","I"))

  part2 <- clustr  #unique(clustr)

  data.append <- cbind(1:nrow(prop_crime_per00_people_), prop_crime_per00_people_[,-1]) #head(data.append) data.append <- data  #data <-

  colnames(data.append) <- c("code", 1:(ncol(data.append)-1))
  data.append <- as.data.frame(data.append)

  data.long.melted <- melt(data.append, id="code")#head(data., id="code"
  head(data.long.melted)

  data.long.melted <- cbind(data.long.melted, rep(part2, (ncol(data.append)-1)))#head(data.long.melted) nrow(data.long.melted)

  colnames(data.long.melted) <- c("OAcode","Year","Crime_Count", "clusters")

  clusters <- list_Letters[data.long.melted$clusters]
  data.long.melted$clusters <- clusters
  colnames(data.long.melted) <- c("OAcode","Year","Crime_Count", "clusters")

}

#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------

head(data.long.melted)#

#use this to calculate percentage (%) change from year 1 to year n
year_uni <- as.vector(unique(data.long.melted$Year))
order_Cluster <- as.vector(unique(data.long.melted$clusters))
clusters_uni <- order_Cluster[order(as.vector(unique(data.long.melted$clusters)))]

change_ave_yr_ALL <- NULL
for(p in 1:length(clusters_uni)){#p<-1
  all_clust_list <- data.long.melted[which(data.long.melted$clusters==clusters_uni[p]),]
  ave_yr <- NULL
  for(m in 1:length(year_uni)){ #m<-1
    yr_ <- all_clust_list[which(as.vector(all_clust_list$Year)==year_uni[m]),]#.
    ave_yr <- c(ave_yr, sum(yr_$Crime_Count))
  }
  change_ave_yr_ALL <- rbind(change_ave_yr_ALL,  ave_yr)
}
change_ave_yr_ALL
change_ave_yr_ALL <- t(change_ave_yr_ALL)

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------

#Calculating the number of trajectories  grp_
length(which(part2%in%grp__[[1]]))
length(which(part2%in%grp__[[1]]))/length(part2) * 100

grp.dat<-data.frame(change_ave_yr_ALL,row.names=1:(ncol(data.append)-1))
names(grp.dat)<-clusters_uni

#reshape the data
p.dat<-data.frame(step=row.names(grp.dat),grp.dat,stringsAsFactors=F) #p.dat
#----------------------------------------------
p.dat<-melt(p.dat,id='step')
p.dat$step<-as.numeric(p.dat$step) #head(p.dat)

class(p.dat$step)
colfunc <- colorRampPalette(c("green", "yellow", "red"))
colours <- c(rep("#00FF00", 5), rep("yellow", 1), rep("red", 3)) #burglary
#transfer the results_Offender
Year <- c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016","2017")

p <- ggplot(p.dat,aes(x=step,y=value)) + theme(legend.position="none")  #grp_
p + geom_area(aes(fill=variable), colour = "gray5", position='fill', size = 0.5) +
  scale_fill_manual(values = colours) +
  scale_x_continuous(breaks=1:10, labels=Year) + theme_light()
#removing the sub-grouplines
p + geom_area(aes(fill=variable), colour = 'NA', position='fill', size = 0.5) +
  scale_fill_manual(values = colours) +
  scale_x_continuous(breaks=1:10, labels=Year) + theme_light()

#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------



#mmD ----------------------------------------------
data_reMelt <- dcast(data.long.melted, OAcode ~ Year, value.var='Crime_Count')
data_reMelt <- cbind(data_reMelt, clusters) #nrow(data_reMelt)

cluster_Group <- as.vector(unique(data_reMelt$clusters)[order(unique(data_reMelt$clusters))])
#---------------------------------------------------------------------------
#plot each cluster....
#---------------------------------------------------------------------------
clusters <- clusters[1:nrow(data)]

#data_reMelt <- dcast(data.long.melted, OAcode ~ Year, value.var='Crime_Count')
#data_reMelt <- cbind(data_reMelt, clusters) #head(data_reMelt)

#cluster_Group <- as.vector(unique(data_reMelt$clusters)[order(unique(data_reMelt$clusters))])

length_Of_Clusters <- NULL

cluster_Units_ALL <- list()


for(i in 1:length(cluster_Group)){ #i<-1

  data_reMelt_Cut <- data_backup[which(data_reMelt$clusters==cluster_Group[i]),]#nrow(data_reMelt) #nrow(data_backup)

  if(is.null(data_reMelt_Cut)){data_reMelt_Cut<-matrix(data_reMelt_Cut,,10)
  colnames(data_reMelt_Cut) <- c("code", "1", "2", "3", "4", "5", "6", "7", "8",
                                 "9", "10")
  }
  #get the corresponding OA label #head(data)# head(agg_Data[1:10,])
  cluster_Units <- as.vector(agg_Data$V1[as.data.frame(data_reMelt_Cut)$code])  ##bbr

  length_Of_Clusters <- rbind(length_Of_Clusters, cbind(i, length(cluster_Units)))

  cluster_Units_ALL[i] <- list(cluster_Units)
}

#---------------------------------------------------------------------------------------
dev.new()
import_map_ <- readOGR(dsn=".", "brisbane_hexmap") #geo_unit_with_POPData
plot(import_map_)
head(import_map_@data)

#TSD_Index <- read.table(file = "TSD_birmingham_2001_2011.csv", sep=",", head = TRUE) #bi
#brs_ineq <- read.table(file=paste(dir2, "deprivation_IndRelSocDis.csv", sep=""), sep=",", head=TRUE) #SA1_16PID#Code  tsd
#head(brs_ineq)

TSD_Index <- read.table(file=paste(dir2, "bris_inequity_Index2.csv", sep=""), sep=",", head=TRUE)

head(TSD_Index)  #TOWNSNDRANK2001; TOWNSNDRANK2011 #nrow(TSD_Index)

#import the deprivation data and join to shapefile...
import_map <- merge(import_map_, TSD_Index, by.x="SA1_16PID", by.y="Code")
import_map <- import_map@data #
head(import_map) #import_map[which(import_map$code==""),]

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
    colat_2 <- import_map[which(import_map$SA1_16PID %in% colat_),]  #as.vector(colat_2$code) #head(import_map)
    colate_All <- rbind(colate_All, colat_2) #nrow(colate_All)
  }
  list_D <- rbind(list_D, colate_All) #nrow(list_D)#head(list_D) #length(unique(list_D$code))
}
#length(which(list_D$TOWNSNDRANK2001=="5"))
#---------------------------------------------------------------------------------------------

#NOTEEEEEEE! please, note that the deprivation class annotated as 1 is actually 1 here
#the arrangement below takes care of it..... when I call "5", I am calling the most deprived.....
#dep_Classes <- c(5, 4, 3, 2, 1)
dep_Classes <- c(1, 2, 3, 4, 5)

both_Yr <- NULL

for(i in 1:length(dep_Classes)){#i<-1
  #get the total crime rate in the most deprived, the least deprived.
  #get the list of area in this group
  #remember to change the YEAR...
  #for(j in 1:2){ #j<-1;  for 2001 and 2011
  #for 2001
  #  if(j==1){
  dat_dep_1 <- list_D[which(list_D$quintile==dep_Classes[i]),] #head(dat_dep_1)#nrow(list_D) #head(list_D) #nrow(dat_dep_1)
  #collating the stats of the three major groups
  sub_list_D1 <- NULL
  #collating the cluster solution...
  for(q in 1:length(grp_)){ #q<-1
    colate_All <- NULL
    #collating main groups and not the subgroups
    for(f in 1:length(grp_[[q]])){ # f<-1
      colat_ <- cluster_Units_ALL[[grp_[[q]][f]]]
      #collate from the import map the lsoa that happens in colat_
      colat_2 <- dat_dep_1[which(dat_dep_1$SA1_16PID %in% colat_),]  #as.vector(colat_2$code) #nrow(colat_2)
      colate_All <- rbind(colate_All, colat_2) #nrow(colate_All)
    }
    sub_list_D1 <- rbind(sub_list_D1, nrow(colate_All)) #nrow(list_D)
  }

  both_Yr <- cbind(both_Yr, sub_list_D1)
}

write.table(both_Yr, file="brs.csv", sep=",", row.names = F)

#------------------------------------------------------------------------------------------------------------------
#STUDY AREA 3 - GLASGOW
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
write.table(data, file=paste(dir2, "data_exp.csv", sep=""), sep=",", row.names = F)
Sys.sleep(5)
#----------------------------------------------------
#to here..





#-------------------------------------------------------------------------------------------------------------------
#reading a shapefile from a directory
dir <- "C:/Users/monsu/Documents/GitHub/Packages/akmedoids v0.1.3/data2/shp1"
ff <- list.files(dir, pattern="\\.shp$", full.names=TRUE)
geo_unit_with_POPData <- lapply(ff, shapefile)
head(geo_unit_with_POPData@data) #nrow(geo_unit_with_POPData@data) #geo_unit_with_POPData[[1]][1,] plot(geo_unit_with_POPData)
