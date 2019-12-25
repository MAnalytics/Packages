#plotting the citywide trends






#libraries #--------------------------------------------------
#install akmedoids:
rm(list = ls())
' "Build" > "Install and Rebuild" '

#library(rgdal)
#install.packages("raster")
#library(raster)
library(foreign)

#workspace setting #------------------------------------------
#study_Area <- "wm_bm_"
study_Area <- "wm_bm_"
crime_type <- "PC_"
unit <- "LSOA"

year_to <- 2015
#LSOA
#------------------------------------------------------------------------------------------------------------------
#STUDY AREA 1 - BIRMINGHAM
#------------------------------------------------------------------------------------------------------------------
#Import Birmingham study area
#data <- read.table(paste("C:/Users/monsu/Documents/GitHub/Packages/v013/data2/",
#study_Area, "LSOA_", crime_type, "01_12.csv", sep=""), sep=",", head=TRUE)  #head(data) #nrow(data)



#MULTIPLE REGRESSION ANALYSIS

setwd("C:/Users/monsu/Documents/MMU DOCUMENTS/G/Removable Disk/20180613/Monsuru_WMD_12062018/")

################################2011 : PREPARING 2001 DATA
#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------
################################2011
#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------


  #from "C:\Users\monsu\Documents\MMU DOCUMENTS\G\Removable Disk\20180613\Monsuru_WMD_12062018/code_to_append_all_CRIME_DATA"
  #the directory above contains other variable such as 'jsa', 'socio-housing' etc.
  #data aggregation (this one is burglary only)
  #data <- read.table("burglary_agg.csv", sep=",", head=TRUE) #head(data)  data[which(as.vector(data$code)=="E00046064"),12]

  #data used in the JQC paper
  data <- read.table("C:/Users/monsu/Documents/GitHub/Packages/inter_comparison_Micro/data1_1/wm_bm_OA_PC_01_16.csv", sep=",", head=TRUE)  #head(data) #nrow(data)
  head(data)

  nrow(data)
  #------------------------------------------------------
  #TO GENERATE DATA IN LSOA UNITS
  #read the nested data
  #extract the Birmingham OA lists#head(data)
  birm_codes <- as.vector(data$code)
  WM_nested_Details <- read.table("west-midlands-lookup-full-2011.csv", sep=",", head=TRUE)#
  head(WM_nested_Details)
  #head(nested_Details)
  WM_nested_Details_OAcode <- as.vector(WM_nested_Details$OAcode)
  Birm_nested_Details_OA <- WM_nested_Details[which(WM_nested_Details_OAcode %in% birm_codes),] #
  head(Birm_nested_Details_OA)#Birm_nested_Details_OA[1:16,]
  #now aggregated the data by LSOA using the nest code table 
  uni_LSOA <- as.vector(unique(Birm_nested_Details_OA$LSOAcod))#head(data)
  
  #aggregated the OA to LSOA
  agg_Data <- NULL
  
  for(r in 1:length(uni_LSOA)){ #r = 1
    ind_ <- as.vector(Birm_nested_Details_OA$OAcode[which(Birm_nested_Details_OA$LSOAcod == uni_LSOA[r])])
    sub_Data_ <- data[which(data$code %in%ind_),]
    sum_sub_Data_ <- colSums(sub_Data_[,2:ncol(sub_Data_)])
    agg_Data <- rbind(agg_Data, c(uni_LSOA[r], as.vector(sum_sub_Data_)))
  }#head(as.data.frame(agg_Data))
  agg_Data <- as.data.frame(agg_Data)
  data <- agg_Data
  #colnames(agg_Data) <- c("")#
  head(data)
  
  data <- data[,1:length(2002:((year_to)+1))] # 2011-2012
  head(data)
  nrow(data)
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
  
  
##data <- read.table("C:/Users/monsu/Documents/GitHub/Packages/inter_comparison_Micro/data1_1/wm_bm_PC_LSOA_.csv", sep=",", head=TRUE)  #head(data) #nrow(data)
##head(data)
##data <- cbind(data$code, data[,4:(ncol(data)-1)]) #adjust this...
##head(data)

colnames(data) <- c("code", paste("p", 2002:year_to, sep=""))
head(data)

#---------------------------------#library(foreign)
#read the attribute table of the ESRI data (for 'study_area1'), including the population data
df <- read.dbf(paste("C:/Users/monsu/Documents/GitHub/Packages/v013/data2/shp1/",
                     study_Area, "LSOA_", "pop_02_12.dbf", sep=""))#
head(df) #nrow(df)
others_ <- read.table("C:/Users/monsu/Documents/GitHub/Packages/inter_comparison_Micro/data1_1/WM_MID_YEAR_POP_ESIMATE_2002to2017.csv", sep = ",", head=TRUE)
#head(others_)
uni_codes <- as.vector(df$code) #length(uni_codes)


all_ <- NULL
for(t in 1:length(uni_codes)){#t<-1
  id_ <- which(others_$LSOA11CD==uni_codes[t])
  sub_ <- others_[id_, 4:ncol(others_)]
  all_ <- rbind(all_, sub_)
}
all_ <- cbind(uni_codes, all_)
#row.names(all_) <- uni_codes
colnames(all_) <- c("code", paste("p", 2002:(year_to+1), sep="")) #
head(all_)#nrow(all_)
#---------------------------------

df <- all_[,1:(length(2002:(year_to+1)))]
head(df)

#move the first row
#df <- as.data.frame(cbind(df$code, df[,1:(ncol(df)-1)]))   #df[which(df[,1]=="E01009419"),]
df <- as.data.frame(df)
head(df) #nrow(df) mode(df)
#subset the crime data with LSOA ids  #head(data)
data_sub <- as.data.frame(data[which(data$code %in% as.vector(df[,1])),])  #
head(data_sub)  #nrow(data_sub) #data[1,] #mode(data_sub)# #library(akmedoids)

#library(akmedoids)
#----------------------------------------------------------
#Calculate 'rates' ##==========
crime_per_000_people <- rates(data_sub, denomin=df, id_field=TRUE,
                              multiplier = 1000)
head(crime_per_000_people)  #nrow(crime_per_000_people)

geo_unique <- as.vector(crime_per_000_people$code)

agg_Data <- as.data.frame(cbind(V1=geo_unique, crime_per_000_people[,2:ncol(crime_per_000_people)]))  #head(agg_Data)
head(agg_Data)

#data <- agg_Data
#head(data)


#-------------------------
#-------------------------
#------------------------- #library(ggplot2)
#calculate total crime
dat_1 <- crime_per_000_people
#dat_1 <- data
total_crime <- NULL
#average trendline
for(a in 2:ncol(dat_1)){#111111 a<-1
  total_crime <- c(total_crime, mean(as.numeric(as.character(dat_1[ ,a]))))
}#
total_crime
daf = data.frame(date=c(rep(2002:year_to, by=1)),
                value=total_crime)
daf
ggplot(daf, aes(date, value)) + geom_line(colour="black", lty=1, lwd=1)  #daf2

#head(mtcars)
#-------------------------
#-------------------------
#-------------------------

#running this to identify the outlier
#crime_per_000_people <- prop_crime_per000_people
#---------------------------------------------------------------
if(study_Area == "wm_bm_" && crime_type == "PC_000"){  #head(crime_per_000_people)
  out_List <- NULL
  for(w in 1:nrow(crime_per_000_people)){#w<-1
    sn_ <- length(which(crime_per_000_people[w,2:ncol(crime_per_000_people)]>0.075))  #1605
    if(sn_>0){
      out_List <- c(out_List,w)
    }
  }
  ####out_List<-c(604, 617)  
  out_List<-c(609, 623)
  #replace each of these with next two records#data[1605:1606,]
  for(w in 1:length(out_List)){#w<-1
    crime_per_000_people[out_List[w],2:ncol(crime_per_000_people)] <- crime_per_000_people[(out_List[w]+2),2:ncol(crime_per_000_people)]
  }
}
#-------#head(data)
#---------------------------------------------------------------
#Calculate 'proportions' ##===========
#prop_crime_per000_people <- props(crime_per_000_people, id_field = TRUE,
#digits=4, scale = 1)  #head(prop_crime_per000_people) #prop_crime_per000_people[,2]

#Calculate 'proportions' ##===========50/sum(as.numeric(as.character(crime_per_000_people[,2])))

prop_crime_per000_people <- props(crime_per_000_people, id_field = TRUE,
                                  digits=6, scale = 1)  #head(prop_crime_per000_people) #prop_crime_per000_people[,2]

#multiply by 100
prop_crime_per000_people <- cbind(prop_crime_per000_people[,1], prop_crime_per000_people[,2:ncol(prop_crime_per000_people)]*100)
colnames(prop_crime_per000_people) <- c("code", paste("p", 2002:(year_to), sep="")) #

head(prop_crime_per000_people) #prop_crime_per000_people[,2]   #318/sum(as.numeric(as.vector(crime_per_000_people[,2])))

#prop_crime_per000_people <- prop_crime_per000_people[,2:ncol(prop_crime_per000_people)]
#head(prop_crime_per000_people)


data_backup <- prop_crime_per000_people
colnames(data_backup)<- c("ID", paste("X", 1:length(2002:year_to), sep="")) 
 

#clustering: 'akmedoids.clust' ##==========
cluster_output <- akmedoids.clust(prop_crime_per000_people, id_field = TRUE,
                                  method = "linear", k = c(3,20))

#clustering: 'akmedoids.clust' ##==========
#cluster_output <- akmedoids.clust(prop_crime_per000_people, id_field = TRUE,
                                  #method = "linear", k = 13)#bm _ LSOA
cluster_output <- akmedoids.clust(prop_crime_per000_people, id_field = TRUE,
                                  method = "linear", k = 13)


#9 #no removal, 3,3,3
#retrieve cluster attributes: 'statPrint' ##======== cluster_output
clustr <- as.vector(cluster_output$optimSolution)

clustr <- as.vector(cluster_output$solution)

#line plot  #prop_crime_per000_people <- as.data.frame(prop_crime_per000_people)
print(statPrint(clustr, prop_crime_per000_people, id_field=TRUE, reference = 1,
                N.quant = 8, type="lines", y.scaling="free"))
#areal plot
print(statPrint(clustr, prop_crime_per000_people, id_field=TRUE, reference = 1,
                N.quant = 4, type="stacked"))

p.values <- Cmedians.direction(clustr, traj=prop_crime_per000_people, id_field=TRUE, Nsample=9)
p.values
bm_result_ <- prop_crime_per000_people
bm_traj_ <- clustr

#-----------------------------------------
#deprivation data
#read the attribute table of the ESRI data (for 'study_area1'), including the population data
code <- df$code

#path to the data sets
dr_ <- "C:/Users/monsu/Documents/GitHub/Packages/inter_comparison_Micro/"

tsd <- tsd_ind_(code = uni_codes, unit = "LSOA", n_classes = 5, path=dr_)
head(tsd) #nrow(tsd)


#-----------------------------------------

#class together all groups with increasing with increasing.. and run logistic regression of with 0 & 1...
#summarise the result in terms of the descriptive and the significance of the association)
#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------

study_Area <- "wm_bm_"
crime_type <- "PC_"
unit <- "OA"

year_to <- 2015

#OUTPUT AREA
#------------------------------------------------------------------------------------------------------------------
#STUDY AREA 1 - BIRMINGHAM
#------------------------------------------------------------------------------------------------------------------
#Import Birmingham study area
data <- read.table("C:/Users/monsu/Documents/GitHub/Packages/inter_comparison_Micro/data1_1/wm_bm_PC_OA_.csv", sep=",", head=TRUE)  #head(data) #nrow(data)
head(data)
data <- cbind(data$code, data[,4:ncol(data)])

data <- data[,1:length(2002:((year_to)+1))] # 2011-2012
head(data)

head(data)
colnames(data) <- c("code", paste("p", 2002:year_to, sep=""))
head(data)

#---------------------------------
#read the attribute table of the ESRI data (for 'study_area1'), including the population data
df <- read.table("C:/Users/monsu/Documents/GitHub/Packages/v013/data2/shp1/population_WM_OA_02_16.csv", sep=",", head=TRUE)
head(df)    

df <- df[,1:(length(2002:(year_to+1)))]
head(df)

colnames(df) <- c("code", paste("p", 2002:year_to, sep="")) #
head(df)#nrow(all_)
#---------------------------------

uni_codes <- df$code  #length(uni_codes)

#move the first row
#df <- as.data.frame(cbind(df$code, df[,1:(ncol(df)-1)]))   #df[which(df[,1]=="E01009510"),]
#df <- as.data.frame(df)
head(df) #nrow(df) mode(df)
#subset the crime data with LSOA ids
data_sub <- as.data.frame(data[which(data$code %in% as.vector(df[,1])),])  #head(data_sub)  nrow(data_sub) #data[1,] #mode(data_sub)# #library(akmedoids)
#Calculate 'rates' ##==========#library(akmedoids)
crime_per_000_people <- rates(data_sub, denomin=df, id_field=TRUE,
                              multiplier = 100)
head(crime_per_000_people)  #nrow(crime_per_000_people)

crime_per_000_people[which(crime_per_000_people$code=="E00045759"),]

geo_unique <- as.vector(crime_per_000_people$code)

agg_Data <- as.data.frame(cbind(V1=geo_unique, crime_per_000_people[,2:ncol(crime_per_000_people)]))  #head(agg_Data)
head(agg_Data)

#-------------------------
#-------------------------
#------------------------- #library(ggplot2)
#calculate total crime
dat_1 <- crime_per_000_people
total_crime <- NULL
#average trendline
for(a in 2:ncol(dat_1)){#111111 a<-1
  total_crime <- c(total_crime, mean(as.numeric(as.character(dat_1[ ,a]))))
}#

daf = data.frame(date=c(rep(2002:year_to, by=1)),
                 value=total_crime)

ggplot(daf, aes(date, value)) + geom_line(colour="black", lty=1, lwd=1)

#head(mtcars)
#-------------------------
#-------------------------
#-------------------------

#running this to identify the outlier
#crime_per_000_people <- prop_crime_per000_people
#---------------------------------------------------------------
if(study_Area == "wm_bm_" && crime_type == "PC_000"){  #head(crime_per_000_people)
  out_List <- NULL
  for(w in 1:nrow(crime_per_000_people)){#w<-1
    sn_ <- length(which(crime_per_000_people[w,2:ncol(crime_per_000_people)]>0.04))  #1605 OA bm 0.04
    if(sn_>0){
      out_List <- c(out_List,w)
    }
  }
  #out_List<-c(604, 617)
  #out_List<-c(1307, 2682, 3213)
  #replace each of these with next two records#data[1605:1606,]
  for(w in 1:length(out_List)){#w<-1
    crime_per_000_people[out_List[w],2:ncol(crime_per_000_people)] <- crime_per_000_people[(out_List[w]+2),2:ncol(crime_per_000_people)]
  }
}
#-------#head(data)
#---------------------------------------------------------------
#Calculate 'proportions' ##===========
#prop_crime_per000_people <- props(crime_per_000_people, id_field = TRUE,
#digits=4, scale = 1)  #head(prop_crime_per000_people) #prop_crime_per000_people[,2]

#Calculate 'proportions' ##===========
prop_crime_per000_people <- props(crime_per_000_people, id_field = TRUE,
                                  digits=6, scale = 1)  #head(prop_crime_per000_people) #prop_crime_per000_people[,2]
head(prop_crime_per000_people) #prop_crime_per000_people[,2]   #318/sum(as.numeric(as.vector(crime_per_000_people[,2])))

#multiply by 100
prop_crime_per000_people <- cbind(prop_crime_per000_people[,1], prop_crime_per000_people[,2:ncol(prop_crime_per000_people)]*100)
colnames(prop_crime_per000_people) <- c("code", paste("p", 2002:(year_to), sep="")) #

head(prop_crime_per000_people) #prop_crime_per000_people[,2]   #318/sum(as.numeric(as.vector(crime_per_000_people[,2])))

data_backup <- prop_crime_per000_people
colnames(data_backup)<- c("ID", paste("X", 1:length(2002:year_to), sep="")) 

#no removal #6
#clustering: 'akmedoids.clust' ##========== prop_crime_per000_people <- crime_per_000_people
cluster_output <- akmedoids.clust(prop_crime_per000_people, id_field = TRUE,
                                  method = "linear", k = c(3,20))

#clustering: 'akmedoids.clust' ##==========
cluster_output <- akmedoids.clust(prop_crime_per000_people, id_field = TRUE,
                                  method = "linear", k = 11)

#retrieve cluster attributes: 'statPrint' ##======== cluster_output
clustr <- as.vector(cluster_output$optimSolution)
#clustr <- as.vector(cluster_output$solution)
#line plot
print(statPrint(clustr, prop_crime_per000_people, id_field=TRUE, reference = 1,
                N.quant = 8, type="lines", y.scaling="free"))
#areal plot
#print(statPrint(clustr, prop_crime_per000_people, id_field=TRUE, reference = 1,
                #N.quant = 4, type="stacked"))
##p.values <- Cmedians.direction(clustr, traj=prop_crime_per000_people, id_field=TRUE, Nsample=9)
##p.values
##bm_result_ <- prop_crime_per000_people
##bm_traj_ <- clustr

#-----------------------------------------
#deprivation data
#read the attribute table of the ESRI data (for 'study_area1'), including the population data
code <- df$code

#path to the data sets
dr_ <- "C:/Users/monsu/Documents/GitHub/Packages/inter_comparison_Micro/"

tsd <- tsd_ind_(code = uni_codes, unit = "OA", n_classes = 5, path=dr_)
head(tsd) #nrow(tsd)


#-----------------------------------------

#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------







#DATA ZONE
#------------------------------------------------------------------------------------------------------------------
#STUDY AREA 2 - GLASGOW
#------------------------------------------------------------------------------------------------------------------

study_Area <- "gl2_"
crime_type <- "PC_"
unit <- "LSOA"

year_to <- 2015

#Import Birmingham study area
#data <- read.table(paste("C:/Users/monsu/Documents/GitHub/Packages/v013/data2/",
#study_Area, "LSOA_", crime_type, "01_12.csv", sep=""), sep=",", head=TRUE)  #head(data) #nrow(data)
data <- read.table("C:/Users/monsu/Documents/GitHub/Packages/inter_comparison_Micro/data2_2/gl_PC_DZ_.csv", sep=",", head=TRUE)  #head(data) #nrow(data)
head(data)
data <- cbind(data$code, data[, 18:31])
colnames(data) <- c("code", paste("p", 2002:2015, sep=""))
head(data)

#---------------------------------------
#crop out the data zones for glasgow
df <- read.dbf(paste("C:/Users/monsu/Documents/GitHub/Packages/v013/data2/shp2/",
                     study_Area, "LSOA_", "pop_02_12.dbf", sep="")) #nrow(df)#
head(df)
uni_codes <- as.vector(df$code)
#read remaining years..
#others <- read.table("C:/Users/monsu/Documents/GitHub/Packages/inter_comparison_Micro/data2_2/scotland_datazone_pop_others.csv", sep=",", head=TRUE) #head(others)
others <- read.table("C:/Users/monsu/Documents/GitHub/Packages/inter_comparison_Micro/data2_2/scotland_datazone_pop_others_.csv", sep=",", head=TRUE) #head(others)
head(others)
all_ <- NULL
for(t in 1:length(uni_codes)){ #t<-9
  id_ <- which(others$code==uni_codes[t])
  sub_ <- c(as.numeric(as.character(df[t,1:(ncol(df)-1)])),
            as.numeric(as.character(as.matrix(others[id_,2:(ncol(others)-1)]))))
  all_ <- rbind(all_, sub_) #head(all_)  #all_[1:10,]
}
#getwd() write.table(all_, file="w.csv", sep=",", row.names=F)

all_ <- cbind(uni_codes, all_)
row.names(all_) <- 1:nrow(all_)
colnames(data) <- c("code", paste("p", 2002:year_to, sep=""))
head(all_)#nrow(all_)
all_ <- as.data.frame(all_) #head(all_)
#all_[which(all_$code=="S01009758"),]

df <- all_
#--------------------------------------
head(df)  #df[which(df$uni_codes=="S01009758"),]
#move the first row
#df <- as.data.frame(cbind(df$code, df[,1:(ncol(df)-1)]))   #df[which(df[,1]=="E01009510"),]
#head(df) #nrow(df) mode(df)
#subset the crime data with LSOA ids
data_sub <- as.data.frame(data[which(data$code %in% as.vector(df[,1])),])  #
head(data_sub)  #nrow(data_sub) #data[1,] #mode(data_sub)
#Calculate 'rates' ##==========
crime_per_000_people <- rates(data_sub, denomin=df, id_field=TRUE,
                              multiplier = 1000)   #
head(crime_per_000_people)  #nrow(crime_per_000_people) #

#write.table(crime_per_000_people, file="C:/Users/monsu/Documents/GitHub/Packages/inter_comparison_Micro/data2_2/crime_per_000_people_LSOA.csv", 
##              sep=",", row.names=F) #getwd()

crime_per_000_people <- read.table(file="C:/Users/monsu/Documents/GitHub/Packages/inter_comparison_Micro/data2_2/crime_per_000_people_LSOA.csv", 
                                   sep=",", head=TRUE)

geo_unique <- as.vector(crime_per_000_people$code)

agg_Data <- as.data.frame(cbind(V1=geo_unique, crime_per_000_people[,2:ncol(crime_per_000_people)]))  #head(agg_Data)
head(agg_Data)

#-------------------------
#-------------------------
#------------------------- #library(ggplot2)
#citywide trend
#to fill up infinite entries
dat_1 <- crime_per_000_people
#dat_1 <- data #head(dat_1)
total_crime <- NULL
#average trendline
for(a in 2:ncol(dat_1)){#111111 a<-13
  total_crime <- c(total_crime, mean(as.numeric(as.character(dat_1[ ,a]))))
}#


daf2 = data.frame(date=c(rep(2002:year_to, by=1)),
                 value=total_crime)



ggplot(daf2, aes(date, value)) + geom_line(colour="black", lty=1, lwd=1)

head(mtcars)
#-------------------------
#combine
daty_ <- data.frame(Area=rep(c("Birm", "Glaz"), each=length(c(2002:year_to))),
           Year=rep(c(2002:year_to), 2),
           Crime_per_thou_people=c(daf$value, daf2$value))

p <- ggplot(data=daty_, aes(x=Year, y= Crime_per_thou_people, group=Area)) +
  geom_line(aes(color=Area))+
  geom_point(aes(shape=Area, color=Area)) + ylim(0, 160)

p <- p + theme_minimal() 

p + theme(legend.position="right")


#-------------------------
#-------------------------

#running this to identify the outlier
#crime_per_000_people <- prop_crime_per000_people

#---------------------------------------------------------------
if(study_Area == "gl2" && crime_type == "PC_"){  #head(crime_per_000_people)
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
                                  digits=6, scale = 1)  #
head(prop_crime_per000_people) #prop_crime_per000_people[,2]

data_backup <- prop_crime_per000_people
colnames(data_backup)<- colnames(data_backup)<- c("ID", paste("X", 1:length(2002:year_to), sep="")) 


#clustering: 'akmedoids.clust' ##==========
cluster_output <- akmedoids.clust(prop_crime_per000_people, id_field = TRUE,
                                  method = "linear", k = c(3,20))

cluster_output <- akmedoids.clust(prop_crime_per000_people, id_field = TRUE,
                                  method = "linear", k = 9)


#9 # no removal..
#retrieve cluster attributes: 'statPrint' ##========
clustr <- as.vector(cluster_output$optimSolution)  #nrow(prop_crime_per000_people)
#retrieve cluster attributes: 'statPrint' ##========
clustr <- as.vector(cluster_output$solution)

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


#-----------------------------------------grp_
#deprivation data
#read the attribute table of the ESRI data (for 'study_area1'), including the population data
code <- uni_codes

#path to the data sets
dr_ <- "C:/Users/monsu/Documents/GitHub/Packages/inter_comparison_Micro/"

tsd <- tsd_ind_(code = code, unit = "LSOA", n_classes = 5, path=dr_)
head(tsd) #nrow(tsd)

#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------


#OUTPUT AREA
#------------------------------------------------------------------------------------------------------------------
#STUDY AREA 2 - GLASGOW
#------------------------------------------------------------------------------------------------------------------

study_Area <- "gl2_"
crime_type <- "PC_"
unit <- "OA"

year_to <- 2015

#Import Birmingham study area
#data <- read.table(paste("C:/Users/monsu/Documents/GitHub/Packages/v013/data2/",
#study_Area, "LSOA_", crime_type, "01_12.csv", sep=""), sep=",", head=TRUE)  #head(data) #nrow(data)
data <- read.table("C:/Users/monsu/Documents/GitHub/Packages/inter_comparison_Micro/data2_2/gl_PC_OA_.csv", sep=",", head=TRUE)  #head(data) #nrow(data)
head(data)
data <- cbind(data$code, data[, 18:31])
colnames(data) <- c("code", paste("p", 2002:((year_to)), sep=""))
head(data)
nrow(data)
#---------------------------------------
#crop out the data zones for glasgow
#df <- data

uni_codes <- as.vector(data$code)  #length(unique(uni_codes))


#------------------------------------------------
#crop out 2011 population based on the codes...
d_2011 <- read.table(file="C:/Users/monsu/Documents/GitHub/Packages/inter_comparison_Micro/data2_2/scotland_OA_pop_2011.csv", 
                     sep=",", head=TRUE) 
head(d_2011)
d_2011 <- d_2011[which(d_2011$Scotland %in% uni_codes),]
colnames(d_2011) <- c("code","p2011")#nrow(d_2011)
row.names(d_2011) <- 1:nrow(d_2011)

#------------------------------------------------
#for 2001 population (convert to 2011 units using the "BDC_Geoconverter.R")
d_2001 <- read.table(file="C:/Users/monsu/Documents/GitHub/Packages/inter_comparison_Micro/data2_2/scotland_OA_pop_2001.csv", 
                     sep=",", head=TRUE) 
head(d_2001)

setwd("C:/Users/monsu/Documents/GitHub/Packages/inter_comparison_Micro/data2_2/")


##city="Glasgow" 
##unit = "OA"
setwd("C:/Users/monsu/Documents/UNI DESKTOP BACKUP _13062019/desktop/FoSS Slides_/")
converted_Data_2011 <- Geo_converter_BDC(data=d_2001, city=city, unit=unit, includeAll=FALSE) 
head(converted_Data_2011)

d_2001 <- as.data.frame(converted_Data_2011)
head(d_2001)  #nrow(d_2001[1:50,])

#subset 
d_2001 <- d_2001[d_2001$code %in% d_2011$code,]
head(d_2001)
row.names(d_2001) <- 1:nrow(d_2001)
nrow(d_2001)

#subset 
d_2011 <- d_2011[d_2011$code %in% d_2001$code,]
head(d_2011)
row.names(d_2011) <- 1:nrow(d_2001)
nrow(d_2011)

data_all <- matrix(0, nrow(d_2001), 2)
cd_ <- d_2001$code
for(r in 1:length(cd_)){#r<-1
  y_ <- d_2001[which(d_2001$code==as.vector(cd_[r])),]
  data_all[r,1] <- as.numeric(as.vector(y_$p2001)) #head(data_all)
  y_ <- d_2011[which(d_2011$code==as.vector(cd_[r])),]
  data_all[r,2] <- as.numeric(as.vector(y_$p2011)) #
}
head(data_all)

data_all <- cbind(as.character(cd_), data_all)

#write.table(data_all, file="C:/Users/monsu/Documents/GitHub/Packages/inter_comparison_Micro/data2_2/OA_pop_2001_2011.csv", 
 #               sep=",", row.names=F)

#-------------------------------------
#read after edit in excel
glz_OA_pop <- read.table(file="C:/Users/monsu/Documents/GitHub/Packages/inter_comparison_Micro/data2_2/OA_pop_2001_2011_.csv", 
                          sep=",", head=TRUE)
head(glz_OA_pop)


all_ <- glz_OA_pop
#colnames(all_) <- c("code", paste("p", 2002:2016, sep="")) #
head(all_)#nrow(all_)
all_ <- as.data.frame(all_) #head(all_)
#all_[which(all_$code=="S01009758"),]

df <- all_
#--------------------------------------
head(df)
#move the first row
#df <- as.data.frame(cbind(df$code, df[,1:(ncol(df)-1)]))   #df[which(df[,1]=="E01009510"),]
#head(df) #nrow(df) mode(df)
#subset the crime data with LSOA ids
data_sub <- as.data.frame(data[which(data$code %in% as.vector(df[,1])),])  #head(data_sub)  nrow(data_sub) #data[1,] #mode(data_sub)
#Calculate 'rates' ##==========

#-----------------------------------------------------------------------------------
#subset 'incident data' and 'population data' to contain those that have population only
data_sub <- data[which(data$code %in% df$code),] #
head(data_sub)
nrow(data_sub)

df <- df[which(df$code %in% data_sub$code),]
nrow(df)

#data imputation for df
df <- dataImputation(traj=df, id_field = TRUE, method = 2, replace_with = 1, fill_zeros = FALSE)
head(df)

df <- df[,1:(length(2002:(year_to+1)))]
head(df)

colnames(df) <- c("code", paste("p", 2002:year_to, sep="")) #
head(df)#nrow(all_)
#---------------------------------

uni_codes <- df$code  #length(uni_codes)


crime_per_000_people <- rates(data_sub, denomin=df, id_field=TRUE,
                              multiplier = 100)   #
head(crime_per_000_people)  #nrow(crime_per_000_people) #

geo_unique <- as.vector(crime_per_000_people$code)

agg_Data <- as.data.frame(cbind(V1=geo_unique, crime_per_000_people[,2:ncol(crime_per_000_people)]))  #head(agg_Data)
head(agg_Data)

##write.table(crime_per_000_people, file="crime_per_000_people_.csv", sep=",", row.names=F)
crime_per_000_people <- read.table(file="C:/Users/monsu/Documents/GitHub/Packages/inter_comparison_Micro/data2_2/crime_per_000_people_.csv",
                                   sep=",", head=TRUE)

crime_per_000_people <- crime_per_000_people[,1:(length(2002:(year_to+1)))]
head(df)

colnames(df) <- c("code", paste("p", 2002:year_to, sep="")) #
head(df)#nrow(all_)

#-------------------------
#-------------------------
#------------------------- #library(ggplot2)
#citywide trend
#to fill up infinite entries
dat_1 <- crime_per_000_people
#dat_1 <- data #head(dat_1)
total_crime <- NULL
#average trendline
for(a in 2:ncol(dat_1)){#111111 a<-13
  total_crime <- c(total_crime, mean(as.numeric(as.character(dat_1[ ,a]))))
}#


#daf2 = data.frame(date=c(rep(2002:2016, by=1)),
                  #value=total_crime)



ggplot(daf2, aes(date, value)) + geom_line(colour="black", lty=1, lwd=1)

#-------------------------
#combine
daty_ <- data.frame(Area=rep(c("Birm", "Glaz"), each=15),
                    Year=rep(c(2002:2016), 2),
                    Crime_per_thou_people=c(daf$value, daf2$value))

p <- ggplot(data=daty_, aes(x=Year, y= Crime_per_thou_people, group=Area)) +
  geom_line(aes(color=Area))+
  geom_point(aes(shape=Area, color=Area))

p <- p + theme_minimal()

p + theme(legend.position="right")


#-------------------------
#-------------------------

#running this to identify the outlier
#crime_per_000_people <- prop_crime_per000_people

#---------------------------------------------------------------
if(study_Area == "gl2" && crime_type == "PC_"){  #head(crime_per_000_people)
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
                                  digits=8, scale = 1)  #
head(prop_crime_per000_people) #prop_crime_per000_people[,2]

#multiply by 100
prop_crime_per000_people <- cbind(prop_crime_per000_people[,1], prop_crime_per000_people[,2:ncol(prop_crime_per000_people)]*100)
colnames(prop_crime_per000_people) <- c("code", paste("p", 2002:(year_to), sep="")) #

head(prop_crime_per000_people) #prop_crime_per000_people[,2]   #318/sum(as.numeric(as.vector(crime_per_000_people[,2])))

#prop_crime_per000_people <- prop_crime_per000_people[,2:ncol(prop_crime_per000_people)]
#head(prop_crime_per000_people)


data_backup <- prop_crime_per000_people
colnames(data_backup)<- c("ID", paste("X", 1:length(2002:year_to), sep="")) 

#clustering: 'akmedoids.clust' ##==========
cluster_output <- akmedoids.clust(prop_crime_per000_people, id_field = TRUE,
                                  method = "linear", k = c(3,20))

cluster_output <- akmedoids.clust(prop_crime_per000_people, id_field = TRUE,
                                 method = "linear", k = 6)
#retrieve cluster attributes: 'statPrint' ##========
clustr <- as.vector(cluster_output$optimSolution)
clustr <- as.vector(cluster_output$solution)
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

#-----------------------------------------
#deprivation data
#read the attribute table of the ESRI data (for 'study_area1'), including the population data
code <- uni_codes

#path to the data sets
dr_ <- "C:/Users/monsu/Documents/GitHub/Packages/inter_comparison_Micro/"

tsd <- tsd_ind_(code = code, unit = "LSOA", n_classes = 5, path=dr_)
head(tsd) #nrow(tsd)
















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




#----------------------------------------------------------
#code for townsend index


path="C:/Users/monsu/Documents/GitHub/Packages/inter_comparison_Micro/"

#getwd()
#---------------------------------------------------------
#to calculate deprivation (poverty) index 2011 for England
#---------------------------------------------------------

#get the lsoa code of England.
#read the attribute table of the ESRI data (for 'study_area1'), including the population data
df <- read.dbf(paste("C:/Users/monsu/Documents/GitHub/Packages/v013/data2/shp1/",
                     study_Area, "LSOA_", "pop_02_12.dbf", sep=""))#
head(df)
nrow(df)
code <- df$code

tsd_ind_ <- function(code = code, unit = "LSOA", n_classes = 10, path=path){

  #import the 2011 data
  if(unit == "LSOA"|unit == "DZ"){
    ukdata_ <- read.csv(file=paste(path, "Dataset- 2011 UK LSOA.csv", sep=""), sep=",", head=TRUE) #head(ukdata_)  #nrow(ukdata_)
  }

  if(unit == "OA"|unit == "Output Area"){
    ukdata_ <- read.csv(file=paste(path, "Dataset- 2011 UK Output Area.csv", sep=""), sep=",", head=TRUE) #head(ukdata_)  #nrow(ukdata_)
  }

  #crop the england uk
  ukdata_ <- ukdata_[which(ukdata_$GEO_CODE %in% code), ] #nrow(ukdata_)#head(ukdata_)

  carperc<-((ukdata_$F991/ukdata_$F989)*100)
  homeperc<-(((ukdata_$F2357+ukdata_$F2362+ukdata_$F2368)/ukdata_$F2347)*100)
  overperc<-(((ukdata_$F2081+ukdata_$F2083)/ukdata_$F2075)*100)
  unempperc<-((ukdata_$F248/ukdata_$F244)*100)
  overperc[is.na(overperc)] <- 0
  unempperc[is.na(unempperc)] <- 0
  carperc[is.na(carperc)] <- 0
  homeperc[is.na(homeperc)] <- 0
  lnoverperc<-(log(overperc+1))
  lnunempperc<-(log(unempperc+1))
  zcar<-((carperc-mean(carperc))/sd(carperc))
  zhome<-((homeperc-mean(homeperc))/sd(homeperc))
  zover<-((lnoverperc-mean(lnoverperc))/sd(lnoverperc))
  zunemp<-((lnunempperc-mean(lnunempperc))/sd(lnunempperc))
  TDS<- (zcar + zhome + zover + zunemp)
  ukdata_$TDS<-TDS
  quintile <- cut(TDS, quantile(TDS, probs=0:n_classes/n_classes, na.rm=TRUE), include.lowest=TRUE, labels=FALSE)
  #quintile <- cut(TDS, quantile(TDS, probs=0:n_classes/n_classes, na.rm=TRUE), include.lowest=TRUE, labels=FALSE)
  Townsendscores<-data.frame(ukdata_$GEO_CODE, ukdata_$GEO_LABEL, ukdata_$TDS, quintile)
  names(Townsendscores)[names(Townsendscores) == "ukdata_.GEO_CODE"]<-"geo_code"
  names(Townsendscores)[names(Townsendscores) == "ukdata_.GEO_LABEL"]<-"geo_label"
  names(Townsendscores)[names(Townsendscores) == "ukdata_.TDS"]<-"TDS"
  #write.csv(Townsendscores, "insertfilename.csv")
  #write.table(Townsendscores, file="england_TSD_2011.csv", sep=",", row.names = F)
  head(Townsendscores) #nrow(Townsendscores)
  return(Townsendscores)
  #------------------------------------------------------------------
}

