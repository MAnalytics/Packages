
library(foreign)
#subset the Birmingham OAs


dir1 <- "C:/Users/monsu/Documents/GitHub/Packages/inter_comparison_Micro/data1/"
#read the attribute table of the ESRI data (for 'study_area1'), including the population data

#import Birmigham shapefiles
df <- read.dbf(paste(dir1, "Birmingham_OA_osgb.dbf", sep="")) #head(df) #nrow(df)

#location code
loc_code <- as.vector(df$OAcode)

#subset the dataset using the code
#import the dataset
census_OA <- read.table(file=paste(dir1, "Dataset- 2011 UK Output Area.csv", sep=""), sep=",", head=TRUE)#

census_OA_data <- census_OA[which(census_OA$code%in%loc_code),] #head(census_OA_data)

#transfer
ukdata <- census_OA_data


#ukdata<-read.csv(file.choose(), header=TRUE)
carperc<-((ukdata$F991/ukdata$F989)*100)
homeperc<-(((ukdata$F2357+ukdata$F2362+ukdata$F2368)/ukdata$F2347)*100)
overperc<-(((ukdata$F2081+ukdata$F2083)/ukdata$F2075)*100)
unempperc<-((ukdata$F248/ukdata$F244)*100)
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
ukdata$TDS<-TDS
quintile <- cut(TDS, quantile(TDS, probs=0:5/5, na.rm=TRUE), include.lowest=TRUE, labels=FALSE)
Townsendscores<-data.frame(ukdata$code, ukdata$GEO_LABEL, ukdata$TDS, quintile)
names(Townsendscores)[names(Townsendscores) == "ukdata.code"]<-"geo_code"
names(Townsendscores)[names(Townsendscores) == "ukdata.GEO_LABEL"]<-"geo_label"
names(Townsendscores)[names(Townsendscores) == "ukdata.TDS"]<-"TDS"
write.csv(Townsendscores, "insertfilename.csv")
getwd()
