##&&&&&&&& 2 EDITEDDDD!!

for(q in 1:1){
  
#unit<- "LSOA"
  

  part2 <- clustr

  if(study_Area=="wm_bm_" & unit=="LSOA"){
    #grp_ <- list(c(1:3), c(4:6), c(7:9)) #for b LSOA##repeat no removal but 3,3,3  ...223 remove
    grp_ <- list(c(1:2), c(3:7), c(8:13)) #no removal
    colours <- c(rep("#00FF00", 2), rep("yellow",5), rep("red", 6)) #bm....LSOA
  }
  
  if(study_Area=="wm_bm_" & unit=="OA"){
    #grp_ <- list(c(1:3), c(4:6), c(7:9)) #for b LSOA##repeat no removal but 3,3,3  ...223 remove
    grp_ <- list(c(1:3), c(4:5), c(6:8)) #no removal
    colours <- c(rep("#00FF00", 3), rep("yellow",2), rep("red", 3)) #bm....LSOA
  }
  
  
  if(study_Area=="gl2_" & unit=="LSOA"){
  ## grp_ <- list(c(1), c(2,3,4), c(5,6,7,8,9)) #for burg #7
  grp_ <- list(c(1:3), c(4:6), c(7:9)) #for b LSOA##repeat no removal but 3,3,3  ...223 remove
  colours <- c(rep("#00FF00", 3), rep("yellow",3), rep("red", 3)) #bm....LSOA
  }
  
  if(study_Area=="gl2_" & unit=="OA"){
    ## grp_ <- list(c(1), c(2,3,4), c(5,6,7,8,9)) #for burg #7
    grp_ <- list(c(1:5), c(6:11), c(12:17)) #for b LSOA##repeat no removal but 3,3,3  ...223 remove
    colours <- c(rep("#00FF00", 5), rep("yellow",6), rep("red", 6)) #bm....LSOA
  }
  
  
  #grp_ <- list(c(1:3), c(4:5), c(6:9)) #for for glasgow ''confirmed!

  # part2 <- affectIndivC(data, cluster_center_List_ALL[[7]])  #length(which(part2=="3"))
  #resort data
  combined_data.append_Result <- cbind(geo_unique, part2)  #head(combined_data.append_Result)
  
  #rownames(data.append) <- 1:nrow(data.append)
  data.append <- cbind(1:nrow(prop_crime_per000_people), 
                       prop_crime_per000_people[,2:ncol(prop_crime_per000_people)]) #
  head(data.append) #data.append <- data  #data <- 
  
  #data.append <- data  #testing the proportion
  
  colnames(data.append) <- c("code", 1:length(2002:year_to))
  data.append <- as.data.frame(data.append)  #
  head(data.append)
  
  
  data.long.melted <- melt(data.append, id="code")#head(data., id="code"
  head(data.long.melted)
  
  #data.append <- cbind(data, part2)
  #data.long.melted <- melt(data, id="code")#head(d
  data.long.melted <- cbind(data.long.melted, rep(part2, length(2002:year_to)))#head(data.long.melted)
  
  colnames(data.long.melted) <- c("OAcode","Year","Crime_Count", "clusters")
#}
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


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



head(data.long.melted)##data.long.melted[1:50,]

#use this to calculate percentage (%) change from year 1 to year n
##year_uni <- as.vector(unique(data.long.melted$Year))
##order_Cluster <- as.vector(unique(data.long.melted$clusters))
##clusters_uni <- order_Cluster[order(as.vector(unique(data.long.melted$clusters)))]
##clusters_uni

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
#---------


#------------------------------------------------------------------
##print(statPrint(part2, prop_crime_per000_people, id_field=TRUE, reference = 1,
                ##N.quant = 8, type="lines", y.scaling="free"))

##print(statPrint(part2, prop_crime_per000_people, id_field=TRUE, reference = 1,
                ##N.quant = 8, type="stacked", y.scaling="free"))
#-------------------------------------------------------------------
##}

#Calculating the number of trajectories  grp_
##length(which(part2%in%grp_[[3]]))
##length(which(part2%in%grp_[[3]]))/length(part2) * 100


#create data frame for use with plot
#grp.dat<-matrix(grp.dat,nrow=length(t.step),ncol=length(grps))
grp.dat<-data.frame(change_ave_yr_ALL,row.names=1:length(2002:year_to))
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


colfunc <- colorRampPalette(c("green", "yellow", "red"))
#colours <- colfunc(4)


#birmingham
##colours <- c(rep("#00FF00", 2), rep("yellow",1), rep("red", 2)) #burglary grp_
##colours <- c(rep("#00FF00", 3), rep("yellow",2), rep("red", 4)) #bm....LSOA
##colours <- c(rep("#00FF00", 3), rep("yellow",3), rep("red", 3)) #bm....LSOA do no removal but with 3,3,3

#birmingham
##colours <- c(rep("#00FF00", 3), rep("yellow",3), rep("red", 3)) #burglary grp_

#birmingham LSOA
##grp_ <- list(c(1,2), c(3,4), c(5,6,7)) #for b
##colours <- c(rep("#00FF00", 2), rep("yellow", 2), rep("red", 3)) 
##colours <- c(rep("#00FF00", 2), rep("yellow", 2), rep("red", 2))  #BM OA #length(2002:year_to)

Year <- as.character(c(2002:year_to))

#colours <- 
dev.new()

#Sys.sleep(2)

p <- ggplot(p.dat,aes(x=step,y=value)) + theme(legend.position="none")  #grp_

p + geom_area(aes(fill=variable), colour = "gray5", position='fill', size = 0.5) + 
  scale_fill_manual(values = colours) +
  scale_x_continuous(breaks=1:length(Year), labels=Year) + theme_light()

#removing the sub-grouplines
p + geom_area(aes(fill=variable), colour = 'NA', position='fill', size = 0.5) + 
  scale_fill_manual(values = colours) +
  scale_x_continuous(breaks=1:length(Year), labels=Year) + theme_light()


#-----------------------------------------------------

clusters <- part2
#calculate the cumulative rate of change (first difference) of the mean value ......data.long.melted
clusters <- clusters[1:nrow(prop_crime_per000_people)]

data_reMelt <- dcast(data.long.melted, OAcode ~ Year, value.var='Crime_Count')  #data_reMelt[1:10,]
data_reMelt <- cbind(data_reMelt, clusters) #head(data_reMelt)

cluster_Group <- as.vector(unique(data_reMelt$clusters)[order(unique(data_reMelt$clusters))])


#---------------------------------------------------------------------

#mmD ----------------------------------------------
##data_reMelt <- dcast(data.long.melted, OAcode ~ Year, value.var='Crime_Count')
##data_reMelt <- cbind(data_reMelt, clusters) #head(data_reMelt)

##cluster_Group <- as.vector(unique(data_reMelt$clusters)[order(unique(data_reMelt$clusters))])
#---------------------------------------------------------------------------
#plot each cluster....
#---------------------------------------------------------------------------
#clusters <- clusters[1:nrow(prop_crime_per000_people)]

#data_reMelt <- dcast(data.long.melted, OAcode ~ Year, value.var='Crime_Count')
#data_reMelt <- cbind(data_reMelt, clusters) #head(data_reMelt)

#cluster_Group <- as.vector(unique(data_reMelt$clusters)[order(unique(data_reMelt$clusters))])

length_Of_Clusters <- NULL

cluster_Units_ALL <- list()


for(i in 1:length(cluster_Group)){ #i<-1
  
  data_reMelt_Cut <- data_backup[which(data_reMelt$clusters==cluster_Group[i]),]#head(data_reMelt)
  
  if(is.null(data_reMelt_Cut)){data_reMelt_Cut<-matrix(data_reMelt_Cut,,(length(Year)+1))
  colnames(data_reMelt_Cut) <- c("code", as.character(1:length(Year)))
  }
  #get the corresponding OA label #head(data)# head(agg_Data[1:10,])
  cluster_Units <- as.vector(agg_Data$V1[as.data.frame(data_reMelt_Cut)$ID])
  
  length_Of_Clusters <- rbind(length_Of_Clusters, cbind(i, length(cluster_Units)))
  
  cluster_Units_ALL[i] <- list(cluster_Units)
}

# par()#getwd()

#read-in the polygon shapefiles
#WM_LSOA <- readOGR(dsn=".", "Birmingham_normal_LSOA_osgb")  #head(WM_LSOA)#BM_Centroids
#WM_LSOA_ <- readOGR(dsn=".", "BM_Points")  #head(WM_LSOA@data)
#WM_LSOA <- readOGR(dsn=".", "Birmingham_hexmap_LSOA_osgb") 
##plot(WM_LSOA, pch=1)
#WM_LSOA <- readOGR(dsn=".", "Birmingham_normal_LSOA_osgb")  #head(WM_LSOA) "gl_"
#WM_LSOA <- readOGR(dsn=".", "Birmingham_OA_osgb")  #head(WM_LSOA@data)
#WM_LSOA <- readOGR(dsn=".", paste(study_Area, "geo", sep="")) #geo


#not sure for glasgow
##grp_ <- list(c(1:3), c(4:6), c(7:9)) #for b
##colours <- c(rep("#00FF00", 2), rep("yellow", 2), rep("red", 2)) #violenct

if(unit=="LSOA"){
setwd("C:/Users/monsu/Documents/UNI DESKTOP BACKUP _13062019/desktop/FoSS Slides_/")
WM_LSOA <- readOGR(dsn=".", paste(study_Area, "LSOA_hex", sep="")) #geo_unit_with_POPData   #getwd()
city_Centre <- readOGR(dsn=".", paste(study_Area, "citycentre2_2", sep="")) #geo_unit_with_POPData  
}

if(unit=="OA"){
  setwd("C:/Users/monsu/Documents/UNI DESKTOP BACKUP _13062019/desktop/FoSS Slides_/")
  WM_LSOA <- readOGR(dsn=".", "hex_") #geo_unit_with_POPData #getwd()
  city_Centre <- readOGR(dsn=".", "hexcitycentre") #geo_unit_wit
}
#WM_LSOA <- readOGR(dsn=".", "Birmingham_hexmap_LSOA_osgb")

##WM_LSOA <- readOGR(dsn=".", "BM_Centroids")  #head(WM_LSOA@data)  #plot(WM_LSOA, border="grey", pch=1)

#par(mfrow=c(3,4))
#par(oma=c(0,0,0,0), mar=c(0,0,0,0), mfrow=c(3,1))

par(oma=c(0,0,0,0), mar=c(0,0,0,0), mfrow=c(1,1))


#------------------------------------------------------------------
print(statPrint(part2, prop_crime_per000_people, id_field=TRUE, reference = 1,
                N.quant = 8, type="lines", y.scaling="free"))

##print(statPrint(part2, prop_crime_per000_people, id_field=TRUE, reference = 1,
                ##N.quant = 8, type="stacked", y.scaling="free"))
#-------------------------------------------------------------------

##}
#using my new optimal matrix


##grp_ <- list(c(1:3), c(4:5), c(6:7)) #for b
##colours <- c(rep("#00FF00", 3), rep("yellow", 2), rep("red", 2)) #violenct

sp::plot(WM_LSOA, col="grey", border="grey", pch=16)

#plotting for gradient colour#dev.new()
#--------------------------------------------
cc_ <- 0
countt_ <- 0
#spatial patterning
for(q in 1:length(grp_)){ #q<-3
  
  #for(p in 1:length(cluster_Units_ALL)){ 
  
  #combine
  colat_<- NULL
  
  for(f in 1:length(grp_[[q]])){ # f<-1
    cc_ <- cc_ + 1
    ##colat_ <- c(colat_, cluster_Units_ALL[[grp_[[q]][f]]])
    colat_ <- cluster_Units_ALL[[grp_[[q]][f]]]
    print(length(cluster_Units_ALL[[grp_[[q]][f]]]))
    #}
    
    sp::plot(WM_LSOA[which(as.vector(WM_LSOA$code)%in%colat_),], add=TRUE, col=colours[cc_], border="grey50", pch=16) #check that OA field is correct
    
  }
  #countt_ <- countt_ + length(unlist(grp_[c(q)]))
  
}
#}

sp::plot(city_Centre, col="NA", border="black", pch=16, add=TRUE, lwd=3)



##deprivation analysis...

if(unit=="LSOA"){
#---------------------------------------------------
import_map_ <- readOGR(dsn=".", paste(study_Area, "LSOA_hex", sep="")) #geo_unit_with_POPData  
import_map_ <- import_map_@data
head(import_map_)
}
#lot(import_map)

if(study_Area=="wm_bm_" & unit=="OA"){
  #---------------------------------------------------
  import_map_ <-  readOGR(dsn=".", "hex_") #geo_unit_with_POPData #getwd(
  import_map_ <- import_map_@data
  head(import_map_)
}

if(study_Area=="gl2_" & unit=="OA"){
  #---------------------------------------------------
  import_map_ <-  readOGR(dsn=".", "gl_hex_oa") #geo_unit_with_POPData #getwd(
  import_map_ <- import_map_@data
  head(import_map_)
}




#TSD_Index <- read.table(file = "TSD_birmingham_2001_2011.csv", sep=",", head = TRUE) #bi
#TSD_Index <- read.table(file = "TSD_glasgow_2001_2011.csv", sep=",", head = TRUE) #bi
#path to the data sets

dr_ <- "C:/Users/monsu/Documents/GitHub/Packages/inter_comparison_Micro/"

##TSD_Index <- tsd_ind_(code = uni_codes, unit = "LSOA", n_classes = 5, path=dr_)
##head(TSD_Index) #nrow(tsd)

if(study_Area=="wm_bm_" & unit=="LSOA"){
##TSD_Index <- read.table(file = "C:/Users/monsu/Documents/UNI DESKTOP BACKUP _13062019/desktop/FoSS Slides_/TSD_birmingham_2001_2011.csv", 
                        ##sep=",", head = TRUE) #bi #getwd()
#TSD_Index <- read.table(file = "TSD_glasgow_2001_2011.csv", sep=",", head = TRUE) #bi
##head(TSD_Index)  #TOWNSNDRANK2001; TOWNSNDRANK2011 #nrow(TSD_Index)
##colnames(TSD_Index) <- c("geo_code","TSD01", "TOWNSNDRANK2001","TSD11", "quintile")
##head(TSD_Index) 
TSD_Index <- tsd_ind_(code = uni_codes, unit = "LSOA", n_classes = 5, path=dr_)
head(TSD_Index) #nrow(tsd)
}
#quintile

if(study_Area=="wm_bm_" & unit=="OA"){ 
  TSD_Index <- tsd_ind_(code = uni_codes, unit = unit, n_classes = 5, path=dr_)
  head(TSD_Index) #nrow(tsd)
}


if(study_Area=="gl2_" & unit=="LSOA"){ 
  TSD_Index <- tsd_ind_(code = uni_codes, unit = unit, n_classes = 5, path=dr_)
  head(TSD_Index) #nrow(tsd)
  }

if(study_Area=="gl2_" & unit=="OA"){ 
  TSD_Index <- tsd_ind_(code = uni_codes, unit = unit, n_classes = 5, path=dr_)
  head(TSD_Index) #nrow(tsd)
}

import_map_ <- data.frame(code=matrix(uni_codes,,1))
head(import_map_)

head(TSD_Index)  #TOWNSNDRANK2001; TOWNSNDRANK2011 #nrow(TSD_Index)

#import the deprivation data and join to shapefile...
import_map <- merge(import_map_, TSD_Index, by.x="code", by.y="geo_code") #for birmingham

#import_map <- import_map@data #
head(import_map) #import_map[which(import_map$code==""),]

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
head(list_D)
#NOTEEEEEEE! please, not that the deprivation class annotated as 1 is actually 5
#the arrangement below takes care of it..... when I call "5", I am calling the most deprived.....
dep_Classes <- c(5, 4, 3, 2, 1)

both_Yr <- list()

for(i in 1:length(dep_Classes)){#i<-2

      dat_dep_1 <- list_D[which(list_D$quintile==dep_Classes[i]),] #head(dat_dep_1)
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
      both_Yr[i] <- list(sub_list_D1)
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

store_Mean_ <- as.data.frame(cbind(store_Mean, c("Decr.", "Stab.", "Incr.")))
head(store_Mean_)
colnames(store_Mean_) <- c("1st", "2nd", "3rd", "4th", "5th", "unit")
head(store_Mean_)

write.table(store_Mean_, file="store_Mean.csv", sep=",", row.names = F)

#Sys.sleep(2)

store_Mean <- read.table(file="store_Mean.csv", sep=",", head=TRUE)
colnames(store_Mean) <- c("DQ1", "DQ2", "DQ3", "DQ4", "DQ5", "unit")
head(store_Mean)

#convert to percentage(%)
for(h in 1:(ncol(store_Mean)-1)){#h<-1
  t_s <- sum(store_Mean[ ,h])
  for(g in 1:nrow(store_Mean)){#g<-1
    store_Mean[g,h] <- round((store_Mean[g,h]/t_s), digits = 3)*100
  }
}

store_Mean  
  
  
var_prop

#plotting stacked histograme
#library(tidyr)
#library(plyr)



full.results <- store_Mean

full.results.long <- gather(full.results, value = "var_prop", key = "year", -unit)
head(full.results.long)

full.results.long$unit <- factor(full.results.long$unit, levels = c("Decr.", "Stab.", "Incr."))
head(full.results.long)

#re-orienting the label ##customised

jus_ <- full.results.long
jus_1 <- jus_[1,3]
jus_3 <- jus_[3,3]
jus_[1,3] <- jus_3
jus_[3,3] <- jus_1

jus_4 <- jus_[4,3]
jus_6 <- jus_[6,3]
jus_[4,3] <- jus_6
jus_[6,3] <- jus_4

jus_7 <- jus_[7,3]
jus_9 <- jus_[9,3]
jus_[7,3] <- jus_9
jus_[9,3] <- jus_7

jus_10 <- jus_[10,3]
jus_12 <- jus_[12,3]
jus_[10,3] <- jus_12
jus_[12,3] <- jus_10

jus_13 <- jus_[13,3]
jus_15 <- jus_[15,3]
jus_[13,3] <- jus_15
jus_[15,3] <- jus_13

#}


dev.new()

p <- ggplot(full.results.long) + geom_bar(aes(x = year, y = var_prop, fill = unit), stat = "identity", alpha = 0.98) +
  theme_light() + scale_fill_manual(values = c("green","yellow","red")) +
  scale_x_discrete(expand = c(0.06,0)) + scale_y_continuous(expand = c(0,0)) + 
  labs(x = "Year", y = "Variance proportion", caption = "Figure 3: variance parition by spatial scale")  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=10),
        axis.text.y = element_text(size=10),
        plot.caption = element_text(size=7, hjust = 1),
        legend.background = element_rect(linetype = 1, size = 0.5, colour = 1)) +
        geom_text(data=jus_, aes(x = year, y = var_prop, label = paste(var_prop, "%", sep="")), 
          size = 3.5, position = position_stack(vjust = 0.5))

#Sys.sleep(10)

p


}


p



