#code to create matrix of crime dataset from GM_police_crime_uk data
alt_data <- read.table(file="C:/Users/monsu/Documents/MMU DOCUMENTS/wm_bm_LSOA_PC_01_12.csv", sep=",", head=TRUE)

imp <- read.csv(file="GM_police_crime_uk.csv", sep=",", head=TRUE)

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

GM_crime_data <- final_data

save(GM_crime_data, file="data/GM_crime_data.rda")


