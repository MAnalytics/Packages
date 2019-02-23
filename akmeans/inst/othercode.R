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

gm.crime.sample1 <- read.table(file="C:/Users/monsu/Documents/GitHub/Packages/samp.csv", sep=",", head=FALSE)

save(gm.crime.sample1, file="data/gm.crime.sample1.rda")

Calinski and Harabatz criterion code
References
[1] C. Genolini and B. Falissard "KmL: k-means for longitudinal data" Computational Statistics, vol 25(2), pp 317-328, 2010

[2] C. Genolini and B. Falissard "KmL: A package to cluster longitudinal data" Computer Methods and Programs in Biomedicine, 104, pp e112-121, 2011



