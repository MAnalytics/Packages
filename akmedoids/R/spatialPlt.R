
library(rgdal)
library(ggplot2)

#clustr_with_id  a two-column matrix in which 1st unique field and 2nd column is the cluster membership

shp <- readOGR(dsn="C:/Users/monsu/Documents/GitHub/Packages/akmedoids/data", layer="cityA")
plot(shp, pch=1)

clustr <- as.vector(output$optimSolution)

#combine the location ids to their corresponding cluster membership
clustr_with_id <- as.data.frame(cbind(as.vector(prop_crime_per200_people$location_ids),
                                      clustr))
colnames(clustr_with_id) <- c("id", "gr_member")

#check the data
head(shp@data)


outlierDetect <- function(clustr_with_id, spatdata=shp, sField = "LSOAcod", class_gr = NULL){



}
