#Final Project Script#
#KW 10/24/25-

rm(list=ls())

library(terra)
library(tidyterra)
library(FedData)
library(ggplot2)
library(tigris)
library(raster) 

#NOAA data from 1/1/2001 to 12/31/2021 of flash floods
#in Grand County, Utah
NOAAFlashFlood<-read.csv("/Volumes/GEOG331_F25/kmwright/data/Project_Data/storm_data_search_results.csv")

###HISTOGRAM###

#create histogram of flash flood reports, monthly
NOAAFlashFlood$BEGIN_DATE<-as.Date(NOAAFlashFlood$BEGIN_DATE,format="%m/%d/%Y")

ggplot(data=NOAAFlashFlood,
       aes(x=BEGIN_DATE))+
  geom_histogram(binwidth=30, fill="darkblue")+
  labs(title="Grand County Flash Flood Reports (2001-2021), Monthly",
       x="Date",
       y="Count")



###LAND COVER PLOT###

#Read in Grand County TGER/Line Shape file, 2024
Grand_County_UT<-vect("/Volumes/GEOG331_F25/kmwright/data/Project_Data/Grand_County")

#Use tigris to read in Grand County Shape file, simple 
Grand_County_Tigris<-counties(state="UT",cb=TRUE) %>%
  filter(NAME=="Grand")

#vector of tigris data
grand_vect<-vect(Grand_County_Tigris)

#read in land cover data (NLCD), 2016
nlcd_2016<-rast("/Volumes/class/GEOG331_F25/kmwright/data/Project_Data/Annual_NLCD_LndCov_2016_CU_C1V1/Annual_NLCD_LndCov_2016_CU_C1V1.tif")
#transform coord reference system to match
grand_vect<-project(grand_vect, crs(nlcd_2016))

#crop and mask NLCD to Grand County 
nlcd_cropped<-crop(nlcd_2016, grand_vect)
nlcd_masked<-mask(nlcd_cropped, grand_vect)

#classify NLCD classes with labels and colors
nlcd_classes <- c(11, 12, 21, 22, 23, 24, 31, 41, 42, 43, 52, 71, 81, 82, 90, 95)
nlcd_labels <- c(
  "Open Water", "Perennial Ice/Snow", "Developed, Open Space", "Developed, Low Intensity",
  "Developed, Medium Intensity", "Developed, High Intensity", "Barren Land",
  "Deciduous Forest", "Evergreen Forest", "Mixed Forest", "Shrub/Scrub", "Grassland/Herbaceous",
  "Pasture/Hay", "Cultivated Crops", "Woody Wetlands", "Emergent Herbaceous Wetlands"
)
nlcd_colors <- c(
  "#476BA1", "#D1DEF8", "#DECACA", "#D89382", "#ED0000", "#AA0000", "#B2ADA3",
  "#68AB5F", "#1C5F2C", "#B5CA8F", "#A3CC51", "#DCD939", "#D1D182", "#A3A3A3",
  "#BAD8EA", "#70A3BA"
)

#Lat/Long coordinates form NOAAFlashFlood
FFPoints<-vect(NOAAFlashFlood,geom=c("BEGIN_LON","BEGIN_LAT"), crs = "EPSG:4326")
FFPoints_proj<-project(FFPoints,crs(nlcd_2016))

#create a two panel layout for legend to read clearly
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1)) 

#plot NLCD of Grand County
plot(nlcd_masked, 
     col = nlcd_colors, 
     breaks = c(nlcd_classes, 100), 
     legend=FALSE,
     main = "Grand County, UT NLCD 2016")
#Plot FF points on NLCD
points(FFPoints_proj, col="black",pch=16)


#plot the legend
#remove the margins
par(mar = c(0, 0, 0, 0))
plot.new()
legend("center", 
       legend = nlcd_labels, 
       fill = nlcd_colors, 
       cex = 0.8, 
       bty = "n")
