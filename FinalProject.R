#Final Project Script#
#KW 10/24/25-

rm(list=ls())

library(terra)
library(tidyterra)
library(FedData)
library(ggplot2)
library(tigris)
library(raster) 

#All NOAA data available (1997-2024)
#in Grand County, Utah

#path for mac
NOAAFlashFlood<-read.csv("/Volumes/GEOG331_F25/kmwright/data/Project_Data/allFFeventsGrand.csv")

#path for PC
#NOAAFlashFlood<-read.csv("Z:\\kmwright\\data\\Project_Data\\allFFeventsGrand.csv")

###HISTOGRAM###

#create histogram of flash flood reports, monthly
NOAAFlashFlood$BEGIN_DATE<-as.Date(NOAAFlashFlood$BEGIN_DATE,format="%m/%d/%Y")

ggplot(data=NOAAFlashFlood,
       aes(x=BEGIN_DATE))+
  geom_histogram(binwidth=30, fill="darkblue")+
  labs(title="Grand County Flash Flood Reports (1997-2024), Monthly",
       x="Date",
       y="Count")



###LAND COVER PLOT###

#Read in Grand County TGER/Line Shape file, 2024

#path for mac
Grand_County_UT<-vect("/Volumes/GEOG331_F25/kmwright/data/Project_Data/Grand_County")

#path for PC
#Grand_County_UT<-vect("Z:\\kmwright\\data\\Project_Data\\Grand_County")

#Use tigris to read in Grand County Shape file, simple 
Grand_County_Tigris<-counties(state="UT",cb=TRUE) %>%
  filter(NAME=="Grand")

#vector of tigris data
grand_vect<-vect(Grand_County_Tigris)

#read in land cover data (NLCD), 2024

#path for mac
nlcd_2024<-rast("/Volumes/GEOG331_F25/kmwright/data/Project_Data/Annual_NLCD_LndCov_2024_CU_C1V1_mi7m3sagei6es9.tiff")

#path for PC
#nlcd_2024<-rast("Z:\\kmwright\\data\\Project_Data\\Annual_NLCD_LndCov_2024_CU_C1V1_mi7m3sagei6es9.tiff")

#transform coord reference system to match
grand_vect<-project(grand_vect, crs(nlcd_2024))

#crop and mask NLCD to Grand County 
nlcd_cropped<-crop(nlcd_2024, grand_vect)
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
  "#476BA1", "#faf8f2", "#DECACA", "#D89382", "#ED0000", "#AA0000", "#B2ADA3",
  "#68AB5F", "#1C5F2C", "#B5CA8F", "#f7dd7c", "#DCD939", "#D1D182", "#917214",
  "#BAD8EA", "#70A3BA"
)

#Lat/Long coordinates form NOAAFlashFlood
FFPoints<-vect(NOAAFlashFlood,geom=c("BEGIN_LON","BEGIN_LAT"), crs = "EPSG:4326")
FFPoints_proj<-project(FFPoints,crs(nlcd_2024))

#Change some columns from integer to numeric
FFPoints_proj$DEATHS_DIRECT<-as.numeric(FFPoints_proj$DEATHS_DIRECT)
FFPoints_proj$DAMAGE_PROPERTY_NUM<-as.numeric(FFPoints_proj$DAMAGE_PROPERTY_NUM)

###Plot NLCD 2024###

#create a two panel layout for legend to read clearly
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1)) 

#plot NLCD of Grand County 2024
plot(nlcd_masked, 
     col = nlcd_colors, 
     breaks = c(nlcd_classes, 100), 
     legend=FALSE,
     main = "Grand County, UT NLCD 2024")
#Plot FF points on NLCD
points(FFPoints_proj, col="black",pch=16)

#plot legend for NLCD
#remove the margins
par(mar = c(0, 0, 0, 0))
plot.new()
# NLCD legend
legend("center",
       legend = c(nlcd_labels, "Flash Flood Start Point"),
       fill = c(nlcd_colors, rep(NA, 1)),       
       pch = c(rep(NA, length(nlcd_labels)), 16), 
       col = c(rep(NA, length(nlcd_labels)), "black"),
       pt.cex = 1.2, 
       cex = 0.8,
       bty = "n")


###plot flash flood points on 1997 NLCD###

#path for mac
nlcd_1997<-rast("/Volumes/GEOG331_F25/kmwright/data/Project_Data/Annual_NLCD_LndCov_1997_CU_C1V1_mi7m3sagei6es9.tiff")
#path for PC
#nlcd_1997<-rast("Z:\\kmwright\\data\\Project_Data\\Annual_NLCD_LndCov_1997_CU_C1V1_mi7m3sagei6es9.tiff")
#transform coord reference system to match
grand_vect<-project(grand_vect, crs(nlcd_1997))
#crop and mask NLCD to Grand County 
nlcd_cropped97<-crop(nlcd_1997, grand_vect)
nlcd_masked97<-mask(nlcd_cropped97, grand_vect)

#create a two panel layout for legend to read clearly
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1)) 

#plot NLCD of Grand County 1997
plot(nlcd_masked97, 
     col = nlcd_colors, 
     breaks = c(nlcd_classes, 100), 
     legend=FALSE,
     main = "Grand County, UT NLCD 1997")
#Plot FF points on NLCD
points(FFPoints_proj, col="black",pch=16)

#plot legend for NLCD
#remove the margins
par(mar = c(0, 0, 0, 0))
plot.new()
# NLCD legend
legend("center",
       legend = c(nlcd_labels, "Flash Flood Start Point"),
       fill = c(nlcd_colors, rep(NA, 1)),       
       pch = c(rep(NA, length(nlcd_labels)), 16), 
       col = c(rep(NA, length(nlcd_labels)), "black"),
       pt.cex = 1.2, 
       cex = 0.8,
       bty = "n")

#create a two panel layout for legend to read clearly
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1)) 


###Plot, on NLCD 2024, deaths and property damage###

#plot NLCD of Grand County 2024
plot(nlcd_masked, 
     col = nlcd_colors, 
     breaks = c(nlcd_classes, 100), 
     legend=FALSE,
     main = "Grand County, UT NLCD 2024")
#Plot FF points on NLCD
points(FFPoints_proj[FFPoints_proj$DAMAGE_PROPERTY_NUM==0], col="black",pch=16)
points(FFPoints_proj[FFPoints_proj$DAMAGE_PROPERTY_NUM>=1,FFPoints_proj$DEATHS_DIRECT==0],col="blue",pch=16)
points(FFPoints_proj[FFPoints_proj$DEATHS_DIRECT>=1], col="#DF00FE",pch=16)

#plot legend for NLCD
#remove the margins
par(mar = c(0, 0, 0, 0))
plot.new()
# NLCD legend
legend("center",
       legend = c(nlcd_labels, "Flash Flood Start Point", "Documented Property Damage", "Deaths"),
       fill = c(nlcd_colors, rep(NA, 1), rep(NA,1), rep (NA,1)),       
       pch = c(rep(NA, length(nlcd_labels)), 16, 16, 16), 
       col = c(rep(NA, length(nlcd_labels)), "black", "blue", "#DF00FE"),
       pt.cex = 1.2, 
       cex = 0.8,
       bty = "n")

###Plot property damage over time###

#create month column, number and name
NOAAFlashFlood$Months<-as.integer(format(as.Date(NOAAFlashFlood$BEGIN_DATE),"%m"))
NOAAFlashFlood$Month_Name<-months(as.Date(NOAAFlashFlood$BEGIN_DATE))
#create year column
NOAAFlashFlood$Year<-as.integer(format(as.Date(NOAAFlashFlood$BEGIN_DATE), "%Y"))

#plot property damage by year
ggplot(NOAAFlashFlood, aes(x=Year,y=DAMAGE_PROPERTY_NUM/1e5)) +
  geom_col(fill="blue4") +
  labs(title="Total Property Damage by Year",
       x="Year", y="Property Damage (hundreds of thousands $)") +
  theme_minimal()

#create new dataframe to look at aggregate damage per month
damage_by_month<-aggregate(DAMAGE_PROPERTY_NUM~Month_Name,
                                          data=NOAAFlashFlood,
                                          FUN=sum,na.rm=TRUE)
month_order<-month.name
damage_by_month$Month_Name<-factor(damage_by_month$Month_Name,
                                   levels=month_order)
damage_by_month<-damage_by_month[order(damage_by_month$Month_Name),]

ggplot(damage_by_month, aes(x=Month_Name,y=DAMAGE_PROPERTY_NUM/1e5))+
  geom_col(fill="darkred")+
    labs(title = "Flash Flood Property Damage by Month (1997-2024)",
         x="Month",
         y="Total Property Damage (hundreds of thousands $)")+
    theme_minimal()+
    theme(axis.text.x=element_text(angle=45,hjust=1))




#Notes/to do: create plot of years/months with property damage flash floods
#create plots that looks at whether higher levels of property damage, deaths, 
#and frequent flash floods are spatially correlated

