#Script for attaching land cover data to points

rm(list=ls())

library(terra)
library(tidyterra)
library(FedData)
library(ggplot2)
library(tigris)
library(raster) 
library(ggplot2)
library(dplyr)
library(lubridate)

#path for mac
NOAAFlashFlood<-read.csv("/Volumes/GEOG331_F25/kmwright/data/Project_Data/allFFeventsGrand.csv")

NOAAFlashFlood_clean<-NOAAFlashFlood[
  !is.na(NOAAFlashFlood$BEGIN_LON)&!is.na(NOAAFlashFlood$BEGIN_LAT),
]

FFPoints <- vect(NOAAFlashFlood_clean, geom=c("BEGIN_LON", "BEGIN_LAT"), crs="EPSG:4326")

#path for mac
Grand_County_UT<-vect("/Volumes/GEOG331_F25/kmwright/data/Project_Data/Grand_County")
#Use tigris to read in Grand County Shape file, simple 
Grand_County_Tigris<-counties(state="UT",cb=TRUE) %>%
  filter(NAME=="Grand")
#vector of tigris data
grand_vect<-vect(Grand_County_Tigris)
#read in land cover data (NLCD), 2024
#path for mac
nlcd_2024<-rast("/Volumes/GEOG331_F25/kmwright/data/Project_Data/Annual_NLCD_LndCov_2024_CU_C1V1_mi7m3sagei6es9.tiff")
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

FFPoints_proj <- project(FFPoints, crs(nlcd_masked))

ext(FFPoints_proj)
ext(nlcd_masked)

FFPoints_proj$LC_2024 <- extract(nlcd_masked, FFPoints_proj)[,1]
FFPoints_proj$LC_2024_LABEL <- factor(
  FFPoints_proj$LC_2024,
  levels = nlcd_classes,
  labels = nlcd_labels
)
table(FFPoints_proj$LC_2024_LABEL, useNA="ifany")







