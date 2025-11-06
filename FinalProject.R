#Final Project Script#
#KW 10/24/25-

rm(list=ls())
#install.packages(c("ggplot2"))
#install.packages(c("tidyverse"))                 
#install.packages(c("FedData"))
#install.packages(c("tigris"))
#install.packages(c("sf"))
#install.packages(c("sp"))
#install.packages(c("raster"))
library(raster)
library(FedData)
library(sp)
library(tigris)
library(sf)
library(dplyr)



#NOAA data from 1/1/2001 to 12/31/2021 of flash floods
#in Grand County, Utah
NOAAFlashFlood<-read.csv("Z:\\kmwright\\data\\Project_Data\\storm_data_search_results.csv")

#create histogram of flash flood reports, monthly
NOAAFlashFlood$BEGIN_DATE<-as.Date(NOAAFlashFlood$BEGIN_DATE,format="%m/%d/%Y")

ggplot(data=NOAAFlashFlood,
      aes(x=BEGIN_DATE))+
      geom_histogram(binwidth=30, fill="darkblue")+
      labs(title="Grand County Flash Flood Reports (2001-2021), Monthly",
           x="Date",
           y="Count")

#Land cover dataset using FedData
Grand_County_UT<-st_read("Z:\\kmwright\\data\\Project_Data\\Grand_County")

Grand_County_Tigris<-counties(state="UT",cb=TRUE) %>%
  dplyr::filter(NAME=="Grand")

Grand_County_Tigris_sp<-as(Grand_County_Tigris, "Spatial")

Grand_2015<-get_nlcd(template=Grand_County_Tigris_sp,
                     label="Grand County UT",
                     year=2015,
                     extraction.dir="Z:\\kmwright\\data\\Project_Data")


