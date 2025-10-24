#Final Project Script#
#KW 10/24/25-

rm(list-ls())
#install.packages(c("ggplot2"))
#install.packages(c("tidyverse"))                 

library(ggplot2)

#NOAA data from 1/1/2001 to 12/31/2021 of flash floods
#in Grand County, Utah
NOAAFlashFlood<-read.csv("//Volumes/class/GEOG331_F25/kmwright/data/storm_data_search_results.csv")

NOAAFlashFlood$BEGIN_DATE<-as.Date(NOAAFlashFlood$BEGIN_DATE,format="%m/%d/%Y")

ggplot(data=NOAAFlashFlood,
      aes(x=BEGIN_DATE))+
      geom_histogram(binwidth=30, fill="darkblue")+
      labs(title="Monthly Grand County Flash Flood Reports (2001-2021)",
           x="Date",
           y="Count")
