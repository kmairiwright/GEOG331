#Final project script for various plots (not nlcd maps)
#MOSTLY NOAA DATA
#KW 12/08/25-

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

###Plot property damage over time###

#create month column, number and name
NOAAFlashFlood$Months<-as.integer(format(as.Date(NOAAFlashFlood$BEGIN_DATE),"%m"))
NOAAFlashFlood$Month_Name<-months(as.Date(NOAAFlashFlood$BEGIN_DATE))
#create year column
NOAAFlashFlood$Year<-as.integer(format(as.Date(NOAAFlashFlood$BEGIN_DATE), "%Y"))

  ###plot property damage by year###

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

  ###Plot property damage by month###

ggplot(damage_by_month, aes(x=Month_Name,y=DAMAGE_PROPERTY_NUM/1e5))+
  geom_col(fill="darkred")+
  labs(title = "Flash Flood Property Damage by Month (1997-2024)",
       x="Month",
       y="Total Property Damage (hundreds of thousands $)")+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45,hjust=1))