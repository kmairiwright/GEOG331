#Final project script for various plots (not nlcd maps or nlcd related plots)
#MOSTLY NOAA DATA
#KW 12/08/25-12/15/25

rm(list=ls())

library(ggplot2)
library(dplyr)
library(lubridate)

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

ggplot(NOAAFlashFlood, aes(x=Year,y=DAMAGE_PROPERTY_NUM/1e6)) +
  geom_col(fill="blue4") +
  labs(title="Total Property Damage by Year",
       x="Year", y="Property Damage (millions)") +
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

ggplot(damage_by_month, aes(x=Month_Name,y=DAMAGE_PROPERTY_NUM/1e6))+
  geom_col(fill="darkred")+
  labs(title = "Flash Flood Property Damage by Month (1997-2024)",
       x="Month",
       y="Total Property Damage (millions)")+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45,hjust=1))

###are flash-flood frequency and property damage correlated?###

#FF counts per year
FF_year_counts <- NOAAFlashFlood %>%
  group_by(Year) %>%
  summarize(
    FLOOD_COUNT = n(),
    TOTAL_DAMAGE = sum(DAMAGE_PROPERTY_NUM, na.rm = TRUE),
    DEATHS = sum(DEATHS_DIRECT, na.rm = TRUE)
  )

#flash flood frequency vs property damage plot
ggplot(FF_year_counts, aes(x = FLOOD_COUNT, y = TOTAL_DAMAGE/1e6)) +
  geom_point(color="purple3", size=3) +
  geom_smooth(method="lm", se=FALSE, color="black") +
  labs(title="Relationship Between Flash-Flood Frequency and Property Damage",
       x="Number of Flash Floods in Year",
       y="Total Property Damage (millions)") +
  theme_minimal()

#correlation values
cor(FF_year_counts$FLOOD_COUNT, FF_year_counts$TOTAL_DAMAGE, use = "complete.obs")
cor(FF_year_counts$DEATHS, FF_year_counts$TOTAL_DAMAGE, use = "complete.obs")
cor(FF_year_counts$FLOOD_COUNT, FF_year_counts$DEATHS, use = "complete.obs")

cor.test(FF_year_counts$FLOOD_COUNT, FF_year_counts$TOTAL_DAMAGE)

#property damage vs deaths?
#not a great graph, and no real correlation
ggplot(NOAAFlashFlood, aes(x = DAMAGE_PROPERTY_NUM/1e6, y = DEATHS_DIRECT)) +
  geom_point(alpha=0.6, color="red3") +
  geom_smooth(method="lm", se=FALSE) +
  labs(title="Relationship Between Property Damage and Deaths",
       x="Property Damage (millions)", y="Deaths") +
  theme_minimal()

#dual axis plot

monthly_summary <- NOAAFlashFlood %>%
  group_by(Month_Name) %>%
  summarize(
    Flood_Count = n(),
    Total_Damage = sum(DAMAGE_PROPERTY_NUM, na.rm = TRUE)
  )

# Dual-axis plot (secondary y-axis for damage)
NOAAFlashFlood$Month_Name <- factor(NOAAFlashFlood$Month_Name,
                                    levels = month.name,  # Jan, Feb, ..., Dec
                                    ordered = TRUE)

monthly_summary <- NOAAFlashFlood %>%
  group_by(Month_Name) %>%
  summarize(
    Flood_Count = n(),
    Total_Damage = sum(DAMAGE_PROPERTY_NUM, na.rm = TRUE)
  )

#Dual-axis plot (secondary y-axis for damage)
ggplot(monthly_summary, aes(x = Month_Name)) +
  geom_col(aes(y = Flood_Count), fill = "lightblue") +
  geom_line(aes(y = Total_Damage / 1e6), group = 1, color = "red3", linewidth = 1.2) +
  scale_y_continuous(
    name = "Number of Flash Floods",
    sec.axis = sec_axis(~.*1, name = "Total Property Damage (millions)")
  ) +
  labs(title = "Flash Flood Frequency and Property Damage by Month",
       x = "Month") +
  theme_minimal()








