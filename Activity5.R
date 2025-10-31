#Activity 5
#KW, 10/28/2025-

rm(list=ls())

#load in lubridate
library(lubridate)

#read in stream flow data
datH<-read.csv("Z:\\data\\hw5_data\\stream_flow_data.csv",
               na.strings = c("Eqp"))
head(datH)

#read in precipitation data
#hourly precipitation is in mm
datP <- read.csv("Z:\\data\\hw5_data\\2049867.csv")                            
head(datP)

#only use most reliable measurements
datD<-datH[datH$discharge.flag == "A",]

#### define time for stream flow #####
#convert date and time
datesD <- as.Date(datD$date, "%m/%d/%Y")
#get day of year
datD$doy <- yday(datesD)
#calculate year
datD$year <- year(datesD)
#define time
timesD <- hm(datD$time)

#### define time for precipitation #####    
dateP <- ymd_hm(datP$DATE)
#get day of year
datP$doy <- yday(dateP)
#get year 
datP$year <- year(dateP)

#### get decimal formats #####
#convert time from a string to a more usable format
#with a decimal hour
datD$hour <- hour(timesD ) + (minute(timesD )/60)
#get full decimal time
datD$decDay <- datD$doy + (datD$hour/24)
#calculate a decimal year, but account for leap year
datD$decYear <- ifelse(leap_year(datD$year),datD$year + (datD$decDay/366),
                       datD$year + (datD$decDay/365))

#calculate times for datP                       
datP$hour <- hour(dateP ) + (minute(dateP )/60)
#get full decimal time
datP$decDay <- datP$doy + (datP$hour/24)
#calculate a decimal year, but account for leap year
datP$decYear <- ifelse(leap_year(datP$year),datP$year + (datP$decDay/366),
                       datP$year + (datP$decDay/365)) 

#basic formatting
aveF <- aggregate(datD$discharge, by=list(datD$doy), FUN="mean")
colnames(aveF) <- c("doy","dailyAve")
sdF <- aggregate(datD$discharge, by=list(datD$doy), FUN="sd")
colnames(sdF) <- c("doy","dailySD")


#start new plot
dev.new(width=8,height=8)

#### QUESTION 5 #####

#add month to datD
datD$month<-month(datesD)

#plot discharge graph with months
plot(aveF$doy,aveF$dailyAve,
     type="l",
     xlab="Month",
     ylab=expression(paste("Discharge ft"^"3", "sec"^"-1")),
     lwd=2,
     ylim=c(0,160),
     xlim=c(1,365),
     xaxs="i",yaxs="i",
     axes=FALSE)
#add standard dev.
polygon(c(aveF$doy, rev(aveF$doy)),
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),
        col=rgb(0.392, 0.584, 0.929,.2), 
        border=NA)

#add 2017 discharge line
lines(datD$doy[datD$year==2017],datD$discharge[datD$year==2017],
     lwd=1,
     col=c("#D9544D"))

#add month labels
axis(1, at=c(15,45,74,105,135,166,196,227,258,288,319,349),
     lab=month.abb)
axis(2, seq(0,160, by=20),
     seq(0,160, by=20),
     las = 2)
#extend x-axis
abline(h=0, xpd=FALSE)
#move legend so it doesn't interfere with plot
legend(x=310,y=150, c("mean","1 sd","2017"), 
       lwd=c(2,NA,2),
       col=c("black",rgb(0.392, 0.584, 0.929,.2),"#D9544D"),
       pch=c(NA,15,NA),
       bty="n")
       
#### Question 7 ####

#make a column with just date
datP$jdate<-as.Date(datP$DATE, "%Y%m%d")
#count how many have same date (frequency of date occurrence)
date_counts<-table(datP$jdate)
#isolate dates that have all 24 hrs of data
date_complete<-names(date_counts[date_counts == 24])
#create data frame with 24 hr only data
datPcomplete<-datP[datP$jdate %in% as.Date(date_complete),]

#plot datD data and visualize datPcomplete data distinctly
plot(aveF$doy, aveF$dailyAve,
     type="l",
     xlab="Day of the Year",
     ylab=expression(paste("Discharge ft"^"3", "sec"^"-1"))
)

     
     
     
     
     
     
     
     