#Activity 5
#KW, 10/28/2025-11/3/2025

rm(list=ls())

#load in lubridate
library(lubridate)
#load in ggplot2
library(ggplot2)

#read in stream flow data
datH<-read.csv("/Volumes/GEOG331_F25/data/hw5_data/stream_flow_data.csv",
               na.strings = c("Eqp"))

#read in precipitation data
#hourly precipitation is in mm
datP <- read.csv("/Volumes/GEOG331_F25/data/hw5_data/2049867.csv")                            

#only use most reliable measurements
datD<-datH[datH$discharge.flag == "A",]

##define time for stream flow##
#convert date and time
datesD <- as.Date(datD$date, "%m/%d/%Y")
#get day of year
datD$doy <- yday(datesD)
#calculate year
datD$year <- year(datesD)
#define time
timesD <- hm(datD$time)

##define time for precipitation##
dateP <- ymd_hm(datP$DATE)
#get day of year
datP$doy <- yday(dateP)
#get year 
datP$year <- year(dateP)

##get decimal formats##
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
date_complete<-as.Date(names(date_counts[date_counts == 24]),format="%Y-%m-%d")
#create data frame with 24 hr only data
datPcomplete<-datP[datP$jdate %in% as.Date(date_complete),]

#add date format to datD dataframe
datD$DATE<-datesD
#create column for true/false if 24hrs of precipitation data
datD$full.precip<-datD$DATE %in% date_complete
table(datD$full.precip)

#plot datD data and visualize datPcomplete data distinctly
dev.off()

plot(datD$DATE, datD$discharge,
     type="l",
     col="darkgray",
     lwd=1.5,
     xlab="Date",
     ylab=expression(paste("Discharge ft"^"3", "sec"^"-1")),
     main="Stream Discharge with Full Precipitation Days Symbolized"
)

points (datD$DATE[datD$full.precip],
        datD$discharge[datD$full.precip],
        col="darkblue",
        pch=19)

#add a legend
legend(locator(1),
       legend=c("All data", "24hr precip. data"),
       col=c("darkgray","darkblue"),
       lwd=c(1.5,NA),
       pch=c(NA,19),
       pt.cex=1.2)

#Hydrographs:
#subsest discharge and precipitation within range of interest
hydroD <- datD[datD$doy >= 248 & datD$doy < 250 & datD$year == 2011,]
hydroP <- datP[datP$doy >= 248 & datP$doy < 250 & datP$year == 2011,] 

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1
#ceiling rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit on the 
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl     

par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD$decDay,
     hydroD$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP)){
  polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
            hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
          c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}

#### Question 8 ####
#read date_complete table to find consecutive dates 
#with full precipitation data
date_complete
#subsest discharge and precipitation within (new) range of interest
hydroD2 <- datD[datD$doy >= 62 & datD$doy < 64 & datD$year == 2013,]
hydroP2 <- datP[datP$doy >= 62 & datP$doy < 64 & datP$year == 2013,] 

#get min and max range of plot
yl2 <- floor(min(hydroD2$discharge))-1
yh2 <- ceiling(max(hydroD2$discharge))+1
#minimum and maximum range of precipitation to plot
pl2 <- 0
pm2 <-  ceiling(max(hydroP2$HPCP))+.5
#scale precipitation to fit on plot
hydroP2$pscale <- (((yh2-yl2)/(pm2-pl2)) * hydroP2$HPCP) + yl2

par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD2$decDay,
     hydroD2$discharge, 
     type="l", 
     ylim=c(yl2,yh2), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP2)){
  polygon(c(hydroP2$decDay[i]-0.017,hydroP2$decDay[i]-0.017,
            hydroP2$decDay[i]+0.017,hydroP2$decDay[i]+0.017),
          c(yl2,hydroP2$pscale[i],hydroP2$pscale[i],yl2),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}

#Box and violin plots:
#specify year as a factor
datD$yearPlot <- as.factor(datD$year)
#make a boxplot
ggplot(data= datD, aes(yearPlot,discharge)) + 
  geom_boxplot()
#make a violin plot
ggplot(data= datD, aes(yearPlot,discharge)) + 
  geom_violin()

#### Question 9 ####

#create season column
datD$Season<-ifelse(month(datD$DATE)%in% c(12,1,2), "Winter",
             ifelse(month(datD$DATE)%in% c(3,4,5), "Spring",
             ifelse(month(datD$DATE)%in% c(6,7,8), "Summer", "Fall")))
#turn column into factor for plotting
datD$Season<-factor(datD$Season, 
                    levels=c("Winter","Spring","Summer","Fall"))
#2016 discharge violin plot
ggplot(data=datD[datD$year==2016,],aes(Season,discharge))+
  geom_violin(fill="lightblue")+
  labs(
    title="2016 Seasonal Discharge",
    x="Season",
    y="Discharge"
  ) +
  theme_minimal()+
  theme(
    plot.title=element_text(hjust=0.5)
  )
#2017 violin plot
ggplot(data=datD[datD$year==2017,],aes(Season,discharge))+
  geom_violin(fill="darkblue")+
  labs(
    title="2017 Seasonal Discharge",
    x="Season",
    y="Discharge"
  ) +
  theme_minimal()+
  theme(
    plot.title=element_text(hjust=0.5)
  )







