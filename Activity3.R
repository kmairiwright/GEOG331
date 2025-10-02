#create a function. The names of the arguments for your function will be in parentheses. Everything in curly brackets will be run each time the function is run.
assert <- function(statement,err.message){
  #if evaluates if a statement is true or false for a single item
  if(statement == FALSE){
    print(err.message)
  }
  
}

#check how the statement works
#evaluate a false statement
assert(1 == 2, "error: unequal values")
#evaluate a true statement
assert(2 == 2, "error: unequal values")
#set up assert to check if two vectors are the same length
a <- c(1,2,3,4)
b <- c(8,4,5)
assert(length(a) == length(b), "error: unequal length")

#read in the data file
#skip the first 3 rows since there is additional column info
#specify the the NA is designated differently
datW <- read.csv("Z:\\kmwright\\data\\bewkes_weather.csv",
                 na.strings=c("#N/A"), skip=3, header=FALSE)
#preview data
print(datW[1,])

#get sensor info from file
# this data table will contain all relevant units
sensorInfo <-   read.csv("Z:\\kmwright\\data\\bewkes_weather.csv",
                         na.strings=c("#N/A"), nrows=2)

print(sensorInfo)

#get column names from sensorInfo table
#and set weather station colnames  to be the same
colnames(datW) <-   colnames(sensorInfo)
#preview data
print(datW[1,])

#used for question 3
help("read.csv")

#use install.packages to install lubridate
#install.packages(c("lubridate"))
#it is helpful to comment this line
#working on new computer don't forget uncomment and run lubridate

library(lubridate)

#convert to standardized format
#date format is m/d/y
dates <- mdy_hm(datW$timestamp, tz= "America/New_York")

#calculate day of year
datW$doy <- yday(dates)
#calculate hour in the day
datW$hour <- hour(dates) + (minute(dates)/60)
#calculate decimal day of year
datW$DD <- datW$doy + (datW$hour/24)
#quick preview of new date calculations
datW[1,]

#see how many values have missing data for each sensor observation
#air temperature
length(which(is.na(datW$air.temperature)))
#wind speed
length(which(is.na(datW$wind.speed)))
#precipitation
length(which(is.na(datW$precipitation)))
#soil temperature
length(which(is.na(datW$soil.moisture)))
#soil moisture
length(which(is.na(datW$soil.temp)))

#make a plot with filled in points (using pch)
#line lines
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil moisture (cm3 water per cm3 soil)")

#make a plot with filled in points (using pch)
#line lines
plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of Year",
     ylab="Air temperature (degrees C)")


#Conducting QAQC on air.temp
datW$air.tempQ1 <- ifelse(datW$air.temperature < 0, NA, datW$air.temperature)

#check the values at the extreme range of the data
#and throughout the percentiles
quantile(datW$air.tempQ1)

#look at days with really low air temperature
datW[datW$air.tempQ1 < 8,]  
#look at days with really high air temperature
datW[datW$air.tempQ1 > 33,]  

#plot precipitation and lightning strikes on the same plot
#normalize lighting strikes to match precipitation
lightscale <- (max(datW$precipitation)/max(datW$lightning.acvitivy)) * datW$lightning.acvitivy
#make the plot with precipitation and lightning activity marked
#make it empty to start and add in features
plot(datW$DD , datW$precipitation, xlab = "Day of Year", ylab = "Precipitation & lightning",
     type="n")
#plot precipitation points only when there is precipitation 
#make the points semi-transparent
points(datW$DD[datW$precipitation > 0], datW$precipitation[datW$precipitation > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15)        

#plot lightning points only when there is lightning     
points(datW$DD[lightscale > 0], lightscale[lightscale > 0],
       col= "tomato3", pch=19)

### QUESTION 5 ###

#create assert test for Q5
assert(length(lightscale) == nrow(datW), "error: unequal rows")

#filter out storms in wind and air temperature measurements
#filter all values with lightning that coincides with rainfall greater than 2mm or only rainfall over 5 mm.    
#create a new air temp column
datW$air.tempQ2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$air.tempQ1))

### Question 6 ###

#create a new wind speed column with filtered data
datW$wind.speedQ1 <- ifelse(datW$precipitation >= 2 & datW$lightning.acvitivy >0, NA, 
                      ifelse(datW$precipitation > 5, NA, datW$wind.speed))

#sum(is.na()) will return number of NAs in the data 
#similar to length(which(is.na()))
assert(sum(is.na(datW$wind.speed))==sum(is.na(datW$wind.speedQ1)), "error: unequal NAs")

#plotting wind speed Q1 data, type = b for lines and points
plot(datW$DD, datW$wind.speedQ1, 
     pch = 20, lwd = 1.5, type = "b", 
     xlab="Day of the Year", ylab="Wind Speed (m/s)")

### Question 7 ###

#plot soil moisture and precipitation onto the same graph to compare
#change margins to allow room for 2nd y axis
par(mar=c(5.1,4.1,4.1, 4.1))
plot(datW$DD [datW$soil.moisture>0], datW$soil.moisture, 
     pch = 20, type = "p", 
     xlab = "Day of the Year", ylab = "Soil Moisture (meters cubed per meters cubed)")
#set par(new) to true to allow for secondary data on same plot
par(new= TRUE)
plot(datW$DD [datW$soil.moisture>0], datW$precipitation, 
     pch = 20, col = "red", type = "p", 
     axes = FALSE, xlab ="", ylab = "" )
#create second y axis label
axis(side=4, col="red")
mtext("Precipitation (mm)", side = 4, line = 2, col="red")

#plot soil temperature and air temperature onto the same graph to compare
plot(datW$DD [datW$soil.temp>0], datW$soil.temp, 
     lwd = 2, col = "orange", type = "l", 
     xlab = "Day of the Year", ylab = "Soil and Air Temperature (degrees C)")
lines(datW$DD [datW$soil.temp>0], datW$air.temperature, 
      col="lightblue", lwd = 2)
#add a legend
legend("topright", legend = c("Soil Temperature", "Air Temperature"), 
       col=c("orange", "lightblue"), lty=c(1,1))

### Question 8 ###

#calculating average of air temp, wind speed, soil moisture, and soil temp. in June/July 2018
#Sum of NAs needed to know which observations were ignored
avg_air.temp<-mean(datW$air.temperature, na.rm=TRUE)
sum(is.na(datW$air.temperature))
avg_wind.speed<-mean(datW$wind.speed, na.rm=TRUE)
sum(is.na(datW$wind.speed))
avg_soil.mois<-mean(datW$soil.moisture, na.rm=TRUE)
sum(is.na(datW$soil.moisture))
avg_soil.temp<-mean(datW$soil.temp, na.rm=TRUE)
sum(is.na(datW$soil.temp))
#calculating averages for quality controlled air temperature and wind speed
avg_air.tempQ2<-mean(datW$air.tempQ2, na.rm=TRUE)
sum(is.na(datW$air.tempQ2))
avg_wind.speedQ1<-mean(datW$wind.speedQ1, na.rm=TRUE)
sum(is.na(datW$wind.speedQ1))
#calculating total precipitation in June/July 2018
total_precip<-sum(datW$precipitation, na.rm=TRUE)
sum(is.na(datW$precipitation))

### Question 9 ###
#add all plots to same window (soil moisture, air temperature, soil temperature, and precipitation)
par(mfrow=c(2,2))
plot(datW$DD, datW$soil.moisture, 
     lwd = 2, col = "coral2", type = "l", 
     xlab = "Day of the Year", ylab = "Soil Moisture (meter cubed per meter cubed)")
plot(datW$DD, datW$air.temperature, 
     lwd = 2, col = "cyan4", type = "l", 
     xlab = "Day of the Year", ylab = "Air Temperature (degrees C)")
plot(datW$DD, datW$soil.temp, 
     lwd = 2, col = "darkolivegreen4", type = "l", 
     xlab = "Day of the Year", ylab = "Soil Temperature (degrees C)")
plot(datW$DD, datW$precipitation, 
     lwd = 2, col = "deepskyblue3", type = "l", 
     xlab = "Day of the Year", ylab = "Precipitation (mm)")
     




