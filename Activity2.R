##New script for F25 HW2##

#make a vector of tree heights in meters
heights <- c(30,41,20,22)
#convert to cm
heights_cm <- heights*100
heights_cm
#look at the first tree height
heights[1]
#look at the 2nd and 3rd tree heights
heights[2:3]

help("matrix")
?matrix
#testing if heights is a matrix
is.matrix(heights)

#set up a matrix with 2 columns and fill in by rows
#first argument is the vector of numbers to fill in the matrix
Mat<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=TRUE)
Mat

#set up a matrix that fills in by columns
#first argument is the vector of numbers to fill in the matrix
Mat.bycol<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=FALSE)
Mat.bycol
#subset the matrix to look at row 1, column2
Mat.bycol[1,2]
#look at all values in row 1
Mat.bycol[1,]
#look at all values in column 2
Mat.bycol[,2]

#dataframes
#read in weather station file from your data folder
datW<-read.csv("Z:\\kmwright\\data\\noaa_weather\\2011124.csv",stringsAsFactors=T)
#get more information about the dataframe
str(datW)

#specify a column with a proper date format
#note the format here dataframe$column
#format without $ sign will create a vector
datW$dateF<-as.Date(datW$DATE, "%Y-%m-%d")

#google date formatting in r to find more options and learn more

#create a date column by reformatting the date to only include years
#and indicating that it should be treated as numeric data
datW$year<-as.numeric(format(datW$dateF, "%Y"))

#Question 2, create example vector of each data type with 5 objects in it
#creating a vector of character data with 5 colors
vector_character<-c("blue", "red","green","pink","yellow")
#creating a vector of numeric data with 5 numbers
vector_numeric<-c(1.2,1.1,3.141,2.5,72.9)
#creating a vector of integer data with 5 integers
vector_integer<-c(10L,3L,4L,8L,1L)
#creating a vector of factor data with 5 objects and 3 levels
vector_factor<-factor(c("blue", "red", "green", "blue", "red"))



                      