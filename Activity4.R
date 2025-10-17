#Activity 4
#KW, 10/09/2025-

rm(list=ls())

#use built in iris dataset
head(iris)
#load in tidyverse packages
#install.packages(c("tidyverse"))
library(tidyverse)

#######################
## Part 1: for loops ##
#######################

#iris versicolor data
flower<-iris[iris$Species=="versicolor",]

#creating vector to hold regression variables
regforms1<-c("Sepal.Length~Sepal.Width",
             "Petal.Length~Petal.Width",
             "Sepal.Length~Petal.Length")

#create empty list to hold results
regresults<-list()

#create for loop to run regression model 
for(i in 1:3){
  regresults [[i]]<-lm(as.formula(regforms1[i]), flower)
}

###########################
## Part 2: data in dplyr ##
###########################

#use dplyr to join data of max. height
#to new iris data frame

height<-data.frame(Species=c("virginica","setosa","versicolor"),
                   Height.cm=c(60,100,11.8))
#load in dplyr
library(dplyr)

Iris<-iris

#create new Iris data frame including height
IrisHeight<-left_join(Iris,height,by="Species")

##############################
## Part 3: plots in ggplot2 ##
##############################

#look at base R scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)

#3a. make the same plot in ggplot
#load in ggplot2
library(ggplot2)

ggplot(data=iris, 
       aes=(x=iris$Sepal.Length, y=iris$Sepal.Width)
       + geom_point())

        

