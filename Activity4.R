#Activity 4
#KW, 10/09/2025-

rm(list=ls())

#use built in iris dataset
head(iris)
#load in some tidyverse packages
#install.packages(c("tidyverse"))
library(tidyverse)

## Part 1: for loops##

#using only data for iris versicolor
flower<-iris[iris$Species=="versicolor",]

#write a for loop
#that produces a regression table
#for each of the following relationships
#1. iris  sepal length x width
#2. iris  petal length x width
#3. iris sepal length x petal length

#hint: consider using a list, and also new vectors for regression variables

#create list with formulas to refer to in for loop
regforms<-list(
  flower$Sepal.Length~flower$Sepal.Width,
  flower$Petal.Length~flower$Petal.Width,
  flower$Sepal.Length~flower$Petal.Length
)

#create empty list for regression results
regresults<-list()

#create for loop to run regression tables
for(i in 1:3){
 regresults [[i]]<-lm(regforms[[i]], flower)
}

## Part 2: data in dplyr ##

#use dplyr to join data of maximum height
#to a new iris data frame
height <- data.frame(Species = c("virginica","setosa","versicolor"),
                     Height.cm = c(60,100,11.8))

