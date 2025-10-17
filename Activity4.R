#Activity 4
#KW, 10/09/2025-

rm(list=ls())

#use built in iris dataset
head(iris)
#load in tidyverse packages
#install.packages(c("tidyverse"))
library(tidyverse)

## Part 1: for loops##

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