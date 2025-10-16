#Activity 4
#KW, 10/09/2025-

rm(list=ls())

#use built in iris dataset
head(iris)
#load in some tidyverse packages
install.packages(c("tidyverse"))
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

# hint: consider using a list, and also new vectors for regression variables

regtab_list<-list()

regvar<-c("flower$Sepal.Length", "flower$Sepal.Width", "flower$Petal.Length", "flower$Petal.Width")

for(regvar in 1:4){
  lm(regvar)
}
