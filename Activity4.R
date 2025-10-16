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
