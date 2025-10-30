#In class practice with Land Cover Data
#KW, 10/30-

rm(list=ls())

#install.packages(c("terra"))
library(terra)
#install.packages(c("tidyterra"))
library(tidyterra)
#install.packages(c("FedData"))
library(FedData)

nlcd_meve16 <-get_nlcd(template = FedData::meve,
                      label = "meve",
                      year = 2016,
                      extraction.dir = "Z:\\kmwright\\data")

nlcd_meve16
terra::plot(nlcd_meve16)

#vector dataset
cavm <- vect("Z:\\data\\cp_veg_la_shp")
cavm
head(cavm)
terra::plot(cavm, y = "PHYSIOG")
