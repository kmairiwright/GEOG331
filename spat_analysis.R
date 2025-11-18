#KW, 11/11-

rm(list=ls())

library(terra)
library(tidyterra)
library(FedData)

f <- list.files("Z:/data/landsat", full.names = T)
f

# read in files 3-10 as a single multi-band raster
lc <- rast(f[3:10])

#look at Band 5 to get information about the data
# (Band 5 = NIR)
lc[[5]]

# create a summary of the data values
summary(lc[[5]])

# make a quick plot to see how the data looks
plot(lc[[5]])


# Reflectance values not 0-1 (has to do with how the data was stored, 
#most remote sensing delivered as 8bit or 16bit integers you have to convert, 
#decimals take up too much space)
#use scaling factor to get surface reflectance

# we can perform math on the raster layer right inside the plot call
plot(lc[[5]]*0.0000275-0.2)

# The high values (1-ish) are probably clouds in the imagery
#used scaling factor "on the fly", did not make a permanent object 
#in some cases might want to right equation into file, rn R is storing
#object in memory (not permanent) 

#ndvi = difference between NIR and red, divide by sum of the two to normalize
#band 5=NIR, band 4=red
# mathematicaly ndvi ranges from -1 and 1 (0 and 1)
# calculate ndvi
ndvi <- (lc[[5]]-lc[[4]])/(lc[[5]]+lc[[4]])

names(ndvi) <- "NDVI"
# create a quick plot to see how the data looks
plot(ndvi)

# read in the shape file of dec lands
dec <- vect("Z:/data/NYS_DEC_Lands/")

plot(dec)

# remember from last time that the attribute table just like a data frame
# we'll use this to subset to Madison County
mad_dec <- dec[dec$COUNTY=="MADISON",]

# we could also use the crop function
# what dimensions of our raster layer are used to crop the vector layer
#example of overlay analysis, another approach
lc_dec <- crop(dec,lc)

# lastly, create a buffer around the Madison County DEC lands
#Q is are lands managed by dec more productive
#if you don't do "singlesided" would get inside and outside buffer
# what are the units? 
mad_buf <- buffer(mad_dec, width = 1000, singlesided = T)

# create a plot to look at our Madison County data
plot(mad_dec, col = "red")
plot(mad_buf, col = "yellow", add = T) #add=T adds both plots together

#Here is the analysis: (above is the meat to get to the 3 lines of analysis)

#zonal just gives vector for ndvi number?
#zonal function: mean value of ndvi inside of every mad_dec
#adding as attribute to mad_dec att. table
# calculate ndvi for dec lands
mad_dec$ndvi_in <- zonal(ndvi, mad_dec, fun = "mean")

# calculate ndvi for the buffer outside dec lands
mad_dec$ndvi_out <- zonal(ndvi, mad_buf, fun = "mean")

# calculate the difference to see if DEC lands are more productive
mad_dec$ndvi_dif <- mad_dec$ndvi_in-mad_dec$ndvi_out

#to see ndvi calculations, again stored in memory
#write to file if wanting to continue analysis
head(mad_dec)





