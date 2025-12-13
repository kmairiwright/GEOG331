#Final Project Script#
#SCRIPT FOR FINAL PROJECT WITH NLCD DATA
#KW 10/24/25-

rm(list=ls())

library(terra)
library(tidyterra)
library(FedData)
library(ggplot2)
library(tigris)
library(dplyr)
 
#All NOAA data available (1997-2024) in Grand County, Utah

#path for mac
NOAAFlashFlood<-read.csv("/Volumes/GEOG331_F25/kmwright/data/Project_Data/allFFeventsGrand.csv")
#path for PC
#NOAAFlashFlood<-read.csv("Z:\\kmwright\\data\\Project_Data\\allFFeventsGrand.csv")

###LAND COVER PLOTS###

#Use tigris for Grand County Shape file, simple 
Grand_County_Tigris<-counties(state="UT",cb=TRUE) %>%
  filter(NAME=="Grand")

#vector of tigris data
grand_vect<-vect(Grand_County_Tigris)

#read in land cover data (NLCD), 2024

#path for mac
nlcd_2024<-rast("/Volumes/GEOG331_F25/kmwright/data/Project_Data/Annual_NLCD_LndCov_2024_CU_C1V1_mi7m3sagei6es9.tiff")
#path for PC
#nlcd_2024<-rast("Z:\\kmwright\\data\\Project_Data\\Annual_NLCD_LndCov_2024_CU_C1V1_mi7m3sagei6es9.tiff")

#transform coordinate reference system to match
grand_vect<-project(grand_vect, crs(nlcd_2024))

#crop and mask NLCD to Grand County 
nlcd_cropped<-crop(nlcd_2024, grand_vect)
nlcd_masked<-mask(nlcd_cropped, grand_vect)

#classify NLCD classes with labels and colors
nlcd_classes <- c(11, 12, 21, 22, 23, 24, 31, 41, 42, 43, 52, 71, 81, 82, 90, 95)
nlcd_labels <- c(
  "Open Water", "Perennial Ice/Snow", "Developed, Open Space", "Developed, Low Intensity",
  "Developed, Medium Intensity", "Developed, High Intensity", "Barren Land",
  "Deciduous Forest", "Evergreen Forest", "Mixed Forest", "Shrub/Scrub", "Grassland/Herbaceous",
  "Pasture/Hay", "Cultivated Crops", "Woody Wetlands", "Emergent Herbaceous Wetlands"
)
nlcd_colors <- c(
  "#476BA1", "#faf8f2", "#DECACA", "#D89382", "#ED0000", "#AA0000", "#B2ADA3",
  "#68AB5F", "#1C5F2C", "#B5CA8F", "#f7dd7c", "#DCD939", "#D1D182", "#917214",
  "#BAD8EA", "#70A3BA"
)

#Lat/Long coordinates form NOAAFlashFlood
FFPoints<-vect(NOAAFlashFlood,geom=c("BEGIN_LON","BEGIN_LAT"), crs = "EPSG:4326")
FFPoints_proj<-project(FFPoints,crs(nlcd_2024))

#Change some columns from integer to numeric
FFPoints_proj$DEATHS_DIRECT<-as.numeric(FFPoints_proj$DEATHS_DIRECT)
FFPoints_proj$DAMAGE_PROPERTY_NUM<-as.numeric(FFPoints_proj$DAMAGE_PROPERTY_NUM)

###Plot NLCD 2024###

#create a two panel layout for legend to read clearly
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1)) 
#plot NLCD of Grand County 2024
plot(nlcd_masked, 
     col = nlcd_colors, 
     breaks = c(nlcd_classes, 100), 
     legend=FALSE,
     main = "Grand County, UT NLCD 2024")
#Plot FF points on NLCD
points(FFPoints_proj, col="black",pch=16)

#plot legend for NLCD
#remove the margins
par(mar = c(0, 0, 0, 0))
plot.new()
# NLCD legend
legend("center",
       legend = c(nlcd_labels, "Flash Flood Start Point"),
       fill = c(nlcd_colors, rep(NA, 1)),       
       pch = c(rep(NA, length(nlcd_labels)), 16), 
       col = c(rep(NA, length(nlcd_labels)), "black"),
       pt.cex = 1.2, 
       cex = 0.8,
       bty = "n")


###plot flash flood points on 1997 NLCD###

#path for mac
nlcd_1997<-rast("/Volumes/GEOG331_F25/kmwright/data/Project_Data/Annual_NLCD_LndCov_1997_CU_C1V1_mi7m3sagei6es9.tiff")
#path for PC
#nlcd_1997<-rast("Z:\\kmwright\\data\\Project_Data\\Annual_NLCD_LndCov_1997_CU_C1V1_mi7m3sagei6es9.tiff")

#transform coordinate reference system to match
grand_vect<-project(grand_vect, crs(nlcd_1997))
#crop and mask NLCD to Grand County 
nlcd_cropped97<-crop(nlcd_1997, grand_vect)
nlcd_masked97<-mask(nlcd_cropped97, grand_vect)

#create a two panel layout for legend to read clearly
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1)) 

#plot NLCD of Grand County 1997
plot(nlcd_masked97, 
     col = nlcd_colors, 
     breaks = c(nlcd_classes, 100), 
     legend=FALSE,
     main = "Grand County, UT NLCD 1997")
#Plot FF points on NLCD
points(FFPoints_proj, col="black",pch=16)

#plot legend for NLCD
#remove the margins
par(mar = c(0, 0, 0, 0))
plot.new()
# NLCD legend
legend("center",
       legend = c(nlcd_labels, "Flash Flood Start Point"),
       fill = c(nlcd_colors, rep(NA, 1)),       
       pch = c(rep(NA, length(nlcd_labels)), 16), 
       col = c(rep(NA, length(nlcd_labels)), "black"),
       pt.cex = 1.2, 
       cex = 0.8,
       bty = "n")

#create a two panel layout for legend to read clearly
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1)) 

###Plot, on NLCD 2024, deaths and property damage###

#plot NLCD of Grand County 2024
plot(nlcd_masked, 
     col = nlcd_colors, 
     breaks = c(nlcd_classes, 100), 
     legend=FALSE,
     main = "Grand County, UT NLCD 2024")

#Plot FF points on NLCD
points(FFPoints_proj[FFPoints_proj$DAMAGE_PROPERTY_NUM==0], col="black",pch=16)
points(FFPoints_proj[FFPoints_proj$DAMAGE_PROPERTY_NUM>=1,FFPoints_proj$DEATHS_DIRECT==0],col="blue",pch=16)
points(FFPoints_proj[FFPoints_proj$DEATHS_DIRECT>=1], col="#DF00FE",pch=16)

#plot legend for NLCD
#remove the margins
par(mar = c(0, 0, 0, 0))
plot.new()
# NLCD legend
legend("center",
       legend = c(nlcd_labels, "Flash Flood Start Point", "Documented Property Damage", "Deaths"),
       fill = c(nlcd_colors, rep(NA, 1), rep(NA,1), rep (NA,1)),       
       pch = c(rep(NA, length(nlcd_labels)), 16, 16, 16), 
       col = c(rep(NA, length(nlcd_labels)), "black", "blue", "#DF00FE"),
       pt.cex = 1.2, 
       cex = 0.8,
       bty = "n")

#create map showing "severe" floods? top 25% prop. damage
plot.new()
damage_threshold <- quantile(FFPoints_proj$DAMAGE_PROPERTY_NUM, 0.75, na.rm = TRUE)
severe_floods <- FFPoints_proj[FFPoints_proj$DAMAGE_PROPERTY_NUM >= damage_threshold, ]

plot(nlcd_masked,
     col = nlcd_colors,
     breaks = c(nlcd_classes, 100),
     legend = FALSE,
     main = "Severe Flash Floods 1997-2024 over NLCD 2024, Grand County, UT")
points(severe_floods, col = "black", pch = 16)

### WORKING ON ATTACHING LAND CLASS ###

#force NLCD raster to be numeric
nlcd_2024_num <- classify(nlcd_2024, rcl = cbind(nlcd_classes, nlcd_classes))
nlcd_cropped <- crop(nlcd_2024_num, grand_vect)

#extract numeric values
vals_2024 <- terra::extract(nlcd_cropped, FFPoints_proj, nearest=TRUE)

#attach NLCD codes
FFPoints_proj$NLCD_2024 <- vals_2024[,2]

#labels
nlcd_lookup <- data.frame(code = nlcd_classes, label = nlcd_labels)
FFPoints_proj$NLCD_2024_label <- nlcd_lookup$label[
  match(FFPoints_proj$NLCD_2024, nlcd_lookup$code)
]

#extract to FF points
terra::extract(nlcd_cropped, FFPoints_proj[1,], nearest=TRUE)

#convert flood points to a df for plotting
ff_df <- as.data.frame(FFPoints_proj)

ff_df$DAMAGE_PROPERTY_NUM <- as.numeric(ff_df$DAMAGE_PROPERTY_NUM)

### some summary plots on above data ###

#box plot, property damage by NLCD class
ggplot(ff_df, aes(x = NLCD_2024_label, y = DAMAGE_PROPERTY_NUM)) +
  geom_boxplot(fill = "skyblue", color = "darkblue", outlier.shape = 16, outlier.colour = "red") +
  scale_y_log10(labels = scales::comma) +   # log scale for skewed damage values
  labs(
    title = "Flash Flood Property Damage by Landcover Class (2024, Grand County, UT)",
    x = "NLCD 2024 Landcover Class",
    y = "Property Damage ($, log scale)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14, face = "bold")
  )

#bar graph of FF count
ggplot(ff_df, aes(x = NLCD_2024_label)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Number of Flash Floods by Landcover Class (2024)",
       x = "Landcover Class", y = "Flood Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Look at Propotion of Land Cover Class to frequency of flooding? ###

#raw cell values from the cropped raster
vals <- terra::values(nlcd_cropped)
vals <- vals[!is.na(vals)]

#frequency of each NLCD code
lc_counts <- as.data.frame(table(vals))
colnames(lc_counts) <- c("code","count")

lc_counts$code <- as.numeric(as.character(lc_counts$code))

lc_counts$label <- nlcd_lookup$label[match(lc_counts$code, nlcd_lookup$code)]

#percent area
lc_counts$percent_area <- (lc_counts$count / sum(lc_counts$count)) * 100

#flood counts by landcover
flood_counts <- table(FFPoints_proj$NLCD_2024_label)
flood_df <- as.data.frame(flood_counts)
colnames(flood_df) <- c("label","count")
flood_df$percent_floods <- (flood_df$count / sum(flood_df$count)) * 100

#compare landcover area with flood counts
compare_df <- merge(lc_counts[,c("label","percent_area")],
                    flood_df[,c("label","percent_floods")],
                    by="label", all=TRUE)

#calculate ratio (% floods/ % area)
compare_df$ratio <- compare_df$percent_floods / compare_df$percent_area

#replace NAs with 0
compare_df$percent_floods[is.na(compare_df$percent_floods)] <- 0
compare_df$ratio[is.na(compare_df$ratio)] <- 0

#round values two decimals to make compare_df table easier to reaf
compare_df$percent_area   <- round(compare_df$percent_area, 2)
compare_df$percent_floods <- round(compare_df$percent_floods, 2) 
compare_df$ratio          <- round(compare_df$ratio, 2)          

#maybe a different plot? 
ggplot(compare_df, aes(x=label)) +
  geom_bar(aes(y=percent_area), stat="identity", fill="lightgreen", alpha=0.7) +
  geom_bar(aes(y=percent_floods), stat="identity", fill="steelblue", alpha=0.7) +
  labs(title="Landcover Area vs Flood Frequency (2024, Grand County, UT)",
       x="Landcover Class", y="Percent") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))

ggplot(compare_df, aes(x=label, y=ratio)) +
  geom_bar(stat="identity", fill="purple", alpha=0.7) +
  labs(title="Floods Relative to Landcover Availability (2024)",
       x="Landcover Class", y="Floods % ÷ Area %") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        plot.title = element_text(size=14, face="bold"))

#side by side plot?
plot_df <- compare_df %>%
  pivot_longer(cols = c(percent_area, percent_floods),
               names_to = "type", values_to = "percent")

# 3. Rename type labels for clarity
plot_df$type <- recode(plot_df$type,
                       percent_area   = "Area %",
                       percent_floods = "Flood %")

# 4. Side‑by‑side bar chart
ggplot(plot_df, aes(x = label, y = percent, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(title = "Landcover Area vs Flood Frequency (2024, Grand County, UT)",
       x = "Landcover Class", y = "Percent") +
  scale_fill_manual(values = c("Area %" = "lightgreen", "Flood %" = "steelblue")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 14, face = "bold"))
