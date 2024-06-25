# CREATION DATE 10 June 2024
# MODIFIED DATE: 25 June 2024 

# AUTHOR Maggie Schaffzin (schaffzin@oxy.edu)

# PURPOSE: Mapping study site, storm drain + waste water outputs, and reef sites along L.A. County Coast (PVR + Malibu), Euclidean distance dataset

# TO DO: Merge average density dataset to euclidean distance dataset


# PACKAGES: ----
library(tidyverse) 
library(sf)
library(ggspatial)
library(dplyr)
library(RColorBrewer)
library (raster)
library (png)

# SET WORKING DIRECTORY: ----

setwd("~/Documents/Gorgonian/URC_Gorgonian_Turbidity_Analysis/Data")

## Read in CA map shape ----
californiamap <- st_read("BAS_CA_California.shp")
plot (californiamap)

# check coordinate system
crs(californiamap)

#transform coord system to WGS84
californiamap<- st_transform (californiamap, crs = "EPSG:4326")

##Read wastewater outfall shape
ww_outfalls <- st_read("MAN_SCSR_Major_WW_Dcharge.shp")
plot (ww_outfalls)

#check coordinate system
crs(ww_outfalls)

#transform coord system to WGS84
ww_outfalls<- st_transform (ww_outfalls, crs = "EPSG:4326")

# Read in stormwater outfall shape
sw_outfalls <- st_read("MAN_SCSR_Major_SW_Dcharge.shp")
plot (sw_outfalls)

#check coordinate system
crs(sw_outfalls)

#transform coord system to WGS84
sw_outfalls<- st_transform (sw_outfalls, crs = "EPSG:4326")

##Read Riverine Outfall shape
riverine_outfalls <- st_read("MAN_SCSR_RiverDischarge_pts.shp")
plot (riverine_outfalls)

#check coordinate system
crs(riverine_outfalls)

#transform coord system to WGS84
riverine_outfalls<- st_transform (riverine_outfalls, crs = "EPSG:4326")


#Read Data for Reef Sites
sitedata <- read_csv('Sites_Info_17_Apr_2024.correct.csv') %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

# Extract coordinates from geometry and add them as columns
sitedata <- sitedata %>%
  mutate(Latitude = st_coordinates(.)[,2],
         Longitude = st_coordinates(.)[,1])

# Select relevant columns
sites_selected <- sitedata %>% 
  dplyr::select(Site, Latitude, Longitude)

# Filter based on latitude and longitude ranges
filtered_sites <- sites_selected %>%
  filter(Latitude > -119 & Latitude < -118 & Longitude > 33.6 & Longitude < 34.2)
  
#check coordinate system for Site Data
crs(sitedata)

## mapping SW, WW, riverine outfalls onto study site + Reef Sites

ggplot() +
  geom_sf(data = californiamap) +
  geom_sf(data = sw_outfalls, fill = "blue") +
  geom_sf(data = ww_outfalls, aes(color = "waste water outfalls"), show.legend = "point") +
  scale_color_manual(values = c("purple")) +
  geom_sf(data = riverine_outfalls, color = "magenta") +
  geom_sf(data = sitedata, color = "red", size = 0.05) +
  coord_sf(crs = ,
           xlim = c(-119, -118), ylim = c(33.6, 34.2),
           expand = FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Study Site: SMB/Malibu and PVR") +
  
  #Reading and preparing reference image
  california_image <- readPNG("~/Documents/Gorgonian/CA_Reference_Map.png") +
  
  #Overlaying Reference Image into Map
  annotation_raster(california_image, xmin = -118.95, xmax = -118.7, ymin = 33.65, ymax = 33.85) +
  annotation_north_arrow(location = "topleft", style = north_arrow_fancy_orienteering) +
  theme_bw()

# Calculating Euclidean distance between stormwater outfalls and reef sites
dist_matrix <- st_distance(sw_outfalls, sitedata)

# Convert distance matrix to a data frame
dist_df <- as.data.frame(as.matrix(dist_matrix))
colnames(dist_df) <- sitedata$Site
rownames(dist_df) <- sw_outfalls$Number

# Print distance matrix
print(dist_df)

#Average Distance from each site to all study site storm drain outfall sources
average_distances <- colMeans(dist_df)
average_distances_sw <- data.frame(Site = names(average_distances), Average_Distance_Stormwater = average_distances)
average_distances_final <- average_distances_sw %>% arrange(average_distances)


# Print the average euclidean distances for reef sites and storm water outfalls
print(average_distances_final)

# Calculating Euclidean distance between wastewater outfalls and reef sites
dist_matrix <- st_distance(ww_outfalls, sitedata)

# Convert distance matrix to a data frame
dist_df2 <- as.data.frame(as.matrix(dist_matrix))
colnames(dist_df2) <- sitedata$Site
rownames(dist_df2) <- ww_outfalls$Number

# Print distance matrix
print(dist_df2)

#Average Distance from each site to all study site waste water outfall sources
average_distances <- colMeans(dist_df2)
average_distances_ww <- data.frame(Site = names(average_distances), Average_Distance_Wastewater = average_distances)
average_distances_final2 <- average_distances_ww %>% arrange(average_distances)


# Print the average distances
print(average_distances_final2)

# Calculating Euclidean distance between riverine outfalls and reef sites
dist_matrix <- st_distance(riverine_outfalls, sitedata)

# Convert distance matrix to a data frame
dist_df3 <- as.data.frame(as.matrix(dist_matrix))
colnames(dist_df3) <- sitedata$Site
rownames(dist_df3) <- riverine_outfalls$Number

# Print distance matrix
print(dist_df3)

#Average Distance from each site to all study site waste water outfall sources
average_distances <- colMeans(dist_df3)
average_distances_riverine <- data.frame(Site = names(average_distances), Average_Distance_Riverine = average_distances)
average_distances_final3 <- average_distances_riverine %>% arrange(average_distances)

# Print the average distances
print(average_distances_final3)

#Merging datasets into one dataset of average Euclidean distance between chosen reef sites and stormwater, wastewater, and riverine outfalls
merged_dataset_1_2 <- full_join(average_distances_final, average_distances_final2, by = "Site")
euclidean_dataset <- full_join(merged_dataset_1_2, average_distances_final3, by = "Site")

# Print the merged dataset
print(euclidean_dataset)

#Adding densities to dataset
euclidean_density <- left_join(euclidean_dataset, avg_density_per_site, by = "Site")



