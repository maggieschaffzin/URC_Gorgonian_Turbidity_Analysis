# CREATION DATE 10 June 2024
# MODIFIED DATE: 18 June 2024 

# AUTHOR Maggie Schaffzin (schaffzin@oxy.edu)

# PURPOSE: Mapping storm drain + waste water outputs along L.A. County Coast (PVR + Malibu)

# TO DO: Map reef sites along PVR + Malibu, calculate Euclidean distance


# PACKAGES: ----
library(tidyverse) 
library(sf)
library(ggspatial)
library(dplyr)
library(RColorBrewer)
library (raster)

# SET WORKING DIRECTORY: ----

setwd("~/Documents/Gorgonian/Mapping")

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
  geom_sf(data = sitedata, color = "red", size =0.05) +
  coord_sf(crs = ,
           xlim = c(-119, -118), ylim = c(33.6, 34.2),
           expand = FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Study Site: SMB/Malibu and PVR") +
  theme_bw()

#Merging Gorgonian density into dataset

gorgoniandensity <- read_csv('Schaffzin_Swath_2024-06-12.csv') %>%
left_join(sitedata, gorgoniandensity, by = c("Site" = "Density_m2"))



#Mapping Gorgonian Density


#Creating plots to look for Patterns

#Subsampling Reef Sites


## Calculating Euclidean distance between outfalls and Chosen reef sites


