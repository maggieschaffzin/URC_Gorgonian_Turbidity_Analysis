# CREATION DATE 10 June 2024
# MODIFIED DATE: 12 June 2024 

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

## mapping SW, WW, and riverine outfalls onto study site

ggplot() +
  geom_sf(data = californiamap) +
  geom_sf(data = sw_outfalls, fill = "blue") +
  geom_sf(data = ww_outfalls, aes(color = "waste water"), show.legend = "point") +
  scale_color_manual(values = c("purple")) +
  geom_sf(data = riverine_outfalls, color = "magenta") +
coord_sf(crs = ,
           xlim = c(-119, -118), ylim = c(33.6, 34.2),
           expand = FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Study Site: SMB/Malibu and PVR") +
  theme_bw()


#Read Reef Shape Data




## Calculating Euclidean distance between outfalls and Reef sites


