# CREATION DATE 18 June 2024
# MODIFIED DATE: 18 June 2024 

# AUTHOR Maggie Schaffzin (schaffzin@oxy.edu)

# PURPOSE: Fill in Density Dataset and Merge Density Values to Site Dataset (creating final dataset)

# TO DO: Graph to look for patterns


# PACKAGES: ----
library(tidyverse) 
library(sf)
library(ggspatial)
library(dplyr)
library(RColorBrewer)
library (raster)

# SET WORKING DIRECTORY: ----

setwd("~/Documents/Gorgonian/URC_Gorgonian_Turbidity_Analysis/Data")

#Reading datasets
gorgoniandensity <- read_csv('Schaffzin_Swath_2024-06-12.csv')
sitedata <- read_csv('Sites_Info_17_Apr_2024.correct.csv')

# Replace "N/A" values with 0 for gorgoniandensity
gorgoniandensity$Latitude[is.na(gorgoniandensity$Latitude)] <- 0
gorgoniandensity$Longitude[is.na(gorgoniandensity$Longitude)] <- 0

# Check unique values in gorgoniandensity
unique_sites_gorgonian <- unique(gorgoniandensity$Site)

# Check unique values in sitedata
unique_sites_sitedata <- unique(sitedata$Site)

# Compare unique sites between datasets
setdiff(unique_sites_gorgonian, unique_sites_sitedata)
unique_discrepancies <- setdiff(unique_sites_gorgonian, unique_sites_sitedata)

# Exclude unique discrepancies from gorgoniandensity
gorgoniandensity_clean <- gorgoniandensity[!(gorgoniandensity$Site %in% unique_discrepancies), ]

#Converting datasets to sf
sitedata_sf <- st_as_sf(sitedata, coords = c("Longitude", "Latitude"), crs = 4326)
gorgoniandensity_clean_sf <- st_as_sf(gorgoniandensity_clean, coords = c("Longitude", "Latitude"), crs = 4326)

# Extract coordinates from geometry and add them as columns
sitedata_sf<- sitedata %>%
  mutate(Latitude = st_coordinates(.)[,2],
         Longitude = st_coordinates(.)[,1])
gorgoniandensity_clean <- gorgoniandensity_clean %>%
  mutate(Latitude = st_coordinates(.)[,2],
         Longitude = st_coordinates(.)[,1])

# Join datasets based on site
merged_data <- left_join(gorgoniandensity_clean, sitedata, by = "Site")

# Fill in missing latitude and longitude values
merged_data$Latitude <- ifelse(is.na(merged_data$Latitude.x), merged_data$Latitude.y, merged_data$Latitude.x)
merged_data$Longitude <- ifelse(is.na(merged_data$Longitude.x), merged_data$Longitude.y, merged_data$Longitude.x)

# Select the relevant columns from the merged data
final_data <- merged_data %>% 
  dplyr::select(Site, Latitude.y, Longitude.y, DepthZone, Species, Density_m2)

# Filter combined dataset based on latitude and longitude ranges
finaldata <- final_data %>%
  filter(Latitude.y > -119 & Latitude.y < -118 & 
           Longitude.y > 33.6 & Longitude.y < 34.2)

#View dataset
print(finaldata)
