# CREATION DATE 18 June 2024
# MODIFIED DATE: 24 June 2024 

# AUTHOR Maggie Schaffzin (schaffzin@oxy.edu)

# PURPOSE: Fill in Density Dataset and Merge Density Values to Site Dataset (creating final dataset), create plots to look for patterns

# TO DO: Statistical Analysis


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

# Filter merged dataset based on latitude and longitude ranges
finaldata <- final_data %>%
  filter(Latitude.y > -119 & Latitude.y < -118 & 
           Longitude.y > 33.6 & Longitude.y < 34.2)

#View dataset
nrow(finaldata)

#Ensuring density values are numeric
final_data$Density_m2 <- as.numeric(final_data$Density_m2)

#Calculating average Gorgonian density by site
avg_density_per_site <- final_data %>%
  group_by(Site) %>%
  summarize(avg_density = mean(Density_m2, na.rm = TRUE)) %>%
  arrange(desc(avg_density))

print(avg_density_per_site)

#Calculating average gorgonian density by depth zone
avg_density_per_depth <- final_data %>%
  group_by(DepthZone) %>%
  summarize(avg_density = mean(Density_m2, na.rm = TRUE))

#Plotting Density by Depth Zone
ggplot(avg_density_per_depth, aes(x = DepthZone, y = avg_density)) +
  geom_bar(stat = "identity", color = "black", fill = "red", width = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  geom_text(aes(label = round(avg_density, 2)), vjust = -0.3, size = 5) +
  labs(title = "Average Gorgonian Density by Depth Zone", 
       x = "Depth Zone", 
       y = "Average Density (per m2)") +
  theme_minimal(base_size = 15) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 15), 
    axis.title.y = element_text(face = "bold", size = 15), 
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none" 
  )


#Calculating average gorgonian density by species
avg_density_per_species <- final_data %>%
  group_by(Species) %>%
  summarize(avg_density = mean((Density_m2 * 100), na.rm = TRUE))
print(avg_density_per_species)

#Plotting Density by Species
ggplot(avg_density_per_species, aes(x = Species, y = avg_density)) +
  geom_bar(stat = "identity", color = "black", fill = "blue", width = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  geom_text(aes(label = round(avg_density, 2)), vjust = -0.3, size = 5) +
  labs(title = "Average Gorgonian Density by Species", 
       x = "Species", 
       y = "Average Density (per m2)") +
   theme_minimal(base_size = 15) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 15), 
    axis.title.y = element_text(face = "bold", size = 15), 
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none" 
  )

