# CREATION DATE 18 June 2024
# MODIFIED DATE: 26 June 2024 

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

#Ensuring density values are numeric
final_data$Density_m2 <- as.numeric(final_data$Density_m2)

#Calculating average Gorgonian density by site
avg_density_per_site <- final_data %>%
  group_by(Site) %>%
  summarize(avg_density = mean((Density_m2*100), na.rm = TRUE)) %>%
  arrange(desc(avg_density))

print(avg_density_per_site)

#Calculating average gorgonian density by depth zone
avg_density_per_depth <- final_data %>%
  group_by(DepthZone) %>%
  summarize(avg_density = mean((Density_m2 *100), na.rm = TRUE))

#Plotting Density by Depth Zone
ggplot(avg_density_per_depth, aes(x = DepthZone, y = avg_density)) +
  geom_bar(stat = "identity", color = "black", fill = "red", width = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  geom_text(aes(label = round(avg_density, 2)), vjust = -0.3, size = 5) +
  labs(title = "Average Gorgonian Density by Depth Zone", 
       x = "Depth Zone", 
       y = "Average Density (per 100m2)") +
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
       y = "Average Density (per 100m2)") +
   theme_minimal(base_size = 15) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 15), 
    axis.title.y = element_text(face = "bold", size = 15), 
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none" 
  )

#Calculating average density of MURCAL by Depth Zone
avg_density_muricea <- final_data %>%
  filter (Species == "Muricea californica")  %>%
  group_by(DepthZone) %>%
  summarize(avg_density = mean((Density_m2 *100), na.rm = TRUE))
print(avg_density_muricea)

#Plotting MURCAL Density by Depth Zone
  ggplot(avg_density_muricea, aes(x = DepthZone, y = avg_density)) +
    geom_bar(stat = "identity", color = "black", fill = "lightblue", width = 0.7) +
    scale_fill_brewer(palette = "Set2") +
    geom_text(aes(label = round(avg_density, 2)), vjust = -0.3, size = 5) +
    labs(title = "Average Density of Muricea californica by Depth Zone", 
         x = "Depth Zone", 
         y = "Average Density (per 100m2)") +
    theme_minimal(base_size = 15) + 
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
      axis.title.x = element_text(face = "bold", size = 15), 
      axis.title.y = element_text(face = "bold", size = 15), 
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none" 
    )
 
  #Filtering dataset to only include AR
  ar_sites_data <- final_data %>%
    filter(grepl("AR", Site))
  
  # Calculate average gorgonian density for sites with "AR" in their title
  avg_density_ar_sites <- ar_sites_data %>%
    group_by(Site, DepthZone) %>%
    summarize(avg_density = mean(Density_m2 * 100, na.rm = TRUE)) %>%
    arrange(desc(avg_density))
  print (avg_density_ar_sites)
  
  #Filtering dataset to only include NR
  nr_sites <- final_data %>%
  filter(!grepl("AR", Site))
  
  #Calculating average gorgonian density for NR sites
  avg_density_nr_sites <- nr_sites %>%
    group_by(Site, DepthZone) %>%
    summarize(avg_density = mean(Density_m2 * 100, na.rm = TRUE)) %>%
    arrange(desc(avg_density))
  
  print(avg_density_nr_sites)
  
  #Plotting density at NR Sites by depth
  ggplot(avg_density_nr_sites, aes(x = DepthZone, y = avg_density)) +
    geom_boxplot(alpha = 0.7) +  
    geom_jitter(width = 0.1, alpha = 0.5, color = "black") +  
    scale_fill_brewer(palette = "Pastel1") +  
    labs(x = "Depth Zone", y = "Average Gorgonian Density (100m2)") +  
    ggtitle("Average Gorgonian Density by Depth on NR Sites") +  
    theme(
      text = element_text(family = "Times New Roman"), 
      plot.title = element_text(size = 16, face = "bold"),  
      axis.title = element_text(size = 14),  
      legend.position = "none"  
    )
  
  # Adding a column to indicate site type
  avg_density_ar_sites$site_type <- "AR"
  avg_density_nr_sites$site_type <- "NR"
  
  # Combining data frames
  combined_data <- rbind(avg_density_ar_sites, avg_density_nr_sites)
  
  #Plotting NR. vs AR densities
  ggplot(combined_data, aes(x = site_type, y = avg_density, fill = site_type)) +
    geom_boxplot(alpha = 0.7) +  
    geom_jitter(width = 0.1, alpha = 0.5, color = "black") +  
    scale_fill_brewer(palette = "Pastel1") +  
    labs(x = "Site Type", y = "Average Gorgonian Density (100m2)") +  
    ggtitle("Average Gorgonian Density on AR vs NR Sites") +  
    theme(
      text = element_text(family = "Times New Roman"), 
      plot.title = element_text(size = 16, face = "bold"),  
      axis.title = element_text(size = 14),  
      legend.position = "none"  
    )
  