# CREATION DATE 18 June 2024
# MODIFIED DATE: 28 June 2024 

# AUTHOR Maggie Schaffzin (schaffzin@oxy.edu)

# PURPOSE: Fill in Density Dataset and Merge Density Values to Site Dataset (creating final dataset), Euclidean distance dataset, 

# TO DO: Statistical Analysis on NR vs AR


# PACKAGES: ----
library(tidyverse) 
library(sf)
library(ggspatial)
library(dplyr)
library(RColorBrewer)
library (raster)
library (ggrepel)


# SET WORKING DIRECTORY: ----

setwd("~/Documents/Gorgonian/URC_Gorgonian_Turbidity_Analysis/Data")

#Reading datasets
gorgoniandensity <- read_csv('Schaffzin_Swath_2024-06-12.csv')
sitedata <- read_csv('Sites_Info_17_Apr_2024.correct.csv')
macrodensity <- read_csv('Schaffzin_Swath_2024-06-28-real.csv')

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

# Replace "N/A" values with 0 for macrodensity
macrodensity$Latitude[is.na(macrodensity$Latitude)] <- 0
macrodensity$Longitude[is.na(macrodensity$Longitude)] <- 0

#Converting datasets to sf
sitedata_sf <- st_as_sf(sitedata, coords = c("Longitude", "Latitude"), crs = 4326)
macrodensity_clean_sf <- st_as_sf(gorgoniandensity_clean, coords = c("Longitude", "Latitude"), crs = 4326)

# Extract coordinates from geometry and add them as columns
sitedata_sf<- sitedata %>%
  mutate(Latitude = st_coordinates(.)[,2],
         Longitude = st_coordinates(.)[,1])
macrodensity_clean <- macrodensity_clean %>%
  mutate(Latitude = st_coordinates(.)[,2],
         Longitude = st_coordinates(.)[,1])

# Join datasets based on site
merged_data2 <- left_join(macrodensity_clean, sitedata, by = "Site")

# Fill in missing latitude and longitude values
merged_data2$Latitude <- ifelse(is.na(merged_data2$Latitude.x), merged_data2$Latitude.y, merged_data2$Latitude.x)
merged_data2$Longitude <- ifelse(is.na(merged_data2$Longitude.x), merged_data2$Longitude.y, merged_data2$Longitude.x)

# Select the relevant columns from the merged data
final_data2 <- merged_data2 %>% 
  dplyr::select(Site, DepthZone, Species, SpeciesGroupF, Density_m2, Region)
filtered_data2 <- final_data2 %>% filter(SpeciesGroupF != "gorgonians")

#Ensuring density values are numeric
filtered_data2$Density_m2 <- as.numeric(final_data$Density_m2)

#Ensure DepthZone is retained and PVR is added correctly
gorgoniandensity_clean <- gorgoniandensity_clean %>%
  mutate(DepthZone_new = ifelse(grepl("PVR", Site) == T, "PVR", DepthZone))

# Make DepthZone an ordered factor
gorgoniandensity_clean$DepthZone_new <- factor(gorgoniandensity_clean$DepthZone_new, levels = c("Inner", "Middle", "Outer", "Deep", "ARM", "PVR"))

# Adding a column to indicate site type
mean_density_ar_sites$site_type <- "AR"
mean_density_nr_sites$site_type <- "NR"

# Combining data frames
combined_data <- rbind(mean_density_ar_sites, mean_density_nr_sites)

#Ensure DepthZone is retained and PVR is added correctly for macro
filtered_data2 <- filtered_data2%>%
  mutate(DepthZone_new = ifelse(grepl("PVR", Site) == T, "PVR", DepthZone))

# Make DepthZone an ordered factor for macro
filtered_data2$DepthZone_new <- factor(filtered_data2$DepthZone_new, levels = c("Inner", "Middle", "Outer", "Deep", "ARM", "PVR"))


# Check unique values in macro
unique_sites_macro <- unique(filtered_data2$Site)

# Check unique values in 
unique_sites_final <- unique(final_data$Site)

# Compare unique sites between datasets
setdiff(unique_sites_macro, unique_sites_final)
unique_discrepancies <- setdiff(unique_sites_macro, unique_sites_final)

# Exclude unique discrepancies from final
cleandata_macro <- final_data[!(final_data$Site %in% unique_discrepancies), ]

#Merging Datasets (macro and gorgonian)
mergedboth <- left_join(final_data, filtered_data2, by = "Site")

# Summarize Density by Site, DepthZone, and Species of Gorgonian
dat_gorg <- gorgoniandensity_clean %>%
  group_by(Site, DepthZone, Species) %>%
  summarise(Mean_Density_100m2 = 100 * mean(Density_m2), .groups = 'drop')

#Summarize Density by Site, DepthZone, and Species of Macroalgae
# Calculating minimum Euclidean distance between stormwater outfalls and reef sites
dist_matrix_sw <- st_distance(sw_outfalls, sitedata)

# Convert distance matrix to a data frame
dist_df_sw <- as.data.frame(as.matrix(dist_matrix_sw))
colnames(dist_df_sw) <- sitedata$Site
rownames(dist_df_sw) <- sw_outfalls$Number

# Calculate the minimum distance from each site to the nearest stormwater outfall
min_distances_sw <- apply(dist_df_sw, 2, min)
min_distances_sw_df <- data.frame(Site = names(min_distances_sw), Min_Dist_SW_meters = min_distances_sw)

# Calculating minimum Euclidean distance between wastewater outfalls and reef sites
dist_matrix_ww <- st_distance(ww_outfalls, sitedata)

# Convert distance matrix to a data frame
dist_df_ww <- as.data.frame(as.matrix(dist_matrix_ww))
colnames(dist_df_ww) <- sitedata$Site
rownames(dist_df_ww) <- ww_outfalls$Number

# Calculate the minimum distance from each site to the nearest wastewater outfall
min_distances_ww <- apply(dist_df_ww, 2, min)
min_distances_ww_df <- data.frame(Site = names(min_distances_ww), Min_Dist_WW_meters = min_distances_ww)

# Calculating minimum Euclidean distance between riverine outfalls and reef sites
dist_matrix_riverine <- st_distance(riverine_outfalls, sitedata)

# Convert distance matrix to a data frame
dist_df_riverine <- as.data.frame(as.matrix(dist_matrix_riverine))
colnames(dist_df_riverine) <- sitedata$Site
rownames(dist_df_riverine) <- riverine_outfalls$Number

# Calculate the minimum distance from each site to the nearest riverine outfall
min_distances_riverine <- apply(dist_df_riverine, 2, min)
min_distances_riverine_df <- data.frame(Site = names(min_distances_riverine), Min_Dist_R_meters = min_distances_riverine)


# Merging datasets into one dataset of minimum Euclidean distance between chosen reef sites and stormwater, wastewater, and riverine outfalls
merged_min_distances_1_2 <- full_join(min_distances_sw_df, min_distances_ww_df, by = "Site")
min_euclidean_dataset <- full_join(merged_min_distances_1_2, min_distances_riverine_df, by = "Site")

#Adding column with minimum distance from any type of outfall to reef sites
min_euclidean_dataset <- min_euclidean_dataset %>%
  mutate(Min_Dist_Any_meters = pmin(Min_Dist_SW_meters, Min_Dist_WW_meters, Min_Dist_R_meters, na.rm = TRUE))

# Print the merged dataset
print(min_euclidean_dataset)

#Arranging dataset in ascending distance order by column
min_euclidean_dataset_sorted <- min_euclidean_dataset %>%
  arrange(Min_Dist_SW_meters, Min_Dist_WW_meters, Min_Dist_R_meters, Min_Dist_Any_meters)

# Print the sorted dataset
print(min_euclidean_dataset_sorted)

# Adding densities to dataset (assuming avg_density_per_site is already loaded)
min_euclidean_density <- left_join(min_euclidean_dataset_sorted, mean_density_per_site, by = "Site")

#Excluding Sites with density values of "NA"
min_euclidean_density_filter <- min_euclidean_density %>%
  filter(!is.na(mean_density))
print(min_euclidean_density_filter)

#Summarizing Density
dat_gorg <- gorgoniandensity_clean %>%
  group_by(Site, DepthZone, Species) %>%
  summarise(Mean_Density_100m2 = 100*mean(Density_m2))

#Adding Depth Zone to dataset
min_euclidean_density_final <- left_join(min_euclidean_density_filter, dat_gorg, by = "Site") 

#Grouping data by depth zone, species, and site or analysis
dat_gorg <- gorgoniandensity_clean %>%
  group_by(DepthZone, Species, Site) %>%
  summarise(Mean_Density_100m2 = 100*mean(Density_m2))


