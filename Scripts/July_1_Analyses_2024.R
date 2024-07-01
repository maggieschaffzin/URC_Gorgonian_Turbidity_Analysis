# CREATION DATE 10 June 2024
# MODIFIED DATE: 1st July 2024 

# AUTHOR Maggie Schaffzin (schaffzin@oxy.edu)

# PURPOSE: Mapping study site, storm drain + waste water outputs, and reef sites along L.A. County Coast (PVR + Malibu), Euclidean distance dataset

# TO DO: Linear Regression Model


# PACKAGES: ----
library(tidyverse) 
library(sf)
library(ggspatial)
library(dplyr)
library(RColorBrewer)
library (raster)
library (png)
library (ggplot2)


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

  
#check coordinate system for Site Data
sitedata <- st_transform(sitedata, crs = "EPSG:4326")

# Filter based on latitude and longitude ranges
filtered_sites <- sites_selected %>%
  filter(Latitude > -119 & Latitude < -118 & Longitude > 33.6 & Longitude < 34.2)
#Reading California image
california_image <- readPNG("~/Documents/Gorgonian/CA_Reference_Map.png") 
  
## mapping SW, WW, riverine outfalls onto study site + Reef Sites
p <- ggplot()+
  geom_sf(data = californiamap) +
  geom_sf(data = sw_outfalls, aes(color = "Stormwater Outfalls"), show.legend = FALSE) +
  geom_sf(data = ww_outfalls, aes(color = "Waste Water Outfalls"), show.legend = FALSE) +
  geom_sf(data = riverine_outfalls, aes(color = "Riverine Outfalls"), show.legend = FALSE) +
  geom_sf(data = sitedata, aes(color = "Dive Sites Surveyed"), size = 0.1, show.legend = FALSE) +
  scale_color_manual(values = c("Stormwater Outfalls" = "blue", "Waste Water Outfalls" = "green", 
                                "Riverine Outfalls" = "turquoise", "Dive Sites Surveyed" = "purple")) +
  coord_sf(crs = ,
           xlim = c(-119, -118), ylim = c(33.6, 34.2),
           expand = FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Study Site: SMB/Malibu and PVR") +
  #Overlaying Reference Image into Map
  annotation_raster(california_image, xmin = -118.05, xmax = -118.35, ymin = 33.9, ymax = 34.15) +
  annotation_north_arrow(location = "topleft", style = north_arrow_fancy_orienteering) +
  theme_classic()

# Creating a standalone legend plot
legend_plot <- ggplot() +
  geom_point(aes(x = 1, y = 1, color = "Stormwater Outfalls"), size = 1) +
  geom_point(aes(x = 1, y = 2, color = "Waste Water Outfalls"), size = 1) +
  geom_point(aes(x = 1, y = 1, color = "Riverine Outfalls"), size = 1) +
  geom_point(aes(x = 1, y = 2, color = "Dive Sites Surveyed"), size = 1) +
  scale_color_manual(values = c("Stormwater Outfalls" = "blue", "Waste Water Outfalls" = "green","Riverine Outfalls" = "turquoise", "Dive Sites Surveyed" = "purple" )) +
  guides(color = guide_legend(title = NULL)) +
  theme_void()

# Adding legend to the plot
p +
  annotation_custom(grob = ggplotGrob(legend_plot), 
                    xmin = -118.9, xmax = -118.7, ymin = 33.7, ymax = 33.9)

#Adding Scale Bar

#Calculating mean Gorgonian density by site
mean_density_per_site <- final_data %>%
  group_by(Site, DepthZone) %>%
  summarize(mean_density = mean((Density_m2*100), na.rm = TRUE)) %>%
  arrange(desc(mean_density))

print(mean_density_per_site)

# Make DepthZone an ordered factor
mean_density_per_site$DepthZone <- factor(mean_density_per_site$DepthZone, levels = c("Inner", "Middle", "Outer", "Deep", "ARM"))

#Calculating mean gorgonian density by depth zone
mean_density_per_depth <- final_data %>%
  group_by(DepthZone) %>%
  summarize(mean_density = mean((Density_m2 *100), na.rm = TRUE))

# Make DepthZone an ordered factor
mean_density_per_depth$DepthZone <- factor(mean_density_per_depth$DepthZone, levels = c("Inner", "Middle", "Outer", "Deep", "ARM"))

#Plotting Density by Depth Zone
ggplot(mean_density_per_depth, aes(x = DepthZone, y = mean_density)) +
  geom_bar(stat = "identity", color = "black", fill = "red", width = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  geom_text(aes(label = round(mean_density, 2)), vjust = -0.3, size = 5) +
  labs(title = "Mean Gorgonian Density by Depth Zone", 
       x = "Depth Zone", 
       y = "Mean Density (per 100m2)") +
  theme_minimal(base_size = 15) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 15), 
    axis.title.y = element_text(face = "bold", size = 15), 
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none" 
  )

# Summarize Density by Site, DepthZone, and Species of Gorgonian
dat_gorg <- gorgoniandensity_clean %>%
  group_by(Site, DepthZone, Species) %>%
  summarise(Mean_Density_100m2 = 100 * mean(Density_m2), .groups = 'drop')

#Calculating mean gorgonian density by species
mean_density_per_species <- final_data %>%
  group_by(Species) %>%
  summarize(mean_density = mean((Density_m2 * 100), na.rm = TRUE))
print(mean_density_per_species)

#Plotting Density by Species
ggplot(mean_density_per_species, aes(x = Species, y = mean_density)) +
  geom_bar(stat = "identity", color = "black", fill = "blue", width = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  geom_text(aes(label = round(mean_density, 2)), vjust = -0.3, size = 5) +
  labs(title = "Mean Gorgonian Density by Species", 
       x = "Species", 
       y = "Mean Density (per 100m2)") +
  theme_minimal(base_size = 15) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 15), 
    axis.title.y = element_text(face = "bold", size = 15), 
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none" 
  )

#Calculating mean density of MURCAL by Depth Zone
mean_density_muricea <- final_data %>%
  filter (Species == "Muricea californica")  %>%
  group_by(DepthZone) %>%
  summarize(mean_density = mean((Density_m2 *100), na.rm = TRUE))
print(mean_density_muricea)

# Make DepthZone an ordered factor
mean_density_muricea$DepthZone <- factor(mean_density_muricea$DepthZone, levels = c("Inner", "Middle", "Outer", "Deep", "ARM"))

#Plotting MURCAL Density by Depth Zone
ggplot(mean_density_muricea, aes(x = DepthZone, y = mean_density)) +
  geom_bar(stat = "identity", color = "black", fill = "lightblue", width = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  geom_text(aes(label = round(mean_density, 2)), vjust = -0.3, size = 5) +
  labs(title = "Mean Density of Muricea californica by Depth Zone", 
       x = "Depth Zone", 
       y = "Mean Density (per 100m2)") +
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

# Calculate mean gorgonian density for sites with "AR" in their title
mean_density_ar_sites <- ar_sites_data %>%
  group_by(Site, DepthZone) %>%
  summarize(mean_density = mean(Density_m2 * 100, na.rm = TRUE)) %>%
  arrange(desc(mean_density))
print (mean_density_ar_sites)

#Filtering dataset to only include NR
nr_sites <- final_data %>%
  filter(!grepl("ARM", DepthZone))

# Calculating mean gorgonian density for all  NR sites
mean_density_nr_sites <- nr_sites %>%
  group_by(Site, DepthZone) %>%
  summarize(mean_density = mean(Density_m2 * 100, na.rm = TRUE)) %>%
  arrange(desc(mean_density))


print(mean_density_nr_sites)

#Make DepthZone an ordered factor
mean_density_nr_sites$DepthZone <- factor(mean_density_nr_sites$DepthZone, levels = c("Inner", "Middle", "Outer", "Deep", "ARM"))

#Installing ggrepel
#install.packages("ggrepel")

#Plotting density at NR Sites by depth
ggplot(mean_density_nr_sites, aes(x = DepthZone, y = mean_density)) +
  geom_boxplot(alpha = 0.7) +  
  geom_jitter(width = 0.1, alpha = 0.5, color = "black") +
  geom_text_repel(aes(label = ifelse(mean_density >= 50, Site, "")), size = 3) + 
  scale_fill_brewer(palette = "Pastel1") +  
  labs(x = "Depth Zone", y = "Mean Gorgonian Density (100m2)") +  
  ggtitle("Mean Gorgonian Density by Depth on NR Sites") +  
  theme(
    text = element_text(family = "Times New Roman"), 
    plot.title = element_text(size = 16, face = "bold"),  
    axis.title = element_text(size = 14),  
    legend.position = "none"  
  )

#Data exploration: Filtering dataset to only include certain Malibu sites, looking at by year
#nr_sites <- final_data %>%
  #filter(grepl("Big Rock|El Matador|La Piedra|El Pescador|Lechuza|Leo Carrillo|Nicholas Canyon East|El Sol|Point Dume|Portugese Point", Site))

#Adding Sample Year
#nr_sites<- nr_sites %>%
  #left_join(gorgoniandensity_clean %>% dplyr::select(Site, DepthZone, SampleYear),
           # by = c("Site", "DepthZone"))

#Calculating mean gorgonian density for Malibu NR sites
#mean_density_nr_sites <- nr_sites %>%
  #group_by(Site, DepthZone, SampleYear) %>%
  #summarize(mean_density = mean(Density_m2 * 100, na.rm = TRUE)) %>%
  #arrange(desc(mean_density))

#Plotting Malibu NR Reef sites
#ggplot(mean_density_nr_sites, aes(x = SampleYear, y = mean_density, group = Site)) +
  #geom_point(aes(color = Site)) +  
  #geom_jitter(width = 0.1, alpha = 0.5, color = "black") +
  #geom_text_repel(aes(label = ifelse(mean_density >= 25, Site, "")), size = 3) + 
  #scale_color_brewer(palette = "Paired") +  
  #labs(x = "Sample Year", y = "Mean Gorgonian Density (100m2)", title = "Mean Gorgonian Density by Depth on NR Sites") +  
 # theme(
    #text = element_text(family = "Times New Roman"), 
    #plot.title = element_text(size = 16, face = "bold"),  
    #axis.title = element_text(size = 14),  
    #legend.position = "none")

#Site chart for Chelsea
#site_list <- mean_density_per_site %>%
#dplyr::select(Site)
#print(site_list)
#write.csv(site_list, file = "site_list_schaffzin_srp24", row.names = FALSE)

#Plotting NR. vs AR densities
ggplot(combined_data, aes(x = site_type, y = mean_density, fill = site_type)) +
  geom_boxplot(alpha = 0.7) +  
  geom_jitter(width = 0.1, alpha = 0.5, color = "black") +  
  scale_fill_brewer(palette = "Pastel1") +  
  labs(x = "Site Type", y = "Mean Gorgonian Density (100m2)") +  
  ggtitle("Mean Gorgonian Density on AR vs NR Sites") +  
  theme(
    text = element_text(family = "Times New Roman"), 
    plot.title = element_text(size = 16, face = "bold"),  
    axis.title = element_text(size = 14),  
    legend.position = "none"  
  )

#Linear Model for SW
lm_model <- lm(mean_density ~ Min_Dist_SW_meters, data = min_euclidean_density_final)

# Print the summary of the linear model
summary(lm_model)

#Scatterplot for Linear Model for SW
p<- ggplot(data = min_euclidean_density_final, aes(x = Min_Dist_SW_meters, y = Mean_Density_100m2)) +
  geom_point(aes(color = DepthZone)) +  
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +  
  labs(x = "Minimum Distance to Stormwater Outfall (meters)", y = "Mean Density (per 100m2)") +  
  ggtitle("Linear Regression of Mean Density by Minimum Distance to Stormwater Outfall") +  
  theme_minimal()+
  facet_grid(Species ~ DepthZone, scales = "free")
p

#Linear Model for SW
lm_model <- lm(mean_density ~ Min_Dist_SW_meters, data = min_euclidean_density_final)

# Print the summary of the linear model
summary(lm_model)

#Scatterplot for Linear Model for SW
p<- ggplot(data = min_euclidean_density_final, aes(x = Min_Dist_SW_meters, y = Mean_Density_100m2)) +
  geom_point(aes(color = DepthZone)) +  
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +  
  labs(x = "Minimum Distance to Stormwater Outfall (meters)", y = "Mean Density (per 100m2)") +  
  ggtitle("Linear Regression of Mean Density by Minimum Distance to Stormwater Outfall") +  
  theme_minimal()+
  facet_grid(Species ~ DepthZone, scales = "free_y")
p
#Calculate slope from lm_model (SW)
slope <- coef(lm_model)["Min_Dist_SW_meters"]

#Add Slope to Scatterplot
p<- p + annotate("text", x = max(min_euclidean_density_final$Min_Dist_SW_meters) - 10, 
           y = min(min_euclidean_density_final$mean_density) + 10,
           label = paste("Slope:", round(slope, 4)), 
           color = "black", size = 5, hjust = 1)
print(p)

#Linear Model for WW
lm_model2 <- lm(mean_density ~ Min_Dist_WW_meters, data = min_euclidean_density_final)

# Print the summary of the linear model
summary(lm_model2)

#Scatterplot for Linear Model for WW
p2 <- ggplot(data = min_euclidean_density_final, aes(x = Min_Dist_WW_meters, y = Mean_Density_100m2)) +
  geom_point(aes(color = DepthZone)) +  
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +  
  labs(x = "Minimum Distance to Wastewater Outfall (meters)", y = "Mean Density (per 100m2)") +  
  ggtitle("Linear Regression of Mean Density by Minimum Distance to Wastewater Outfall") +  
  theme_minimal()+
  facet_grid(Species ~ DepthZone, scales = "free")
p2
#Calculate slope from lm_model (WW)
slope <- coef(lm_model)["Min_Dist_WW_meters"]

#Add Slope to Scatterplot
p2<- p2 + annotate("text", x = max(min_euclidean_density_final$Min_Dist_WW_meters) - 10, 
                 y = min(min_euclidean_density_final$mean_density) + 10,
                 label = paste("Slope:", round(slope, 4)), 
                 color = "black", size = 5, hjust = 1)
print(p2)

#Linear Model for Riverine
lm_model3 <- lm(mean_density ~ Min_Dist_R_meters, data = min_euclidean_density_final)

# Print the summary of the linear model
summary(lm_model3)

#Scatterplot for Linear Model for Riverine
p3 <- ggplot(data = min_euclidean_density_final, aes(x = Min_Dist_R_meters, y = Mean_Density_100m2)) +
  geom_point(aes(color = DepthZone)) +  
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +  
  labs(x = "Minimum Distance to Riverine Outfall (meters)", y = "Mean Density (per 100m2)") +  
  ggtitle("Linear Regression of Mean Density by Minimum Distance to Riverine Outfall") +  
  theme_classic()+
  facet_grid(Species ~ DepthZone, scales = "free")
p3

#Calculate slope from lm_model (Riverine)
slope <- coef(lm_model)["Min_Dist_R_meters"]

#Add Slope to Scatterplot
p3<- p3 + annotate("text", x = max(min_euclidean_density_final$Min_Dist_WW_meters) - 10, 
                   y = min(min_euclidean_density_final$mean_density) + 10,
                   label = paste("Slope:", round(slope, 4)), 
                   color = "black", size = 5, hjust = 1)
print(p3)

#Linear Model for MINIMUM ANYTHING
lm_model4 <- lm(mean_density ~ Min_Dist_Any_meters, data = min_euclidean_density_final)

# Print the summary of the linear model
summary(lm_model4)

#Scatterplot for Linear Model for WW
p4 <- ggplot(data = min_euclidean_density_final, aes(x = Min_Dist_Any_meters, y = Mean_Density_100m2)) +
  geom_point(aes(color = DepthZone)) +  
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +  
  labs(x = "Minimum Distance to Any Outfall Source (meters)", y = "Mean Density (per 100m2)") +  
  ggtitle("Linear Regression of Mean Density by Minimum Distance to Any Outfall Source") +  
  theme_classic()+
  facet_grid(Species ~ DepthZone, scales = "free")
p4

#Calculate slope from lm_model (WW)
slope <- coef(lm_model)["Min_Dist_Any_meters"]

#Add Slope to Scatterplot
p4<- p4 + annotate("text", x = max(min_euclidean_density_final$Min_Dist_Any_meters) - 10, 
                   y = min(min_euclidean_density_final$mean_density) + 10,
                   label = paste("Slope:", round(slope, 4)), 
                   color = "black", size = 5, hjust = 1)
print(p4)


# Calculate the mean density per 100m2 by DepthZone and SpeciesGroupF for MACRO and GORG
mean_density_both <- macrodensity_subset %>%
  group_by(Site, DepthZone, SpeciesGroupF) %>%
  summarize(mean_density_per_100m2 = mean(Density_m2) * 100, .groups = 'drop')

# View the result
print(mean_density_both)

#changing kelp column name 
mean_density_both<- mean_density_both %>%
  mutate(SpeciesGroupF = ifelse(SpeciesGroupF == "kelp - understory", "kelp", SpeciesGroupF))

# View the result
print(mean_density_both)

#Make DepthZone an ordered factor including PVR
mean_density_both$DepthZone <- factor(mean_density_both$DepthZone, levels = c("Inner", "Middle", "Outer", "Deep", "ARM", "PVR"))

# Reshape the data to have separate columns for mean density of GORG and MACRO
mean_density_wide <- mean_density_both %>%
  pivot_wider(
    names_from = SpeciesGroupF,
    values_from = mean_density_per_100m2,
    names_prefix = "mean_density_"
  )
#View the result
print(mean_density_wide)

#Linear Model for Macro vs Gorg Density
lm_model5 <- lm(mean_density_gorgonians ~ mean_density_kelp, data = mean_density_wide)

# Print the summary of the linear model
summary(lm_model5)

#Scatterplot for Linear Model for Macro vs Gorg Density
p5 <- ggplot(data = mean_density_wide, aes(x = mean_density_gorgonians^(1/4), y = mean_density_kelp^(1/4))) +
  geom_point(aes(color = DepthZone)) +  
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +  
  labs(x = "Mean Gorgonian Density (per 100m2)", y = "Mean Macroalgae Density (per 100m2)") +  
  ggtitle("Linear Regression of Mean Gorgonian Density to Mean Macroalgae Density") +  
  theme_classic()
p5




