# CREATION DATE 10 June 2024
# MODIFIED DATE: 23 July 2024 

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
crs(sitedata)
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
  
#Create the main map
p2 <- ggplot() +
    geom_sf(data = californiamap, fill = "antique white", color = "black") +
    geom_sf(data = sw_outfalls, aes(color = "Stormwater Outfalls"), show.legend = FALSE, fill = "blue") +
    geom_sf(data = ww_outfalls, aes(color = "Waste Water Outfalls"), show.legend = FALSE) +
    geom_sf(data = riverine_outfalls, aes(color = "Riverine Outfalls"), show.legend = FALSE) +
    geom_sf(data = sitedata, aes(color = "Dive Sites Surveyed"), size = 0.1, show.legend = FALSE) +
    scale_color_manual(values = c("Stormwater Outfalls" = "blue", "Waste Water Outfalls" = "green", 
                                  "Riverine Outfalls" = "red", "Dive Sites Surveyed" = "purple")) +
    coord_sf(crs = st_crs(californiamap), # Set CRS to match your data
             xlim = c(xmin, xmax), ylim = c(ymin, ymax),
             expand = FALSE) +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("Study Site") +

#Overlaying Reference Image into Map
annotation_raster(california_image, xmin = -118.05, xmax = -118.35, ymin = 33.9, ymax = 34.15) +
annotation_north_arrow(location = "tl", style = north_arrow_fancy_orienteering) +
theme_classic() +
  theme(panel.background = element_rect(fill = "light blue"), # Light blue water
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1), # Neatline
     plot.background = element_rect(fill = "white"),
     panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
          text = element_text(family = "Times New Roman"))
  
# Creating a standalone legend plot
  legend_plot <- ggplot() +
    geom_point(aes(x = 1, y = 1, color = "Stormwater Outfalls"), size = 1) +
    geom_point(aes(x = 1, y = 2, color = "Waste Water Outfalls"), size = 1) +
    geom_point(aes(x = 1, y = 1, color = "Riverine Outfalls"), size = 1) +
    geom_point(aes(x = 1, y = 2, color = "Dive Sites Surveyed"), size = 1) +
    scale_color_manual(values = c("Stormwater Outfalls" = "blue", "Waste Water Outfalls" = "green","Riverine Outfalls" = "red", "Dive Sites Surveyed" = "purple" )) +
    guides(color = guide_legend(title = NULL)) +
    theme_void()
  
# Combine the map, legend, and scale bar
  final_plot <- p2 +
    annotation_custom(grob = ggplotGrob(legend_plot), 
                      xmin = -118.9, xmax = -118.7, ymin = 33.7, ymax = 33.9) 
   
# Display the final map
  print(final_plot)

#Saving Map for Poster
ggsave("~/Documents/Gorgonian/URC_Gorgonian_Turbidity_Analysis/Figures/StudySite.png", plot = final_plot, width = 8, height = 6, dpi = 300)
  
  
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

# Make DepthZone an ordered factor
combined_data$DepthZone <- factor(combined_data$DepthZone, levels = c("Inner", "Middle", "Outer", "Deep", "ARM"))
#Plotting density at NR Sites by depth
ggplot(combined_data, aes(x = DepthZone, y = mean_density)) +
  geom_boxplot(alpha = 0.7, fill = "lightblue", color = "darkblue") +
  geom_jitter(width = 0.1, alpha = 0.6, color = "blue", shape = 16, size = 2) +
  geom_text_repel(aes(label = ifelse(mean_density >= 62, Site, "")), size = 3, box.padding = 0.3) +
  labs(
    x = "Depth Zone",
    y = "Mean Gorgonian Density (100m²)",
    title = "Mean Gorgonian Density by Depth on NR Sites"
  ) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "none"
  )

#Running ANOVA for NR vs AR
anova_result <- aov(mean_density ~ site_type, data = combined_data)
summary(anova_result)

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
    
    
#Creating Site Type columns
combined_data2 <- mean_density_per_site_clean %>%
  mutate(SiteType = ifelse(DepthZone %in% c("Inner", "Middle", "Outer", "Deep"), "NR", 
                           ifelse(DepthZone == "ARM", "AR", NA)))

# Make DepthZone an ordered factor
combined_data2$DepthZone <- factor(combined_data2$DepthZone, levels = c("Inner", "Middle", "Outer", "Deep", "ARM"))
# Check the first few rows to ensure it worked
head(combined_data2)

#Defining Pastel Colors
pastel_colors <- c("#FBB4AE", "#B3CDE3", "#CCEBC5", "#FED9A6", "#DECBE4")

#Plotting NR. vs AR densities
p1<- ggplot(combined_data2, aes(x = SiteType, y = mean_density, fill = DepthZone)) +
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.6, alpha = 0.7, color = "black") +
  scale_fill_manual(values = pastel_colors, breaks = c("Inner", "Middle", "Outer", "Deep", "ARM"), labels = c("Inner", "Middle", "Outer", "Deep", "AR")) +
  geom_text_repel(aes(label = ifelse(SiteType == "NR" & mean_density >= 54, Site, "")), 
                  size = 3, 
                  box.padding = 0.3, 
                  point.padding = 0.5, 
                  min.segment.length = 0.2, 
                  max.overlaps = Inf) +
  labs(
    x = "Site Type",
    y = "Mean Gorgonian Density (100m²)",
    fill = "Depth Zone" 
  ) +
  theme_classic(base_family = "Times New Roman") +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, face = "italic", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"), 
    legend.position = "right",
    legend.box = "horizontal",
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  )

p1
#Saving Plot for Figure
ggsave("~/Documents/Gorgonian/URC_Gorgonian_Turbidity_Analysis/Figures/NRAR.png", plot = p1, width = 6, height = 8, dpi = 300)

#Linear Model for SW
lm_model <- lm(mean_density ~ Min_Dist_SW_meters, data = min_euclidean_density_final)

# Print the summary of the linear model
summary(lm_model)

#Scatterplot for Linear Model for SW
p<- ggplot(data = min_euclidean_density_final, aes(x = Min_Dist_SW_meters, y = Mean_Density_100m2)) +
  geom_point(aes(color = DepthZone)) +  
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE) +  
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
lm_model4 <- lm(Min_Dist_Any_meters ~ mean_density, data = min_euclidean_density_final)

# Print the summary of the linear model
summary(lm_model4)

#Adding Region to Any Point Source Dataset
min_euc_dist_final<- left_join(min_euclidean_density_final, sitedata_clean, by = "Site")

#Filtering Data to exclude datapoints in ARs that aren't ARM
filtered_data <- min_euc_dist_final %>%
  filter(!is.na(Region) & 
           (Region %in% c("Palos Verdes", "Malibu") | 
              !(Region == "Artificial Reef" & DepthZone.y %in% c("Inner", "Middle", "Outer"))))
#Changing DepthZone
# Make DepthZone an ordered factor
filtered_data$DepthZone.y <- factor(filtered_data$DepthZone.y, levels = c("Inner", "Middle", "Outer", "Deep", "ARM"))

#Renaming DepthZone.y to DepthZone for the plot
filtered_data <- filtered_data %>%
  rename(DepthZone = DepthZone.y)


#Scatterplot for Linear Model for Any Point Source
p4 <- ggplot(data = filtered_data, aes(x = Min_Dist_Any_meters, y = Mean_Density_100m2)) +
  geom_point(aes(color = DepthZone), size = 3, alpha = 0.7) +  # Adjust point size and transparency
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "black") +  
  labs(

    x = "Minimum Distance to Any Outfall Source (meters)",
    y = "Mean Golden Gorgonian Density (per 100m²)",
    color = "Depth Zone"
  ) +  
  scale_color_manual(values = pastel_colors) +  # Apply custom colors
  theme_minimal(base_family = "Times New Roman") +  # Use a minimal theme as the base
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, face = "italic", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    strip.text.x = element_text(size = 12, color = "darkblue"),
    strip.text.y = element_text(size = 12),
    strip.background = element_rect(fill = "lightblue", color = NA),
    panel.background = element_rect(fill = "aliceblue", color = NA),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  facet_grid(DepthZone ~ Region)

p4

#Saving Euclidean Figure for Poster
#Exporting Figure for Poster
ggsave("~/Documents/Gorgonian/URC_Gorgonian_Turbidity_Analysis/Figures/Euclidean.png", plot = p4, width = 8, height = 6, dpi = 300)

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

#Plotting Mean Density to Relief Index
ggplot(habitat_info_density, aes(x = Relief_index, y = mean_density)) +
  geom_point() +
  labs(title = "Mean Density vs. Relief Index",
       x = "Relief Index",
       y = "Mean Density") +
  theme_classic()

#Linear Model for Mean Density vs Relief Index
lm_model6 <- lm(Relief_index ~ mean_density, data = habitat_info_density)

# Print the summary of the linear model
summary(lm_model6)

#Scatterplot for Linear Model for Relief Index
p5 <- ggplot(data = habitat_info_density, aes(x = Relief_index, y = mean_density)) +
  geom_point(aes(color = DepthZone)) +  
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +  
  labs(x = "Relief Index", y = "Mean Density (per 100m2)") +  
  ggtitle("Linear Regression of Mean Density by Relief Index") +  
  theme_classic()+
  facet_grid(scales = "free")
p5

#Plotting Mean Density to Relief SD
ggplot(habitat_info_density, aes(x = Relief_SD, y = mean_density)) +
  geom_point() +
  labs(title = "Mean Density vs. Relief Standard Deviation",
       x = "Relief Standard Deviation",
       y = "Mean Density") +
  theme_classic()

#Linear Model for Mean Density vs Relief SD
lm_model7 <- lm(Relief_SD ~ mean_density, data = habitat_info_density)

# Print the summary of the linear model
summary(lm_model7)

#Scatterplot for Linear Model for Relief Index
p6 <- ggplot(data = habitat_info_density, aes(x = Relief_SD, y = mean_density)) +
  geom_point(aes(color = DepthZone)) +  
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +  
  labs(x = "Relief Standard Deviation (SD)", y = "Mean Density (per 100m2)") +  
  ggtitle("Linear Regression of Mean Density by Relief SD") +  
  theme_classic()+
  facet_grid(scales = "free")
p6

#Plotting Mean Density to Substrate Index
ggplot(habitat_info_density, aes(x = Substrate_index, y = mean_density)) +
  geom_point() +
  labs(title = "Mean Density vs. Substrate Index",
       x = "Substrate Index",
       y = "Mean Density") +
  theme_classic()

#Linear Model for Mean Density vs Substrate Index
lm_model8 <- lm(Substrate_index ~ mean_density, data = habitat_info_density)

Include # Print the summary of the linear model
summary(lm_model8)

#Scatterplot for Linear Model for Substrate Index
p7 <- ggplot(data = habitat_info_density, aes(x = Substrate_index, y = mean_density)) +
  geom_point(aes(color = DepthZone)) +  
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +  
  labs(x = "Substrate Index", y = "Mean Density (per 100m2)") +  
  ggtitle("Linear Regression of Mean Density by Substrate Index") +  
  theme_classic()+
  facet_grid(scales = "free")
p7

#Plotting Mean Density to Substrate SD
ggplot(habitat_info_density, aes(x = Substrate_SD, y = mean_density)) +
  geom_point() +
  labs(title = "Mean Density vs. Substrate Standard Deviation (SD)",
       x = "Substrate SD",
       y = "Mean Density") +
  theme_classic()

#Linear Model for Mean Density vs Substrate SD
lm_model9 <- lm(Substrate_SD ~ mean_density, data = habitat_info_density)

# Print the summary of the linear model
summary(lm_model9)

#Scatterplot for Linear Model for Substrate SD
p8 <- ggplot(data = habitat_info_density, aes(x = Substrate_SD, y = mean_density)) +
  geom_point(aes(color = DepthZone)) +  
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +  
  labs(x = "Substrate Standard Deviation (SD)", y = "Mean Density (per 100m2)") +  
  ggtitle("Linear Regression of Mean Density by Substrate SD") +  
  theme_classic()+
  facet_grid(scales = "free")
p8

# Filter out ARM DepthZone
habitat_info_density_3 <- habitat_info_density_2 %>%
  filter(Region != "Artificial Reef")

#Linear Model for Mean Density vs Substrate SD
lm_model10 <- lm(Substrate_SD ~ mean_density, data = habitat_info_density_3)

# Print the summary of the linear model
summary(lm_model10)

#Make DepthZone an ordered factor including PVR
habitat_info_density_3$DepthZone <- factor(habitat_info_density_3$DepthZone, levels = c("Inner", "Middle", "Outer", "Deep", "ARM", "PVR"))

# Fit linear models with zero intercepts for each region
malibu_data <- subset(habitat_info_density_3, Region == "Malibu")
pv_data <- subset(habitat_info_density_3, Region == "Palos Verdes")

malibu_model <- lm(mean_density ~ Substrate_SD - 1, data = malibu_data)
pv_model <- lm(mean_density ~ Substrate_SD - 1, data = pv_data)

# Generate predictions
malibu_data$predicted <- predict(malibu_model, newdata = malibu_data)
pv_data$predicted <- predict(pv_model, newdata = pv_data)
malibu_data$se <- predict(malibu_model, newdata = malibu_data, se.fit = TRUE)$se.fit
pv_data$se <- predict(pv_model, newdata = pv_data, se.fit = TRUE)$se.fit

# Combine data back
combined_data2 <- rbind(malibu_data, pv_data)

# Define colors and alpha values
region_colors <- c("Malibu" = "purple", "Palos Verdes" = "blue")
alpha_values <- c("Inner" = 0.25, "Middle" = 0.5, "Outer" = 0.75, "Deep" = 1.0)

# Scatterplot with enhanced aesthetics
p9 <- ggplot(data = habitat_info_density_3, aes(x = Substrate_SD, y = mean_density)) +
  geom_point(aes(color = Region, alpha = DepthZone), size = 3, shape = 16) + 
  geom_line(data = combined_data2, aes(x = Substrate_SD, y = predicted, group = Region), color = "black", linewidth = 1) +
  geom_ribbon(data = combined_data2, aes(x = Substrate_SD, ymin = predicted - se, ymax = predicted + se, fill = Region), alpha = 0.2) +
  labs(
    x = "Substrate SD",
    y = "Mean Gorgonian Density (per 100m²)",
    alpha = "Depth Zone"
  ) +  
  scale_color_manual(values = region_colors) +
  scale_alpha_manual(values = alpha_values) + 
  scale_fill_manual(values = region_colors) +
  theme_classic(base_family = "Times New Roman") + 
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, face = "italic", hjust = 0.5),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.margin = margin(10, 10, 10, 10),
    strip.text = element_text(size = 12, color = "darkblue"),
    strip.background = element_rect(fill = "lightblue", color = NA),
  ) +
  facet_wrap(~ Region)

print(p9)

#Exporting Figure for Poster
ggsave("~/Documents/Gorgonian/URC_Gorgonian_Turbidity_Analysis/Figures/SubstrateSD.png", plot = p9, width = 8, height = 6, dpi = 300)


# Scatterplot for Linear Model for Substrate Index only Natural Reefs
p10 <- ggplot(data = habitat_info_density_filtered, aes(x = Substrate_index, y = mean_density)) +
  geom_point(aes(color = DepthZone)) +  
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "black") +  
  labs(x = "Substrate Index", y = "Mean Density (per 100m2)") +  
  ggtitle("Linear Regression of Mean Density by Substrate Index") +  
  theme_classic() +
  facet_grid(scales = "free")

# Print the plot
p10

#Linear Model for Mean Density vs Substrate Index
lm_model11 <- lm(Substrate_index ~ mean_density, data = habitat_info_density_filtered)

# Print the summary of the linear model
summary(lm_model11)

# Scatterplot for Linear Model for Relief SD only Natural Reefs
p11 <- ggplot(data = habitat_info_density_filtered, aes(x = Relief_SD, y = mean_density)) +
  geom_point(aes(color = DepthZone)) +  
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +  
  labs(x = "Relief SD", y = "Mean Density (per 100m2)") +  
  ggtitle("Linear Regression of Mean Density by Relief SD") +  
  theme_classic() +
  facet_grid(scales = "free")

# Print the plot
p11

#Linear Model for Mean Density vs Substrate Index
lm_model12 <- lm(Relief_SD ~ mean_density, data = habitat_info_density_filtered)

# Print the summary of the linear model
summary(lm_model12)


# Scatterplot for Linear Model for Relief Index only Natural Reefs
p12 <- ggplot(data = habitat_info_density_filtered, aes(x = Relief_index, y = mean_density)) +
  geom_point(aes(color = DepthZone)) +  
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +  
  labs(x = "Relief Index", y = "Mean Density (per 100m2)") +  
  ggtitle("Linear Regression of Mean Density by Relief Index") +  
  theme_classic() +
  facet_grid(scales = "free")

# Print the plot
p12

#Linear Model for Mean Density vs Substrate Index
lm_model13 <- lm(Relief_index ~ mean_density, data = habitat_info_density_filtered)

# Print the summary of the linear model
summary(lm_model13)

# Filter to include only ARM DepthZone
habitat_info_density_ARM <- habitat_info_density %>%
  filter(DepthZone == "ARM")

# Scatterplot for Linear Model for Relief Index only Natural Reefs
p13 <- ggplot(data = habitat_info_density_ARM, aes(x = Relief_index, y = mean_density)) +
  geom_point(aes(color = DepthZone)) +  
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +  
  labs(x = "Relief Index", y = "Mean Density (per 100m2)") +  
  ggtitle("Linear Regression of Mean Density by Relief Index on AR") +  
  theme_classic() +
  facet_grid(scales = "free")

# Print the plot
p13

#Linear Model for Mean Density vs Substrate Index
lm_model14 <- lm(Relief_index ~ mean_density, data = habitat_info_density_ARM)

# Print the summary of the linear model
summary(lm_model14)

# Scatterplot for Linear Model for Relief SD only Natural Reefs
p14 <- ggplot(data = habitat_info_density_ARM, aes(x = Relief_SD, y = mean_density)) +
  geom_point(aes(color = DepthZone)) +  
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +  
  labs(x = "Relief Standard Deviation (SD)", y = "Mean Density (per 100m2)") +  
  ggtitle("Linear Regression of Mean Density by Relief SD on AR") +  
  theme_classic() +
  facet_grid(scales = "free")

# Print the plot
p14

#Linear Model for Mean Density vs Substrate Index
lm_model15 <- lm(Relief_SD ~ mean_density, data = habitat_info_density_ARM)

# Print the summary of the linear model
summary(lm_model15)

# Scatterplot for Linear Model for Substrate Index only Natural Reefs
p15 <- ggplot(data = habitat_info_density_ARM, aes(x = Substrate_index, y = mean_density)) +
  geom_point(aes(color = DepthZone)) +  
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +  
  labs(x = "Substrate Index", y = "Mean Density (per 100m2)") +  
  ggtitle("Linear Regression of Mean Density by Substrate Index on AR") +  
  theme_classic() +
  facet_grid(scales = "free")

# Print the plot
p15

#Linear Model for Mean Density vs Substrate Index
lm_model16 <- lm(Substrate_index ~ mean_density, data = habitat_info_density_ARM)

# Print the summary of the linear model
summary(lm_model16)

# Scatterplot for Linear Model for Substrate SD only Natural Reefs
p16 <- ggplot(data = habitat_info_density_ARM, aes(x = Substrate_SD, y = mean_density)) +
  geom_point(aes(color = DepthZone)) +  
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +  
  labs(x = "Substrate Standard Deviation (SD)", y = "Mean Density (per 100m2)") +  
  ggtitle("Linear Regression of Mean Density by Substrate SD on AR") +  
  theme_classic() +
  facet_grid(scales = "free")

# Print the plot
p16

#Linear Model for Mean Density vs Substrate Index
lm_model17 <- lm(Substrate_SD ~ mean_density, data = habitat_info_density_ARM)

# Print the summary of the linear model
summary(lm_model17)

# Filter to include only Middle DepthZone
habitat_info_density_PV <- habitat_info_density %>%
  filter(Site == "Middle")

# Scatterplot for Linear Model for Substrate SD only Natural Reefs
p17 <- ggplot(data = habitat_info_density_deep, aes(x = Substrate_SD, y = mean_density)) +
  geom_point() +  
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") + 
  geom_text_repel(aes(label = ifelse(mean_density >= 5, Site, "")), size = 3) + 
  labs(x = "Substrate Standard Deviation (SD)", y = "Mean Density (per 100m2)") +  
  ggtitle("Linear Regression of Mean Density by Substrate SD (Middle DepthZone)") +  
  theme_classic() +
  facet_grid(scales = "free")

# Print the plot
p17

Y#Linear Model for Mean Density vs Substrate SD for Middle Depth Zone
lm_model18 <- lm(Substrate_SD ~ mean_density, data = habitat_info_density_deep)

# Print the summary of the linear model
summary(lm_model18)

# Scatterplot for Linear Model for Substrate Index only Natural Reefs
p18 <- ggplot(data = habitat_info_density_deep, aes(x = Substrate_index, y = mean_density)) +
  geom_point() +  
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +  
  labs(x = "Substrate Index", y = "Mean Density (per 100m2)") +  
  ggtitle("Linear Regression of Mean Density by Substrate Index (Deep DepthZone)") +  
  theme_classic() +
  facet_grid(scales = "free")

# Print the plot
p18

#Linear Model for Mean Density vs Substrate SD for Middle Depth Zone
lm_model19 <- lm(Substrate_index ~ mean_density, data = habitat_info_density_deep)

# Print the summary of the linear model
summary(lm_model19)

# Scatterplot for Linear Model for Relief Index for Middle Depth Zone
p19 <- ggplot(data = habitat_info_density_deep, aes(x = Relief_index, y = mean_density)) +
  geom_point() +  
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +  
  labs(x = "Relief Index", y = "Mean Density (per 100m2)") +  
  ggtitle("Linear Regression of Mean Density by Relief Index (Outer DepthZone)") +  
  theme_classic() +
  facet_grid(scales = "free")

# Print the plot
p19

#Linear Model for Mean Density vs Substrate SD for Middle Depth Zone
lm_model20 <- lm(Relief_index ~ mean_density, data = habitat_info_density_deep)

# Print the summary of the linear model
summary(lm_model20)


# Scatterplot for Linear Model for Relief SD only Natural Reefs
p20 <- ggplot(data = habitat_info_density_deep, aes(x = Relief_SD, y = mean_density)) +
  geom_point() +  
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +  
  labs(x = "Relief SD", y = "Mean Density (per 100m2)") +  
  ggtitle("Linear Regression of Mean Density by Relief SD (Outer DepthZone)") +  
  theme_classic() +
  facet_grid()

# Print the plot
p20

#Linear Model for Mean Density vs Relie SD for Middle Depth Zone
lm_model21 <- lm(Relief_SD ~ mean_density, data = habitat_info_density_deep)

# Filter to include only PVR Sites
habitat_Info_density_PVR <- habitat_info_density %>%
  filter(Site %in% c("PVR 2A", "PVR 2B", "PVR 2C", "PVR 4B", "PVR 4C", "PVR 5A", "PVR 5B", "PVR 5C", "PVR 6A", "PVR 6C", "PVR 7A", "PVR 7B", "PVR 7C", "PVR 8A", "PVR 8B", "PVR 8C", "PVR 4D", "PVR 6D"))

# Scatterplot for Linear Model for Substrate SD only PVR
p13 <- ggplot(data = habitat_Info_density_PVR, aes(x = Substrate_SD, y = mean_density)) +
  geom_point(aes(color = DepthZone)) +  
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +  
  geom_text_repel(aes(label = ifelse(mean_density >= 0, Site, "")), size = 3) + 
  labs(x = "Substrate SD", y = "Mean Density (per 100m2)") +  
  ggtitle("Linear Regression of Mean Density by Substrate SD on PVR") +  
  theme_classic() +
  facet_grid(scales = "free")

# Print the plot
p13

#Linear Model for Mean Density vs Substrate SD for PVR
lm_model21 <- lm(Substrate_SD ~ mean_density, data = habitat_Info_density_PVR)

#Results of Linear Model
summary(lm_model21)

#Linear Model for CCA vs Gorg Density
lm_model22 <- lm(mean_density ~ BRS_per_cov.mean, data = crucoregorg)

# Print the summary of the linear model
summary(lm_model22)


# Scatterplot for Linear Model for Macro vs Gorg Density
p14 <- ggplot(data = crucoregorg_region, aes(x = BRS_per_cov.mean, y = mean_density)) +
  geom_point(aes(color = DepthZone)) +  
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +  
  labs(x = "Mean CCA Density (per 100m2)", y = "Mean Gorgonian Density (per 100m2)") +  
  ggtitle("Linear Regression of Mean Gorgonian Density to Mean CCA Density") + 
  geom_text_repel(aes(label = ifelse(mean_density >= 20, Site, "")), size = 3, box.padding = 0.3) +
  theme_classic() +
  facet_grid(DepthZone ~ Region, scales = "free")

p14
