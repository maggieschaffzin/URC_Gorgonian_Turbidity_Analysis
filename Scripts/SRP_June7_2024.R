# CREATION DATE 10 June 2024
# MODIFIED DATE 

# AUTHOR Maggie Schaffzin (schaffzin@oxy.edu)

# PURPOSE: Mapping storm drain + waste water outputs along L.A. County Coast (PVR + Malibu)

# TO DO: Map reef sites along PVR + Malibu, calculate Euclidean distance


# PACKAGES: ----
library(tidyverse) 
library(sf)
library(ggspatial)
library(dplyr)
library(RColorBrewer)

# SET WORKING DIRECTORY: ----

setwd("~/Desktop/Gorgonian")

## Read in mapping data ----

californiamap <- st_read ("/Users/maggieschaffzin/Desktop/Gorgonian/CA_Map_Nov2023")

#install.packages("ggplot2")
library(ggplot2)
ggplot (californiamap)