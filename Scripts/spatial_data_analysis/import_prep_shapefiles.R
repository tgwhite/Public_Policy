
# Notes -------------------------------------------------------------------

# Taylor G. White
# 05/20/15
# Imports, cleans, and exports tiger shapefiles

# Libraries and options ---------------------------------------------------

library(plyr)
library(dplyr)
library(stringr)
library(data.table)
library(maptools)
library(rgeos)
library(rgdal)
library(ggmap)


# Helper parameters -------------------------------------------------------

parent_dir = "C:/Users/taylor/Dropbox/WildPolicy/Data/geographic_data/shapefiles"
shapefile_folders = list.files(parent_dir)

## note that folders are named in the same convention as the tiger shapefiles -- 'tl_2014_us_county' and 'tl_2014_us_state' for example

# Import, clean, export shapefiles ----------------------------------------

lapply(shapefile_folders, function(folder_name){  
  
  filename_short = str_extract_all(folder_name, pattern = "[[:alpha:]]{2}_[[:alpha:]]+") %>% as.character()
  
  setwd(paste(parent_dir, folder_name, sep = "/"))
  selected_shapefiles <- readOGR(dsn = ".", layer = folder_name)
  
  # extract polygon data and update file  
  polygon_data <- 
    selected_shapefiles@data 
  
  # detect latitute and longitude variables and run a quick update
  lon_var = subset(names(polygon_data), str_detect(names(polygon_data), ignore.case("lon")))
  lat_var = subset(names(polygon_data), str_detect(names(polygon_data), ignore.case("lat")))
  
  polygon_data$longitude = polygon_data[[lon_var]] %>% as.numeric()
  polygon_data$latitude = polygon_data[[lat_var]] %>% as.numeric()
  
  # prep for data.table merge
  polygon_data = data.table(polygon_data, key = "GEOID")
      
  selected_shapefiles_df <- 
    fortify(selected_shapefiles, region = "GEOID") %>%
    data.table(key = "id")
  
  # merge shapefiles with polygon data
  selected_shapefiles_with_names <- 
    selected_shapefiles_df[polygon_data]
  
  
  # export data and clean memory
  save(list = selected_shapefiles_with_names, file = paste0(filename_short, "_prepped.rdata"))
  rm(selected_shapefiles_with_names, selected_shapefiles_df, polygon_data)
  gc()
})
