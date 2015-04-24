
# Notes -------------------------------------------------------------------

# FIPS Class Codes
# H1:  identifies an active county or statistically equivalent entity that does not qualify under subclass C7 or H6.
# H4:  identifies a legally defined inactive or nonfunctioning county or statistically equivalent entity that does not qualify under subclass H6.
# H5:  identifies census areas in Alaska, a statistical county equivalent entity.
# H6:  identifies a county or statistically equivalent entity that is areally coextensive or governmentally consolidated with an incorporated place, part of an incorporated place, or a consolidated city. 
# C7:  identifies an incorporated place that is an independent city; that is, it also serves as a county equivalent because it is not part of any county, and a minor civil division (MCD) equivalent because it is not part of any MCD.


#----------------------------------------------#

## Hadley's notes ##
# https://github.com/hadley/ggplot2/wiki/plotting-polygon-shapefiles

## stackoverflow examples ## 
# http://stackoverflow.com/questions/21840563/complex-maps-in-r-using-ggplot2
# http://stackoverflow.com/questions/20774732/choropleth-maps-in-r-tiger-shapefile-issue

# Libraries and options ---------------------------------------------------

library(maptools)
library(rgeos)
library(rgdal)
library(ggplot2)
library(ggmap)
library(plyr)
library(dplyr)
library(data.table)

options(stringaAsFactors = F)


# Helper files ------------------------------------------------------------

# # get county fips codes
# county_state_fips <- fread("http://www2.census.gov/geo/docs/reference/codes/files/national_county.txt", 
#                               header = F, colClasses = "character")
# 
# 
# setnames(county_state_fips, names(county_state_fips), 
#          c("state_short", "STATEFP", "COUNTYFP", "county_desc", "fips_class_code")) 
# 
# county_state_fips_upd <- 
#   county_state_fips %>%
#   mutate(
#     GEOID = paste0(STATEFP, COUNTYFP)
#     )


# Import shapefiles -------------------------------------------------------

setwd("C:/Users/taylor/Dropbox/WildPolicy/Data/geographic_data/shapefiles/US/counties/tl_2014_us_county")

county_shapefiles <- readOGR(dsn = ".", layer = "tl_2014_us_county")

# 
# sink(file = "county_shapefiles_str.txt")
# str(county_shapefiles)
# sink()

# extract polygon data and update file
polygon_data <- 
  county_shapefiles@data %>%
  mutate(
    polygon_center_lon = INTPTLON %>% as.character() %>% as.numeric(),
    polygon_center_lat = INTPTLAT %>% as.character() %>% as.numeric()
    ) %>%
  data.table(key = "GEOID")

county_shapefiles_df <- 
  fortify(county_shapefiles, region = "GEOID") %>%
  data.table(key = "id")

# merge on county names, etc
county_shapefiles_with_names <- 
  county_shapefiles_df[polygon_data]

# subset to CA only
CA_only <- 
  county_shapefiles_with_names[STATEFP == "06", ]

# map it ! 
ggplot(CA_only, aes(long, lat, fill = NAME)) +
  geom_polygon(colour = "white") +
  scale_fill_hue(guide = F, l = 50) +
  geom_text(data = polygon_data[STATEFP == "06", ], 
            aes(x = polygon_center_lon, y = polygon_center_lat, label = NAME), colour = "black", size = 3, fontface = "bold")


