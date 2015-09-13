
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

#----------------------------------------------#

# Helper parameters -------------------------------------------------------

get_shapefile_data = function(
  
  shapefile_dir, shapefile_name, 
  info_type = c("points", "points_polygons"), 
  
  output_opts = list(
    export_to_db = T,
    db_con = NULL, 
    table_name = NULL, 
    overwrite_existing = F,
    append_to_existing = F
  )) {
  
  require(maptools)
  require(rgeos)
  require(rgdal)
  require(ggplot2)
  require(ggmap)
  require(plyr)
  require(dplyr)
  require(data.table)
  require(RSQLite)
  require(stringr)  
  
  
  # Helper function ---------------------------------------------------------
  
  output_shapefile_info = function(shapefile_data, opts){
    
    if (output_opts$export_to_db) {
      
      stopifnot(!is.null(output_opts$table_name))        
      dbWriteTable(
        output_opts$db_con, output_opts$table_name, shapefile_data,       
        overwrite = output_opts$overwrite_existing, append = output_opts$append_to_existing
      )

      dbDisconnect(output_opts$db_con)
    
    } else {
      write.csv(shapefile_data, paste0(shapefile_dir, "/", shapefile_name, "_", info_type, ".csv"), row.names = F)
    }  
  }
  
  
  # Establish DB connection, if any -----------------------------------------
  
  if (output_opts$export_to_db & is.null(output_opts$db_con)) {  
    output_opts$db_con = dbConnect(SQLite(), paste0(shapefile_dir, "/", shapefile_name, ".sqlite"))
  }
  
  if (is.null(output_opts$table_name)) {
    output_opts$table_name = paste(shapefile_name, info_type, sep = "_")
  }
  
  
  # Import, clean, export shapefile -----------------------------------------
  
  setwd(shapefile_dir)
  shapefile_import <- readOGR(dsn = ".", layer = shapefile_name)  
  
  ## extract points data (i.e. unique on geography level) ## 
  shapefile_points = shapefile_import@data
  
  if (info_type == "points") {
    
    output_shapefile_info(shapefile_points, output_opts)
    return(shapefile_points)    
    
  } else {
        
    ## fortify polygons for easy ggplot2 plotting ## 
    shapefile_polygons <- 
      fortify(shapefile_import) 
    
    ## export results ##
    output_shapefile_info(shapefile_polygons, output_opts)
    
    return(combined_points_polygons)
  } 
  
}


shapefile_data = get_shapefile_data(
  shapefile_dir = "C:/Users/taylor/Dropbox/WildPolicy/Data/geographic_data/shapefiles/tl_2014_us_county", 
  shapefile_name = "tl_2014_us_county", 
  info_type = "points_polygons", 
  output_opts = list(
    export_to_db = T,
    db_con = NULL, 
    table_name = NULL, 
    overwrite_existing = T,
    append_to_existing = F
  ))

list.files("C:/Users/taylor/Dropbox/WildPolicy/Data/geographic_data/shapefiles/")

