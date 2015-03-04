
get_zillow_data <- function(
  
  })
  

summary_level = c("state", "MSA", "county", "city", "zip", "neighborhood")
summary_level = "state"
download_out_path = "C:\Users\taylor\Dropbox\WildPolicy\Data\Community Data\zillow\city_data"


# Libraries and options 
library(stringr)
library(RCurl)
library(XML)
library(plyr)
library(dplyr)
library(data.table)

download_url <- 
  switch(summary_level, 
         state = "http://files.zillowstatic.com/research/public/State.zip", 
         MSA = "http://files.zillowstatic.com/research/public/Metro.zip", 
         county = "http://files.zillowstatic.com/research/public/County.zip", 
         city = "http://files.zillowstatic.com/research/public/City.zip", 
         zip = "http://files.zillowstatic.com/research/public/Zip.zip", 
         neighborhood = "http://files.zillowstatic.com/research/public/Neighborhood.zip")


download.file("http:/campaignfinance.cdn.sos.ca.gov/dbwebexport.zip", 
              destfile = "dbwebexport.zip")

# unzip files
unzip("dbwebexport.zip", exdir = "extracted_files")
