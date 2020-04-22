
library(jsonlite)
library(ggmap)

run_geocode = F

### get IMHE data ###

library(tidyverse)
setwd("~/Public_Policy/Projects/COVID-19")
zip_url = 'https://ihmecovid19storage.blob.core.windows.net/latest/ihme-covid19.zip'
dir.create('data/imhe_zip')

existing_zips = list.files('data/imhe_zip', recursive = T, full.names = T)

file.remove(existing_zips)
existing_folders = list.files('data/imhe_zip', recursive = T, full.names = T, include.dirs = T)
unlink(existing_folders, recursive = T)

download.file(zip_url, destfile = 'data/imhe_zip/ihme-covid19.zip')
unzip('data/imhe_zip/ihme-covid19.zip', exdir = 'data/imhe_zip')

new_download = list.files('data/imhe_zip', recursive = T, full.names = T)
new_dirs = list.files('data/imhe_zip', recursive = T, full.names = T, include.dirs = T)
the_csv = new_download[str_detect(new_download, '.csv')]
file.copy(the_csv, 'data/imhe_zip/most_recent_imhe_projections.csv')
unlink(new_dirs, recursive = T)

# clean up testing sites # 
if (run_geocode) {
  
  test_sites_in = fromJSON("data/EVIVE_scarped_COVID19TestingSites.json")
  
  stacked_testing_info = map(test_sites_in, function(inner_list){
    if ('data.frame' %in% class(inner_list)) {
      if ('location' %in% names(inner_list)) {
        return(inner_list)
      } else {
        return(NULL)
      }
    } else {
      return(NULL)
    }
  }) %>% 
    bind_rows()
  
  
  locations_with_addresses = filter(stacked_testing_info, 
                                    str_detect(location, '[0-9]+'))
  
  # geocoded_testing_sites = geocode(locations_with_addresses$location, output = 'more')
  
  locations_with_addresses_lat_long = bind_cols(locations_with_addresses, geocoded_testing_sites)
  
  stacked_testing_info_geocoded = left_join(
    stacked_testing_info, locations_with_addresses_lat_long
  ) %>%
    mutate(
      state_zip = str_extract(address, '[a-z]{2} [0-9]{5}'),
      state_abbr = str_extract(state_zip, '[a-z]{2}') %>% toupper(),
      zip = str_extract(state_zip, '[0-9]{5}')
    )
  
  write.csv(stacked_testing_info_geocoded, 'data/cleaned_testing_sites.csv', row.names = F)
  
}
# 
# library(webdriver)
# https://github.com/rstudio/webdriver
# pjs <- run_phantomjs()
# ses <- Session$new(port = pjs$port)
# ses$go("https://r-pkg.org/pkg/callr")
# ses$takeScreenshot()
# install <- ses$findElement(".install-package")
# install$getName()
# search2 <- ses$executeScript("return document.getElementById('cran-input');")
# search2$getName()
# search <- ses$findElement("#cran-input")
# search$sendKeys("html", key$enter)
# ses$getUrl()