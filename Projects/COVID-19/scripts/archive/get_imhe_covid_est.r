
library(tidyverse)
setwd("~/Public_Policy/Projects/COVID-19")
zip_url = 'https://ihmecovid19storage.blob.core.windows.net/latest/ihme-covid19.zip'
dir.create('data/imhe_zip')
download.file(zip_url, destfile = 'data/imhe_zip/ihme-covid19.zip')
unzip('data/imhe_zip/ihme-covid19.zip', exdir = 'data/imhe_zip')


