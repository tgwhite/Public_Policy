
# Libraries and options ---------------------------------------------------

library(maptools)
library(rgeos)
library(rgdal)
library(ggplot2)
library(ggmap)
library(plyr)
library(dplyr)
library(data.table)
library(stringr)
library(foreign)

options(stringsAsFactors = F)


# Helper functions --------------------------------------------------------

source("C:/Users/taylor/Dropbox/WildPolicy/Public_Scripts/Scripts/public_data_downloads/download_CA_election_results.R")

# Import census data ------------------------------------------------------

setwd("C:/Users/taylor/Dropbox/WildPolicy/Data/Census/tract")

social_header = fread("ACS_13_5YR_DP02_with_ann.csv", nrows = 1, data.table = F)
social_var_mapping = data.frame(
  var_name = names(social_header),
  description = as.character(social_header[1,]))

social_estimate_vars = filter(social_var_mapping, str_detect(description, fixed("estimate", ignore_case = T)))

social_stats = fread("ACS_13_5YR_DP02_with_ann.csv", skip = 2, data.table = T, header = F)
setnames(social_stats, names(social_stats), social_var_mapping$var_name)

## --- ## 
economic_header = fread("ACS_13_5YR_DP03_with_ann.csv", nrows = 1, data.table = F)
economic_var_mapping = data.frame(
  var_name = names(economic_header),
  description = as.character(economic_header[1,]))

economic_estimate_vars = filter(economic_var_mapping, str_detect(description, fixed("estimate", ignore_case = T)))
economic_stats = fread("ACS_13_5YR_DP03_with_ann.csv", skip = 2, data.table = T, header = F)
setnames(economic_stats, names(economic_stats), economic_var_mapping$var_name) 

id_keep_var_list = c("GEO.id", "GEO.id2", "GEO.display-label")

# Select social and economic stats ----------------------------------------

spanish_speakers_stats = filter(social_estimate_vars, 
                                description == "Estimate; LANGUAGE SPOKEN AT HOME - Population 5 years and over - Spanish" |
                                  description == "Estimate; LANGUAGE SPOKEN AT HOME - Population 5 years and over")


hs_graduate_stats = filter(social_estimate_vars, 
                           description == "Estimate; EDUCATIONAL ATTAINMENT - Population 25 years and over - High school graduate (includes equivalency)" |
                             description == "Estimate; EDUCATIONAL ATTAINMENT - Population 25 years and over")

unemployed_stats = filter(economic_var_mapping, 
                          description == "Estimate; EMPLOYMENT STATUS - Population 16 years and over - In labor force - Civilian labor force" |
                            description == "Estimate; EMPLOYMENT STATUS - Population 16 years and over - In labor force - Civilian labor force - Unemployed")


no_health_insurance = filter(economic_estimate_vars, 
                            description == "Estimate; HEALTH INSURANCE COVERAGE - Civilian noninstitutionalized population" |
                              description == "Estimate; HEALTH INSURANCE COVERAGE - Civilian noninstitutionalized population - No health insurance coverage")

# Extract vars ------------------------------------------------------------

selected_economic_vars = economic_stats[, c(id_keep_var_list, 
                                            unemployed_stats$var_name, no_health_insurance$var_name), with = F]

selected_social_vars = social_stats[, c(id_keep_var_list, hs_graduate_stats$var_name, spanish_speakers_stats$var_name), with = F]



# merge social and economic stats plus district mappings -------------------

## district mappings ##
setwd("C:/Users/taylor/Dropbox/WildPolicy/Data/geographic_data/mappings/CA_legislative_districts")
assembly_state_equiv <-
  read.dbf("2011_assembly_state_equiv.dbf", as.is = T) 

## block mappings ## 
setwd("C:/Users/taylor/Dropbox/WildPolicy/Data/geographic_data/mappings/blocks_relationship_files/COL2010_TAB2010_ST_06_v2")
block_mappings <- 
  fread("COL2010_TAB2010_ST_06_v2.txt", colClasses = "character") %>%
  mutate(
    block_full = paste0(TAB_STATE_2010, TAB_COUNTY_2010, TRACT_2010, BLK_2010),
    tract_full = paste0(TAB_STATE_2010, TAB_COUNTY_2010, TRACT_2010)
    )

## merge district and block mappings ## 
merged_blocks_tracts = inner_join(assembly_state_equiv, block_mappings, by = c("BLOCK" = "block_full"))

## merge stats ## 
merged_social_economic_vars = inner_join(selected_economic_vars, selected_social_vars) %>%
  mutate(
    tract = str_extract_all(GEO.id, pattern = "US[[:digit:]]+") %>% as.character() %>% str_replace_all(pattern = "US", replacement = "")
    )

## merge stats and mappings ## 
stats_districts_merged <-
  inner_join(as.data.frame(merged_social_economic_vars), merged_blocks_tracts, by = c("tract" = "tract_full")) %>%
  data.table() 


# Compute demographic and economic stats and output ----------------------------------------

## subset to tracts only (tract level data duplicated in merge with blocks) ## 

stats_by_tract = select(stats_districts_merged, tract, contains("HC"), contains("district"), contains("geo")) %>% unique()

stats_by_AD <- 
  stats_by_tract[, {
    list(
      pct_spanish_speakers = sum(HC01_VC174)/sum(HC01_VC170) * 100,
      pct_unemployed = sum(HC01_VC07)/sum(HC01_VC05) * 100,
      pct_no_health_insurage = sum(HC01_VC134)/sum(HC01_VC130) * 100
    )
  }, by = DISTRICTID] %>%
  arrange(DISTRICTID)


## output stats by AD ## 
setwd("C:/Users/taylor/Dropbox/WildPolicy/Public_Scripts/Output/politics")
write.csv(stats_by_AD, "selected_dem_econ_stats_by_AD.csv", row.names = F)


# Get shapefiles for districts and census tracts --------------------------

# setwd("C:/Users/taylor/Dropbox/WildPolicy/Data/geographic_data/shapefiles/2011_assembly_state_shp")
# AD_shapefiles <- readOGR(dsn = ".", layer = "2011_assembly_state")
# 
# AD_polygon_data <- 
#   AD_shapefiles@data %>%
#   data.table(key = "ID")
# 
# AD_shapefiles_df <- 
#   fortify(AD_shapefiles) 
# 


# Get voting data by year -------------------------------------------------
  
setwd("C:/Users/taylor/Dropbox/WildPolicy/Data/Voting/Berkeley Statewide Data/election_results")

all_election_results <- lapply(list.files(recursive = T), function(filename){
  
  if (str_detect(filename, fixed(".csv"))) {
    fread(filename) %>% 
      mutate(
        filename = rep(filename)
        )
  }
}) %>% 
  rbindlist(fill = T)
head(all_election_results)
names(all_election_results)
selected_election_results <-
  all_election_results %>%
  select(
    filename, contains("reg"), contains("gov"), contains("adist"), contains("vote"), contains("prs")
    ) 

# table(selected_election_results$filename)
# table(is.na(selected_election_results$PRSDEM), selected_election_results$filename) 
# table(is.na(selected_election_results$PRSDEM01), selected_election_results$filename) 

# Downoad voter reg and election stats -------------------------------------------------

download.file(
  "http://statewidedatabase.org/pub/data/P14/state/state_p14_sov_data_by_p14_srprec.zip",
  "state_p14_sov_data_by_p14_srprec.zip"
  )
unzip("state_p14_sov_data_by_p14_srprec.zip")

download.file(  
  "http://statewidedatabase.org/pub/data/P12/state/state_p12_registration_by_p12_srprec.zip", 
  destfile = "voter_reg/state_p12_registration_by_p12_srprec.zip"
)

unzip(zipfile = "voter_reg/state_p12_registration_by_p12_srprec.zip", 
      exdir = "voter_reg")

download.file(
  "http://statewidedatabase.org/pub/data/P14/state/state_p14_registration_by_p14_srprec.zip", 
  destfile = "voter_reg/state_p14_registration_by_p14_srprec.zip"
)

unzip(zipfile = "voter_reg/state_p14_registration_by_p14_srprec.zip", 
      exdir = "voter_reg")


# Primary and general stats -----------------------------------------------

primary_2014 = read.dbf("state_p14_sov_data_by_p14_srprec.dbf")

assembly_only <- 
  select(primary_2014, contains("ass"))

primary_2014$total_assembly = rowSums(assembly_only)

assm_stats <- 
  group_by(primary_2014, ADDIST) %>%
  summarize(
    total_dem = sum(ASSDEM01, ASSDEM02, ASSDEM03, ASSDEM04, ASSDEM05),
    percent = total_dem/sum(total_assembly)
    )
