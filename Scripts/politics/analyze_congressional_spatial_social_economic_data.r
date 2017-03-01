
library(maptools)
library(rgeos)
library(rgdal)
library(ggplot2)
# library(ggmap)
library(plyr)
library(dplyr)
library(data.table)
library(stringr)
library(maps)
library(htmltab)
options(stringaAsFactors = F)


# Helper files ------------------------------------------------------------

## get congressional election results 

# https://github.com/TimeMagazine/congressional-election-results/tree/master/data

time_results_base = 'https://github.com/TimeMagazine/congressional-election-results/blob/master/data/results_%s.csv'

## us and state map data
us_map = map_data('usa')
us_state_map = map_data('state')

## get fips codes
county_state_fips <- fread("http://www2.census.gov/geo/docs/reference/codes/files/national_county.txt", 
                              header = F, colClasses = "character")
setnames(county_state_fips, names(county_state_fips), 
         c("state_short", "STATEFP", "COUNTYFP", "county_desc", "fips_class_code")) 

state_fips = select(county_state_fips, contains('state')) %>%
unique()

## get CD shapefile data
setwd('~/congressional_research/tl_2016_us_cd115')

cd_shapefiles <- readOGR(dsn = ".", layer = "tl_2016_us_cd115")
polygon_data <- 
  cd_shapefiles@data %>%
  mutate(
    polygon_center_lon = INTPTLON %>% as.character() %>% as.numeric(),
    polygon_center_lat = INTPTLAT %>% as.character() %>% as.numeric()
    ) %>%
  data.table(key = "GEOID")

cd_shapefiles_df <- 
  fortify(cd_shapefiles, region = "GEOID") %>%
  data.table(key = "id")

cd_shapefiles_with_names <- 
  cd_shapefiles_df[polygon_data] %>%
  left_join(state_fips) %>%
  mutate(
    cd_number = str_extract(NAMELSAD, '[0-9]+') %>% as.numeric()
    ) 

cd_continental_us = 
  cd_shapefiles_with_names %>%
  filter(
    state_short %in% state.abb[!state.abb %in% c('AK', 'HI')]   
    )

## get insurance coverage by congressional district 
setwd('~/congressional_research/social_economic_data/congressional_district')

# GEO.id  GEO.id2 GEO.display-label
# Id  Id2 Geography

# HC01_VC130
# Estimate; HEALTH INSURANCE COVERAGE - Civilian noninstitutionalized population

# HC01_VC133
# Estimate; HEALTH INSURANCE COVERAGE - Civilian noninstitutionalized population - With health insurance coverage - With public coverage

# HC01_VC134
# Estimate; HEALTH INSURANCE COVERAGE - Civilian noninstitutionalized population - No health insurance coverage

colname_mappings = read.csv('ACS_15_5YR_DP03_with_ann.csv', nrows = 1)
cd_selected_economic_characteristics = fread('ACS_15_5YR_DP03_with_ann.csv', skip = 2, header = F, colClasses = 'character')
names(cd_selected_economic_characteristics) = names(colname_mappings)

selected_econ_vars = select(cd_selected_economic_characteristics, contains('GEO'), contains('VC13'))
econ_varnames = names(selected_econ_vars)[str_detect(names(selected_econ_vars), 'VC')]

# convert to numeric
for (var in econ_varnames) {
  selected_econ_vars[[var]] = str_replace_all(selected_econ_vars[[var]], '[^0-9\\.]', '') %>%
  as.numeric()
}

# final cleanup
selected_econ_vars_upd = 
selected_econ_vars %>%
mutate(
  percent_non_institutionalized_public_cov = HC01_VC133 / HC01_VC130,
  percent_non_institutionalized_no_insurance = HC01_VC134 / HC01_VC130,
  STATEFP = str_extract(GEO.id, 'US[0-9]{2}') %>% str_replace('US', ''),
  cd_number = str_extract(GEO.display.label, 'District [0-9]+') %>% str_extract('[0-9]+') %>% as.numeric()
  ) %>%
left_join(state_fips) %>%
arrange(-percent_non_institutionalized_public_cov) %>%
data.table(key = c('state_short', 'cd_number'))

## merge health stats and shapefiles 
cd_continental_us = data.table(cd_continental_us, key = c('state_short', 'cd_number'))

dist_polygon_info = select(cd_continental_us, state_short, cd_number, polygon_center_lat, polygon_center_lon) %>% 
unique()

cd_shape_health_dat = 
  selected_econ_vars_upd[cd_continental_us]

## plot the districts 
ca_health_coverage_map = 
  ggplot(cd_shape_health_dat %>% filter(state_short == 'CA'), aes(long, lat, group = group)) +
    # geom_polygon(data = us_map) +
  geom_polygon(data = us_state_map %>% filter(region == 'california')) +
  geom_polygon(aes(fill = percent_non_institutionalized_public_cov * 100), colour = 'black') +
  geom_text(data = dist_polygon_info %>% filter(state_short == 'CA'), aes(polygon_center_lon, polygon_center_lat, label = cd_number, group = NA), colour = 'white', size = 2) +
  coord_quickmap() +
  scale_fill_continuous(name = '% with public health coverage') +
  theme(
    legend.position = 'bottom', 
    axis.text = element_blank(),
    axis.ticks = element_blank()
    ) +
  labs(
    title = 'Mapping Health Coverage by Congressional District',
    subtitle = 'Percent of Civilian Non-Institutionalized Population With Public Health Coverage',
    caption = 'Source: 2015 ACS 5 Year Estimates',
    x = '', y = ''
    )

setwd('~')
ggsave('california_public_health_coverage_map_by_cd.png', plot = ca_health_coverage_map, height = 7, width = 6, units = 'in', dpi = 500)


ca_health_coverage_map = 
  ggplot(cd_shape_health_dat %>% filter(state_short == 'CA'), aes(long, lat, group = group)) +
    # geom_polygon(data = us_map) +
  geom_polygon(data = us_state_map %>% filter(region == 'california')) +
  geom_polygon(aes(fill = percent_non_institutionalized_no_insurance * 100), colour = 'black') +
  geom_text(data = dist_polygon_info %>% filter(state_short == 'CA'), aes(polygon_center_lon, polygon_center_lat, label = cd_number, group = NA), colour = 'white', size = 2) +
  coord_quickmap() +
  scale_fill_continuous(name = '% with no health coverage') +
  theme(
    legend.position = 'bottom', 
    axis.text = element_blank(),
    axis.ticks = element_blank()
    ) +
  labs(
    title = 'Mapping Health Coverage by Congressional District',
    subtitle = 'Percent of Civilian Non-Institutionalized Population Without Health Coverage',
    caption = 'Source: 2015 ACS 5 Year Estimates',
    x = '', y = ''
    )

setwd('~')
ggsave('california_no_health_coverage_map_by_cd.png', plot = ca_health_coverage_map, height = 7, width = 6, units = 'in', dpi = 500)