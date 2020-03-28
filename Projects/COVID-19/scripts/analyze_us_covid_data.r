# https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
# https://covidtracking.com/api/
# https://covid.ourworldindata.org

library(plotly)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(data.table)
library(countrycode)
library(viridis)
library(gganimate)
library(gifski)
library(tidyverse)
library(lmtest)
library(WDI)
library(plotly)
library(USAboundaries)
library(sf)
library(fuzzyjoin)
library(tigris)
library(scales)
library(ggthemes)
library(usmap)
library(cowplot)

setwd("~/Public_Policy/Projects/COVID-19")

us_states_shp = us_states()
us_map = USAboundaries::us_boundaries()
us_states_tigris = tigris::states()

state_geo_center = us_states_tigris@data %>%
  mutate(
    lat = as.numeric(INTPTLAT),
    long = as.numeric(INTPTLON)
  ) %>%
  rename(
    state_abbr = STUSPS
  )

us_covid_data = read_csv('https://covidtracking.com/api/us/daily.csv') %>%
  mutate(
    date = as.Date(as.character(date), format = '%Y%m%d'),
    location = 'United States',
    location_type = 'country', 
    data_source = 'covidtracking.com',
    location_key = paste(location, location_type, data_source, sep = '|')
  ) %>%
  arrange(date) %>%
  rename(
    total_cases = positive,
    total_deaths = death,
    total_tests = total
  ) %>%
  mutate(
    new_cases = total_cases - lag(total_cases, 1),
    new_deaths = total_deaths - lag(total_deaths, 1),
    t = as.numeric(date - min(date)),
    potential_real_cases = total_deaths/0.006
  )

us_states_covid_data = read_csv('http://covidtracking.com/api/states/daily.csv') %>%
  mutate(
    date = as.Date(as.character(date), format = '%Y%m%d'),
    location_type = 'US State', 
    data_source = 'covidtracking.com'
  ) %>%
  rename(
    total_cases = positive,
    total_deaths = death,
    total_tests = total,
    location = state
  ) %>%
  mutate(
    location_key = paste(location, location_type, data_source, sep = '|'),
    new_cases = total_cases - lag(total_cases, 1),
    new_deaths = total_deaths - lag(total_deaths, 1)
  )


all_covid_data_stacked = bind_rows(us_covid_data, us_states_covid_data) %>%
  arrange(location_key, date) %>%
  pivot_longer(cols = c('new_cases', 'new_deaths', 'total_cases', 'total_deaths'),
               names_to = c('measure'), values_to = 'value') %>%
  data.table()



# compute first differences, pct changes, etc. 
all_covid_data_diffs = 
  all_covid_data_stacked[, {
    lag_value = lag(value, 1)
    diff_value = value - lag_value
    pct_change_value = diff_value / lag_value
    
    lag_4_value = lag(value, 4)
    lag_5_value = lag(value, 5)
    lag_6_value = lag(value, 6)
    
    list(
      date = date,
      value = value,
      lag_value = lag_value, 
      diff_value = diff_value, 
      pct_change_value = pct_change_value,
      lag_4_value = lag_4_value, 
      lag_5_value = lag_5_value,
      lag_6_value = lag_6_value, 
      first_value = date[date == min(date)],
      last_value = date[date == max(date)],
      value_past_100 = min(date[value >= 100])
    )
    
  }, by = list(location_key, location, location_type, data_source, measure)] %>%
  pivot_wider(
    id_cols = c('location_key', 'location', 'location_type','data_source', 'date'),
    names_from = 'measure', 
    values_from = c('value', 'lag_value', 'diff_value', 
                    'pct_change_value', 'lag_4_value', 'lag_5_value',
                    'lag_6_value')
  )


case_100_dates = group_by(all_covid_data_diffs, location_key) %>%
  summarize(
    date_case_100 = min(date[value_total_cases >= 100])
  )

all_covid_data_diffs_dates = left_join(all_covid_data_diffs, case_100_dates) %>%
  mutate(
    days_since_case_100 = as.numeric(date - date_case_100)
  ) %>%
  arrange(location_key, date)


latest_state_data = 
  all_covid_data_diffs_dates %>% filter(location_type == 'US State') %>%
  group_by(location_key, location) %>%
  summarize(
    last_cases = tail(value_total_cases, 1),
    new_cases = tail(diff_value_total_cases, 1),
    last_date = max(date)
  )

# create map for all 50 states 
US_state_data = left_join(us_states_shp, latest_state_data, by = c('state_abbr' = 'location')) %>% 
  left_join(state_geo_center)

lower_48 = state.name[!state.name %in% c('Hawaii', 'Alaska')]
lower_48_state_data = filter(US_state_data, name %in% lower_48)

alaska_data = filter(US_state_data, name %in% 'Alaska')
hawaii_data = filter(US_state_data, name %in% 'Hawaii')


world <- ne_countries(scale = "medium", returnclass = "sf")
usa <- subset(world, admin == "United States of America")

v_scale = scale_fill_viridis(name = 'Fill', guide = F) 

mainland <- ggplot() +
  geom_sf(fill = NA) +  
  geom_sf(data = lower_48_state_data, aes(fill = log(last_cases)), show.legend = F) +
  geom_sf_text(data = lower_48_state_data, aes(long, lat, label = comma(last_cases, accuracy = 1)),
               colour = 'black', fontface='bold', size = 2.5) +
  v_scale +
  coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), ylim = c(-2300000,
                                                                     730000)) +
  labs(x = '', y = '',
       caption = 'Chart: Taylor G. White\nData: covidtracking.com',
       title = 'U.S. COVID-19 Cases by State', subtitle = sprintf('As of %s',
                                                                  unique(format(latest_state_data$last_date, '%B %d')))) +
  
  theme_map() +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    title = element_text(size = 16),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(hjust = 0)
  )

alaska <- ggplot() +
  geom_sf(data = alaska_data, aes(fill = log(last_cases))) +
  geom_sf_text(data = alaska_data, aes(long, lat, label = comma(last_cases, accuracy = 1)),
               colour = 'black', fontface='bold', size = 2.5) +
  v_scale +
  coord_sf(crs = st_crs(3467), xlim = c(-2400000, 1600000), ylim = c(200000,
                                                                     2500000), expand = FALSE, datum = NA) +
  labs(x='', y='') +
  theme_map() 

hawaii  <- ggplot(data = hawaii_data) +
  geom_sf(data = hawaii_data, aes(fill = log(last_cases))) +
  geom_sf_text(data = hawaii_data, aes(long, lat, label = comma(last_cases, accuracy = 1)),
               colour = 'black', fontface='bold', size = 2.5) +
  v_scale +
  coord_sf(crs = st_crs(4135), xlim = c(-161, -154), ylim = c(18,
                                                              23), expand = FALSE, datum = NA) +
  labs(x='', y='') +
  theme_map()

mainland +
  annotation_custom(
    grob = ggplotGrob(alaska),
    xmin = -2750000,
    xmax = -2750000 + (1600000 - (-2400000))/2.5,
    ymin = -2450000,
    ymax = -2450000 + (2500000 - 200000)/2.5
  ) +
  annotation_custom(
    grob = ggplotGrob(hawaii),
    xmin = -1250000,
    xmax = -1250000 + (-154 - (-161))*120000,
    ymin = -2450000,
    ymax = -2450000 + (23 - 18)*120000
  )
ggsave('output/latest_cv_state_map_50.png', height = 6, width = 8, units = 'in', dpi = 800)




##### draw case plots by state #####
head(all_covid_data_diffs_dates)
