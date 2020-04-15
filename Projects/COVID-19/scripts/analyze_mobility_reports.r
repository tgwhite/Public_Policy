
# https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/by_year/readme.txt
# https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt

library(tidyverse)
library(data.table)
library(xgboost)
library(USAboundaries)
library(fuzzyjoin)
library(mgcv)
library(ggrepel)

setwd("C:/Users/csq/Downloads")
weather_2020 = fread('2020.csv') %>% setorder(V1, V2, V3)
names(weather_2020) = c('station', 'date', 'type', 'value', 'm_flag', 'q_flag', 's_flag', 'time')
weather_2020$date_upd = as.Date(weather_2020$date %>% as.character(), format = '%Y%m%d')
weather_2020$country = str_extract(weather_2020$station, '^([A-Z]{2})')
setkey(weather_2020, country, type)

weather_sub = weather_2020[J(c('US', 'IT', 'KS', 'GM')), ]

rm(weather_2020)
gc()

# ID            1-11   Character
# LATITUDE     13-20   Real
# LONGITUDE    22-30   Real
# ELEVATION    32-37   Real
# STATE        39-40   Character
# NAME         42-71   Character
# GSN FLAG     73-75   Character
# HCN/CRN FLAG 77-79   Character
# WMO ID       81-85   Character

stations = read_table('ghcnd-stations.txt', col_names = F)
names(stations) = c('station', 'latitude', 'longitude', 'ELEVATION', 'STATE', 'NAME', 'GSN FLAG', 'WMO ID')
stations$country = str_extract(stations$station, '^([A-Z]{2})')
map_dbl(stations, function(x){sum(is.na(x))})


us_cities_shp = us_cities()
us_cities_coord = st_coordinates(us_cities_shp) %>% as.data.frame()
names(us_cities_coord) = c('longitude', 'latitude')
us_cities_shp_fin = bind_cols(us_cities_shp, us_cities_coord)

us_cities_shp_fin$geometry = NULL
selected_cities = filter(us_cities_shp_fin, str_detect(city, '^(New York City)|^(San Francisco)|(Seattle)|(Los Angeles)'))
city_stations = geo_left_join(selected_cities, stations %>% filter(country == 'US'), max_dist = 50)

weather_sub_station_meta = left_join(weather_sub, stations) %>% select(-date) %>%
  mutate(
    temp_f = ifelse(type == 'TAVG', (value/10) * 9/5 + 32, value)
  ) %>%
  left_join(
    city_stations %>% select(station, city)
  ) %>%
  mutate(
    entity_name = ifelse(is.na(city), country, city)
  )

station_sub = filter(weather_sub_station_meta, !is.na(city) | country %in% c('IT', 'KS', 'GM'))

avg_temp_sub = station_sub %>% filter(type == 'TAVG')
avg_temp_by_entity = group_by(avg_temp_sub, country, entity_name, date_upd) %>%
  summarize(
    mean_temp = mean(temp_f)
  )

ggplot(avg_temp_by_entity, aes(date_upd, mean_temp, colour = entity_name)) +
  facet_wrap(~country) +
  geom_line()


# stations2 = read_table2('ghcnd-stations.txt', col_names = F)


# ID = 11 character station identification code
# YEAR/MONTH/DAY = 8 character date in YYYYMMDD format (e.g. 19860529 = May 29, 1986)
# ELEMENT = 4 character indicator of element type 
# DATA VALUE = 5 character data value for ELEMENT 
# M-FLAG = 1 character Measurement Flag 
# Q-FLAG = 1 character Quality Flag 
# S-FLAG = 1 character Source Flag 
# OBS-TIME = 4-character time of observation in hour-minute format (i.e. 0700 =7:00 am)



##### pull in case data ####
johns_hopkins_cases = read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv') %>%
  pivot_longer(cols = matches('^([0-9])'), names_to = 'date', values_to = 'cases') %>%
  filter(
    `Country/Region` %in% c('Korea, South', 'Italy', 'Germany')
  ) %>%
  mutate(
    entity_name = recode(`Country/Region`, `Korea, South` = 'KS', `Germany` = 'GM', `Italy` = 'IT'),
    date = as.Date(date, format = '%m/%d/%y')
  )


nyt_county_data = read_csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv') %>%
  arrange(county, state, fips, date) %>%
  select(-deaths)

selected_counties_cases = filter(nyt_county_data, 
       (county == 'King' & state == 'Washington') |
       county %in% c('San Francisco', 'Los Angeles', 'New York City')) %>%
  mutate(
    entity_name = recode(county, `King` = 'Seattle')
  )


setwd("~/Public_Policy/Projects/COVID-19")

apple_mobility_dat = read_csv('data/applemobilitytrends-2020-04-13.csv') %>%
  pivot_longer(cols = matches('^([0-9+]{4})'), names_to = 'date') %>%
  mutate(
    date = as.Date(date),
    week_day = lubridate::wday(date),
    weekend_ind = ifelse(week_day %in% c(7, 1), 'Weekend', "Week Day"),
    entity_name = recode(region, `San Francisco - Bay Area` = 'San Francisco', `Italy` = 'IT', `Republic of Korea` = 'KS', `Germany` = 'GM')
  ) %>%
  left_join(
    avg_temp_by_entity, by = c('entity_name', 'date' = 'date_upd')
  ) %>%
  left_join(
    selected_counties_cases 
  ) %>%
  left_join(
    johns_hopkins_cases , by = c('date', 'entity_name')
  ) %>%
  mutate(
    cases = coalesce(cases.x, cases.y, as.numeric(NA))
  ) %>%
  select(-cases.x, -cases.y) %>%
  filter(region != 'Germany')
  

walking_data = filter(apple_mobility_dat, transportation_type == 'walking', !is.na(mean_temp))

pre_covid_walking_model = lm(value ~ as.numeric(date) * region + week_day + I(week_day^2) + I(week_day^3) + mean_temp + weekend_ind, data = walking_data %>% 
                               filter(date <= '2020-02-15'))


walking_data_mat = model.matrix(value ~ as.numeric(date)*region + week_day + I(week_day^2) + I(week_day^3) + mean_temp + weekend_ind, data = walking_data)

train_dat = walking_data %>% filter(date <= '2020-02-15')
model_mat = model.matrix(value ~ as.numeric(date)*region + week_day + I(week_day^2) + I(week_day^3)+ mean_temp + weekend_ind + region, data = train_dat)
pre_covid_walking_model_boost = xgboost(data = model_mat, label = train_dat$value, nrounds = 20)

walking_data_fin = mutate(
  walking_data, 
  predicted_walking_baseline = predict(pre_covid_walking_model_boost, newdata = walking_data_mat),
  percent_of_predicted = value / predicted_walking_baseline
) %>%
  data.table() %>%
  arrange(region, date)


smoothed_pct_of_predicted = map(unique(walking_data_fin$region), function(the_region){
  region_sub = filter(walking_data_fin, region == the_region) %>%
    mutate(
      lag_cases = lag(cases, 1),
      new_cases = cases - lag_cases
    )
  
  smoother = loess(percent_of_predicted ~ as.numeric(date), data = region_sub, span = 0.2)
  region_sub$smoothed_percent_of_predicted = predict(smoother)
  return(region_sub)
}) %>%
  bind_rows()


stats_by_region = group_by(smoothed_pct_of_predicted, region) %>%
  summarize(
    date_80 = min(date[smoothed_percent_of_predicted <= 0.8]),
    date_80_val = smoothed_percent_of_predicted[date == date_80],
    date_80_cases = cases[date == date_80],
    last_date = max(date),
    last_val = smoothed_percent_of_predicted[date == last_date]
  )


ggplot(smoothed_pct_of_predicted, aes(date, smoothed_percent_of_predicted, colour = region)) +
  # geom_vline(data = filter(stats_by_region, region == 'New York City'), aes(xintercept = date_80, colour = region), show.legend = F) +
  geom_line(size = 0.25, show.legend = F) +
  # geom_point(data = stats_by_region, aes(date_80, date_80_val, size = date_80_cases)) +
  geom_point(aes(size = new_cases)) +
  geom_point(data = stats_by_region, aes(date_80, date_80_val), size = 2, shape = 6) +
  # geom_segment(data = stats_by_region, aes(x = date_80, y = 0, yend = date_80_val, xend = date_80, colour = region), size = 0.25) +
  geom_text_repel(data = stats_by_region, aes(date_80, date_80_val, label = paste0(format(date_80, '%b %d'), '\n', comma(date_80_cases, accuracy = 1))), size = 3.5, fontface = 'bold', min.segment.length = 0) +
  scale_size(name = 'Daily New Cases',range = c(0, 3), label = comma) +
  scale_y_continuous(labels = percent, breaks = seq(0, 1.5, by = 0.1)) +
  
  geom_text_repel(data = stats_by_region, aes(last_date + 3, last_val, label = paste0(region, ', ', percent(last_val, accuracy = 1))), size = 3) +
  labs(
    x = '', y = 'Percent of Baseline Activity',
    title = 'Rate of Mobility Through the COVID-19 Epidemic',
    subtitle = "Mobility data shows level of requests for walking directions from Apple maps for selected regions, which measures both potential COVID-19 exposure and economic activity. Baseline data is estimated using a boosted tree algorithm and then smoothed with LOESS. Triangles show date at which activity falls below 80% of baseline along with total cases." %>%
      str_wrap(150),
    caption = 'Chart: Taylor G. White\nData: Apple, Johns Hopkins CSSE, New York Times, NOAA'
  ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = 'bottom',
    plot.subtitle = element_text(size = 11, face = 'italic'),
    plot.caption = element_text(size = 10, face = 'italic', hjust = 0)
  ) +
  scale_colour_hue(guide = F, name = 'Region') +
  scale_x_date(date_breaks = '1 week', limits = c(as.Date('2020-01-20'), max(smoothed_pct_of_predicted$date) + 4), date_labels = '%b %d')
# ggsave('output/walking_mobility_data_by_date.png', height = 10, width = 12, units = 'in', dpi = 800)


