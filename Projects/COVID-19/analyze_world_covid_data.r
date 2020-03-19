# https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html


library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(data.table)
library(countrycode)
library(viridisLite)
library(gganimate)
library(gifski)
library(tidyverse)
library(lmtest)


all_covid_data = read_csv('https://covid.ourworldindata.org/data/full_data.csv') %>%
  arrange(location, date) %>% 
  mutate_if(is.numeric, function(x) ifelse(is.na(x), 0, x)) %>%
  pivot_longer(cols = c('new_cases', 'new_deaths', 'total_cases', 'total_deaths'),
               names_to = c('measure'), values_to = 'value') %>%
  data.table()

all_covid_data_diffs = 
  all_covid_data[, {
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
    
  }, by = list(location, measure)] %>%
  pivot_wider(
    id_cols = c('location', 'date'),
    names_from = 'measure', 
    values_from = c('value', 'lag_value', 'diff_value', 'pct_change_value', 'lag_4_value', 'lag_5_value',
                    'lag_6_value')
  )
  
head(all_covid_data_diffs)
names(all_covid_data_diffs)
ggplot(all_covid_data_diffs, aes())

acf(all_covid_data_diffs$value_new_cases)
acf(all_covid_data_diffs$value_new_deaths)
a = pacf(all_covid_data_diffs$value_new_deaths)

head(all_covid_data_diffs)
ggplot(all_covid_data_diffs, aes())

?acf
ccf(all_covid_data_diffs$value_new_cases, all_covid_data_diffs$value_new_deaths)
ccf(all_covid_data_diffs$value_new_deaths, all_covid_data_diffs$value_new_cases)

ccfvalues = ccf(all_covid_data_diffs$value_new_cases, all_covid_data_diffs$value_new_deaths)
grangertest(value_new_deaths ~ value_new_cases, order = 10, data = all_covid_data_diffs)
grangertest(value_new_cases ~ value_new_deaths, order = 10, data = all_covid_data_diffs)

ggplot(all_covid_data_diffs %>% filter(location != 'World'), aes(value_new_cases, value_new_deaths)) +
  geom_point() +
  stat_smooth(method = 'lm')


ggplot(all_covid_data_diffs %>% filter(location == 'United States', date >= as.Date('2020-02-01')), 
       aes(date, value)) +
  facet_wrap(~measure, scales = 'free_y') +
  geom_line()
  # geom_point() +
  # geom_linerange(aes(ymin = 0, ymax = pct_change_value))



latest_covid_data = all_data[, {

world <- ne_countries(scale = "medium", returnclass = "sf")
head(world)
world$geounit[!world$geounit %in% latest_data$location]
setdiff(world$geounit, latest_data$location)
setdiff(latest_data$location, world$geounit)

ggplot(data = world) +
  geom_sf()

head(all_data)
