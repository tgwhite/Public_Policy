library(tidyverse)
library(WDI)
library(data.table)
library(countrycode)
library(zoo)
library(scales)
library(ggforce)
library(viridisLite)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(cowplot)
# library(RcppRoll)
# install.packages('roll')
library(roll)
library(gganimate)
library(gifski)
library(readxl)
library(sf)
library(RColorBrewer)
library(gridExtra)
library(fpc)
library(dbscan)
library(factoextra)
library(ggrepel)

setwd("~/Public_Policy/Projects/COVID-19")


# population_by_state = filter(annual_population_by_state, date == max(date))
population_by_state = read_csv('data/state_pop_2018.csv', skip = 1) %>%
  rename(
    state_name = `Geographic Area Name`,
    value = `Estimate!!Total`
  )

us_population = sum(population_by_state$value)


state_stringency = read_csv("https://raw.githubusercontent.com/OxCGRT/USA-covid-policy/master/data/OxCGRT_US_latest.csv") %>%
  mutate(
    date = as.Date(Date %>% as.character(), format = '%Y%m%d')
  )


state_name_mappings = tibble(state_name = state.name, state_abbr = state.abb)

us_states_covid_data = read_csv('http://covidtracking.com/api/states/daily.csv') %>%
  mutate(
    date = as.Date(as.character(date), format = '%Y%m%d'),
    location_type = 'US State', 
    data_source = 'covidtracking.com',
    location_key = paste(state, location_type, data_source, sep = '|')
  ) %>%
  arrange(location_key, date) %>%
  rename(
    total_cases = positive,
    total_deaths = death,
    total_tests = total,
    location = state
  ) %>%
  mutate(
    percent_positive_cases = total_cases / (total_cases + negative),
    tests_with_results = negative + total_cases,
    case_fatality_rate = total_deaths / total_cases
  ) %>%
  left_join(
    state_name_mappings, by = c('location' = 'state_abbr')
  ) %>%
  left_join(
    select(population_by_state, state_name, state_pop = value)
  ) %>%
  left_join(
    state_stringency, by = c('state_name' = 'RegionName', 'date' = 'date')
  )


# get ranks of stringency based on either case 100/1000 date 


dim(us_states_covid_data)
dim(us_states_covid_data)

names(state_stringency)
state_stringency$RegionName %>% table()
?month
?day
ggplot(state_stringency, aes(lubridate::day(date), StringencyIndex, group = RegionName)) +
  theme_bw() +
  facet_wrap(~month(date)) +
  geom_step(colour = 'gray') 
  # geom_hline(aes(yintercept = mean(StringencyIndex, na.rm = T)), colour = 'red') 

  # geom_step(data = filter(state_stringency, RegionName %in% c('Florida', 'Arizona', 'Texas', 'South Dakota')), aes(colour = RegionName), size = 1.25)

