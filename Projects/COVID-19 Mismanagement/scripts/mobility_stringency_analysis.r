library(tidyverse)
library(data.table)

setwd("~/Public_Policy/Projects/COVID-19")
nordics = c('Japan', 'South Korea', 'Italy', 'Germany', 'United States', 'Sweden')


oxford_stringency_index = read.csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv") %>%
  rename(
    country = CountryName
  ) %>%
  mutate(
    entity_name = ifelse(is.na(RegionName) | RegionName == "", country, RegionName),
    stringency_geo_type = ifelse(entity_name == country, 'country', 'region'),
    date = as.Date(Date %>% as.character(), format = '%Y%m%d')
  ) 
View(oxford_stringency_index)

country_stringency = filter(oxford_stringency_index, stringency_geo_type == 'country')


#### read in mobility data and join everything else on ####
mobility_dataset_df = tibble(
  dsn = list.files('data', pattern = 'applemobilitytrends', full.names = T),
  date = str_extract(dsn, '[0-9]{4}-[0-9]{2}-[0-9]{2}') %>% as.Date()
) %>% 
  arrange(date) %>% 
  tail(1)

apple_mobility_dat = read_csv(mobility_dataset_df$dsn) %>%
  pivot_longer(cols = matches('^([0-9+]{4})'), names_to = 'date') %>%
  mutate(
    date = as.Date(date),
    week_day = lubridate::wday(date),
    weekend_ind = ifelse(week_day %in% c(7, 1), 'Weekend', "Week Day"),
    entity_name = recode(region, 
                         `UK` = 'United Kingdom',
                         `San Francisco - Bay Area` = 'San Francisco', 
                         `Republic of Korea` = 'South Korea')
  ) %>%
  left_join(
    country_stringency, by = c('entity_name', 'date')
  ) %>%
  mutate(
    entity_name = recode(entity_name, `Korea, South` = 'South Korea')
  )


nordic_countries_walking = filter(apple_mobility_dat, geo_type == 'country/region', 
                                  entity_name %in% nordics, transportation_type == 'walking')
names(nordic_countries_walking)
# install.packages('plotly')
ggplot(nordic_countries_walking, aes(date, GovernmentResponseIndex, colour = entity_name)) +
  geom_step(size = 1) +
  geom_point(aes(size = ConfirmedDeaths))

View(nordic_countries_walking)


group_by(apple_mobility_dat, geo_type, entity_name, transportation_type) %>%
  summarize(
    mean_val = mean(value, na.rm = T),
    mean_stringency = mean(StringencyIndex, na.rm = T)
  ) %>%
  filter(!is.na(mean_stringency))

# 
