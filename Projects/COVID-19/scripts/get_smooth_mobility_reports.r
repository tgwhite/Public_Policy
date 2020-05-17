

library(tidyverse)
library(data.table)
library(xgboost)

library(USAboundaries)
library(fuzzyjoin)
library(mgcv)
library(ggrepel)
library(cowplot)
library(sf)
library(RColorBrewer)
library(viridisLite)
library(scales)

##### pull in case data ####
johns_hopkins_cases = read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv') %>%
  pivot_longer(cols = matches('^([0-9])'), names_to = 'date', values_to = 'cases') %>%
  filter(
    # `Country/Region` %in% c('Korea, South', 'Italy', 'Germany')
  ) %>%
  mutate(
    date = as.Date(date, format = '%m/%d/%y'),
    is_country = is.na(`Province/State`)
  ) %>%
  rename(
    country_name = `Country/Region`
  ) %>%
  mutate(
    country_name = recode(country_name, `US` = 'United States'),
    entity_name = country_name
  )


#### get weather data from NOAA ####
# https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/by_year
# https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/by_year/readme.txt
# https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt

setwd("~/../Downloads")
# download.file('https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/by_year/2020.csv.gz', destfile = '2020_weather.csv.gz')

weather_2020 = fread('2020_weather.csv.gz') %>% setorder(V1, V2, V3)
names(weather_2020) = c('station', 'date', 'type', 'value', 'm_flag', 'q_flag', 's_flag', 'time')
weather_2020$date_upd = as.Date(weather_2020$date %>% as.character(), format = '%Y%m%d')
weather_2020$country = str_extract(weather_2020$station, '^([A-Z]{2})')

weather_country_list = read_table("ghcnd-countries.txt", col_names = F) 
names(weather_country_list) = c('country', 'country_name')

weather_2020_fin = left_join(weather_2020, weather_country_list) %>% data.table()
setkey(weather_2020_fin, country_name, type)
cases_weather_country_list = intersect(weather_country_list$country_name, johns_hopkins_cases$country_name)

weather_sub = weather_2020_fin[J(cases_weather_country_list), ]
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
# check for missing values
map_dbl(stations, function(x){sum(is.na(x))})


#### get locations of us cities and get closest weather stations ####
setwd("~/Public_Policy/Projects/COVID-19")

us_cities_shp = us_cities()

us_cities_coord = st_coordinates(us_cities_shp) %>% as.data.frame()
names(us_cities_coord) = c('longitude', 'latitude')
us_cities_shp_fin = bind_cols(us_cities_shp, us_cities_coord)

us_cities_shp_fin$geometry = NULL
selected_cities = filter(us_cities_shp_fin, 
                         str_detect(city, 
                  '^(New York City)|^(San Francisco)|(Seattle)|(Los Angeles)'))

# join on stations within 50 miles of cities
city_stations = geo_left_join(selected_cities, stations %>% filter(country == 'US'), max_dist = 50)

# average temp across entities
weather_sub_station_meta = left_join(weather_sub, stations) %>% select(-date) %>%
  mutate(
    temp_f = ifelse(type == 'TAVG', (value/10) * 9/5 + 32, value)
  ) %>%
  left_join(
    city_stations %>% select(station, city)
  ) %>%
  mutate(
    entity_name = ifelse(is.na(city), country_name, city)
  )

avg_temp_sub = weather_sub_station_meta %>% filter(type == 'TAVG')
avg_temp_by_entity = group_by(avg_temp_sub, country, country_name, entity_name, date_upd) %>%
  summarize(
    mean_temp = mean(temp_f)
  )

# use counties as proxies for city cases # 
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

### read oxford stringency data ###


oxford_stringency_index = read_csv("https://github.com/OxCGRT/covid-policy-tracker/raw/master/data/OxCGRT_latest.csv") %>%
  rename(
    entity_name = CountryName
  ) %>%
  mutate(
    entity_name = recode(entity_name, `South Korea` = 'Korea, South'),
    date = as.Date(Date %>% as.character(), format = '%Y%m%d')
  )

### disaggregate jh data -- some countries have provinces/states included ###
johns_hopkins_cases_dt = arrange(johns_hopkins_cases, date, country_name) %>% data.table()
johns_hopkins_cases_dt_agg = johns_hopkins_cases_dt[, {
  the_df = data.frame(date, cases) %>%
    group_by(date) %>%
    summarize(
      the_cases = sum(cases)
    )
  list(
    date = the_df$date,
    cases = the_df$the_cases
  )
}, by = list(country_name, entity_name)] %>%
  mutate(
    entity_name = recode(entity_name, `Taiwan*` = 'Taiwan')
  )

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
                         `Republic of Korea` = 'Korea, South')
  ) %>%
  left_join(
    avg_temp_by_entity, by = c('entity_name', 'date' = 'date_upd')
  ) %>%
  left_join(
    selected_counties_cases 
  ) %>%
  left_join(
    johns_hopkins_cases_dt_agg , by = c('date', 'entity_name')
  ) %>%
  left_join(
    oxford_stringency_index
  ) %>%
  mutate(
    cases = coalesce(cases.x, cases.y, as.numeric(NA))
  ) %>%
  select(-cases.x, -cases.y) %>%
  mutate(
    entity_name = recode(entity_name, `Korea, South` = 'South Korea')
  )

#### subset to walking data -- this seems more comparable across countries since driving/transit trades off ###
walking_data = filter(apple_mobility_dat, transportation_type == 'walking', !is.na(mean_temp), !is.na(value))

# make sure no entities are counted multiple times # 
obs_by_entity = group_by(walking_data, entity_name) %>%
  summarize(
    obs = n(),
    n_dates = n_distinct(date)
  )
stopifnot(!any(obs_by_entity$obs != obs_by_entity$n_dates))

### fit a model for baseline period to capture the seasonality of mobility ####
baseline_period_end = as.Date('2020-02-15')

case_ten_dates = group_by(walking_data, entity_name) %>%
  summarize(
    date_case_ten = min(date[cases >= 10], na.rm = T)
  ) %>%
  arrange(date_case_ten)

baseline_data = left_join(walking_data, case_ten_dates) %>%
  filter(
    date <= pmin(date_case_ten, baseline_period_end)
  )
the_formula = as.formula('value ~ as.numeric(date) * region * mean_temp + week_day + I(week_day^2)  + weekend_ind')
pre_covid_walking_model = lm(the_formula, data = baseline_data)
the_anova = anova(pre_covid_walking_model)
the_anova$rsquared = the_anova$`Sum Sq`/sum(the_anova$`Sum Sq`)
the_anova$predictor = row.names(the_anova)
the_anova = arrange(the_anova, -rsquared)


train_dat = baseline_data
model_mat = model.matrix(the_formula, data = train_dat)
pre_covid_walking_model_boost = xgboost(data = model_mat, label = train_dat$value, nrounds = 100)

# walking_dat_sub = select(walking_data, region, date, value, mean_temp, week_day, weekend_ind)
# head(walking_dat_sub)
# na_rm_this = na.omit(walking_dat_sub)
# anti_join(walking_dat_sub, na_rm_this)
walking_data_mat = model.matrix(the_formula, data = walking_data)


walking_data_fin = mutate(
  walking_data, 
  predicted_walking_baseline = predict(pre_covid_walking_model_boost, newdata = walking_data_mat),
  percent_of_predicted = value / predicted_walking_baseline
) %>%
  data.table() %>%
  arrange(region, date)

smoothed_pct_of_predicted = map(unique(walking_data_fin$entity_name), function(the_entity){
  region_sub = filter(walking_data_fin, entity_name == the_entity) %>%
    mutate(
      lag_cases = lag(cases, 1),
      new_cases = cases - lag_cases
    )
  
  smoother = loess(percent_of_predicted ~ as.numeric(date), data = region_sub, span = 0.2)
  region_sub$smoothed_percent_of_predicted = predict(smoother)
  return(region_sub)
}) %>%
  bind_rows() %>%
  rename(
    mobility = value
  )
  


stats_by_region = group_by(smoothed_pct_of_predicted, entity_name) %>%
  summarize(
    obs = n(),
    date_80 = min(date[smoothed_percent_of_predicted <= 0.8]),
    date_80_val = smoothed_percent_of_predicted[date == date_80],
    date_80_cases = cases[date == date_80],
    last_date = max(date),
    last_val = smoothed_percent_of_predicted[date == last_date]
  )

write.csv(smoothed_pct_of_predicted, 'data/covid_cases_mobility_stringency.csv', row.names = F)

# ggplot(smoothed_pct_of_predicted, aes(StringencyIndex, percent_of_predicted)) +
#   geom_point() +
#   stat_smooth()

avg_by_date = group_by(smoothed_pct_of_predicted, date) %>%
  summarize(
    avg_mobility = mean(mobility, na.rm = T),
    avg_smoothed_percent_of_predicted = mean(smoothed_percent_of_predicted, na.rm = T)
  )
ggplot(avg_by_date, aes(avg_smoothed_percent_of_predicted, avg_mobility)) +
  geom_point()

the_pal = brewer.pal(3, 'Set1')

last_vals_by_country = group_by(smoothed_pct_of_predicted, entity_name) %>%
  summarize(
    last_date = max(date),
    last_cases = cases[date == last_date],
    last_mobility = mobility[date == last_date],
    last_predicted_walking_baseline = predicted_walking_baseline[date == last_date],
    last_smoothed_percent_of_predicted = smoothed_percent_of_predicted[date == last_date]
  )

original_plot = ggplot(smoothed_pct_of_predicted, aes(date, mobility, group = entity_name, colour = StringencyIndex)) +
  theme_bw() +
  scale_colour_viridis_c(name = 'Stringency Index', option = 'C') +
  geom_line(size = 0.25, alpha = 0.6) +
  # geom_line(data = avg_by_date, aes(y = avg_mobility, group = NULL), colour = the_pal[1], size = 1) +
  labs(
    x = '', y = 'Mobility Index', 
    title = 'Reported Mobility Index (Walking)'
  ) +
    theme(
      legend.position = c(0.85, 0.8),
      # plot.background = element_rect(fill = 'black'),
      # panel.background = element_rect(fill = 'black'),
      # panel.grid = element_line(colour = 'white'),
      panel.grid.minor = element_blank()) +
    scale_y_continuous(breaks = seq(0, 350, by = 50))


selected_entities = c('United States', 'Italy', 'South Korea', 'Germany')
linetype_data = data.frame(vals = c('Actual', 'Predicted'), date = rep(baseline_period_end, 2), mobility = rep(100, 2)) 
  
original_with_predictions = 
  smoothed_pct_of_predicted %>% filter(entity_name %in% selected_entities) %>%
  ggplot(aes(date, colour = entity_name)) +
  theme_bw() +
  scale_linetype(name = '') +
  geom_line(aes(y = mobility), size = 0.5, show.legend = F) +
  scale_colour_hue(guide = F) +
  geom_line(data = linetype_data, aes(x = date, y = mobility, linetype = vals, colour = NULL), alpha = 0) +
  guides(linetype = guide_legend(override.aes = list(alpha = 1))) +
  labs(x = '', y = 'Mobility Index',
       title = 'Actual Mobility Index vs. Predicted Without COVID-19, Selected Countries'
       ) +
  theme(
    legend.position = c(0.1, 0.25),
    # plot.background = element_rect(fill = 'black'),
    # panel.background = element_rect(fill = 'black'),
    # panel.grid = element_line(colour = 'white'),
    panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = seq(0, 200, by = 50)) +
  geom_line(aes(y = predicted_walking_baseline), linetype = 'dashed', show.legend = T) 
  # geom_text_repel(data = last_vals_by_country %>% filter(entity_name %in% selected_entities), 
                  # aes(x = last_date + 4, y = last_predicted_walking_baseline, label = paste(entity_name, '(Predicted)'))) +
  # geom_text_repel(data = last_vals_by_country %>% filter(entity_name %in% selected_entities), 
                  # aes(x = last_date + 4, y = last_mobility, label = paste(entity_name, '(Actual)')))


smoothed_plot = ggplot(smoothed_pct_of_predicted, aes(date, smoothed_percent_of_predicted, group = entity_name, colour = StringencyIndex)) +
  geom_line(size = 0.25, show.legend = F, alpha = 0.6) + 
  # geom_line(data = avg_by_date, aes(y = avg_smoothed_percent_of_predicted, group = NULL), colour = the_pal[1], size = 1) +
  theme_bw() +
  scale_colour_viridis_c(name = 'Stringency Index', option = 'C') +
  labs(
    x = '', y = 'Percent of Predicted Baseline Mobility Index',
    title = 'Smoothed Mobility Index, All Available Countries'
  ) +
  
  theme(
    panel.grid.minor = element_blank()
        # plot.background = element_rect(fill = 'black'),
        # panel.background = element_rect(fill = 'black'),
        # panel.grid = element_line(colour = 'white')
        ) +
  scale_y_continuous(labels = percent, limits = c(0, 1.3), breaks = seq(0, 1.3, by = 0.2))

smoothed_sub = 
  smoothed_pct_of_predicted %>% filter(entity_name %in% selected_entities) %>%
  ggplot(aes(date, colour = entity_name)) +
  theme_bw() +
  geom_line(aes(y = smoothed_percent_of_predicted), size = 0.75, show.legend = T) +
  labs(
    x = '', y = 'Percent of Predicted Baseline Mobility Index', 
    title = 'Smoothed Mobility Index, Selected Countries'
  ) +
  theme(
    legend.position = c(0.1, 0.25),
    # plot.background = element_rect(fill = 'black'),
    # panel.background = element_rect(fill = 'black'),
    # panel.grid = element_line(colour = 'white'),
    panel.grid.minor = element_blank()) +
  scale_colour_hue(name = '') +
  guides(colour = guide_legend(override.aes = list(size = 1))) +
  scale_y_continuous(labels = percent, limits = c(0, 1.3), breaks = seq(0, 1.3, by = 0.2)) 
  # geom_text_repel(data = last_vals_by_country %>% filter(entity_name %in% selected_entities), 
  #                 aes(x = last_date , y = 
  #                       last_smoothed_percent_of_predicted, label = paste(entity_name, percent(last_smoothed_percent_of_predicted, accuracy = 1))), 
  #                 show.legend = F, hjust=1, nudge_x = 4, 
  #                 direction = 'y', segment.colour = 'gray', size = 2)


comb_plot = plot_grid(original_plot, original_with_predictions, smoothed_plot, smoothed_sub)
comb_plot_sub = add_sub(comb_plot, "Chart: Taylor G. White\nData: Apple (Mobility), Oxford (Stringency), Johns Hopkins CSSE, NYT, NOAA", 
               x = 0.05, y = 0.35, hjust = 0, size = 10, fontface = 'italic')

title <- ggdraw() + 
  draw_label(
    "The Month the Earth Stood Still: Reduction in Movement Associated with COVID-19 and Government Restrictions",
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
final_plot = plot_grid(
  title, comb_plot_sub,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.05, 1)
)

# final_plot
save_plot('output/smoothed_mobility_index_data.png', plot = final_plot, 
          base_height = 12, 
           base_width = 14, units = 'in', dpi = 400)


# smoothed_pct_of_predicted %>%
#   ggplot(aes(date, smoothed_percent_of_predicted, colour = entity_name)) +
#   geom_vline(data = filter(stats_by_region, region == 'New York City'), aes(xintercept = date_80, colour = region), show.legend = F) +
#   geom_line(size = 0.25, show.legend = F) +
#   geom_point(data = stats_by_region, aes(date_80, date_80_val, size = date_80_cases)) +
#   geom_point(aes(size = new_cases)) +
#   geom_point(data = stats_by_region, aes(date_80, date_80_val), size = 2, shape = 6) +
#   geom_segment(data = stats_by_region, aes(x = date_80, y = 0, yend = date_80_val, xend = date_80, colour = region), size = 0.25) +
#   geom_text_repel(data = stats_by_region, aes(date_80, date_80_val, label = paste0(format(date_80, '%b %d'), '\n', comma(date_80_cases, accuracy = 1))), size = 3.5, fontface = 'bold', min.segment.length = 0) +
#   scale_size(name = 'Daily New Cases',range = c(0, 3), label = comma) +
#   scale_y_continuous(labels = percent, breaks = seq(0, 1.5, by = 0.1)) +
#   
#   geom_text_repel(data = stats_by_region, aes(last_date + 3, last_val, label = paste0(entity_name, ', ', percent(last_val, accuracy = 1))), size = 3) +
#   labs(
#     x = '', y = 'Percent of Baseline Activity',
#     title = 'Rate of Mobility Through the COVID-19 Epidemic',
#     subtitle = "Mobility data shows level of requests for walking directions from Apple maps for selected regions, which measures both potential COVID-19 exposure and economic activity. Baseline data is estimated using a boosted tree algorithm and then smoothed with LOESS. Triangles show date at which activity falls below 80% of baseline along with total cases." %>%
#       str_wrap(150),
#     caption = 'Chart: Taylor G. White\nData: Apple, Johns Hopkins CSSE, New York Times, NOAA'
#   ) +
#   theme_bw() +
#   theme(
#     panel.grid.minor = element_blank(),
#     legend.position = 'bottom',
#     plot.subtitle = element_text(size = 11, face = 'italic'),
#     plot.caption = element_text(size = 10, face = 'italic', hjust = 0)
#   ) +
#   scale_colour_hue(guide = F, name = 'Region') +
#   scale_x_date(date_breaks = '1 week', limits = c(as.Date('2020-01-20'), max(smoothed_pct_of_predicted$date) + 4), date_labels = '%b %d')
# ggsave('output/walking_mobility_data_by_date.png', height = 10, width = 12, units = 'in', dpi = 800)
# 
# 
