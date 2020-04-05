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

### get state and US population data  ###
fred_sqlite = dbConnect(SQLite(), dbname= "data/fred_sqlite.sqlite")

state_economic_data = dbGetQuery(fred_sqlite, 'select * from state_economic_data') %>%
  mutate(
    date = as.Date(date, origin = '1970-01-01'),
    title_clean = str_extract(title, '(.* in )|(.* for )') %>% str_replace('( in )|( for )', ''),
    title_for_col = paste0("x_", str_replace_all(title_clean, '[ \\-%,]', '_'))
  ) %>% 
  arrange(state_name, title_clean, date) %>%
  data.table() %>%
  # accidentally inserted duplicate records for Alabama, dedup here (need to index this db later)
  unique(by = c('state_name', 'title_clean', 'date'))


population_by_state = filter(state_economic_data, title_clean == 'Resident Population')
population_by_state = filter(population_by_state, date == max(date))
us_population = sum(population_by_state$value)

## get shapefile data ## 
us_counties_shp = us_counties()
us_states_shp = us_states()
us_map = USAboundaries::us_boundaries()
us_states_tigris = tigris::states()
us_counties_tigris = tigris::counties()

state_geo_center = us_states_tigris@data %>%
  mutate(
    lat = as.numeric(INTPTLAT),
    long = as.numeric(INTPTLON)
  ) %>%
  rename(
    state_abbr = STUSPS
  )

#### load and clean state covid data ####
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
    percent_positive_cases = total_cases / (total_cases + negative),
    tests_with_results = negative + total_cases,
    case_fatality_rate = total_deaths / total_cases
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
  )


all_covid_data_stacked = bind_rows(us_covid_data, us_states_covid_data) %>%
  arrange(location_key, date) %>%
  pivot_longer(cols = c('total_cases', 'total_deaths', 'total_tests', 
                        'percent_positive_cases', 'case_fatality_rate', 'tests_with_results'),
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
      time = as.integer(date - min(date)),
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
    id_cols = c('location_key', 'location', 'location_type','data_source', 'date', 'time'),
    names_from = 'measure', 
    values_from = c('value', 'lag_value', 'diff_value', 
                    'pct_change_value', 'lag_4_value', 'lag_5_value',
                    'lag_6_value')
  ) %>%
  left_join(
    state_name_mappings, by = c('location' = 'state_abbr')
  ) %>%
  left_join(
    select(population_by_state, state_name, state_pop = value)
  ) %>%
  left_join(
    tibble(location = 'United States', us_pop = us_population), by = 'location'
  ) %>%
  mutate(
    location_name = ifelse(is.na(state_name), location, state_name),
    population = ifelse(is.na(state_pop), us_pop, state_pop),
    pop_100k = population/1000,
    state_pop = NULL, us_pop = NULL
  )


case_100_dates = group_by(all_covid_data_diffs, location_key) %>%
  summarize(
    date_case_20 = min(date[value_total_cases >= 20]),
    has_30_days = as.numeric(max(date[value_total_cases >= 20]) - min(date[value_total_cases >= 20]) >= 30)
  )

# final, clean dataset with all sorts of calculations complete #
all_covid_data_diffs_dates = left_join(all_covid_data_diffs, case_100_dates) %>%
  mutate(
    new_tests_per_100k = diff_value_total_tests / pop_100k,
    tests_per_100k = value_total_tests / pop_100k,
    cases_per_100k = value_total_cases / pop_100k,
    deaths_per_100k = value_total_deaths / pop_100k,
    days_since_case_20 = as.numeric(date - date_case_20),
    percent_positive_new_tests = ifelse(time == 0, value_total_cases/value_tests_with_results, diff_value_total_cases / diff_value_tests_with_results)
  ) %>%
  arrange(location_key, date)

select(all_covid_data_diffs_dates, has_30_days, date, location_name, diff_value_total_tests, value_total_tests, new_tests_per_100k) %>% View()

##### plot tests per 100k #####
all_covid_data_diffs_dates %>%
  filter(days_since_case_20 >= 0, location == 'United States') %>%
ggplot(aes(days_since_case_20, new_tests_per_100k)) +
  theme_bw() +
  geom_bar(stat = 'identity') +
  labs(
    x = '\nDays Since Case 20', 
    y = 'Daily Tests Per 100k Population\n',
    title = 'COVID-19 Tests Per 100k Population', 
    subtitle = sprintf('United States, through %s', max(all_covid_data_diffs_dates$date) %>% format('%B %d')),
    caption = 'Chart: Taylor G. White\nData: covidtracking.com'
  ) +
  scale_y_continuous(breaks = seq(0, 500, by = 100)) +
  scale_x_continuous(breaks = seq(0, 30, by = 5)) +
  theme(
    title = element_text(size = 16),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(hjust = 0, face = 'italic', size = 10)
  )


# check to make sure the population data looks good
select(all_covid_data_diffs_dates, location, population) %>% 
  data.table() %>%
  unique(by = 'location') %>%
  View()

latest_state_data = 
  all_covid_data_diffs_dates %>%
  group_by(location_key, location) %>%
  summarize(
    last_cases = tail(value_total_cases, 1),
    last_cases_100k = tail(cases_per_100k, 1),
    last_deaths_100k = tail(deaths_per_100k, 1),
    new_cases = tail(diff_value_total_cases, 1),
    last_date = max(date)
  )



##### load and clean county covid data #####

# https://transportgeography.org/?page_id=8565
# https://en.wikipedia.org/wiki/Gravity_model_of_trade
# use county by county commute flows as distance estimate or input to gravitational model
# what proportion of the combined counties' population flows between the two counties? 
# what proportion of the combined counties' commuters flows between the two counties? 
# flights between cities
# https://www.researchgate.net/profile/N_Pavlis/publication/226883469_A_Preliminary_Gravitational_Model_to_Degree_2160/links/53ff0a6c0cf283c3583c3ff9.pdf


nyt_county_data = read_csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv') %>%
  arrange(county, state, fips, date)

latest_county_data = group_by(nyt_county_data, county, state, fips) %>%
  summarize(
    latest_date = max(date),
    latest_cases = tail(cases, 1),
    latest_deaths = tail(deaths, 1)
  )

all_county_data_geo = left_join(us_counties_tigris@data, nyt_county_data, by = c('GEOID' = 'fips')) %>%
  mutate(
    lat = as.numeric(INTPTLAT),
    long = as.numeric(INTPTLON)
  )

latest_county_data_geo = left_join(us_counties_tigris@data, latest_county_data, by = c('GEOID' = 'fips')) %>%
  mutate(
    lat = as.numeric(INTPTLAT),
    long = as.numeric(INTPTLON)
  )

##### create map for all 50 states #####
US_state_data = left_join(us_states_shp, latest_state_data, by = c('state_abbr' = 'location')) %>% 
  left_join(state_geo_center)

lower_48 = state.name[!state.name %in% c('Hawaii', 'Alaska')]
lower_48_state_data = filter(US_state_data, name %in% lower_48)

alaska_data = filter(US_state_data, name %in% 'Alaska')
hawaii_data = filter(US_state_data, name %in% 'Hawaii')


world <- ne_countries(scale = "medium", returnclass = "sf")
usa <- subset(world, admin == "United States of America")

v_scale = scale_fill_viridis(name = 'Fill', guide = F) 
# v_scale = scale_fill_gradient(guide = F, low = 'white', high = 'red')
# ?scale_fill_gradient2
# summary(latest_state_data$last_cases_100k %>% log())

mainland <- ggplot() +
  geom_sf(fill = NA) +  
  geom_sf(data = lower_48_state_data, aes(fill = log(last_cases_100k)), show.legend = F, alpha = 0.75) +
  geom_sf_text(data = lower_48_state_data, aes(long, lat, label = comma(last_cases_100k, accuracy = 1)),
               colour = 'black', fontface='bold', size = 2) +
  v_scale +
  coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), ylim = c(-2300000,
                                                                     730000)) +
  labs(x = '', y = '',
       caption = 'Chart: Taylor G. White\nData: covidtracking.com',
       title = 'U.S. COVID-19 Cases by State, Per 100k Population', subtitle = sprintf('As of %s',
                                                                  unique(format(latest_state_data$last_date, '%B %d')))) +
  
  theme_map() +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    title = element_text(size = 16),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(hjust = 0, face = 'italic', size = 10)
  )

alaska <- ggplot() +
  geom_sf(data = alaska_data, aes(fill = log(last_cases_100k)), alpha = 0.75) +
  geom_sf_text(data = alaska_data, aes(long, lat, label = comma(last_cases_100k, accuracy = 1)),
               colour = 'black', fontface='bold', size = 2) +
  v_scale +
  coord_sf(crs = st_crs(3467), xlim = c(-2400000, 1600000), ylim = c(200000,
                                                                     2500000), expand = FALSE, datum = NA) +
  labs(x='', y='') +
  theme_map() 

hawaii  <- ggplot(data = hawaii_data) +
  geom_sf(data = hawaii_data, aes(fill = log(last_cases_100k)), alpha = 0.75) +
  geom_sf_text(data = hawaii_data, aes(long, lat, label = comma(last_cases_100k, accuracy = 1)),
               colour = 'black', fontface='bold', size = 2) +
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

##### take a look at testing data, normed by population information #####

us_data = filter(all_covid_data_diffs_dates, days_since_case_20 >= 7, location == 'United States')
ggplot(us_data, aes(percent_positive_new_tests, diff_value_total_cases)) + 
  geom_point(aes(alpha = time, size = time)) +
  # stat_smooth(method = 'gam', formula = y ~ s(x, k=7))
stat_smooth(method = 'lm', formula = y ~ poly(x, 5))

all_covid_data_diffs_dates %>% filter(location %in% c('WA', 'CA', 'NY', 'FL', 'NJ')) %>%
  ggplot() +
  geom_line(aes(days_since_case_20, pct_change_value_total_cases, fill = location))

filter(all_covid_data_diffs_dates, location %in%  c(
  'United States',
  'WA', 'NY', 'CA', 'LA', 'NJ', 'MI', 'FL'
  # , 'FL', 'CO', 
  # 'MI', 'IL',' 'LA', 'NJ', 'TX', 'GA'
)
) %>% 
  filter(days_since_case_20 >= 15) %>%
  ggplot(aes(value_percent_positive_cases, value_case_fatality_rate, colour = location)) +
  geom_point(aes(alpha = days_since_case_20, size = log(value_total_cases))) +
  scale_size(guide = F, range = c(0, 3)) +
  scale_alpha(guide = F, range = c(0.3, 1)) +
  geom_path(aes(alpha = days_since_case_20), size = 0.75) +
  scale_y_continuous(breaks = seq(0, 0.1, by = 0.01), limits = c(0, 0.05), labels = percent) +
  scale_x_continuous(labels = percent, breaks = seq(0, 0.4, by = 0.1)) +
  theme(
    strip.background = element_rect(fill = 'darkgray'),
    strip.text = element_text(face = 'bold'),
    text = element_text(colour = 'white'),
    axis.text = element_text(colour = 'white'),
    title = element_text(size = 16, colour = 'white'),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    plot.caption = element_text(size = 10, hjust = 0),
    legend.title = element_text(size = 14),
    plot.subtitle = element_text(size = 11, face = 'italic'),
    plot.background = element_rect(fill = 'black'),
    legend.background = element_rect(fill = 'black'),
    panel.background = element_rect(fill = 'black'),
    panel.grid.minor  = element_blank(),
    panel.grid.major = element_line(size = 0.25),
    legend.position = 'bottom'
  ) +
  guides(colour = guide_legend(override.aes = list(size = 2))) +
  scale_colour_hue(name = '') +
  labs(
    x = '\nPercent Positive Cases', y = 'Case Fatality Rate\n', 
    title = 'Paths of Case Fatality Rates and Positive Test Rates',
    subtitle = paste0('U.S. and Selected States, Through ', max(all_covid_data_diffs_dates$date) %>% format('%B %d'))
  )
ggsave('output/positive_cases_vs_case_fatality_rate.png', height = 8, width = 8, units = 'in', dpi = 800)



cfr_states = filter(all_covid_data_diffs_dates, days_since_case_20 >= 0, location_type == 'US State') %>%
  ggplot( aes(days_since_case_20, value_case_fatality_rate)) +
  geom_hline(aes(yintercept = 0.02), colour = 'black', size = 1.25) +
  # geom_hline(aes(yintercept = 0.01), colour = 'orange', size = 1) +
  geom_line(aes(group = location), alpha = 0.2) + 
  scale_y_continuous(labels = percent, breaks = seq(0, 0.25, by = 0.01)) +
  geom_line(data = filter(all_covid_data_diffs_dates, days_since_case_20 >= 0, location_type != 'US State'), colour = 'red', size = 1) +
  geom_point(data = filter(all_covid_data_diffs_dates, days_since_case_20 >= 0, location_type != 'US State'), colour = 'red', size = 1) 
ggplotly(cfr_states)

ggplot(aes(days_since_case_20, diff_value_total_deaths)) +
  geom_bar(stat = 'identity')


a = all_covid_data_diffs_dates %>% filter(days_since_case_20 >= 0, has_30_days) %>%
  ggplot(aes(days_since_case_20, cases_per_100k, colour = location_name)) +
  geom_line() +
  geom_point()
ggplotly(a)