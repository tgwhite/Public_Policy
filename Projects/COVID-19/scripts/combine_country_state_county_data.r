# https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
# https://covidtracking.com/api/
# https://covid.ourworldindata.org
library(R0)
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
library(RcppRoll)
library(sqldf)
library(albersusa)
library(RColorBrewer)
library(quantreg)
library(ggrepel)

# entity_name
# entity_type
# country
# date
# cases
# deaths
# FIPs (if available)
# latitude / longitude

us_counties_shp = us_counties()
us_states_shp = us_states()
us_map = USAboundaries::us_boundaries()
us_states_tigris = tigris::states()
us_counties_tigris = tigris::counties()

## population by area ## 

setwd("~/Public_Policy/Projects/COVID-19")

r0_window_size = 7
est_r0_window = function(new_cases, catch = F) {
  # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7014672/
  
  mGT <- generation.time("weibull", c(6.4, 2.3))
  # new_cases = a$the_cases
  if (!catch) {
    r0_est = est.R0.EG(new_cases, mGT, begin=as.integer(1), end=as.integer(length(new_cases)))$R    
    return(r0_est)
  } else {
    r0_est = NA
    tryCatch({
      r0_est = est.R0.EG(new_cases, mGT, begin=as.integer(1), end=as.integer(length(new_cases)))$R    
    }, error = function(e){
      return(r0_est)  
    })
    
  }
  
}


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


annual_population_by_state = filter(state_economic_data, title_clean == 'Resident Population') %>%
  mutate(
    year = year(date)
  ) 

population_by_state = filter(annual_population_by_state, date == max(date))
us_population = sum(population_by_state$value)

latest_country_pop = read_csv('data/latest_country_pop.csv')
county_pops = read_csv('data/ACSDP5Y2018.DP05_data_with_overlays_2020-04-16T114550.csv', skip = 2, col_names = F)
county_pops_names = read_csv('data/ACSDP5Y2018.DP05_data_with_overlays_2020-04-16T114550.csv', n_max = 1, col_names = T)
names(county_pops) = names(county_pops_names)

county_pops_sub = select(county_pops, GEO_ID, NAME, county_pop = DP05_0033E) %>%
  mutate(
    fips = str_remove(GEO_ID, '0500000US')
  )



## get shapefile data ## 
# us_counties_shp = us_counties()
# us_states_shp = us_states()
# us_map = USAboundaries::us_boundaries()
# us_states_tigris = tigris::states()
# us_counties_tigris = tigris::counties()


# remotes::install_git("https://git.sr.ht/~hrbrmstr/albersusa")
# https://github.com/hrbrmstr/albersusa

us_sf <- usa_sf("laea")
cty_sf <- counties_sf("aeqd")

state_geo_center = us_states_tigris@data %>%
  mutate(
    lat = as.numeric(INTPTLAT),
    long = as.numeric(INTPTLON)
  ) %>%
  rename(
    state_abbr = STUSPS
  )
head(state_geo_center)
### get lockdown dates ###
lockdown_dates = read_csv('data/lockdown_dates.csv') %>% 
  rename(location = Place, 
         country = Country,
         lockdown_start = `Start date`, lockdown_end = `End date`, level = Level) %>%
  mutate(
    level = recode(level, National = 'Country'),
    location = ifelse(is.na(location), country, location),
    location_type = ifelse(is.na(location), 'Country', 
                           ifelse(location %in% state.name & level == 'State', 'US State', level))
  )

names(lockdown_dates) = names(lockdown_dates) %>% str_replace_all(' ', '_')

all_countries = unique(lockdown_dates$country)
countries_with_lockdown_dates = filter(lockdown_dates, location_type == 'Country') %>% pull(country)
countries_wo_lockdown_dates = setdiff(all_countries, countries_with_lockdown_dates)

lockdowns_by_country = lockdown_dates %>% filter(country %in% countries_wo_lockdown_dates) %>%
  group_by( country) %>%
  summarize(
    lockdown_start = min(lockdown_start, na.rm = T),
    lockdown_end = max(lockdown_end)
  ) %>%
  mutate(
    location_type = 'Country', location = country
  )

lockdown_dates_fin = bind_rows(lockdown_dates, lockdowns_by_country)

##### country data ####
johns_hopkins_cases = read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv') %>%
  pivot_longer(cols = matches('^([0-9])'), names_to = 'date', values_to = 'cases') 

johns_hopkins_deaths = read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv') %>%
  pivot_longer(cols = matches('^([0-9])'), names_to = 'date', values_to = 'deaths')

jh_joined = left_join(johns_hopkins_cases, johns_hopkins_deaths) %>%
  mutate(
    date = as.Date(date, format = '%m/%d/%y')
  ) %>%
  rename(
    state = `Province/State`,
    country = `Country/Region`,
    total_cases = cases,
    total_deaths = deaths,
    latitude = Lat,
    longitude = Long
  ) 

jh_summary_by_country = group_by(jh_joined, country, date) %>%
  summarize(
    obs = n(),
    n_cases = sum(total_cases, na.rm =T),
    n_deaths = sum(total_deaths,na.rm = T)
  ) %>%
  ungroup() %>%
  rename(
    total_deaths = n_deaths, 
    total_cases = n_cases
  ) %>%
  mutate(
    total_tests = total_cases,
    tests_with_results = total_cases,
    state = NA,
    location = ifelse(is.na(state), country, state),
    location_type = ifelse(is.na(state), 'country', 'state/province'),
    data_source = 'Johns Hopkins CSSE',
    location_key = paste(country, state, location_type, location, data_source, sep = '|')
  ) %>%
  arrange(location_key, date)

#### US and state data ####
us_covid_data = read_csv('https://covidtracking.com/api/us/daily.csv') %>%
  mutate(
    date = as.Date(as.character(date), format = '%Y%m%d'),
    location = 'United States',
    country = 'United States',
    state = NA,
    location_type = 'country', 
    data_source = 'covidtracking.com',
    location_key = paste(country, state, location_type, location, data_source, sep = '|')
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

state_name_mappings_info = tibble(state_name = state.name, state_abbr = state.abb) %>%
  left_join(
    state_geo_center, by = c('state_abbr')
  ) %>%
  rename(
    fips = STATEFP
  ) %>%
  select(
    state_name, state_abbr, fips, latitude = lat, longitude = long
  )

us_states_covid_data = read_csv('http://covidtracking.com/api/states/daily.csv') %>%
  mutate(
    date = as.Date(as.character(date), format = '%Y%m%d'),
    location_type = 'US State', 
    country = 'United States',
    data_source = 'covidtracking.com'  
  ) %>%
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
    state_name_mappings_info, by = c('location' = 'state_abbr')
  ) %>%
  rename(
    state = state_name
  ) %>%
  mutate(
    location_key = paste(country, state, location_type, location, data_source, sep = '|')
  ) %>%
  arrange(location_key, date)

#### county data ####
nyt_county_data = read_csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv') %>%
  arrange(county, state, fips, date) %>%
  rename(
    location = county,
    total_cases = cases,
    total_deaths = deaths
  ) %>%
  mutate(
    total_tests = total_cases,
    tests_with_results = total_cases,
    location_type = 'County',
    country = 'United States',
    data_source = 'NYT',
    location_key = paste(country, state, location_type, location, data_source, sep = '|')
  ) %>%
  arrange(location_key, date)
nyt_county_data[nyt_county_data$location == 'New York City','fips'] = '36061'


all_covid_data_stacked = bind_rows(
  us_states_covid_data   
  , us_covid_data,
  
  nyt_county_data,
  jh_summary_by_country
) %>%
  arrange(location_key, date) %>%
  select(-percent_positive_cases, -case_fatality_rate) %>%
  pivot_longer(cols = c('total_cases', 'total_deaths', 'total_tests', 
                        'tests_with_results'),
               names_to = c('measure'), values_to = 'value') %>%
  data.table() 

# compute first differences, pct changes, etc. by state
all_covid_data_diffs = 
  all_covid_data_stacked[, {
    # the_dat = filter(all_covid_data_stacked, location_key == 'United States|Alaska|County|Kodiak Island Borough|NYT',
    # measure=='total_cases')
    # attach(the_dat)
    # detach(the_dat)
    # cat("\nkey ==",.BY[[1]],"\n\n")
    # cat("\nmeasure ==",.BY[[2]],"\n\n")
    
    lag_value = lag(value, 1)
    diff_value = value - lag_value
    pct_change_value = diff_value / lag_value
    
    cum_diff_value = value - lag_value
    cum_diff_value[is.na(cum_diff_value)] = value[1]
    
    lag_4_value = lag(value, 4)
    lag_5_value = lag(value, 5)
    lag_6_value = lag(value, 6)
    
    cum_lag_4_diff_value = lag(cum_diff_value, 4)
    cum_lag_5_diff_value = lag(cum_diff_value, 5)
    cum_lag_6_diff_value = lag(cum_diff_value, 6)
    
    if (length(value) < 2) {
      value_avg_3 = as.numeric(NA)
      diff_value_avg_3 = as.numeric(NA)
    } else {
      value_avg_3 = c(rep(NA, 2), roll_mean(value, 3))
      diff_value_avg_3 = c(rep(NA, 2), roll_mean(diff_value, 3))  
    }
    
    list(
      time = as.integer(date - min(date)),
      date = date,
      value = value,
      lag_value = lag_value, 
      diff_value = diff_value, 
      pct_change_value = pct_change_value,
      value_avg_3 = value_avg_3, 
      diff_value_avg_3 = diff_value_avg_3, 
      lag_4_value = lag_4_value, 
      lag_5_value = lag_5_value,
      lag_6_value = lag_6_value, 
      cum_diff_value = cum_diff_value, 
      cum_lag_4_diff_value = cum_lag_4_diff_value, 
      cum_lag_5_diff_value = cum_lag_5_diff_value, 
      cum_lag_6_diff_value = cum_lag_6_diff_value, 
      first_value = date[date == min(date)],
      last_value = date[date == max(date)],
      value_past_100 = min(date[value >= 100])
    )
    
  }, by = list(location_key, measure)] %>%
  pivot_wider(
    id_cols = c('location_key', 'date'),
    names_from = 'measure', 
    values_from = c('value', 'lag_value', 'diff_value', 
                    'pct_change_value', 'lag_4_value', 'lag_5_value',
                    'lag_6_value', 'value_avg_3', 'diff_value_avg_3', 
                    'cum_lag_4_diff_value', 'cum_lag_5_diff_value', 
                    'cum_lag_6_diff_value', 'cum_diff_value')
  ) 

id_cols = dplyr::select(all_covid_data_stacked, location_key, 
                        location, location_type, fips, state, country, data_source, latitude, longitude) %>% unique()

# add on population # 
all_covid_data_diffs_clean = 
  all_covid_data_diffs %>%
  left_join(
    id_cols
  ) %>%
  left_join(
    select(population_by_state, state_name, state_pop = value), by = c('state' = 'state_name')
  ) %>%
  left_join(
    latest_country_pop %>% select(-year, country_pop = population), 
    by = c('country')
  ) %>%
  left_join(
    county_pops_sub
  ) %>%
  mutate(
    state_pop = state_pop * 1000,
    population = coalesce(county_pop, state_pop, country_pop),
    pop_100k = population / 100e5
  )


### get timing of when the 20th case occurred ### 
case_dates = group_by(all_covid_data_diffs_clean, location_key) %>%
  summarize(
    date_case_20 = min(date[value_total_cases >= 20]),
    date_case_100 = min(date[value_total_cases >= 100])
  )

covid_data_diffs_excl_county = filter(all_covid_data_diffs_clean, location_type != 'County') %>% data.table()

### one more set of by-state computations, to get r0 and other stats ###

effective_r0_dat = covid_data_diffs_excl_county[, {
  # ny = filter(covid_data_diffs_excl_county, location == 'NY')
# attach(ny)
  
  # the_dat = filter(all_covid_data_diffs_dt, location_key == 'United States|Alaska|County|Nome Census Area|NYT')

  cat("\nkey ==",.BY[[1]],"\n\n")
  # cat("\nmeasure ==",.BY[[2]],"\n\n")
  
  cum_diff_value_total_cases[value_total_cases == 0] = NA
  
  # what is the r0 of cases on a rolling 6 day basis? This uses the last six days, computes r0, and then pushes the computations
  # forward six days to show the r0 of the cases themselves
  new_cases_zoo = zoo(cum_diff_value_total_cases, 1:length(cum_diff_value_total_cases))
  r0_rolling = rep(NA, length(new_cases_zoo)) %>% as.numeric()
  if (length(r0_rolling)>= r0_window_size) {
    tryCatch({
      r0_rolling = c(rep(NA, r0_window_size-1), rollapply(new_cases_zoo %>% na.approx(new_cases_zoo, na.rm = F), r0_window_size, est_r0_window)) %>% 
        lead(r0_window_size) %>% as.numeric()  
    }, error = function(e){
      print( e)
      
    })
    
  }
  
  # Median is 5.1 days, mean is 6.4 days
  # https://annals.org/aim/fullarticle/2762808/incubation-period-coronavirus-disease-2019-covid-19-from-publicly-reported
  # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7014672/
  effective_r0 = cum_diff_value_total_cases / cum_lag_6_diff_value_total_cases
  # 
  # # there are NAs because of the lags, there are infinite values because of missing data
  effective_r0_nas = ifelse(is.infinite(effective_r0) | effective_r0 == 0, NA, effective_r0) %>% as.numeric()
  # 
  # # interpolate the cases
  effective_r0_interpolated = zoo(effective_r0_nas, 1:length(effective_r0_nas)) %>% na.approx(na.rm = F) %>% as.numeric()
  # 
  # # there are data integrity issues. Limit max r0 to 20 and min to 0
  effective_r0_interpolated = pmin(effective_r0_interpolated, 20)
  effective_r0_interpolated = pmax(effective_r0_interpolated, 0)
  
  # this needs to be pushed back to where the cases originated
  effective_r0_interpolated_lead = lead(effective_r0_interpolated, 6)
  
  # get rolling average positive tests for last seven days
  percent_positive_new_tests = cum_diff_value_total_cases / cum_diff_value_tests_with_results
  
  
  if (length(cum_diff_value_tests_with_results) < 6) {
    rolling3_tests_with_results = rep(NA, length(cum_diff_value_tests_with_results)) %>% as.numeric()
    rolling7_tests_with_results = rep(NA, length(cum_diff_value_tests_with_results)) %>% as.numeric()
    rolling3_new_cases = rep(NA, length(cum_diff_value_tests_with_results)) %>% as.numeric()
    
    rolling3_percent_positive_new_tests = rep(NA, length(cum_diff_value_tests_with_results)) %>% as.numeric()
    rolling7_percent_positive_new_tests = rep(NA, length(cum_diff_value_tests_with_results)) %>% as.numeric()
    delta_roll_3_7 = rolling3_percent_positive_new_tests - rolling7_percent_positive_new_tests
    r0_rolling_lead_7 = rep(NA, length(cum_diff_value_tests_with_results)) %>% as.numeric()
    effective_r0_interpolated_lead_7 = rep(NA, length(cum_diff_value_tests_with_results)) %>% as.numeric()
  } else {
    rolling3_tests_with_results = c(rep(NA, 2), roll_mean(cum_diff_value_tests_with_results, 3))
    rolling7_tests_with_results = c(rep(NA, 6), roll_mean(cum_diff_value_tests_with_results, 7))
    rolling3_new_cases = c(rep(NA, 2), roll_mean(cum_diff_value_total_cases, 3))
    
    rolling3_percent_positive_new_tests = c(rep(NA, 2), roll_mean(percent_positive_new_tests, 3))
    rolling7_percent_positive_new_tests = c(rep(NA, 6), roll_mean(percent_positive_new_tests, 7))
    delta_roll_3_7 = rolling3_percent_positive_new_tests - rolling7_percent_positive_new_tests
    r0_rolling_lead_7 = lead(r0_rolling, 7)
    effective_r0_interpolated_lead_7 = lead(effective_r0_interpolated, 7)
  }
  
  lag_percent_positive_new_tests = lag(percent_positive_new_tests, 1)
  delta_percent_positive_new_tests = percent_positive_new_tests - lag_percent_positive_new_tests
  
  # limit R0 to 30 max and 0 min
  r0_rolling_lead_7 = pmin(r0_rolling_lead_7, 30)
  r0_rolling_lead_7 = pmax(r0_rolling_lead_7, 0)
  
  list(
    date = date,
    effective_r0_interpolated = effective_r0_interpolated_lead,
    effective_r0_interpolated_lead_7 = effective_r0_interpolated_lead_7,
    r0_rolling = r0_rolling,
    r0_rolling_lead_7 = r0_rolling_lead_7, 
    percent_positive_new_tests = percent_positive_new_tests,
    lag_percent_positive_new_tests = lag_percent_positive_new_tests, 
    delta_percent_positive_new_tests = delta_percent_positive_new_tests,
    rolling3_new_cases = rolling3_new_cases,
    rolling3_tests_with_results = rolling3_tests_with_results, 
    rolling7_tests_with_results = rolling7_tests_with_results,
    rolling3_percent_positive_new_tests = rolling3_percent_positive_new_tests,
    rolling7_percent_positive_new_tests = rolling7_percent_positive_new_tests,
    delta_roll_3_7_percent_positive_new_tests = delta_roll_3_7
  )
}, by = list(location_key, location)] 

covid_cases_mobility_stringency = fread('data/covid_cases_mobility_stringency.csv') %>%
  mutate(
    date = as.Date(date),
    country = country_name.x,
    country = recode(country, `Korea, South` = "South Korea"),
    state = as.character(NA),
    location = entity_name,
    location_type = ifelse(country == location, 'country', 'County')
  ) %>% 
  select(
    date, country, location, location_type, StringencyIndex, mobility, predicted_walking_baseline,
    percent_of_predicted, smoothed_percent_of_predicted, fips
  )


fips_by_area = group_by(covid_cases_mobility_stringency, location) %>%
  summarize(
    fips = fips[!is.na(fips)][1] %>% as.character()
  )

covid_cases_mobility_stringency_fin = left_join(covid_cases_mobility_stringency %>% select(-fips), fips_by_area)


# final, clean dataset with all sorts of calculations complete #
all_covid_data_diffs_dates = left_join(all_covid_data_diffs_clean, case_dates) %>%
  left_join(effective_r0_dat) %>%
  mutate(
    location = recode(location, `Korea, South` = "South Korea"),
    country = recode(country, `Korea, South` = "South Korea"),
    location = ifelse(location_type == 'US State', state, location)
  ) %>%
  full_join(covid_cases_mobility_stringency_fin, by = c('date', 'location', 'location_type', 'country')) %>%
  mutate(location_type = recode(location_type, `country` = 'Country')) %>%
  left_join(lockdown_dates_fin %>% select(location, country, lockdown_start, lockdown_end, location_type),
            by = c('location', 'location_type', 'country')) %>%
  mutate(
    days_since_lockdown_start = as.numeric(date - lockdown_start),
    lockdown_period = ifelse(is.na(lockdown_start), 'No Lockdown', ifelse(days_since_lockdown_start < 0, 'Pre-Lockdown', 'Post-Lockdown')) %>%
      factor() %>% relevel(ref = 'Pre-Lockdown'),
    days_since_case_20 = as.numeric(date - date_case_20),
    days_since_case_100 = as.numeric(date - date_case_100),
    new_tests_per_100k = cum_diff_value_total_tests / pop_100k,
    tests_per_100k = value_total_tests / pop_100k,
    cases_per_100k = value_total_cases / pop_100k,
    new_cases_per_100k = cum_diff_value_total_cases / pop_100k,
    deaths_per_100k = value_total_deaths / pop_100k,
    diff_value_avg_3_total_tests_per_100k = diff_value_avg_3_total_tests / pop_100k,
    week_day = lubridate::wday(date),
    weekend_ind = ifelse(week_day %in% c(7, 1), 'Weekend', "Week Day"),
  ) %>%
  arrange(location_key, date) %>%
  select(-fips.y) %>% 
  rename(
    fips = fips.x
  )

filter(all_covid_data_diffs_dates, country == 'South Korea') %>% select(location, country, StringencyIndex, date, contains('mobility'), location_type)
write.csv(all_covid_data_diffs_dates, 'data/countries_states_county_covid_calcs.csv', row.names = F)

