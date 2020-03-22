# https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
# https://covidtracking.com/api/
# https://covid.ourworldindata.org

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
library(WDI)
library(plotly)
library(USAboundaries)
library(sf)
library(fuzzyjoin)
library(tigris)

# install.packages('tigris')

# install.packages(c('tidyverse', 'data.table', 'rnaturalearth', 'rgeos', 
#                    'WDI', 'lmtest', 'gifski', 'gganimate', 'viridisLite'))
# install.packages(c('plotly', 'rnaturalearthdata', 'USAboundaries', 'countrycode', 'quantmod'))
# install.packages("remotes")
# remotes::install_github("ropensci/USAboundariesData")

us_cities_shp = us_cities() %>%
  mutate(
    province_state_city = paste0(city, ', ', state_abbr)
  )

us_counties_shp = us_counties() %>%
  mutate(
    province_state_city = ifelse(state_abbr == 'LA', paste0(name, ' Parish, ', state_abbr), paste0(name, ' County, ', state_abbr))
  )
us_counties_shp$geometry[1]

county_shp = tigris::counties()


johns_hopkins_cases = read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv')
johns_hopkins_deaths = read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv')
johns_hopkins_recovered = read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv')

table(johns_hopkins_cases$`Province/State`)

cities_and_counties_and_states = c(state.name, us_cities_shp$province_state_city, us_counties_shp$province_state_city)
us_cities_provinces = filter(johns_hopkins_cases, `Country/Region` == 'US')

any(!us_cities_provinces %in% cities_and_counties_and_states)
setdiff(us_cities_provinces$`Province/State`, cities_and_counties_and_states)

stanislaus = filter(johns_hopkins_cases, str_detect(`Province/State`, 'Stanislaus'))
stan = filter(us_counties_shp, name == 'Stanislaus')
st_coordinates(stan)
?us_counties

# pull in COVID data, mostly from WHO
all_covid_data = read_csv('https://covid.ourworldindata.org/data/full_data.csv') %>%
  arrange(location, date) %>% 
  mutate_if(is.numeric, function(x) ifelse(is.na(x), 0, x)) %>%
  filter(location != 'World') %>%
  mutate(
    location_type = 'country', 
    data_source = 'WHO-ourworldindata',
    location_key = paste(location, location_type, data_source, sep = '|')
  )
  
filter(all_covid_data, location == 'Italy') %>% tail(1)

us_covid_data = read_csv('https://covidtracking.com/api/us/daily.csv') %>%
  mutate(
    date = as.Date(as.character(date), format = '%Y%m%d'),
    location = 'United States',
    location_type = 'country', 
    data_source = 'covidtracking.com',
    location_key = paste(location, location_type, data_source, sep = '|')
  ) %>%
  rename(
    total_cases = positive,
    total_deaths = death,
    total_tests = total
  ) %>%
  mutate(
    new_cases = total_cases - lag(total_cases, 1),
    new_deaths = total_deaths - lag(total_deaths, 1)
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

all_covid_data_stacked = bind_rows(all_covid_data, us_covid_data, us_states_covid_data) %>%
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
  filter(date >= date_case_100) %>%
  mutate(
    days_since_case_100 = as.numeric(date - date_case_100)
  )

daily_growth_stats = group_by(all_covid_data_diffs_dates, days_since_case_100) %>%
  summarize(
    n_countries = n_distinct(location),
    median_daily_change_cases = median(pct_change_value_total_cases, na.rm = T),
    mean_daily_change_cases = mean(pct_change_value_total_cases, na.rm = T),
    mean_daily_change_deaths = mean(pct_change_value_total_deaths, na.rm = T),
    median_daily_change_deaths = median(pct_change_value_total_deaths, na.rm = T)
  )

table(all_covid_data_diffs_dates$location)
ggplot(all_covid_data_diffs_dates %>% filter(location_type == 'US State'), aes(days_since_case_100, pct_change_value_total_cases)) +
  geom_line(aes(alpha = days_since_case_100, group = location)) +
  geom_line(data = filter(all_covid_data_diffs_dates, location == 'United States', data_source == 'covidtracking.com'),
            colour = 'red', size = 1) +
  geom_line(data = filter(all_covid_data_diffs_dates, location == 'Italy'), colour = 'forestgreen', size = 1) +
  scale_y_continuous(limits = c(0, 1)) +
  # geom_line(data = daily_growth_stats %>% filter(n_countries >= 5), aes(x = days_since_case_100, y = median_daily_change_cases), size = 1, colour = 'blue')
  scale_alpha(range = c(0.1, 1)) 
  

ggplotly(a)

ggplot(all_covid_data_diffs_dates, aes(pct_change_value_total_cases)) +
  stat_density(position = 'identity')
summary(all_covid_data_diffs_dates$pct_change_value_total_cases)


# (7000/104)^(1/15) - 1

# are new cases just the first difference? mostly 
with(all_covid_data_diffs, summary(diff_value_total_cases - value_new_cases))


top_countries = 
  group_by(all_covid_data_diffs, location) %>%
  summarize(
    last_value_cases = tail(value_total_cases, 1)
  ) %>%
  arrange(-last_value_cases) %>% head(20)


all_covid_data_diffs_top = filter(all_covid_data_diffs_dates, location %in% top_countries$location)


a = ggplot(all_covid_data_diffs_top, aes(date, value_total_cases, colour = location)) + 
  geom_line(size = 1) +
  geom_point()

ggplotly(a)
# estimate crude r0 
crude_r0 = lm(value_total_cases ~ lag_5_value_total_cases, data = all_covid_data_diffs)
summary(crude_r0)

names(all_covid_data_diffs)
head(all_covid_data_diffs)
names(all_covid_data_diffs)
ggplot(all_covid_data_diffs, aes())

acf(all_covid_data_diffs$value_new_cases)
acf(all_covid_data_diffs$value_new_deaths)

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




setdiff(world$geounit, latest_data$location)
setdiff(latest_data$location, world$geounit)


head(all_data)

#### get world bank data on each country #####
WDIsearch('population') %>% View()
wdi_indicators = c('SP.POP.65UP.TO.ZS', 'SP.URB.TOTL.IN.ZS', 
                   'SP.URB.MCTY.UR.ZS', 'SH.STA.ACCH.ZS', 
                   'SH.MED.NURS.ZS', 'SH.STA.DIAB.ZS', 
                   'SP.POP.65UP.TO.ZS', 'SP.POP.TOTL', 'SP.POP.LAND.ZS', 'SH.XPD.PCAP', 
                   'SH.MED.CMHW.P3', 'SH.XPD.OOPC.CH.ZS', 'SH.XPD.CHEX.GD.ZS')
wdi_descriptions = map(wdi_indicators, function(x){
  WDIsearch(string = x, field = 'indicator', short = F) %>% 
    t() %>%
    as.data.frame() %>% 
    mutate(
      orig_indicator = x
    )
}) %>%
  bind_rows() %>%
  filter(
    indicator == orig_indicator
  )

WDI_data_long = map(wdi_indicators, function(x){
  tryCatch({
    download = WDI(indicator = x, start = 1965, end = 2020, extra = T) %>% 
      mutate(indicator = x)
    names(download)[names(download) == x] = 'value'  
    return(download)
  }, error = function(e){
    print(e)
    cat('error with ', x, '\n')
    return(NULL)
  })

})

wdi_data_stacked = bind_rows(WDI_data_long) %>% 
  left_join(wdi_descriptions) %>% select(-matches('V[0-9]'))


#### map everything ####

world <- ne_countries(scale = "medium", returnclass = "sf")
ggplot(data = world) +
  geom_sf()
