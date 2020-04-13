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


# questions to answer: 
# how have case / death rates changed recently?
# how have transmission rates changes over time?
# what do we know about mortality rates? 
# how many cases are missing?
# what is the geographic distribution of cases, and how does proximity affect transmission? 


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


population_by_state = filter(state_economic_data, title_clean == 'Resident Population')
population_by_state = filter(population_by_state, date == max(date))
us_population = sum(population_by_state$value)

## get shapefile data ## 
us_counties_shp = us_counties()
us_states_shp = us_states()
us_map = USAboundaries::us_boundaries()
us_states_tigris = tigris::states()
us_counties_tigris = tigris::counties()


# remotes::install_git("https://git.sr.ht/~hrbrmstr/albersusa")
# https://github.com/hrbrmstr/albersusa
library(albersusa)
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

### get lockdown dates ###
  us_lockdown_dates = read_csv('https://covid19-lockdown-tracker.netlify.com/lockdown_dates.csv') %>% 
  filter(Country == 'United States', Level == 'State') %>%
    select(-Country, -Confirmed, -Level) %>%
  rename(location_name = Place, lockdown_start = `Start date`, lockdown_end = `End date`)

names(us_lockdown_dates) = names(us_lockdown_dates) %>% str_replace_all(' ', '_')

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


# compute first differences, pct changes, etc. by state
all_covid_data_diffs = 
  all_covid_data_stacked[, {
    
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
    
    value_avg_3 = c(rep(NA, 2), roll_mean(value, 3))
    diff_value_avg_3 = c(rep(NA, 2), roll_mean(diff_value, 3))
    
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
    
  }, by = list(location_key, location, location_type, data_source, measure)] %>%
  pivot_wider(
    id_cols = c('location_key', 'location', 'location_type','data_source', 'date', 'time'),
    names_from = 'measure', 
    values_from = c('value', 'lag_value', 'diff_value', 
                    'pct_change_value', 'lag_4_value', 'lag_5_value',
                    'lag_6_value', 'value_avg_3', 'diff_value_avg_3', 
                    'cum_lag_4_diff_value', 'cum_lag_5_diff_value', 
                    'cum_lag_6_diff_value', 'cum_diff_value')
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

### get timing of when the 20th case occurred ### 
case_20_dates = group_by(all_covid_data_diffs, location_key) %>%
  summarize(
    date_case_20 = min(date[value_total_cases >= 20]),
    has_30_days = as.numeric(max(date[value_total_cases >= 20]) - min(date[value_total_cases >= 20]) >= 30)
  )

all_covid_data_diffs_dt = data.table(all_covid_data_diffs)



### one more set of by-state computations, to get r0 and other stats ###
effective_r0_dat = all_covid_data_diffs_dt[, {
  
  cum_diff_value_total_cases[value_total_cases == 0] = NA
  
  # what is the r0 of cases on a rolling 6 day basis? This uses the last six days, computes r0, and then pushes the computations
  # forward six days to show the r0 of the cases themselves
  new_cases_zoo = zoo(cum_diff_value_total_cases, 1:length(cum_diff_value_total_cases))
  r0_rolling = rep(NA, length(new_cases_zoo)) %>% as.numeric()
  
  tryCatch({
    r0_rolling = c(rep(NA, r0_window_size-1), rollapply(new_cases_zoo %>% na.approx(new_cases_zoo, na.rm = F), r0_window_size, est_r0_window)) %>% lead(window_size) %>% as.numeric()  
  }, error = function(e){
    print( e)
    
  })
  
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
  
  lag_percent_positive_new_tests = lag(percent_positive_new_tests, 1)
  delta_percent_positive_new_tests = percent_positive_new_tests - lag_percent_positive_new_tests
  
  rolling3_percent_positive_new_tests = c(rep(NA, 2), roll_mean(percent_positive_new_tests, 3))
  rolling7_percent_positive_new_tests = c(rep(NA, 6), roll_mean(percent_positive_new_tests, 7))
  delta_roll_3_7 = rolling3_percent_positive_new_tests - rolling7_percent_positive_new_tests
  
  # there is some thinking that there is a severe lag in case reporting, use a weeklong lag
  # the reason we use lead 
  r0_rolling_lead_7 = lead(r0_rolling, 7)
  effective_r0_interpolated_lead_7 = lead(effective_r0_interpolated, 7)
  
  list(
    date = date,
    effective_r0_interpolated = effective_r0_interpolated_lead,
    effective_r0_interpolated_lead_7 = effective_r0_interpolated_lead_7,
    r0_rolling = r0_rolling,
    r0_rolling_lead_7 = r0_rolling_lead_7, 
    percent_positive_new_tests = percent_positive_new_tests,
    lag_percent_positive_new_tests = lag_percent_positive_new_tests, 
    delta_percent_positive_new_tests = delta_percent_positive_new_tests,
    rolling3_percent_positive_new_tests = rolling3_percent_positive_new_tests,
    rolling7_percent_positive_new_tests = rolling7_percent_positive_new_tests,
    delta_roll_3_7_percent_positive_new_tests = delta_roll_3_7
  )
}, by = list(location_key, location)] 


# what is the r0 of cases on a rolling 6 day basis? This uses the last six days, computes r0, and then pushes the computations
# forward six days to show the r0 of the cases themselves


# final, clean dataset with all sorts of calculations complete #
all_covid_data_diffs_dates = left_join(all_covid_data_diffs, case_20_dates) %>%
  left_join(effective_r0_dat) %>%
  left_join(us_lockdown_dates) %>%
  mutate(
    days_since_lockdown_start = as.numeric(date - lockdown_start),
    lockdown_period = ifelse(is.na(lockdown_start), 'No Lockdown', ifelse(days_since_lockdown_start < 0, 'Pre-Lockdown', 'Post-Lockdown')) %>%
      factor() %>% relevel(ref = 'Pre-Lockdown'),
    days_since_case_20 = as.numeric(date - date_case_20),
    new_tests_per_100k = cum_diff_value_total_tests / pop_100k,
    tests_per_100k = value_total_tests / pop_100k,
    cases_per_100k = value_total_cases / pop_100k,
    new_cases_per_100k = cum_diff_value_total_cases / pop_100k,
    deaths_per_100k = value_total_deaths / pop_100k,
    diff_value_avg_3_total_tests_per_100k = diff_value_avg_3_total_tests / pop_100k,
    week_day = lubridate::wday(date),
    weekend_ind = ifelse(week_day %in% c(7, 1), 'Weekend', "Week Day"),
    cum_diff_value_total_cases_adj = pmax(cum_diff_value_total_cases, 0),
    cum_diff_value_tests_with_results_adj = pmax(cum_diff_value_total_cases_adj, cum_diff_value_tests_with_results, 0),
    cum_diff_value_tests_with_results_adj = ifelse(cum_diff_value_tests_with_results_adj == 0, NA, cum_diff_value_tests_with_results_adj)
  ) %>%
  arrange(location_key, date) %>%
  filter(
    location %in% c(state.abb, 'United States')
  )

write.csv(all_covid_data_diffs_dates, 'data/us_covid_data_by_state_with_calcs.csv', row.names = F)


latest_state_data = filter(all_covid_data_diffs_dates, location != 'United States', date == max(date)) %>% 
  arrange(-value_total_cases) %>%
  mutate(
    location_factor = factor(location, levels = location)
  )
select(latest_state_data, location, value_total_cases)
all_covid_data_diffs_dates$location_factor = factor(all_covid_data_diffs_dates$location, 
                                                    levels = latest_state_data$location)

filter(all_covid_data_diffs_dates, 
       location %in% head(latest_state_data, 50)$location, date >= as.Date('2020-03-11')) %>%
  ggplot() +
  facet_wrap(~location_factor, scales = 'free_y', ncol = 5) +
  geom_area(aes(date, rolling7_percent_positive_new_tests), fill = 'black', alpha = .4) +
  geom_line(aes(date, rolling3_percent_positive_new_tests), colour = 'blue', size = 0.75) +
  # geom_point(aes(date, rolling3_percent_positive_new_tests), colour = 'blue') +
  # geom_point(aes(date, rolling3_percent_positive_new_tests, size = new_cases_per_100k), colour = 'red') +
  scale_size(range = c(1, 5)) +
  theme_minimal()
ggsave('output/three_vs_seven_day_avg_positive_tests.png', height = 8, width = 10, units = 'in', dpi = 800)  


# get last data by state
# latest_state_data = 
#   all_covid_data_diffs_dates %>%
#   filter(location != 'United States') %>%
#   group_by(location_key, location, location_name) %>%
#   summarize(
#     last_cases = tail(value_total_cases, 1),
#     last_cases_100k = tail(cases_per_100k, 1),
#     last_deaths_100k = tail(deaths_per_100k, 1),
#     new_cases = tail(cum_diff_value_total_cases, 1),
#     last_date = max(date)
#   ) %>%
#   arrange(-last_cases) %>%
#   ungroup() 


## compute stats by lockdown period
state_lockdown_period_calcs = group_by(all_covid_data_diffs_dates, location, lockdown_period) %>%
  summarize(
    period_start = min(date[value_total_cases > 0]),
    period_end = max(date[value_total_cases > 0]),
    period_length = as.numeric(period_end - period_start),
    starting_cases = min(value_total_cases[value_total_cases > 0]),
    ending_cases = max(value_total_cases[value_total_cases > 0]),
    period_r0 = est_r0_window(cum_diff_value_total_cases, catch = T),
    period_daily_geometric_growth = (ending_cases / starting_cases)^(1/length(value_total_cases[value_total_cases > 0])) - 1
  ) %>%
  filter(location != 'United States', !is.na(lockdown_period), !is.na(ending_cases), location %in% state.abb)

# push wide
wide_state_lockdown_period_calcs = pivot_wider(state_lockdown_period_calcs, 
                                               id_cols = c('location'), 
                                               names_from = lockdown_period, 
                                               values_from = c('period_r0', 'period_end','period_daily_geometric_growth', 'starting_cases', 'ending_cases'))
names(wide_state_lockdown_period_calcs) = str_replace_all(names(wide_state_lockdown_period_calcs), '[ \\-]', '_')


post_ending_cases = filter(state_lockdown_period_calcs, lockdown_period != 'Pre-Lockdown') %>%
  arrange(-ending_cases) 

state_lockdown_period_calcs = state_lockdown_period_calcs %>%
  mutate(
    location_factor = factor(location, levels = rev(post_ending_cases$location))
  )


location_levels = filter(latest_state_data_sub, location %in% selected_locations) %>% pull(location_name)
location_sub = filter(all_covid_data_diffs_dates, location %in% selected_locations) %>%
  mutate(
    location_name = factor(location_name, levels = location_levels)
  )


##### show relationship between geometric daily growth vs R0 ####
ggplot(state_lockdown_period_calcs, aes(period_daily_geometric_growth, period_r0)) +
  # geom_path(colour = 'white') +
  # theme_dark() +
  geom_point(aes(colour = lockdown_period, size = ending_cases, alpha = period_length)) +
  # theme_classic() +
  theme_bw() +
  theme(
    # plot.background = element_rect(fill= 'black'),
    # panel.background = element_rect(fill = 'black'),
    # panel.grid = element_line(colour = 'white')
    # panel.border = element_rect(colour='white')
    # axis.line = element_line(colour = 'white')
  ) +
  scale_y_continuous(breaks = seq(0, 13, by = 1)) +
  scale_alpha(guide = F) +
  scale_x_continuous(labels = percent, breaks = seq(0, 0.6, by = 0.1)) +
  labs(
    x = '\nDaily Average Growth', y = 'Reproduction Number (R0)\n',
    title = 'COVID-19 Daily Average Case Growth and R0, Pre vs. Post Lockdown',
    subtitle = sprintf('U.S. States, through %s', max(state_lockdown_period_calcs$period_end) %>% format('%B %d'))
  ) +
  scale_colour_hue(name = 'Period') +
  scale_size(name = 'Ending Cases', labels = comma)
  
ggsave('output/daily_average_case_growth_versus_r0.png', height = 6, width = 8, units = 'in', dpi = 800)
  

all_covid_data_diffs_dates %>% filter(location %in% c('NY', 'CA', 'WA', 'United States')) %>%
  ggplot(aes(date, percent_positive_new_tests_rolling_7, colour = location)) +
  geom_line() +
  geom_point(aes(size = cum_diff_value_tests_with_results))


##### plot effective r0 versus lockdown dates #####

selected_early_states = filter(all_covid_data_diffs_dates, location %in% c('CA', 'NY', 'WA'))
  


all_covid_data_diffs_dates %>% filter(location %in% c('NY', 'CA', 'WA'), !is.na(r0_rolling), days_since_case_20 >= 5) %>%
  ggplot(aes(date, r0_rolling)) +
  geom_hline(aes(yintercept = 0)) +
  geom_hline(aes(yintercept = 1), colour = 'red') +
  geom_hline(aes(yintercept = 2.28), linetype = 'dashed') +
  facet_wrap(~location_name) +
  scale_size(range = c(0.5, 4)) +
  # scale_alpha(range = c(0.5, 1)) +
  geom_line(colour = 'blue') +
  geom_point(aes(size = cum_diff_value_total_cases,  colour = weekend_ind)) +
  
  
  labs(y = 'Seven-Day R0 of New Cases', x = '') +
  # stat_smooth(method = 'gam') +
  geom_vline(data = latest_state_data %>% filter(location %in% c('NY', 'CA', 'WA')), aes(xintercept = lockdown_start)) 
ggsave('output/rolling_ro_by_early_states.png', height = 4, width = 9, units = 'in', dpi = 800)





main_plot = 
filter(location_sub, days_since_case_20 >= 5) %>%
  ggplot() +
  geom_hline(aes(yintercept = 0)) +
  geom_hline(aes(yintercept = 1), colour = 'red') +
  geom_hline(aes(yintercept = 2.28), linetype = 'dashed') +
  geom_vline(data = us_lockdown_dates %>% filter(location_name %in% unique(location_sub$location_name)), aes(xintercept = lockdown_start)) +
  geom_point(aes(date, effective_r0_interpolated, colour = weekend_ind)) +
  stat_smooth(aes(date, effective_r0_interpolated), se = F, method = 'gam') +
  scale_colour_manual(name = '', values = c('Week Day' = 'steelblue', 'Weekend' = 'orange')) +
  theme_bw() +
  theme(
    title = element_text(size = 14),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    plot.caption = element_text(size = 10, hjust = 0, face = 'italic'),
    plot.subtitle = element_text(size = 11, face = 'italic'),
    axis.text.x = element_text(angle = 45),
    plot.margin = unit(c(0.25, 0.5, 0.25, 0.25), 'in'),
    legend.position = 'bottom',
    strip.text = element_text(face = 'bold')
  ) +
  labs(
    x = NULL, y = 'Daily Effective COVID-19 Reproduction Number (r0)\n(New Cases / New Cases Generated 5 Days Later)\n', 
    title = 'Lockdown Effects: COVID-19 Daily Effective R0 by State', 
    subtitle = sprintf('Top 12 U.S. states by cases, through %s. Vertical lines show state lockdown dates. Dashed line shows average COVID-19 r0 and red line shows r0=1, below which is decreasing case growth.\nNote that each state has its own scale.', max(location_sub$date) %>% format('%B %d')),
    caption = 'Chart: Taylor G. White\nData: covidtracking.com, auravision.ai, CNN'
  ) 

free_plot = main_plot +
  facet_wrap(~factor(location_name, levels = location_levels), ncol = 4, scales = 'free_y') 
# ggsave('output/effective_r0_top_12_states_free.png', height= 11, width = 14.5, units = 'in', dpi = 800, plot = free_plot)

same_scale_plot = main_plot +
  facet_wrap(~factor(location_name, levels = location_levels), ncol = 4)
# ggsave('output/effective_r0_top_12_states.png', height= 11, width = 14.5, units = 'in', dpi = 800, plot = same_scale_plot)

# select(all_covid_data_diffs_dates, has_30_days, date, location_name, diff_value_total_tests, value_total_tests, new_tests_per_100k) %>% View()

##### plot new cases per 100k vs percent positive new cases #####

all_covid_data_diffs_dates %>%
  filter(days_since_case_20 >= 0, location %in% c('NY', 'LA', 'CA', 'MI')) %>%
  ggplot(aes(days_since_case_20, new_cases_per_100k)) +
  geom_point(aes(alpha = days_since_case_20, colour = location, size = new_tests_per_100k)) 
# coord_cartesian(xlim = c(0, 1)) 


##### plot tests per 100k #####

all_covid_data_diffs_dates %>%
  filter(days_since_case_20 >= 0, location == 'United States') %>%
  ggplot(aes(days_since_case_20, new_tests_per_100k)) +
  theme_bw() +
  geom_bar(stat = 'identity') +
  geom_line(aes(y = diff_value_avg_3_total_tests_per_100k), colour = 'red', size = 1) +
  labs(
    x = '\nDays Since Case 20', 
    y = 'Daily Tests Per 100k Population\n',
    title = 'COVID-19 Tests Per 100k Population', 
    subtitle = sprintf('United States, through %s. Red line is the rolling 3-day average.', max(all_covid_data_diffs_dates$date) %>% format('%B %d')),
    caption = 'Chart: Taylor G. White\nData: covidtracking.com'
  ) +
  scale_y_continuous(breaks = seq(0, 500, by = 100)) +
  scale_x_continuous(breaks = seq(0, 30, by = 5)) +
  theme(
    title = element_text(size = 16),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(hjust = 0, face = 'italic', size = 10)
  )
ggsave('output/us_daily_covid_tests_per_day.png', height = 6, width = 8, units = 'in', dpi = 800)




##### plot states with earliest infections #####

all_covid_data_diffs_dates %>%
  filter(days_since_case_20 >= 0, has_30_days, location != 'United States') %>%
  ggplot(aes(days_since_case_20, cases_per_100k, colour = location_name)) +
  theme_bw() +
  geom_line(data = filter(all_covid_data_diffs_dates, days_since_case_20 >= 0, !has_30_days), aes(group = location_name), colour = 'black', alpha = 0.5) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    x = '\nDays Since Case 20', 
    y = 'Cases Per 100k Population\n',
    title = 'COVID-19 Cases Per 100k Population', 
    subtitle = sprintf('Selected U.S. states, through %s.', max(all_covid_data_diffs_dates$date) %>% format('%B %d')),
    caption = 'Chart: Taylor G. White\nData: covidtracking.com'
  ) +
  scale_y_continuous(breaks = seq(0, 6000, by = 1000), labels = comma) +
  scale_x_continuous(breaks = seq(0, 30, by = 5)) +
  theme(
    title = element_text(size = 16),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(hjust = 0, face = 'italic', size = 10)
  ) +
  scale_colour_hue(name = '')
ggsave('output/cases_per_100k_early_states.png', height = 6, width = 8, units = 'in', dpi = 800)


# check to make sure the population data looks good
select(all_covid_data_diffs_dates, location, population) %>% 
  data.table() %>%
  unique(by = 'location') 


##### create map for all 50 states #####
US_state_data = left_join(us_sf, latest_state_data, by = c('iso_3166_2' = 'location')) %>% left_join(state_geo_center, by = c('iso_3166_2' = 'state_abbr'))

ggplot() +
  geom_sf(data = US_state_data, aes(fill = log(cases_per_100k)), alpha = 0.75, size = 0.25) +
  scale_fill_viridis(guide = F, option = 'C') +
  geom_sf_text(data = US_state_data, aes(long, lat, label = comma(cases_per_100k, accuracy = 1)),
               colour = 'black', fontface='bold', size = 2) +
  theme_map() +
  labs(x = '', y = '',
       caption = 'Chart: Taylor G. White\nData: covidtracking.com',
       title = 'U.S. COVID-19 Cases by State, Per 100k Population', subtitle = sprintf('As of %s',
                                                                                       unique(format(latest_state_data$date, '%B %d')))) +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    title = element_text(size = 16),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(hjust = 0, face = 'italic', size = 10)
  )

ggsave('output/latest_cv_state_map_50.png', height = 6, width = 8, units = 'in', dpi = 800)

# show changes in test results
US_state_data$rolling7_percent_positive_new_tests
ggplot() +
  geom_sf(data = US_state_data, aes(fill = delta_roll_3_7_percent_positive_new_tests/rolling7_percent_positive_new_tests), alpha = 0.75, size = 0.25) +
  # scale_fill_viridis(guide = F, option = 'D') +
  scale_fill_gradient2(guide = F, low='blue', high = 'red', mid = 'purple4', midpoint = 0) +
  geom_sf_text(data = US_state_data, aes(long, lat, label = percent(delta_roll_3_7_percent_positive_new_tests/rolling7_percent_positive_new_tests, accuracy = 1)),
               colour = 'black', fontface='bold', size = 2) +
  theme_map() +
  labs(x = '', y = '',
       caption = 'Chart: Taylor G. White\nData: covidtracking.com',
       title = 'U.S. COVID-19 Cases by State, Per 100k Population', subtitle = sprintf('As of %s',
                                                                                       unique(format(latest_state_data$date, '%B %d')))) +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    title = element_text(size = 16),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(hjust = 0, face = 'italic', size = 10)
  )

# ggsave('output/latest_cv_state_map_50.png', height = 6, width = 8, units = 'in', dpi = 800)


# world <- ne_countries(scale = "medium", returnclass = "sf")
# usa <- subset(world, admin == "United States of America")

##### take a look at testing data, normed by population information #####

selected_locations = c(
  'United States',
  'WA', 'NY', 'CA', 'LA', 'NJ', 'MI', 'FL', 'MA', 'PA', 'IL', 'GA'
  # , 'FL', 'CO', 
  # 'MI', 'IL',' 'LA', 'NJ', 'TX', 'GA'
)


cfr_path_dat = filter(all_covid_data_diffs_dates, location %in%  selected_locations, days_since_case_20 >= 15)
last_cfr_path_dat = filter(cfr_path_dat, date == max(date))



#### plot overall cfr and positive tests ####
latest_state_data$cases_per_100k

ggplot(latest_state_data, aes(value_percent_positive_cases, value_case_fatality_rate, size = cases_per_100k)) +
  theme_bw() +
  geom_text(aes(label = location), show.legend = F) +
  scale_alpha(range = c(0.2, 1)) +
  scale_size(guide = F, range = c(2, 6)) +
  geom_hline(aes(yintercept = 0.01)) +
  scale_y_continuous(breaks = seq(0, 0.1, by = 0.01), limits = c(0, 0.05), labels = percent) +
  scale_x_continuous(labels = percent, breaks = seq(0, 0.5, by = 0.05)) +
  labs(
    x = 'Percent Positive Tests', y = 'Case Fatality Rate',
    title = 'COVID-19 Case Fatality Rates vs. Percent Positive Tests',
    subtitle = sprintf('U.S. states, through %s. Latest data shown for each state.', max(all_covid_data_diffs_dates$date) %>% format('%B %d')),
    caption = 'Chart: Taylor G. White\nData: covidtracking.com, FRED'
  ) +
  theme(
    title = element_text(size = 14),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    plot.caption = element_text(size = 10, hjust = 0, face = 'italic'),
    plot.subtitle = element_text(size = 11, face = 'italic')
  ) +
  # stat_smooth(method = 'lm', formula = y ~ poly(x, 4), show.legend = F, se = F) +
  stat_smooth(method = 'gam', se=F)
ggsave('output/all_states_positive_cases_vs_case_fatality_rate.png', height = 6, width = 8, units = 'in', dpi = 800)


#### plot prevalence ####
ggplot(all_latest_cases, aes(value_percent_positive_cases, cases_per_100k, size = value_case_fatality_rate)) +
  theme_bw() +
  geom_point(aes(alpha = value_case_fatality_rate)) +
  # geom_text(aes(label = location), show.legend = F) +
  scale_alpha(name = 'Case Fatality Rate', range = c(0.2, 1)) +
  scale_size(name = 'Case Fatality Rate', range = c(0, 5)) +
  geom_hline(aes(yintercept = 0.01)) +
  # scale_y_continuous(breaks = seq(0, 0.1, by = 0.01), limits = c(0, 0.05), labels = percent) +
  scale_x_continuous(labels = percent, breaks = seq(0, 0.5, by = 0.05)) +
  labs(
    x = 'Percent Positive Tests', y = 'Cases Per 100k Population',
    title = 'COVID-19 Case Fatality Rates vs. Percent Positive Tests',
    subtitle = sprintf('U.S. states, through %s. Latest data shown for each state.', max(all_covid_data_diffs_dates$date) %>% format('%B %d')),
    caption = 'Chart: Taylor G. White\nData: covidtracking.com, FRED'
  ) +
  theme(
    title = element_text(size = 14),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    plot.caption = element_text(size = 10, hjust = 0, face = 'italic'),
    plot.subtitle = element_text(size = 11, face = 'italic')
  ) +
stat_smooth(method = 'gam', se = F, show.legend  = F)
ggsave('output/all_states_positive_cases_vs_cases_per_100k.png', height = 6, width = 8, units = 'in', dpi = 800)


#### plot paths of selected states' CFR versus positive tests ####
cfr_path_dat %>%
  ggplot(aes(value_percent_positive_cases, value_case_fatality_rate, colour = location)) +
  scale_size(guide = F, range = c(0, 3)) +
  scale_alpha(guide = F, range = c(0.3, 0.8)) +
  geom_point(aes(alpha = days_since_case_20, size = log(value_total_cases))) +
  geom_path(aes(alpha = days_since_case_20), size = 0.75) +
  geom_text_repel(data = last_cfr_path_dat, aes(label = location)) +
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
    
    legend.title = element_text(size = 14),
    plot.caption = element_text(size = 10, hjust = 0, face = 'italic'),
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
    x = '\nPercent Positive Tests', y = 'Case Fatality Rate\n', 
    title = 'Paths of Case Fatality Rates and Positive Test Rates',
    subtitle = sprintf('U.S. and selected states, through %s. Data shown from 15 days following 20th case.', max(all_covid_data_diffs_dates$date) %>% format('%B %d')),
    caption = 'Chart: Taylor G. White\nData: covidtracking.com'
  )
ggsave('output/positive_cases_vs_case_fatality_rate.png', height = 8, width = 8, units = 'in', dpi = 800)


dark_theme = theme(
  strip.background = element_rect(fill = 'darkgray'),
  strip.text = element_text(face = 'bold'),
  text = element_text(colour = 'white'),
  axis.text = element_text(colour = 'white'),
  title = element_text(size = 16, colour = 'white'),
  axis.title = element_text(size = 14),
  legend.text = element_text(size = 12),
  plot.caption = element_text(size = 10, hjust = 0, face = 'italic'),
  legend.title = element_text(size = 14),
  plot.subtitle = element_text(size = 11, face = 'italic'),
  plot.background = element_rect(fill = 'black'),
  legend.background = element_rect(fill = 'black'),
  panel.background = element_rect(fill = 'black'),
  panel.grid.minor  = element_blank(),
  panel.grid.major = element_line(size = 0.25),
  panel.grid.major.x = element_blank(),
  legend.position = 'bottom', 
  axis.ticks = element_line(colour = 'white')
)


filter(all_covid_data_diffs_dates, days_since_case_20 >= 0, location_type == 'US State') %>%
  ggplot( aes(days_since_case_20, value_case_fatality_rate)) +
  theme_bw() +
  geom_hline(aes(yintercept = 0.01), colour = '#1b9e77', size = 0.75, linetype='dashed') +
  geom_line(aes(group = location, alpha = days_since_case_20),  colour = 'white') + 
  scale_alpha(guide = F, range = c(0.15, 0.5)) +
  scale_y_continuous(labels = percent, breaks = seq(0, 0.17, by = 0.02), limits = c(0, 0.17)) +
  scale_x_continuous(breaks = seq(5, 30, by = 5), limits = c(5, max(all_covid_data_diffs_dates$days_since_case_20))) +
  geom_line(data = filter(all_covid_data_diffs_dates, days_since_case_20 >= 0, location_type != 'US State'),  aes(colour = location), size = 1) +
  geom_point(data = filter(all_covid_data_diffs_dates, days_since_case_20 >= 0, location_type != 'US State'), aes(colour = location), size = 1) +
  labs(
    x = 'Days Since Case 20', y = 'Case Fatality Rate', 
    title = 'COVID-19 Case Fatality Rates Over Time',
    subtitle = sprintf('By U.S. state, through %s', max(all_covid_data_diffs_dates$date) %>% format('%B %d')), 
    caption = 'Chart: Taylor G. White\nData: covidtracking.com'
  ) +
  # theme(
  #   title = element_text(size = 16),
  #   axis.title = element_text(size = 14),
  #   legend.text = element_text(size = 12),
  #   plot.caption = element_text(size = 10, hjust = 0),
  #   legend.title = element_text(size = 14),
  #   plot.subtitle = element_text(size = 11, face = 'italic'),
  #   legend.position = 'bottom'
  # ) +
  dark_theme +
  scale_colour_manual(name = '', labels = c('United States' = 'U.S. Overall'), values = c('#d95f02'))
ggsave('output/case_fatality_rate_by_state.png', height = 7, width = 7, units = 'in', dpi = 800)

#### estimate effects of reduced transmission on case load ####

# ny_data = all_covid_data_diffs_dates %>% filter(location %in% c('CA')) 

# https://stats.stackexchange.com/questions/160552/why-is-nls-giving-me-singular-gradient-matrix-at-initial-parameter-estimates
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6962332/
# https://www.medrxiv.org/content/10.1101/2020.04.02.20051466v1.full.pdf
# get_r0_nls = function(df) {
#   model.0 = lm(log(value_total_cases) ~ I(time/6.4), data = df)
#   simple_exponential_model = nls(value_total_cases ~  case_networks * r0^(time/6.4), data = df, 
#                                  start = list(case_networks = exp(coef(model.0)[1]), r0 = coef(model.0)[2]))  
#   return(simple_exponential_model)
# }
# 
# pre_lockdown = get_r0_nls(ny_data)
# # summary(pre_lockdown)
# # est_r0_window(ny_data$cum_diff_value_total_cases)
# 
# ny_data$pred = predict(pre_lockdown)
# ggplot(ny_data, aes(time)) +
#   geom_line(aes(y=pred)) +
#   geom_line(aes(y = value_total_cases), colour = 'red')
# 


# Solve and plot.



# pars vector with 2 values: the transmission and recovery rates. The names of these
# values must be "beta", and "gamma", respectively.
# init vector with 3 values: the initial proportion of susceptibles, infectious and recovered. The names of these values must be "S", "I" and "R", respectively.
# time time sequence for which output is wanted; the first value of times must be the
# initial time.
# ... further arguments passed to ode function.


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


