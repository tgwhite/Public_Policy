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
library(lmtest)
library(xgboost)
library(igraph)

out_location = "~/Public_Policy/Projects/COVID-19 Mismanagement/output"

large_text_theme = theme(
  plot.title = element_text(size = 24),
  plot.subtitle = element_text(size = 18, face = 'italic'),
  plot.caption = element_text(size = 13, face = 'italic', hjust = 0),
  axis.text = element_text(size = 16),
  axis.title = element_text(size = 18)
) 

#### use the imf projections for growth 

setwd("~/Public_Policy/Projects/COVID-19")
nordic_countries = c('Sweden', 'Finland', 'Norway', 'Denmark')
top_europe = c('Spain', 'United Kingdom', 'Italy', 'France', 'Germany', 'Belgium')

##### get map data #####
world <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  mutate(
    name = recode(name, 
                  `Dem. Rep. Korea` = 'South Korea', 
                  `Czech Rep.` = 'Czech Republic', 
                  `Slovakia` = 'Slovak Republic',
                  `Bosnia and Herz.` = 'Bosnia and Herzegovina',
                  `Macedonia` = 'North Macedonia')
  )


europe = filter(world, continent == 'Europe') 

europe_cropped <- st_crop(europe, xmin = -24, xmax = 45,
                          ymin = 30, ymax = 73)

political_freedom_index = read_csv('https://object.cato.org/sites/cato.org/files/human-freedom-index-files/human-freedom-index-2019.csv')

# a = st_intersects(europe_cropped, europe_cropped)
# sweden_intersects = a[which(europe_cropped$name == 'Sweden')] %>% unlist()
# 
# europe_cropped$name[sweden_intersects]


# countrycode(sourcevar = 'South Korea', destination = 'iso3c', origin = 'un.name.en')

##### Population data #####
wdi_indicators = c(
  'SP.POP.TOTL', # population
  'NE.TRD.GNFS.ZS', # trade / GDP 
  'SP.POP.65UP.TO.ZS', # age 65+ % of population
  'NY.GDP.PCAP.CD', # per capita gdp 
  'SI.POV.GINI', # gini index
  'SH.XPD.CHEX.GD.ZS', # health exp % gdp
  'ST.INT.ARVL', # tourist arrivals
  'SP.URB.TOTL.IN.ZS' # Urban population %
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

wdi_data_stacked = bind_rows(WDI_data_long) 
wdi_data_wide = pivot_wider(wdi_data_stacked, id_cols = c('country', 'year', 'income', 'region'), values_from = 'value', names_from = 'indicator') %>%
  arrange(country, year)


##### OECD Data trust in government #####
setwd("~/Public_Policy/Projects/COVID-19 Mismanagement/data/OECD")

trust_in_government = fread('DP_LIVE_30102020213545055.csv') %>%
  mutate(
    country = countrycode(LOCATION, origin = 'iso3c', destination = 'country.name'),
    trust_in_government_pct = Value / 100
  ) %>%
  rename(
    year = TIME
  ) %>%
  group_by(country) %>%
  summarize(
    mean_trust_in_gov = mean(trust_in_government_pct), 
    last_trust_in_gov = trust_in_government_pct[year == max(year)]
  ) %>%
  ungroup()
# summarize_if(trust_in_government, is.character, unique)


latest_country_pop = filter(wdi_data_wide) %>%
  group_by(country, income, region) %>%
  summarize(
    latest_pop_year = max(year[!is.na(SP.POP.TOTL)], na.rm = T),
    population = SP.POP.TOTL[year == latest_pop_year],
    gini_index = tail(SI.POV.GINI[!is.na(SI.POV.GINI)], 1),
    international_tourism = tail(ST.INT.ARVL[!is.na(ST.INT.ARVL)], 1),
    trade_pct_gdp = tail(NE.TRD.GNFS.ZS[!is.na(NE.TRD.GNFS.ZS)], 1) / 100,
    gdp_per_capita_us = tail(NY.GDP.PCAP.CD[!is.na(NY.GDP.PCAP.CD)], 1),
    pop_pct_65_over = tail(SP.POP.65UP.TO.ZS[!is.na(SP.POP.65UP.TO.ZS)], 1) / 100,
    health_exp_gdp = tail(SH.XPD.CHEX.GD.ZS[!is.na(SH.XPD.CHEX.GD.ZS)], 1) / 100,
    urban_pop_pct = tail(SP.URB.TOTL.IN.ZS[!is.na(SP.URB.TOTL.IN.ZS)], 1) / 100
  ) %>%
  rename(
    year = latest_pop_year
  ) %>%
  left_join(world, by = c('country'= 'name')) %>%
  left_join(trust_in_government) %>%
  mutate(
    country = recode(country, `Korea, Rep.` = 'South Korea', `Russian Federation` = 'Russia')
  )

##### stringency and mobility data #####



setwd("~/Public_Policy/Projects/COVID-19")

oxford_stringency_index = read.csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv") %>%
  rename(
    country = CountryName
  ) %>%
  mutate(
    entity_name = ifelse(is.na(RegionName) | RegionName == "", country, RegionName),
    stringency_geo_type = ifelse(entity_name == country, 'country', 'region'),
    date = as.Date(Date %>% as.character(), format = '%Y%m%d')
  ) 


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
  ) 

country_mobility_data = filter(apple_mobility_dat, geo_type == 'country/region', transportation_type == 'walking')

country_stringency = filter(oxford_stringency_index, stringency_geo_type == 'country') %>%
  left_join(country_mobility_data, by = c('country' = 'entity_name', 'date' = 'date')) %>%
  rename(
    mobility = value
  )


##### Covid data #####

johns_hopkins_cases = read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv') %>%
  pivot_longer(cols = matches('^([0-9])'), names_to = 'date', values_to = 'cases')

johns_hopkins_deaths = read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv') %>%
  pivot_longer(cols = matches('^([0-9])'), names_to = 'date', values_to = 'deaths')

jh_joined = left_join(johns_hopkins_cases, johns_hopkins_deaths, by = c('Province/State', 'Country/Region', 'date')) %>%
  select(-contains('lat'), -contains('long')) %>%
  mutate(
    date_upd = as.Date(date, format = '%m/%d/%y')
  ) 


names(jh_joined) = names(jh_joined) %>% tolower() %>% str_replace('[\\/]', '_')

jh_with_pop = mutate(jh_joined, is_country = is.na(province_state)) %>%
  rename(country = country_region) %>%
  arrange(province_state, country, date_upd) %>%
  mutate(
    country = recode(country, 
                     `Russian Federation` = 'Russia',
                     US = 'United States', 
                     `Korea, South` = 'South Korea', Czechia = 'Czech Republic', Slovakia = 'Slovak Republic')
  ) %>%
  left_join(
    latest_country_pop
  ) 
head(jh_with_pop)

deaths_by_country_province = group_by(jh_with_pop, country, province_state) %>%
  summarize(
    max_cases = max(cases),
    max_deaths = max(deaths),
    last_date = max(date_upd)
  )

covid_deaths_by_country_date = group_by(jh_with_pop, country, province_state, date = date_upd) %>%
  summarize(
    cumulative_cases = max(cases),
    cumulative_deaths = sum(deaths)
  ) %>%
  group_by(country, date) %>%
  summarize(
    cumulative_cases = sum(cumulative_cases),
    cumulative_deaths = sum(cumulative_deaths)
  ) %>%
  left_join(country_stringency) %>%
  left_join(
    select(latest_country_pop %>% ungroup(), country, population)
  ) %>%
  arrange(country, date) %>%
  mutate(
    Stringency_z = (StringencyIndex - mean(StringencyIndex, na.rm = T)) / sd(StringencyIndex, na.rm = T),
    Stringency_median_over_iqr = (StringencyIndex - median(StringencyIndex, na.rm = T)) / IQR(StringencyIndex ,na.rm = T)
  ) %>%
  data.table() 

##### final covid daily data #####
options(na.action = na.exclude)
covid_deaths_by_country_date_diffs = covid_deaths_by_country_date[, {
  # us = filter(covid_deaths_by_country_date, country == 'Norway')
  # attach(us)
  # detach(us)
  # 
  new_deaths = c(NA, diff(cumulative_deaths))
  death_50_date = date[cumulative_deaths >= 50][1]
  days_since_death_50_date = as.numeric(date - death_50_date)
  mortality_rate = cumulative_deaths / population 
  
  new_cases = c(NA, diff(cumulative_cases))
  roll_7_new_cases = roll_mean(new_cases, 7)
  
  roll_7_new_deaths = roll_mean(new_deaths, 7)
  roll_14_new_deaths = roll_mean(new_deaths, 14)
  roll_7_new_deaths_per_100k = (roll_7_new_deaths / population) * 1e5
  peak_daily_deaths = roll_7_new_deaths_per_100k >= quantile(roll_7_new_deaths_per_100k, probs = 0.75, na.rm = T)
  peak_daily_deaths_lead_14 = lead(peak_daily_deaths, 14)
  
  smoothed_roll_7_new_deaths_per_100k = rep(NA, length(date)) %>% as.numeric()
  tryCatch({
    smoothed_roll_7_new_deaths_per_100k = loess(roll_7_new_deaths_per_100k ~ as.integer(date), span = 0.2) %>% predict()  
  }, error = function(e){
    
    cat("\nerror with smoothed_roll_7_new_deaths_per_100k for",.BY[[1]],"\n\n")
  })
  roll_7_new_cases_per_100k = (roll_7_new_cases / population) * 1e5
  rolling_regression_new_cases = roll_lm(as.integer(date), roll_7_new_cases_per_100k, width = 7)
  rolling_regression_new_cases_7 = rolling_regression_new_cases$coefficients[,2]
  
  roll_7_new_cases_per_100k_q3_month = roll_quantile(roll_7_new_cases_per_100k, 30, p = 0.75)
  
  
  rolling_regression_smoothed = roll_lm(as.integer(date), smoothed_roll_7_new_deaths_per_100k, width = 5)
  
  rolling_regression = roll_lm(as.integer(date), roll_7_new_deaths_per_100k, width = 7)
  rolling_regression_14 = roll_lm(as.integer(date), roll_7_new_deaths_per_100k, width = 14)
  
  regression_coefs_7_smoothed = rolling_regression_smoothed$coefficients[,2]
  quarter_sd = sd(regression_coefs_7_smoothed, na.rm = T) / 3
  
  inflection_point = between(regression_coefs_7_smoothed, 0 - quarter_sd, 0 + quarter_sd)
  n_pos_slopes = roll_sum(regression_coefs_7_smoothed > 0, 7)
  n_neg_slopes = roll_sum(regression_coefs_7_smoothed < 0, 7)
  
  inflection_point_desc = ifelse(inflection_point & n_pos_slopes >= 4, 'peak', ifelse(inflection_point & n_neg_slopes >= 4, 'floor', 'normal')) %>% as.character()
  inflection_point_desc[inflection_point_desc == 'normal' & n_pos_slopes>= 4] = 'increasing'
  inflection_point_desc[inflection_point_desc == 'normal' & n_neg_slopes >= 4] = 'decreasing'
  
  floor_peak_cycles = data.frame(
    inflection_point_desc,
    date,
    roll_7_new_deaths_per_100k
  ) %>%
    filter(inflection_point_desc %in% c('floor', 'peak') & ifelse(inflection_point_desc == 'peak', roll_7_new_deaths_per_100k > 0, T))
  
  # capture window start and end times
  
  if (nrow(floor_peak_cycles) > 0) {
    window_catcher = list()
    last_desc = NA
    window_it = 0
    window_df = NA
    for (it in 1:nrow(floor_peak_cycles)) {
      # it = 1
      this_desc = floor_peak_cycles$inflection_point_desc[it]
      
      if (is.na(last_desc) | this_desc != last_desc) {
        if (window_it > 0) {
          window_df$end_date = floor_peak_cycles$date[it - 1]
          window_catcher[[length(window_catcher) + 1]] = window_df
        }
        window_it = window_it + 1
        window_df = data.frame(
          window_start = floor_peak_cycles$date[it],
          this_desc,
          window_it
        )  
      } 
      last_desc = this_desc
    }
    windows_stacked = bind_rows(window_catcher)
    first_floor_to_floor = filter(windows_stacked, this_desc == 'floor') 
    if (nrow(first_floor_to_floor) >= 2) {
      first_floor_to_floor_sub = head(first_floor_to_floor, 2)
      in_first_cycle = between(date, first_floor_to_floor_sub$end_date[1], first_floor_to_floor_sub$end_date[2])  
    } else {
      in_first_cycle = rep(NA, length(date))
    }
  } else {
    in_first_cycle = rep(NA, length(date))
  }
  
  
  regression_coefs = rolling_regression$coefficients[,2]
  regression_coefs_14 = rolling_regression_14$coefficients[,2]
  
  roll_7_new_deaths_avg = roll_7_new_deaths / roll_14_new_deaths
  roll_7_new_deaths_avg_roll = roll_mean(roll_7_new_deaths_avg, 7)
  
  phases = sign(roll_7_new_deaths_avg_roll - 1)
  
  
  
  roll_7_new_deaths_rollsum_7 = roll_sum(roll_7_new_deaths_avg > 1, 7)
  avg_equalized = between(roll_7_new_deaths_avg_roll, 0.95, 1.05)
  in_equal_band = c(rep(NA, 13), roll_sum(avg_equalized, 14))
  avg_equalized - lag(avg_equalized)
  
  in_death_peak = roll_7_new_deaths_rollsum_7 == 7
  peak_changes = in_death_peak - lag(in_death_peak, 1)
  peak_changes_filled = ifelse(peak_changes == 0, roll_7_new_deaths_rollsum_7, peak_changes)
  new_deaths_pct_of_max = new_deaths / max(new_deaths, na.rm = T)
  # peak_date = date[new_deaths == max(new_deaths, na.rm = T) & phase_descs == 'pos after na' ] %>% min(na.rm = T)
  
  # loop over roll_7_new_deaths_rollsum_7. A peak window is when this exceeds 7 for a while and then comes back down 
  new_cases_per_100k = (new_cases / population ) * 1e5
  pct_change_new_cases_per_100k = (new_cases_per_100k - lag(new_cases_per_100k)) / lag(new_cases_per_100k)
  
  
  pct_change_roll_7_new_cases_per_100k = (roll_7_new_cases_per_100k - lag(roll_7_new_cases_per_100k)) / lag(roll_7_new_cases_per_100k)
  
  list(
    days_since_death_50_date = days_since_death_50_date, 
    date = date, 
    in_first_cycle = in_first_cycle,
    new_cases_elevated = roll_7_new_cases_per_100k > roll_7_new_cases_per_100k_q3_month,
    rolling_regression_new_cases_7 = rolling_regression_new_cases_7,
    peak_daily_deaths_lead_14 = lead(peak_daily_deaths, 14),
    inflection_point_desc = inflection_point_desc,
    n_pos_slopes = n_pos_slopes, 
    n_neg_slopes = n_neg_slopes,
    
    roll_7_new_cases_per_100k = roll_7_new_cases_per_100k,
    pct_change_roll_7_new_cases_per_100k = pct_change_roll_7_new_cases_per_100k,
    # phase_descs = phase_descs,
    new_deaths_pct_of_max = new_deaths_pct_of_max,
    # initial_peak = phase_descs == 'pos after na',
    new_deaths = new_deaths,
    mortality_rate = mortality_rate,
    new_deaths_per_100k = (new_deaths / population) * 1e5,
    
    roll_7_new_deaths = roll_7_new_deaths,
    roll_14_new_deaths = roll_14_new_deaths, 
    new_cases_per_100k = new_cases_per_100k,
    pct_change_new_cases_per_100k = pct_change_new_cases_per_100k, 
    
    regression_coefs_new_deaths7_100k = regression_coefs,
    regression_coefs_new_deaths14_100k = regression_coefs_14, 
    regression_coefs_new_deaths7_100k_smoothed = regression_coefs_7_smoothed,
    
    smoothed_roll_7_new_deaths_per_100k = smoothed_roll_7_new_deaths_per_100k,
    
    roll_7_new_deaths_per_100k = roll_7_new_deaths_per_100k,
    inflection_point = inflection_point,
    roll_14_new_deaths_per_100k = (roll_14_new_deaths / population) * 1e5,
    
    mortality_per_100k = mortality_rate * 1e5,
    
    new_deaths_pct_14_avg = new_deaths / roll_14_new_deaths,
    new_deaths_pct_7_avg = new_deaths / roll_7_new_deaths,
    roll_7_new_deaths_avg_roll = roll_7_new_deaths_avg_roll, 
    roll_7_new_deaths_avg = roll_7_new_deaths_avg,
    roll_7_new_deaths_rollsum_7 = roll_7_new_deaths_rollsum_7,
    new_cases = new_cases, 
    roll_7_new_cases = roll_7_new_cases,
    
    roll_7_new_deaths_pct_max = roll_7_new_deaths / max(roll_7_new_deaths, na.rm = T),
    roll_7_new_deaths_pct = cume_dist(roll_7_new_deaths),
    StringencyIndex_pct = cume_dist(StringencyIndex),
    cumulative_cases = cumulative_cases, 
    cumulative_deaths = cumulative_deaths,
    ContainmentHealthIndex  = ContainmentHealthIndex , 
    EconomicSupportIndex = EconomicSupportIndex ,
    GovernmentResponseIndex = GovernmentResponseIndex , 
    StringencyIndex = StringencyIndex,
    StringencyIndex_diff = StringencyIndex - lag(StringencyIndex),
    StringencyIndex_pct_change = (StringencyIndex - lag(StringencyIndex)) / lag(StringencyIndex),
    one_week_stringency = roll_mean(StringencyIndex, 7),
    two_week_stringency = roll_mean(StringencyIndex, 14),
    Stringency_z = Stringency_z,
    Stringency_median_over_iqr = Stringency_median_over_iqr,
    StringencyIndex_percentile = cume_dist(StringencyIndex),
    change_StringencyIndex = c(NA, diff(StringencyIndex, 1)),
    mobility = mobility,
    mobility_roll_7 = roll_mean(mobility, 7),
    daily_cumulative_deaths_percent_of_total = cumulative_cases / max(cumulative_cases)
  )
  
}, by = list(country)] %>%
  left_join(latest_country_pop %>% select(-population)) 

# avg_stringency_by_date = group_by(covid_deaths_by_country_date_diffs, date) %>%
#   summarize(
#     mean_stringency_by_date = mean(StringencyIndex, na.rm = T),
#     mean_mobility_by_date = mean(mobility, na.rm = T),
#     smoothed_mobility_mean_by_date = mean(mobility_roll_7, na.rm = T),
#     max_stringency_by_date = max(StringencyIndex, na.rm = T),
#     median_stringency_by_date = median(StringencyIndex, na.rm = T)
#   )
# covid_deaths_by_country_date_diffs = left_join(covid_deaths_by_country_date_diffs, avg_stringency_by_date)


filter(covid_deaths_by_country_date_diffs, country == 'Italy') %>%
  ggplot(aes(date, rolling_regression_new_cases_7)) +
  geom_bar(stat = 'identity')


filter(covid_deaths_by_country_date_diffs, country == 'Sweden') %>%
  ggplot(aes(date, roll_7_new_deaths_per_100k)) +
  geom_bar(stat = 'identity', aes(fill = in_first_cycle))

covid_deaths_by_country_date_diffs

# 
# stringency_model = lm(StringencyIndex ~ lag(StringencyIndex, 1) + mean_stringency_by_date + 
#                         roll_7_new_deaths_per_100k + roll_7_new_cases_per_100k, data = covid_deaths_by_country_date_diffs)


boost_sub = select(covid_deaths_by_country_date_diffs, country, date, 
                   StringencyIndex, roll_7_new_cases_per_100k, roll_7_new_deaths_per_100k, mortality_per_100k, rolling_regression_new_cases_7) %>% na.omit()

model_predictions_by_country = unique(boost_sub$country) %>%
  map(function(the_country){
    
    not_country_sub = filter(boost_sub, country != the_country)
    stats_by_date = group_by(not_country_sub, date) %>% 
      summarize(
        q20_stringency_date = quantile(StringencyIndex, probs = 0.2, na.rm = T),
        mean_stringency_date = mean(StringencyIndex, probs = 0.5, na.rm = T),
        q80_stringency_date = quantile(StringencyIndex, probs = 0.8, na.rm = T)
      )
    not_country_sub = left_join(not_country_sub, stats_by_date)
    
    country_sub = filter(boost_sub, country == the_country) %>% left_join(stats_by_date)
    
    the_formula = as.formula('StringencyIndex ~ mean_stringency_date + q20_stringency_date + q80_stringency_date + roll_7_new_cases_per_100k + roll_7_new_deaths_per_100k + date')
    stringency_model = lm(the_formula, data = not_country_sub)
    
    model_mat = model.matrix(the_formula, data = not_country_sub)    
    boosted_stringency_model = xgboost(data = model_mat, label = not_country_sub$StringencyIndex, nrounds = 100,verbose = 0)    
    
    country_model_mat = model.matrix(the_formula, data = country_sub)    
    country_sub$boosted_stringency = predict(boosted_stringency_model, newdata = country_model_mat)
    country_sub$linear_stringency = predict(stringency_model, newdata = country_sub)
    
    ensemble_prediction = lm(StringencyIndex ~ linear_stringency + boosted_stringency, data = country_sub)
    country_sub$ensemble_stringency_prediction = predict(ensemble_prediction)
    
    return(country_sub)
  }) %>%
  bind_rows()


stringency_model_correlations = group_by(model_predictions_by_country, country) %>%
  summarize(
    stringency_correlation = cor(ensemble_stringency_prediction, StringencyIndex)
  )

covid_deaths_by_country_date_diffs = 
  left_join(
    covid_deaths_by_country_date_diffs,
    model_predictions_by_country %>% select(country, date, boosted_stringency, linear_stringency, ensemble_stringency_prediction)
  ) %>%
  mutate(
    ensemble_stringency_prediction_error = ensemble_stringency_prediction - StringencyIndex
  )



# 
# daily_stats = group_by(covid_deaths_by_country_date_diffs, date) %>%
#   summarize(
#     mean_roll_7_new_deaths_per_100k = mean(roll_7_new_deaths_per_100k, na.rm = T),
#     mean_stringency = mean(ContainmentHealthIndex, na.rm = T),
#     median_stringency = median(ContainmentHealthIndex, na.rm = T),
#     mean_roll_7_new_deaths_pct = mean(roll_7_new_deaths_pct, na.rm = T),
#     countries_near_peak = n_distinct(country[roll_7_new_deaths_pct > 0.75]),
#     mean_stringency_new_deaths_20 = mean(ContainmentHealthIndex[roll_7_new_deaths_pct >= 0.2], na.rm = T),
#     mean_stringency_new_deaths_40 = mean(ContainmentHealthIndex[roll_7_new_deaths_pct >= 0.4], na.rm = T),
#     mean_stringency_new_deaths_60 = mean(ContainmentHealthIndex[roll_7_new_deaths_pct >= 0.6], na.rm = T),
#     mean_stringency_new_deaths_80 = mean(ContainmentHealthIndex[roll_7_new_deaths_pct >= 0.8], na.rm = T)
#   )

# match high periods with stringency, stringency being measured across countries

# covid_deaths_by_country_date_diffs = 
#   left_join(covid_deaths_by_country_date_diffs, daily_stats) %>%
#   mutate(
#     containment_mean_diff = ContainmentHealthIndex - mean(ContainmentHealthIndex, na.rm = T),
#     new_deaths_pct_mean = new_deaths_per_100k / mean(new_deaths_per_100k, na.rm = T),
#     roll_7_new_deaths_pct_mean = roll_7_new_deaths_per_100k / mean(roll_7_new_deaths_per_100k, na.rm = T),
#     roll_7_new_deaths_per_100k_pct_mean = roll_7_new_deaths_per_100k / mean_roll_7_new_deaths_per_100k
#   )



ggplot(covid_deaths_by_country_date_diffs %>% filter(country %in% c('United States')), aes(date)) +
  geom_line(aes(y = roll_14_new_deaths_per_100k), colour = 'blue') +
  geom_line(aes(y = roll_7_new_deaths_per_100k), colour = 'red') +
  geom_point(aes(y = roll_7_new_deaths_avg_roll, colour = roll_7_new_deaths_rollsum_7 == 7)) +
  geom_line(aes(y = roll_7_new_deaths_avg_roll), colour = 'black') 

ggplot(covid_deaths_by_country_date_diffs %>% filter(country %in% c('Brazil')), aes(date)) +
  facet_wrap(~country) +
  # geom_line(aes(y = roll_2_new_deaths), colour = 'red') +
  # geom_line(aes(y = roll_3_new_deaths)) +
  geom_line(aes(y = roll_7_new_deaths), colour = 'red') +
  geom_line(aes(y = roll_14_new_deaths))

ggplot(covid_deaths_by_country_date_diffs %>% filter(country %in% c('Brazil', 'Italy', 'Sweden')), aes(date)) +
  facet_wrap(~country) +
  geom_line(aes(y = roll_7_new_deaths_per_100k), colour = 'black') +
  geom_point(aes(y = roll_7_new_deaths_per_100k, colour = roll_7_new_deaths_pct_change > 0)) 




covid_deaths_by_country_date_diffs %>% filter(country %in% c('Italy'), month(date) == 10) %>% 
  select(date, roll_7_new_deaths_avg_roll)


filter(covid_deaths_by_country_date_diffs, roll_7_new_deaths_rollsum_7 == 7, country == 'Sweden')

ggplot(covid_deaths_by_country_date_diffs %>% filter(country %in% c('Sweden')), aes(date)) +
  geom_step(aes(y = roll_7_new_deaths_rollsum_7)) +
  geom_point(aes(y = roll_7_new_deaths_rollsum_7, colour = roll_7_new_deaths_rollsum_7 == 7))

ggplot(covid_deaths_by_country_date_diffs %>% filter(country %in% c('United States')), aes(date)) +
  geom_step(aes(y = roll_7_new_deaths_rollsum_7)) +
  geom_point(aes(y = roll_7_new_deaths_rollsum_7, colour = roll_7_new_deaths_rollsum_7 == 7))

ggplot(covid_deaths_by_country_date_diffs %>% filter(country %in% c('Brazil')), aes(date)) +
  geom_step(aes(y = roll_7_new_deaths_rollsum_7)) +
  geom_point(aes(y = roll_7_new_deaths_rollsum_7, colour = roll_7_new_deaths_rollsum_7 == 7))

ggplot(covid_deaths_by_country_date_diffs %>% filter(country %in% c('Sweden', 'Italy')), aes(date)) +
  geom_step(aes(y = roll_7_new_deaths_rollsum_7)) +
  facet_wrap(~country) +
  geom_point(aes(y = roll_7_new_deaths_rollsum_7, fill = StringencyIndex, size = StringencyIndex), pch = 21) +
  scale_fill_viridis_c(option = 'A')


ggplot(covid_deaths_by_country_date_diffs %>% filter(country %in% c('Sweden', 'Italy'), roll_7_new_deaths_rollsum_7 == 7), aes(date)) +
  geom_point(aes(y = roll_7_new_deaths_per_100k, colour = country)) +
  geom_hline(aes(yintercept = 1))


#### analyze peak covid vs. stringency during the peak ####
ggplot(covid_deaths_by_country_date_diffs %>% 
         filter(country %in% c('Sweden', 'Italy', 'United States', 'Brazil', 'United Kingdom', 'Spain', 'Peru'), 
                roll_7_new_deaths_rollsum_7 == 7), aes(date)) +
  geom_point(aes(y = roll_7_new_deaths_per_100k, colour = country)) +
  geom_hline(aes(yintercept = 1))


ggplot(covid_deaths_by_country_date_diffs %>% filter(country %in% c('United States', 'Sweden', 'Italy')), aes(date)) +
  geom_line(aes(y = roll_7_new_deaths_per_100k, colour = country)) +
  geom_hline(aes(yintercept = 1))

ggplot(covid_deaths_by_country_date_diffs %>% filter(country %in% c('United States', 'Sweden', 'Italy')), aes(date)) +
  geom_line(aes(y = roll_7_new_deaths_pct_max, colour = country)) +
  geom_hline(aes(yintercept = 1))

ggplot(covid_deaths_by_country_date_diffs %>% filter(country %in% c('United States', 'Sweden', 'Italy')), aes(date)) +
  geom_line(aes(y = ContainmentHealthIndex, colour = country)) 


ggplot(covid_deaths_by_country_date_diffs %>% filter(country %in% c('United States', 'Sweden', 'Italy')), aes(date)) +
  geom_line(aes(y = roll_7_new_deaths_pct_mean, colour = country)) +
  geom_hline(aes(yintercept = 1))

ggplot(covid_deaths_by_country_date_diffs %>% filter(country %in% c('United States', 'Sweden', 'Italy')), aes(Stringency_z, roll_7_new_deaths_pct_mean)) +
  geom_point()



ggplot(covid_deaths_by_country_date_diffs %>% filter(country %in% c('United States')), aes(date)) +
  geom_line(aes(y = roll_14_new_deaths, colour = country)) 


ggplot(covid_deaths_by_country_date_diffs %>% filter(country %in% c('United States', 'Sweden', 'Italy')), aes(date, colour = country)) +
  geom_line(aes(y = roll_7_new_deaths_avg)) 


ggplot(covid_deaths_by_country_date_diffs %>% filter(country %in% c('United States', 'Sweden', 'Italy')), aes(date, colour = country)) +
  geom_line(aes(y = roll_7_new_deaths_per_100k_pct_mean)) 

group_by(covid_deaths_by_country_date_diffs, country) %>%
  summarize(
    median_roll_7_new_deaths_per_100k_pct_mean = median(roll_7_new_deaths_per_100k_pct_mean, na.rm = T)
  ) %>%
  arrange(
    -median_roll_7_new_deaths_per_100k_pct_mean
  ) %>%
  View()




ggplot(daily_stringency_stats, aes(date)) +
  geom_line(aes(y = countries_near_peak))

ggplot(covid_deaths_by_country_date_diffs, aes(date)) +
  geom_line(aes(y = countries_near_peak))

ggplot(daily_stringency_stats, aes(date)) +
  geom_line(aes(y = mean_roll_7_new_deaths_pct))




filter(covid_deaths_by_country_date_diffs, country %in% nordic_countries) %>%
  ggplot(aes(date, stringency_pct_new_deaths_interaction, colour = country)) +
  geom_point()


covid_deaths_by_country_date_diffs_dt = data.table(arrange(covid_deaths_by_country_date_diffs, region, country, date))


stats_by_region = covid_deaths_by_country_date_diffs_dt[!is.na(date),{
  the_countries = country
  
  total_pop = sum(filter(latest_country_pop, country %in% the_countries)$population, na.rm = T)
  
  country_df = data.frame(country, date, new_deaths) %>%
    group_by(date) %>%
    summarize(
      total_new_deaths = sum(new_deaths, na.rm = T)
    ) %>%
    ungroup() %>%
    mutate(
      total_new_deaths_100k = (total_new_deaths / total_pop) * 1e5,
      total_new_death_100k_roll_7 = c(rep(NA, 6), roll_mean(total_new_deaths_100k, 7))
    )
  
  
  country_df
  
}, by = list(region)] %>%
  filter(!is.na(region))


##### IMF Data #####
setwd("~/Public_Policy/Projects/COVID-19 Mismanagement/data")
imf_real_gdp_projections = read_excel("IMF october projections data.xlsx", 'all countries projections')
neg_character = imf_real_gdp_projections$`2013` %>% str_extract('^([^0-9]{1})') %>% unique() %>% na.omit() %>% as.character()

imf_real_gdp_projections = imf_real_gdp_projections %>% 
  mutate_all(function(x){
    y = str_replace(x, neg_character, '-')
    y_num = as.numeric(y)
    na_y = sum(is.na(y))
    na_y_num = sum(is.na(y_num))
    if (na_y_num <= na_y) {
      return(y_num)
    } else {
      return(y)
    }
  }) %>%
  mutate(
    entity = recode(entity, `Korea` = 'South Korea')
  ) 




##### OECD Data #####
setwd("~/Public_Policy/Projects/COVID-19 Mismanagement/data/OECD")

## MEASURE -- PC_CHGPY -- SAME PERIOD PRIOR YEAR
## PC_CHGPP 
quarterly_gdp = fread('quarterly_gdp.csv') %>%
  filter(FREQUENCY == 'Q', SUBJECT == 'TOT', MEASURE == 'PC_CHGPP') %>%
  mutate(
    Value_Pct = Value / 100,
    quarter = str_extract(TIME, 'Q[0-9]{1}') %>% str_remove('Q') %>% as.numeric(),
    year = str_extract(TIME, '[0-9]{4}') %>% as.numeric(),
    year_qtr = as.yearqtr(paste(year, quarter, sep = '-'), format = '%Y-%q'),
    country = countrycode(LOCATION, origin = 'iso3c', destination = 'country.name')
  )

monthly_unemployment_rate = fread('unemployment_rate.csv') %>%
  filter(SUBJECT == 'TOT', FREQUENCY == 'M') %>%
  mutate(
    Value_Pct = Value / 100,
    year = str_extract(TIME, '[0-9]{4}') %>% as.numeric(),
    month = str_extract(TIME, '\\-[0-9]{2}') %>% str_remove('-') %>% as.numeric(),
    month_date = as.Date(paste(year, month, '01', sep = '-')),
    country = countrycode(LOCATION, origin = 'iso3c', destination = 'country.name')
  ) %>%
  filter(!is.na(country)) %>%
  arrange(country, month_date)

# find best as-of month
last_month_by_country = group_by(monthly_unemployment_rate, country) %>%
  summarize(
    last_month = max(month_date)
  )

most_common_months_latest_data =
  last_month_by_country %>%
  group_by(last_month) %>%
  summarize(obs = n()) %>%
  arrange(-obs)

selected_ur_countries = filter(last_month_by_country, last_month >= most_common_months_latest_data$last_month[1])

monthly_2020_unemployment_rate_dt = filter(monthly_unemployment_rate,
                                           year == 2020,
                                           month_date <= most_common_months_latest_data$last_month[1],
                                           country %in% selected_ur_countries$country
) %>%
  data.table()

monthly_2020_unemployment_rate_dt_indexes = monthly_2020_unemployment_rate_dt[, {
  starting_value = Value_Pct[1]
  list(
    val_index = Value_Pct / starting_value,
    Value_Pct = Value_Pct,
    month_date = month_date,
    is_last_date = month_date == max(month_date)
  )
  
}, by = list(country)]

covid_ur_indexes = monthly_2020_unemployment_rate_dt[, {
  starting_value = Value_Pct[1]
  ending_value = tail(Value_Pct, 1)
  
  avg_value = mean(Value_Pct)
  val_index = Value_Pct / starting_value
  mean_index = mean(val_index)
  
  list(
    obs = length(month_date),
    starting_value = starting_value,
    ending_value = ending_value,
    period_index = ending_value / starting_value,
    mean_index = mean_index,
    avg_value = avg_value,
    starting_month = month_date[1],
    ending_month = tail(month_date, 1)
  )
  
}, by = list(country)] %>%
  arrange(-mean_index) %>%
  mutate(
    country_ur_factor = factor(country, levels = rev(country))
  )

## for each country, calculate UR index since December 2019



# https://fiscaldata.treasury.gov/datasets/monthly-statement-public-debt/summary-of-treasury-securities-outstanding

##### analysis data -- combine everything, compute stats by country #####

covid_stats_by_country = 
  covid_deaths_by_country_date_diffs %>%
  arrange(country, date) %>%
  group_by(country, income) %>%
  summarize(
    first_cycle_time = as.numeric(max(date[in_first_cycle == T], na.rm = T) - min(date[in_first_cycle == T], na.rm = T)),
    first_cycle_deaths = sum(new_deaths[in_first_cycle], na.rm = T),
    mean_stringency_during_first_cycle_increase = mean(StringencyIndex[in_first_cycle & n_pos_slopes >= 4], na.rm = T),
    mean_stringency_error_during_first_cycle_increase = mean(ensemble_stringency_prediction_error[in_first_cycle & n_pos_slopes >= 4], na.rm = T),
    mean_stringency_error_during_first_cycle = mean(ensemble_stringency_prediction_error[in_first_cycle], na.rm = T),
    mean_stringency_errors = mean(ensemble_stringency_prediction_error, na.rm = T),
    max_stringency_during_peak = max(StringencyIndex[peak_daily_deaths_lead_14], na.rm = T),
    mean_stringency_errors_during_peak = mean(ensemble_stringency_prediction_error[peak_daily_deaths_lead_14], na.rm = T),
    max_stringency_errors_during_peak = max(ensemble_stringency_prediction_error[peak_daily_deaths_lead_14], na.rm = T),
    median_stringency_errors = median(ensemble_stringency_prediction_error, na.rm = T),
    max_stringency_errors = max(ensemble_stringency_prediction_error, na.rm = T),
    min_stringency_errors = min(ensemble_stringency_prediction_error, na.rm = T),
    case_1k_date = min(date[cumulative_cases >= 1000], na.rm = T),
    death_100_date = min(date[cumulative_deaths >= 100], na.rm = T),
    peak_deaths_date = min(date[roll_7_new_deaths_per_100k == max(roll_7_new_deaths_per_100k , na.rm = T)], na.rm = T),
    as_of_date = max(date, na.rm = T),
    total_deaths = max(cumulative_deaths, na.rm = T),
    mean_mobility = mean(mobility, na.rm = T),
    mean_mobility_during_first_cycle = mean(mobility[in_first_cycle], na.rm = T),
    mean_mobility_during_first_cycle_increase = mean(mobility[in_first_cycle & n_pos_slopes >= 4], na.rm = T),
    median_mobility = median(mobility, na.rm = T),
    median_mobility_during_peak = median(mobility[peak_daily_deaths_lead_14], na.rm = T),
    max_stringency = max(StringencyIndex, na.rm = T),
    median_stringency = median(StringencyIndex, na.rm = T),
    median_ContainmentHealthIndex = median(ContainmentHealthIndex, na.rm = T),
    max_ContainmentHealthIndex = max(ContainmentHealthIndex, na.rm = T),
    mean_stringency = mean(StringencyIndex, na.rm = T),
    mean_new_deaths_pct_of_max = mean(new_deaths_pct_of_max, na.rm = T),
    mean_new_deaths_per_100k = mean(new_deaths_per_100k, na.rm = T),
    mortality_per_100k = max(mortality_per_100k)
  ) %>%
  ungroup() %>%
  arrange(mortality_per_100k) %>%
  mutate(
    country_ranked_mortality = factor(country, levels = rev(country)),
    country_factor = factor(country, levels = rev(country)),
    case_1k_minus_min = as.numeric(case_1k_date - min(case_1k_date, na.rm = T)),
    death_100_minus_min = as.numeric(death_100_date - min(death_100_date, na.rm = T)),
    peak_date_minus_min = as.numeric(peak_deaths_date - min(peak_deaths_date, na.rm = T))
  ) %>%
  left_join(
    imf_real_gdp_projections, by = c('country' =   "entity")
  ) %>%
  left_join(
    latest_country_pop
  ) %>%
  rename(
    projection_2020 = `2020`
  ) %>%
  # filter(population > 5e6, projection_2020 >= -15) %>%
  mutate(
    first_cycle_mortality_100k = (first_cycle_deaths / population) * 1e5,
    three_year_avg_growth = (`2019` + `2018` + `2017` ) / 3,
    two_year_avg_growth = (`2019` + `2018`) / 2,
    last_year_growth = `2019`,
    diff_projection_avg = (1 - ((1 + three_year_avg_growth / 100) / (1 + projection_2020/100))) * 100,
    diff_projection_avg_simple = projection_2020 - three_year_avg_growth,
    international_tourism_pop = international_tourism / population
  ) %>%
  arrange(-diff_projection_avg) %>%
  mutate(
    country_ranked_gdp = factor(country, levels = rev(country)),
  ) %>%
  arrange(-gdp_per_capita_us)
<<<<<<< HEAD
covid_stats_by_country$

covid_stats_by_country$diff_projection_avg / 100
### output data ###
covid_stats_by_country$geometry = NULL

write.csv(covid_stats_by_country, 'covid_stats_by_country.csv', row.names = F)
# write.csv(covid_deaths_by_country_date_diffs, 'covid_deaths_by_country_date.csv', row.names = F)

=======
>>>>>>> 45dc18cdb0eeda0ffaa3308922c6adce67096026

filter(covid_stats_by_country, income == 'High income') %>%
  ggplot(aes(mean_mobility_during_first_cycle, first_cycle_mortality_100k)) +
  geom_point()

filter(covid_stats_by_country, income == 'High income') %>%
  ggplot(aes(mean_mobility_during_first_cycle_increase, first_cycle_mortality_100k)) +
  geom_point()



filter(covid_stats_by_country, income == 'High income') %>%
  ggplot(aes(mean_stringency_during_first_cycle_increase, mean_mobility_during_first_cycle_increase)) +
  geom_point(aes(size = first_cycle_mortality_100k), pch = 21) +
  scale_size(range = c(2, 12)) +
  stat_smooth(method = 'lm')




filter(covid_stats_by_country, country %in% c('Spain', 'Italy', 'United States', 'Sweden', 'Denmark', 'Germany', 'Belgium', 'France', 'United Kingdom')) %>%
  select(
    country,
    first_cycle_time,
    first_cycle_deaths,
    population,
    mean_stringency_during_first_cycle_increase
  ) %>%
  mutate(
    (first_cycle_deaths / population) * 1e5
  ) %>%
  select(-population) %>%
  arrange(-first_cycle_time)

europe_stats = filter(covid_stats_by_country, continent == 'Europe', population >= 5e6) %>%
  select(
    country,
    first_cycle_time,
    first_cycle_deaths,
    population,
    mean_mobility_during_first_cycle,
    mean_mobility_during_first_cycle_increase, 
    mean_stringency_error_during_first_cycle_increase,
    mean_stringency_during_first_cycle_increase
  ) %>%
  mutate(
    mortality_100k = (first_cycle_deaths / population) * 1e5
  ) %>%
  select(-population) %>%
  arrange(-mean_stringency_error_during_first_cycle_increase) 


europe_stats %>%
  ggplot(aes(mean_stringency_error_during_first_cycle_increase, mortality_100k)) +
  geom_text(aes(label = country))

europe_stats %>%
  ggplot(aes(mean_stringency_during_first_cycle_increase, mean_mobility_during_first_cycle_increase)) +
  geom_text(aes(label = country, size= mortality_100k)) +
  stat_smooth(method = 'lm')

europe_stats %>%
  ggplot(aes(mean_mobility_during_first_cycle_increase, mortality_100k)) +
  geom_text(aes(label = country)) +
  stat_smooth(method = 'lm')



names(covid_stats_by_country)
covid_deaths_by_country_date_diffs$roll_7_new_deaths_per_100k

filter(covid_deaths_by_country_date_diffs, country %in% c('Russia','United States', 'Spain', 'Germany', 'Italy', 'Brazil', 'Peru', 'Mexico', 'Canada', 'Iceland','South Africa', nordic_countries)) %>%
  ggplot(aes(date, roll_7_new_deaths_per_100k, fill = in_first_cycle)) +
  facet_wrap(~country) +
  geom_bar(stat = 'identity')

ggplot(covid_stats_by_country, aes(max_stringency, mortality_per_100k)) +
  geom_point()

group_by(covid_stats_by_country, income) %>%
  summarize(
    median_cycle_time = median(first_cycle_time, na.rm = T),
    median_first_cycle_deaths = median((first_cycle_deaths / population) * 1e5, na.rm = T)
  )
ggplot(covid_stats_by_country, aes(first_cycle_time)) +
  facet_wrap(~income) +
  stat_density() +
  geom_vline(data= covid_stats_by_country %>% filter(country %in% nordic_countries), aes(xintercept = first_cycle_time, colour = country)) +
  geom_text(data= covid_stats_by_country %>% filter(country %in% nordic_countries), aes(x = first_cycle_time, y = 0, label = country, colour = country), angle = 90, vjust = 1)

ggplot(covid_stats_by_country %>% filter(income == 'High income'), aes(first_cycle_time, log((first_cycle_deaths / population) * 1e5))) +
  geom_text(aes(label = country)) + 
  facet_wrap(~income) +
  stat_smooth(method = 'lm') 

ggplot(covid_stats_by_country, aes(first_cycle_time, log((first_cycle_deaths / population) * 1e5))) +
  geom_text(aes(label = country)) + 
  stat_smooth(method = 'lm') 


ggplot(covid_stats_by_country, aes(first_cycle_time, mean_stringency_during_first_cycle_increase)) +
  geom_text(aes(label = country)) + 
  stat_smooth(method = 'lm') 


ggplot(covid_stats_by_country, aes(mean_stringency_during_first_cycle_increase, log((first_cycle_deaths / population) * 1e5))) +
  geom_text(aes(label = country)) + 
  stat_smooth(method = 'lm') 

ggplot(covid_stats_by_country, aes(mean_stringency_error_during_first_cycle_increase, log((first_cycle_deaths / population) * 1e5))) +
  geom_text(aes(label = country)) +
  facet_wrap(~income) +
  stat_smooth(method = 'lm', se = F) 



covid_stats_by_country$mean_stringency_errors_during_peak

ggplot(covid_stats_by_country, aes(mean_stringency_errors_during_peak, mortality_per_100k)) +
  geom_point() +
  stat_smooth()

ggplot(covid_stats_by_country, aes(max_stringency_errors_during_peak, mortality_per_100k)) +
  geom_point() +
  stat_smooth()


filter(covid_stats_by_country, country %in% nordic_countries) %>%
  select(country, mortality_per_100k, max_stringency, mean_stringency_errors, max_stringency_errors, mean_stringency_errors_during_peak)


ggplot(covid_stats_by_country, aes(median_roll_7_new_deaths_per_100k_pct_mean, mortality_per_100k)) +
  geom_text(aes(label = country, colour = region)) +
  stat_smooth()


ggplot(covid_stats_by_country, aes(mean_stringency_errors, mortality_per_100k)) +
  geom_point() +
  stat_smooth(method = 'lm')
ggplot(covid_deaths_by_country_date_diffs %>% filter(country == 'Sweden'), aes(date, stringency_errors)) +
  geom_point()

ggplot(covid_deaths_by_country_date_diffs, aes(stringency_errors, roll_7_new_deaths_per_100k)) + 
  geom_point()

##### test stringency errors against mortality outcomes #####

country_list = unique(covid_deaths_by_country_date_diffs$country)
result_list =  country_list %>% 
  map(function(the_country){
    the_sub = filter(covid_deaths_by_country_date_diffs, country == the_country)
    tryCatch({
      the_model = lm(new_deaths_per_100k ~ lag(ensemble_stringency_prediction_error, 14), data = the_sub)
      return(coefficients(the_model)[2])
    }, error = function(e){
      cat('error for ', the_country, '\n')
      return(NA)
    })
    
  })
names(result_list) = country_list
stringency_analysis_result_df = data.frame(
  country = country_list,
  coefficients = unlist(result_list)
) %>%
  arrange(
    -abs(coefficients)
  )


filter(covid_deaths_by_country_date_diffs, country %in% c('Italy', nordic_countries, 'Sweden', 'United States')) %>%
  ggplot(aes(date, y = smoothed_roll_7_new_deaths_per_100k)) +
  facet_wrap(~country) +
  geom_line() +
  geom_point(aes(colour = inflection_point_desc))

filter(covid_deaths_by_country_date_diffs, country %in% c('Italy', nordic_countries, 'Sweden', 'United States')) %>%
  ggplot(aes(date, y = smoothed_roll_7_new_deaths_per_100k)) +
  facet_wrap(~country) +
  geom_line() +
  geom_point(aes(colour = n_neg_slopes > 4))

min(covid_deaths_by_country_date_diffs$ensemble_stringency_prediction, na.rm = T)
filter(covid_deaths_by_country_date_diffs, country %in% c('Italy', nordic_countries, 'Sweden', 'United States')) %>%
  ggplot(aes(date, y = regression_coefs_new_deaths7_100k_smoothed)) +
  facet_wrap(~country) +
  geom_point(aes(colour = inflection_point_desc))
covid_deaths_by_country_date_diffs$ensemble_stringency_prediction

ggplot(covid_deaths_by_country_date_diffs, aes(ensemble_stringency_prediction, StringencyIndex)) +
  geom_point(alpha = 0.3)

# grangertest(new_deaths_per_100k ~ new_cases_per_100k, order = 14, data = covid_deaths_by_country_date_diffs)
# 
# a = lm(new_deaths_per_100k ~ lag(stringency_errors, 14), data = spain)

ggplot(covid_deaths_by_country_date_diffs, aes())

the_sub = covid_deaths_by_country_date_diffs %>% 
  filter(country %in% c('Belgium','Norway','Germany', 'Sweden', 
                        'South Korea', 'United States', 'Spain', 'Italy', 'Denmark', 'Finland', 'France', 'United Kingdom'))


ggplot(the_sub, aes(date)) +
  facet_wrap(~country) +
  theme_minimal() +
  # geom_ribbon(data = filter(the_sub, predicted_stringency > StringencyIndex), aes(ymin = StringencyIndex, ymax = predicted_stringency), fill = 'red', alpha = 0.4) +
  # geom_ribbon(data = filter(the_sub, predicted_stringency <= StringencyIndex), aes(ymin = predicted_stringency, ymax = StringencyIndex), fill = 'blue', alpha = 0.4)  
  geom_step(aes(y = ensemble_stringency_prediction), colour = '#e41a1c', size = 1) +
  geom_step(aes(y = StringencyIndex), colour = '#377eb8', size = 1)

filter(the_sub, n_pos_slopes >= 4) %>%
  group_by(country) %>%
  summarize(
    sum(lead(new_deaths, 14), na.rm = T)
  )
the_sub$new_deaths

ggplot(aes(date, roll_7_new_cases_per_100k)) +
  facet_wrap(~country) +
  scale_fill_viridis_c(option = 'C') +
  geom_bar(stat = 'identity', aes(fill = ensemble_stringency_prediction_error))

ggplot(the_sub, aes(date, roll_7_new_deaths_per_100k)) +
  facet_wrap(~country) +
  theme_minimal() +
  geom_line() +
  geom_point(aes(colour = ensemble_stringency_prediction_error)) +
  scale_colour_viridis_c(option = 'C')

ggplot(the_sub, aes(lag(ensemble_stringency_prediction_error, 14), roll_7_new_deaths_per_100k)) +
  facet_wrap(~country, scales = 'free') +
  theme_minimal() +
  geom_point(aes(colour = lag(ensemble_stringency_prediction_error, 14) > 0)) +
  stat_smooth(method = 'lm', se = F)




the_sub$new_cases_elevated
ggplot(the_sub, aes(date, rolling_regression_new_cases_7)) +
  facet_wrap(~country) +
  theme_minimal() +
  # geom_ribbon(data = filter(the_sub, predicted_stringency > StringencyIndex), aes(ymin = StringencyIndex, ymax = predicted_stringency), fill = 'red', alpha = 0.4) +
  # geom_ribbon(data = filter(the_sub, predicted_stringency <= StringencyIndex), aes(ymin = predicted_stringency, ymax = StringencyIndex), fill = 'blue', alpha = 0.4)  
  geom_bar(aes(fill = new_cases_elevated), stat = 'identity') 

ggplot(the_sub, aes(date, ensemble_stringency_prediction_error)) +
  facet_wrap(~country) +
  theme_minimal() +
  # geom_ribbon(data = filter(the_sub, predicted_stringency > StringencyIndex), aes(ymin = StringencyIndex, ymax = predicted_stringency), fill = 'red', alpha = 0.4) +
  # geom_ribbon(data = filter(the_sub, predicted_stringency <= StringencyIndex), aes(ymin = predicted_stringency, ymax = StringencyIndex), fill = 'blue', alpha = 0.4)  
  geom_bar(aes(fill = new_cases_elevated), stat = 'identity') 


ggplot(the_sub, aes(date, roll_7_new_cases_per_100k)) +
  facet_wrap(~country) +
  theme_minimal() +
  # geom_ribbon(data = filter(the_sub, predicted_stringency > StringencyIndex), aes(ymin = StringencyIndex, ymax = predicted_stringency), fill = 'red', alpha = 0.4) +
  # geom_ribbon(data = filter(the_sub, predicted_stringency <= StringencyIndex), aes(ymin = predicted_stringency, ymax = StringencyIndex), fill = 'blue', alpha = 0.4)  
  geom_bar(aes(fill = new_cases_elevated), stat = 'identity') 


ggplot(the_sub, aes(rolling_regression_new_cases_7, lead(roll_7_new_deaths_per_100k, 14))) +
  geom_point
the_sub$roll_7_new_deaths_per_100k

ggplot(the_sub, aes(date)) +
  facet_wrap(~country) +
  theme_minimal() +
  # geom_ribbon(data = filter(the_sub, predicted_stringency > StringencyIndex), aes(ymin = StringencyIndex, ymax = predicted_stringency), fill = 'red', alpha = 0.4) +
  # geom_ribbon(data = filter(the_sub, predicted_stringency <= StringencyIndex), aes(ymin = predicted_stringency, ymax = StringencyIndex), fill = 'blue', alpha = 0.4)  
  geom_step(aes(y = linear_stringency), colour = '#e41a1c', size = 1) +
  geom_step(aes(y = StringencyIndex), colour = '#377eb8', size = 1)

ggplot(the_sub, aes(date)) +
  facet_wrap(~country) +
  theme_minimal() +
  # geom_ribbon(data = filter(the_sub, predicted_stringency > StringencyIndex), aes(ymin = StringencyIndex, ymax = predicted_stringency), fill = 'red', alpha = 0.4) +
  # geom_ribbon(data = filter(the_sub, predicted_stringency <= StringencyIndex), aes(ymin = predicted_stringency, ymax = StringencyIndex), fill = 'blue', alpha = 0.4)  
  geom_step(aes(y = boosted_stringency), colour = '#e41a1c', size = 1) +
  geom_step(aes(y = StringencyIndex), colour = '#377eb8', size = 1)





geom_step(aes(y = predicted_stringency), colour = 'orange', size = 1) +
  geom_step(aes(y = StringencyIndex), colour = 'steelblue', size = 1)
?geom_area

filter(covid_deaths_by_country_date_diffs, country %in% head(stringency_analysis_result_df, 12)$country) %>%
  ggplot(aes(lag(ensemble_stringency_prediction_error, 14), new_deaths_per_100k)) +
  geom_point() +
  facet_wrap(~country, scales = 'free') +
  stat_smooth(method = 'lm', se = F)

filter(covid_deaths_by_country_date_diffs, country %in% the_sub$country) %>%
  ggplot(aes(lag(ensemble_stringency_prediction_error, 14), new_deaths_per_100k)) +
  geom_point() +
  facet_wrap(~country, scales = 'free') +
  stat_smooth(method = 'lm', se = F)

filter(covid_deaths_by_country_date_diffs, country %in% (arrange(stringency_analysis_result_df, -coefficients) %>% pull(country) %>% head(12))) %>%
  ggplot(aes(lag(ensemble_stringency_prediction_error, 14), new_deaths_per_100k)) +
  geom_point() +
  facet_wrap(~country, scales = 'free') +
  stat_smooth(method = 'lm', se = F)

filter(covid_deaths_by_country_date_diffs, country %in% (arrange(stringency_analysis_result_df, -coefficients) %>% pull(country) %>% head(12))) %>%
  ggplot(aes(date, roll_7_new_deaths_per_100k)) +
  facet_wrap(~country) +
  geom_line()




stringency_analysis_result_df$coefficients



plot(stringency_model)
ccf(us$stringency_errors, us$new_deaths_per_100k)
a = ccf(us$stringency_errors, us$new_deaths_per_100k)
a

data.frame(a$lag, a$acf)
covid_deaths_by_country_date_diffs$roll_7_new_deaths_per_100k

ggplot(covid_deaths_by_country_date_diffs %>% 
         filter(country %in% c('Sweden', 'Germany', 'Spain', 'United Kingdom', 'United States', 'Italy', 'Brazil', 'Denmark', 'Norway')), aes(date, stringency_errors)) +
  geom_bar(aes(fill = stringency_errors), stat = 'identity') +
  facet_wrap(~country) +
  scale_fill_viridis_c(option = 'C') +
  # scale_fill_gradient2(midpoint = 0, low = 'blue', high = 'red') +
  theme_dark()


ggplot(covid_deaths_by_country_date_diffs %>% filter(country == 'Japan'), aes(date, stringency_errors)) +
  geom_point()

ggplot(covid_deaths_by_country_date_diffs %>% filter(country == 'Japan'), aes(date, new_deaths_per_100k)) +
  geom_point()
covid_deaths_by_country_date_diffs$new_cases_per_100k


covid_deaths_by_country_date_diffs %>% filter(country == 'Japan') %>% pull(stringency_errors) %>% mean(na.rm = T)
covid_deaths_by_country_date_diffs %>% filter(country == 'Sweden') %>% pull(stringency_errors) %>% mean(na.rm = T)
covid_deaths_by_country_date_diffs %>% filter(country == 'Germany') %>% pull(stringency_errors) %>% mean(na.rm = T)
covid_deaths_by_country_date_diffs %>% filter(country == 'Denmark') %>% pull(stringency_errors) %>% mean(na.rm = T)
covid_deaths_by_country_date_diffs %>% filter(country == 'United States') %>% pull(stringency_errors) %>% mean(na.rm = T)
arrange(covid_stats_by_country, -mean_stringency_errors) %>% select(country, mortality_per_100k)


# ggplot(covid_stats_by_country, aes(death_100_minus_min, mortality_per_100k)) + 
#   geom_point(aes(size = mortality_per_100k)) +
#   stat_smooth()

ggplot(covid_stats_by_country %>% filter(continent == 'Europe', death_100_minus_min <= 90), 
       aes(case_1k_minus_min, mortality_per_100k)) + 
  # geom_point(aes(size = mortality_per_100k), pch = 21) +
  geom_text(aes(label = country)) +
  stat_smooth(se = F)


covid_stats_by_country$geometry = NULL
min_mortality_not_zero = covid_stats_by_country$mortality_per_100k[covid_stats_by_country$mortality_per_100k > 0] %>% min()
covid_stats_by_country$mortality_per_100k_log = ifelse(covid_stats_by_country$mortality_per_100k == 0, min_mortality_not_zero, covid_stats_by_country$mortality_per_100k)

europe_map_data = left_join(europe_cropped, covid_stats_by_country, by = c('name' = 'country'))

selected_european_countries = c('Sweden', 'Denmark', 'Finland', 'Norway', 'Germany', 'Italy', 'Spain', 'France', 'United Kingdom', 'Ireland')
ggplot(europe_map_data) +
  geom_sf(aes(fill = mortality_per_100k)) +
  scale_fill_viridis_c(name = 'Deaths Per\n100k Pop.',option = 'A') +
  theme_map() +
  # theme_dark() +
  # theme_minimal() +
  geom_sf_label(data = filter(europe_map_data, name %in% selected_european_countries), aes(label = paste0(name, '\n', comma(mortality_per_100k, accuracy = 0.1))), size = 3.5) +
  labs(
    x = '', y = '', 
    title = 'COVID-19 Mortality Rates in Europe',
    subtitle = sprintf('Data through %s', max(covid_stats_by_country$as_of_date, na.rm=T) %>% format('%b %d, %Y')),
    caption = 'Chart: Taylor G. White\nData: Johns Hopkins CSSE, World Bank'
  ) +
  theme(
    # axis.text = element_blank(),
    plot.subtitle = element_text(face = 'italic'),
    plot.caption = element_text(hjust = 0, face = 'italic')
  )

ggsave('europe_mortality_rate_map.png', height = 9, width = 12, units = 'in', dpi = 600)



ggplot(covid_stats_by_country , aes(gini_index, last_trust_in_gov)) +
  theme_bw() +
  # geom_point(aes(size = mortality_per_100k_log)) +
  geom_text_repel(aes(label = country)) +
  large_text_theme +
  labs(x = 'GINI Index (Inequality)', y = 'Trust in National Government', title = 'Income Inequality vs. Trust in Government') +
  scale_y_continuous(labels = percent) +
  geom_quantile(quantiles = 0.5, size = 1) 

# stat_smooth(method = 'lm')
ggsave('inequality_vs_trust.png', height = 8, width = 10, units = 'in')
# 
# ggplot(covid_stats_by_country, aes(log(international_tourism), log(mortality_per_100k_log))) +
#   geom_point(aes(colour = region)) +
#   stat_smooth(span = 1)
# 
# ggplot(covid_stats_by_country, aes(international_tourism_pop, log(mortality_per_100k_log))) +
#   geom_point(aes(colour = region)) +
#   stat_smooth(span = 1)
# 
# 
# ggplot(covid_stats_by_country, aes(international_tourism_pop, log(mortality_per_100k_log))) +
#   facet_wrap(~continent, scales = 'free_x') +
#   geom_point(aes(size = gdp_per_capita_us)) +
#   stat_smooth(span = 1)
# 
# ggplot(covid_stats_by_country, aes(gini_index, log(mortality_per_100k_log))) +
#   facet_wrap(~continent) +
#   geom_point() +
#   stat_smooth(span = 1)
# 
# ggplot(covid_stats_by_country, aes(gini_index, log(mortality_per_100k_log))) +
#   facet_wrap(~continent) +
#   geom_point() +
#   stat_smooth(span = 1)
# 
# 
# ggplot(covid_stats_by_country, aes(gini_index, log(mortality_per_100k_log))) +
#   geom_point(aes(size = gdp_per_capita_us)) +
#   stat_smooth(span = 1)
# 
# ggplot(covid_stats_by_country, aes(gini_index, gdp_per_capita_us)) +
#   geom_point() +
#   stat_smooth(span = 1)
# 
# ggplot(covid_stats_by_country, aes(health_exp_gdp, log(mortality_per_100k_log))) +
#   geom_point() +
#   stat_smooth(span = 1)
# 
# 
# ggplot(covid_stats_by_country, aes(health_exp_gdp, pop_pct_65_over)) +
#   geom_point(aes(size = gdp_per_capita_us, colour = log(mortality_per_100k_log))) +
#   scale_color_viridis_c(option = 'A') +
#   stat_smooth(method = 'lm')



##### us comparator rank plots #####
setwd(out_location)

# us_comparator_countries = filter(covid_stats_by_country,  str_detect(economy, 'G7') | str_detect(economy, 'Emerging') | income == 'High income', !is.na(projection_2020))
covid_deaths_by_country_date_diffs$new_cases_elevated
covid_deaths_by_country_date_diffs$rolling_regression_new_cases_7
filter(covid_deaths_by_country_date_diffs, country %in% c('Sweden', 'United States', 'Spain', 'Italy', 'Germany')) %>%
  ggplot(aes(date, ensemble_stringency_prediction_error)) +
  facet_wrap(~country) +
  geom_bar(aes(fill = rolling_regression_new_cases_7 > 0), stat = 'identity')

us_comparator_countries = filter(covid_stats_by_country, population > 5e6, projection_2020 >= -15) %>% head(60) 

filter(us_comparator_countries, country == 'United States')

mortality_model = lm(log(mortality_per_100k) ~ log(international_tourism) + max_stringency + gdp_per_capita_us + urban_pop_pct + mean_stringency_errors_during_peak + pop_pct_65_over, data = us_comparator_countries)
mortality_model = lm(log(mortality_per_100k) ~ log(international_tourism) + max_stringency + gdp_per_capita_us + urban_pop_pct + mean_stringency_errors_during_peak + pop_pct_65_over, data = us_comparator_countries)
summary(mortality_model)

growth_sd = sd(us_comparator_countries$projection_2020)
growth_mean = mean(us_comparator_countries$projection_2020)
mortality_iqr = IQR(us_comparator_countries$mortality_per_100k)
mortality_median = median(us_comparator_countries$mortality_per_100k)
us_comparator_countries$diff_projection_avg %>% median()


# stacked_dat = bind_rows(
#   covid_stats_by_country, 
#   mutate(covid_stats_by_country, region = 'Overall')
# ) %>%
#   filter(region %in% c('Overall', 'North America', 'Europe & Central Asia', 'Latin America & Carribean', 'Middle East & North Africa')) %>%
#   mutate(
#     region_upd = recode(region, `North America` = "Europe and North America", 
#                         `Europe & Central Asia` = "Europe and North America", 
#                         `East Asia & Pacific` = 'East Asia, South Asia, and Pacific',
#                         ``) %>%
#       factor(
#         levels = c('East Asia & Pacific', '')
#       )
#   )


ggplot(us_comparator_countries, aes(country_ranked_gdp)) +
  geom_hline(aes(yintercept = 0)) +
  geom_linerange(aes(ymin = projection_2020, ymax = three_year_avg_growth), size = 0.75) +
  geom_point(aes(y = projection_2020), colour = 'firebrick', size = 3) +
  geom_point(aes(y = three_year_avg_growth), colour = 'steelblue', size = 3) + 
  theme_bw() +
  
  labs()


region_order = c('East Asia & Pacific', 'Europe & Central Asia', 'North America', '')

#### growth projections vs. mortality 
# anim = ggplot(stacked_dat, aes(mortality_per_100k_log, diff_projection_avg/100)) +
#   geom_point(aes(colour = region)) +
#   transition_states(region,
#                     transition_length = 2,
#                     state_length = 4) + 
#   enter_fade() +
#   exit_shrink() +
#   # scale_color_brewer(palette = 'Set1') +
#   scale_color_hue(name = 'Region') +
#   stat_smooth(method = 'lm') +
#   theme_bw() +
#   theme(legend.position = 'bottom') +
#   labs(
#     x = 'COVID Mortality Per 100k, Log Scale', y = 'COVID Economic Impact\n2020 Real GDP Growth Projection vs. Three-Year Average'
#   ) + 
#   scale_y_continuous(labels = percent, breaks = seq(-.20, 0.00, by = .05), limits = seq()) +
#   scale_x_continuous(trans = 'log10', labels = trans_format("log10", math_format(10^.x)))
# 
# # anim
# ggsave('economic_impact_vs_mortality.png', height = 9, width = 12, units = 'in', dpi = 400)
# 



## simple calculations ###
median_econ_impact = median(us_comparator_countries$diff_projection_avg, na.rm = T)
median_mortality = median(us_comparator_countries$mortality_per_100k, na.rm = T)

us_data = filter(us_comparator_countries, country == 'United States')
us_calcs = us_data %>% select(mortality_per_100k, diff_projection_avg) %>% summarise_all(median)

us_calcs$mortality_per_100k / median_mortality
us_calcs$diff_projection_avg / median_econ_impact

superior_countries = filter(us_comparator_countries, mortality_per_100k <= us_calcs$mortality_per_100k & diff_projection_avg >= us_calcs$diff_projection_avg, country != 'United States')
length(superior_countries$country) / nrow(us_comparator_countries)


# lives that could have been saved at median mortality
us_data$total_deaths - ((us_data$population / 1e5) * median_mortality)

##### compare mortality and economic outcomes #####

mortality_rank_plot = ggplot(us_comparator_countries, aes(country_ranked_mortality, mortality_per_100k, fill = mortality_per_100k)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  geom_bar(data = filter(us_comparator_countries, country == 'United States'), fill = 'firebrick', stat = 'identity') +
  scale_y_continuous(labels = comma) +
  coord_flip() +
  theme_bw() +
  labs(
    title = 'COVID Mortality Rate',
    subtitle = 'High income countries, minimum 5M population.',
    caption = '\nChart: Taylor G. White\nData: IMF October Economic Outlook, Johns Hopkins CSSE\nVertical lines show median values.',
    x = '', y = '\nCOVID-19 Mortality Rate\n(Deaths / 100k Population)') +
  geom_hline(aes(yintercept = median_mortality), colour = 'darkslategray', size = 0.75) +
  theme(
    plot.title = element_text(size = 28),
    plot.subtitle = element_text(size = 18, face = 'italic'),
    axis.title = element_text(size = 22),
    plot.caption = element_text(size = 14, face = 'italic', hjust = 0),
    axis.text = element_text(size = 20),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.position = 'right', 
    panel.grid.minor = element_blank()
  ) 


gdp_rank_plot = ggplot(us_comparator_countries, aes(country_ranked_gdp, diff_projection_avg/100, fill = diff_projection_avg/100)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  geom_bar(data = filter(us_comparator_countries, country == 'United States'), fill = 'firebrick', stat = 'identity') +
  coord_flip() +
  theme_bw() +
  scale_y_continuous(labels = percent) +
  labs(
    title = 'COVID Economic Impact',
    subtitle = 'High income countries, minimum 5M population.',
    caption = '\n\n\n',
    x = '', y = '\nReal GDP Growth\nProjected Difference from Three Year Average') +
  geom_hline(aes(yintercept = median_econ_impact/100), colour = 'darkslategray', size = 0.75) +
  # geom_segment(aes(x = 5, xend = 20, y = .95 * min(diff_projection_avg/100) , yend = .95 * min(diff_projection_avg/100)), size = 1) + 
  theme(
    plot.title = element_text(size = 28),
    plot.subtitle = element_text(size = 18, face = 'italic'),
    axis.title = element_text(size = 22),
    plot.caption = element_text(size = 14, face = 'italic', hjust = 0),
    axis.text = element_text(size = 20),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.position = 'right', 
    panel.grid.minor = element_blank()
  ) 


combined_plot = plot_grid(mortality_rank_plot, gdp_rank_plot)
save_plot('mortality_growth_comparison_oecd.png', base_height = 15, base_width = 20, 
          units = 'in', dpi = 600, plot = combined_plot)


##### map european mortality rates ##### 

# date_seq = seq.Date(min(europe_map_data_daily$date, na.rm = T), max(europe_map_data_daily$date, na.rm = T), by = 7)

# europe_map_data_daily = left_join(europe_cropped, covid_deaths_by_country_date_diffs, by = c('name' = 'country')) 

# animated_mortality_map = 
#   ggplot(europe_map_data_daily) +
#   geom_sf(aes(fill = mortality_per_100k)) +
#   transition_time(date, range = as.Date(c('2020-02-01', '2020-08-01'))) +
#   scale_fill_viridis_c(name = 'Deaths Per\n100k Pop.',option = 'A') +
#   theme_map() +
#   # theme_dark() +
#   # theme_minimal() +
#   # geom_sf_label(data = filter(europe_map_data, name %in% selected_countries), aes(label = paste0(name, '\n', comma(mortality_per_100k, accuracy = 0.1))), size = 2.5) +
#   labs(
#     x = '', y = '', 
#     title = 'COVID-19 Mortality Rates in Selected European Countries',
#     subtitle = sprintf('Data through {frame_time}'),
#     caption = 'Chart: Taylor G. White\nData: Johns Hopkins CSSE, World Bank'
#   ) +
#   theme(
#     # axis.text = element_blank(),
#     plot.subtitle = element_text(face = 'italic'),
#     plot.caption = element_text(hjust = 0, face = 'italic')
#   )
# 
# 
# # ?transition_reveal
# 
# animate(animated_mortality_map, 
#         nframes = 450,
#         renderer = gifski_renderer("europe_mortality_map.gif"),
#         height = 8, width = 8, units = 'in',  type = 'cairo-png', res = 200)

##### analysis --- covid response and effectiveness #####


# covid_deaths_by_country_date_diffs$country_factor = factor(covid_deaths_by_country_date_diffs$country, levels = covid_stats_by_country$country)
# 
# filter(covid_deaths_by_country_date_diffs, country %in% head(covid_stats_by_country, 9)$country) %>%
#   ggplot(aes(date, roll_7_new_deaths_per_100k, fill = StringencyIndex)) +
#   geom_bar(stat = 'identity') +
#   facet_wrap(~country_factor, nrow=3) +
#   theme_bw() +
#   scale_y_continuous(limits = c(0, 3)) +
#   scale_fill_viridis_c(option = 'A', name = 'Stringency\nIndex') + 
#   theme(
#     strip.text = element_text(face = 'bold', size = 15),
#     axis.title = element_text(size = 14),
#     axis.text = element_text(size = 12),
#     plot.title = element_text(size = 16),
#     plot.caption = element_text(size = 11, face = 'italic', hjust = 0),
#     plot.subtitle = element_text(size = 11, face = 'italic'),
#     strip.background = element_rect(fill = 'white'),
#     panel.grid = element_blank(),
#     panel.background = element_rect(fill = 'black'),
#     legend.text = element_text(size = 11),
#     legend.title = element_text(size = 12)
#   ) +
#   labs(
#     x = '', y = '7 Day Average of Daily Mortality\nPer 100,000 Population\n',
#     # subtitle = 'The stringency index shows the "strictness" or degree of government response to the COVID pandemic. A higher value means a more significant response, not necessarily a better response.',
#     title = 'COVID Daily Mortality vs. Stringency of Government Response\nTop OECD Countries by Mortality Rate, Minimum 5M Population',
#     caption = 'Chart: Taylor G. White\nData: Johns Hopkins CSSE, Oxford OxCGRT'
#   )
# ggsave('daily_mortality_vs_stringency.png', height = 10, width = 14, units = 'in', dpi = 600)
# 




##### Stats by Region #####

stacked_daily_stats_selected_regions = bind_rows(
  us_daily_stats = filter(covid_deaths_by_country_date_diffs, country %in% c('United States', 'Canada')) %>% select(region = country, roll_7_new_deaths_per_100k, date),
  stats_by_region %>% filter(region %in% c('Europe & Central Asia', 'Latin America & Caribbean')) %>% select(region, roll_7_new_deaths_per_100k = total_new_death_100k_roll_7, date)
)

peak_mortality_dates = group_by(stacked_daily_stats_selected_regions, region) %>%
  summarize(
    mean_mortality = mean(roll_7_new_deaths_per_100k, na.rm = T),
    peak_mortality = max(roll_7_new_deaths_per_100k, na.rm = T),
    peak_mortality_date = min(date[roll_7_new_deaths_per_100k == peak_mortality], na.rm = T)
  ) %>%
  arrange(-mean_mortality)

stacked_daily_stats_selected_regions$region_factor = factor(stacked_daily_stats_selected_regions$region, levels = peak_mortality_dates$region)

ggplot(stacked_daily_stats_selected_regions, aes(date, roll_7_new_deaths_per_100k, colour = region_factor)) +
  geom_line(size = 1) + 
  labs(
    y = '7 Day Average of Daily Mortality\nPer 100k Population', 
    x = '',
    caption = sprintf('Chart: Taylor G. White\nData: Johns Hopkins CSSE\nData through %s', max(stacked_daily_stats_selected_regions$date, na.rm = T) %>% format('%b %d')),
    title = 'COVID-19 Daily Mortality, by Region'
  ) +
  theme_bw() +
  # geom_point(data = peak_mortality_dates, aes(peak_mortality_date, peak_mortality, colour = region), size = 3.5, pch = 18) + 
  theme(
    plot.title = element_text(size = 22),
    plot.subtitle = element_text(size = 14, face = 'italic'),
    plot.caption = element_text(hjust = 0, face = 'italic', size = 11),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 16),
    legend.position = 'bottom'
  ) +
  scale_color_brewer(name = '', palette = 'Set1') +
  # scale_colour_hue() +
  scale_x_date(date_breaks = '1 month', date_labels = '%b', limits = c(as.Date('2020-03-01'), max(stats_by_region$date))) +
  guides(colour = guide_legend(override.aes = list(size = 2.5)))
ggsave('average_daily_mortality_by_region.png', height= 10, width = 12, units = 'in', dpi = 600)



##### growth models #####

region_growth_stats_by_country = lapply(unique(covid_stats_by_country$country), function(the_country){
  the_region = filter(covid_stats_by_country, country == the_country)$region
  stats_for_region_excl_country = filter(covid_stats_by_country, region == the_region, country != the_country)
  mean_proj_for_region = mean(stats_for_region_excl_country$projection_2020, na.rm = T)
  covid_deaths = sum(stats_for_region_excl_country$total_deaths, na.rm = T)
  population = sum(stats_for_region_excl_country$population, na.rm = T)
  region_deaths_per_100k = (covid_deaths / population) * 1e5
  data.frame(
    country = the_country, 
    mean_proj_for_region = mean_proj_for_region,
    region_deaths_per_100k = region_deaths_per_100k
  )
}) %>%
  bind_rows()

covid_stats_by_country = left_join(covid_stats_by_country, region_growth_stats_by_country) %>%
  mutate(
    
  )

options(na.action = na.exclude)
# 
# simple_mod = lm(projection_2020 ~ log(mortality_per_100k_log) + last_year_growth, data = covid_stats_by_country)
# 
# simple_mod_region_deaths = lm(projection_2020 ~ log(mortality_per_100k_log) + log(region_deaths_per_100k) + last_year_growth, data = covid_stats_by_country)
# simple_mod_region_proj = lm(projection_2020 ~ log(mortality_per_100k_log) + last_year_growth + mean_proj_for_region, data = covid_stats_by_country)
# simple_mod_region = lm(projection_2020 ~ log(mortality_per_100k_log) + last_year_growth + region, data = covid_stats_by_country)
# simple_mod_region_income = lm(projection_2020 ~ log(mortality_per_100k_log) + last_year_growth + region + income, data = covid_stats_by_country)
# max_health_index = lm(projection_2020 ~ max_ContainmentHealthIndex + pop_pct_65_over + income + trade_pct_gdp + log(mortality_per_100k_log) + last_year_growth, data = covid_stats_by_country)
# max_stringency = lm(projection_2020 ~ max_stringency + pop_pct_65_over + income + trade_pct_gdp + log(mortality_per_100k_log) + last_year_growth, data = covid_stats_by_country)
# max_stringency_region_proj = lm(projection_2020 ~ max_stringency + pop_pct_65_over + income + trade_pct_gdp * mean_proj_for_region + log(mortality_per_100k_log) + last_year_growth, data = covid_stats_by_country)
# median_health_index = lm(projection_2020 ~ median_ContainmentHealthIndex + pop_pct_65_over + income + trade_pct_gdp + log(mortality_per_100k_log) + last_year_growth, data = covid_stats_by_country)
# median_stringency = lm(projection_2020 ~ median_stringency + pop_pct_65_over + income + trade_pct_gdp + log(mortality_per_100k_log) + last_year_growth, data = covid_stats_by_country)
# 


##### Sweden analysis ##### 
nordics = filter(covid_deaths_by_country_date_diffs, country %in% nordic_countries)
ggplot(nordics, aes(date, roll_7_new_deaths_per_100k, fill = StringencyIndex)) +
  geom_bar(stat = 'identity') +
  geom_line(aes(), show.legend = F, size = 0.75) +
  facet_wrap(~country) +
  scale_fill_viridis_c(option = 'A', name = 'Stringency Index') + 
  theme_bw() +
  labs(
    x = '', y = '7 Day Average of New Deaths\nPer 100k Population'
  )

ggplot(nordics, aes(date, roll_7_new_deaths_per_100k, colour = country)) +
  geom_line(size = 0.75) +
  scale_color_brewer(palette = 'Set1', name = '') +
  theme_bw() +
  labs(
    x = '', y = '7 Day Average of New Deaths\nPer 100k Population', 
    title = 'COVID Daily Mortality'
  ) +
  scale_x_date(date_breaks = '1 month', date_labels = '%b') +
  theme(
    legend.position = 'bottom'
  ) +
  guides(
    colour = guide_legend(override.aes = list(size = 2.5))
  )

ggplot(nordics, aes(date, avg_7_mobility, colour = country)) +
  geom_line(size = 0.75) +
  # geom_point(aes(size = roll_7_new_deaths_per_100k)) +
  scale_color_brewer(palette = 'Set1', name = '') +
  theme_bw() +
  labs(
    x = '', y = '7 Day Average Mobility (Walking)', 
    title = 'Mobility Analysis'
  ) +
  scale_x_date(date_breaks = '1 month', date_labels = '%b') +
  theme(
    legend.position = 'bottom'
  ) +
  guides(
    colour = guide_legend(override.aes = list(size = 2.5))
  )


ggplot(nordics, aes(date, roll_7_new_cases_per_100k, colour = country)) +
  geom_line(size = 0.75) +
  geom_point(aes(size = roll_7_new_deaths_per_100k)) +
  scale_color_brewer(palette = 'Set1', name = '') +
  theme_bw() +
  labs(
    x = '', y = '7 Day Average New Cases\nPer 100k', 
    title = 'Mobility Analysis'
  ) +
  scale_x_date(date_breaks = '1 month', date_labels = '%b') +
  theme(
    legend.position = 'bottom'
  ) +
  guides(
    colour = guide_legend(override.aes = list(size = 2.5))
  )
nordics$roll_7_new_deaths_per_100k

# sub = select(nordics %>% filter(country == 'United States'), roll_7_new_deaths_per_100k, roll_7_new_cases_per_100k) %>% na.omit()
# ccf(sub$roll_7_new_cases_per_100k, sub$roll_7_new_deaths_per_100k)
# sub$roll_7_new_deaths_per_100k
# head(sub)
# tail(sub)
# ccf(x-variable name, y-variable name)

countries_with_covid_phases = map(unique(us_comparator_countries$country), function(the_country){
  # the_country = 'Sweden'
  country_sub = filter(covid_deaths_by_country_date_diffs, country == the_country) %>% 
    select(country, roll_7_new_deaths_per_100k, date) %>% 
    mutate(
      date_num = as.numeric(date - min(date, na.rm = T)) %>% log()
    ) %>% 
    na.omit()
  max_mortality = max(country_sub$roll_7_new_deaths_per_100k)
  
  # country_sub$days_to_max_sq = as.numeric(with(country_sub, date - date[roll_7_new_deaths_per_100k == max_mortality]))^2
  
  the_dat = country_sub %>% 
    select(date_num, roll_7_new_deaths_per_100k) %>% as.data.frame() %>% scale() %>% as.data.frame()
  
  
  # Ward Hierarchical Clustering
  
  # data("multishapes")
  # df <- multishapes[, 1:2]
  # km.res <- kmeans(df, 5, nstart = 25)
  # fviz_cluster(km.res, df, frame = FALSE, geom = "point")
  # db <- fpc::dbscan(df, eps = 0.15, MinPts = 5)
  # plot(db, df, main = "DBSCAN", frame = FALSE)
  
  
  # http://www.sthda.com/english/wiki/wiki.php?id_contents=7940
  
  
  n_groups = 3
  
  d <- dist(the_dat, method = "euclidean") # distance matrix
  fit <- hclust(d, method="ward.D")
  # plot(fit) # display dendogram
  groups <- cutree(fit, k=n_groups) # cut tree into 5 clusters
  # draw dendogram with red borders around the 5 clusters
  # rect.hclust(fit, k=3, border="red")
  sweden_roll_7$groups = groups
  # ggplot(sweden_roll_7, aes(date, roll_7_new_deaths_per_100k, colour = factor(groups))) +
  #   geom_point()
  
  km.res <- kmeans(the_dat, n_groups, nstart = 25)
  
  
  
  # dbscan::kNNdistplot(the_dat, k =  5)
  db = fpc::dbscan(the_dat, 0.4, MinPts = 5)
  
  the_dat$kmeans_cluster = km.res$cluster
  the_dat$hclust_cluster = groups
  the_dat$db_cluster = db$cluster
  
  fin_hclust = hclust(dist(the_dat, method = "euclidean"), method="ward.D")
  fin_groups <- cutree(fin_hclust, k=n_groups)
  country_sub = mutate(country_sub,
                       fin_groups = fin_groups,
                       z_score = (roll_7_new_deaths_per_100k - mean(roll_7_new_deaths_per_100k)) / sd(roll_7_new_deaths_per_100k)
  )
  return(country_sub)
}) %>%
  bind_rows()

stats_by_phase = group_by(countries_with_covid_phases, country, fin_groups) %>%
  summarize(
    phase_time = as.numeric(max(date) - min(date)),
    mean_mortality = mean(roll_7_new_deaths_per_100k)
  ) %>%
  arrange(
    -mean_mortality
  )

country_group_stats = group_by(stats_by_phase, country) %>%
  summarize(
    highest_mortality = max(mean_mortality),
    highest_mortality_phase = fin_groups[mean_mortality == highest_mortality],
    highest_mortality_phase_time = phase_time[mean_mortality == highest_mortality]
  ) %>% 
  ungroup() %>%
  arrange(-highest_mortality)


phase_2_peak_countries = filter(country_group_stats, highest_mortality_phase == 2)
ggplot(phase_2_peak_countries %>% head(12), aes(country, highest_mortality_phase_time)) +
  geom_bar(stat = 'identity') +
  coord_flip()

# ggplot(stats_by_phase, aes(time, mean_mortality, shape = factor(fin_groups), colour = country)) + 
#   geom_point()

selected_countries = countries_with_covid_phases %>% 
  filter(country %in% head(phase_2_peak_countries, 12)$country) 

ggplot(selected_countries, aes(date, roll_7_new_deaths_per_100k)) +
  geom_point(aes(colour = factor(fin_groups))) + 
  facet_wrap(~country, scales = 'free_y') 


##### nordics analysis #####
nordics_daily_stats = filter(covid_deaths_by_country_date_diffs, country %in% nordic_countries) 

nordics_daily_stats$urban_pop_pct
# output helper data 
names(us_comparator_countries)
us_comparator_countries$total_deaths
nordic_table_data = filter(us_comparator_countries, country %in% nordic_countries) %>% 
  select(country, three_year_avg_growth, projection_2020, diff_projection_avg, mortality_per_100k, 
         population, median_stringency, max_stringency, gdp_per_capita_us, trade_pct_gdp, 
         total_deaths,  
         urban_pop_pct, 
         pop_pct_65_over, international_tourism, case_1k_date) %>%  as.data.frame() %>%
  arrange(-diff_projection_avg)



economic_table = select(nordic_table_data, 
                        `IMF Projected Growth, 2020` = projection_2020,
                        `3-Year Avg. Growth` = three_year_avg_growth,
                        `Diff. from Avg. Growth`= diff_projection_avg,
                        `GDP Per Capita` = gdp_per_capita_us,
                        `Annual Tourist\nArrivals (M)` = international_tourism,
                        `Trade/GDP` = trade_pct_gdp
) %>%
  mutate(
    `3-Year Avg. Growth` = percent(`3-Year Avg. Growth`/100, accuracy = 0.1),
    `GDP Per Capita` = dollar(`GDP Per Capita`, accuracy = 1),
    `Diff. from Avg. Growth` = percent(`Diff. from Avg. Growth`/100, accuracy = 0.1),
    `IMF Projected Growth, 2020` = percent(`IMF Projected Growth, 2020`/ 100, accuracy = 0.1),
    `Annual Tourist\nArrivals (M)` = round(`Annual Tourist\nArrivals (M)`/1e6, 1),
    `Trade/GDP` = percent(`Trade/GDP`, accuracy = 1)
  )

row.names(economic_table) = nordic_table_data$country

demographics_table = select(nordic_table_data, 
                            
                            `Population (M)` = population,
                            `COVID Deaths` = total_deaths,
                            `Mortality Per 100k` = mortality_per_100k, 
                            `Max. Stringency` = max_stringency,
                            `Date Cases > 1k` = case_1k_date,
                            `Urban Pop.` = urban_pop_pct,
                            `Pop. Age 65+` = pop_pct_65_over
                            
) %>%
  mutate(
    `Population (M)` = round(`Population (M)`/1e6, 1),
    `COVID Deaths`= comma(`COVID Deaths`),
    `Mortality Per 100k` = round(`Mortality Per 100k`, 1),
    `Urban Pop.` = percent(`Urban Pop.`, accuracy = 0.1),
    `Pop. Age 65+` = percent(`Pop. Age 65+`, accuracy = 0.1),
    `Date Cases > 1k` = format(`Date Cases > 1k`, '%b-%d'),
    `Max. Stringency` = round(`Max. Stringency`, 1)
  )

row.names(demographics_table) = nordic_table_data$country

label_data$peak_deaths_date
peak_deaths_data = filter(covid_stats_by_country, country %in% nordic_countries)
label_data = inner_join(nordics_daily_stats, peak_deaths_data, by = c('country', 'date' = 'peak_deaths_date'))

mortality_plot = ggplot(nordics_daily_stats, aes(date, roll_7_new_deaths_per_100k, colour = country)) +
  theme_bw() +
  
  geom_line(size = 1, show.legend = F) +
  geom_label_repel(data = label_data, aes(date, roll_7_new_deaths_per_100k, label = country), 
                   show.legend = F, size = 4.5) +
  labs(
    y = '7 Day Average of Daily Mortality\nPer 100k Population', 
    x = '',
    # caption = 'Chart: Taylor G. White\nData: Johns Hopkins CSSE',
    title = "Comparing COVID Outcomes Across Nordic Countries"
  ) +
  theme(
    plot.title = element_text(size = 28),
    plot.subtitle  = element_text(hjust = 0, face = 'italic', size = 18),
    plot.caption = element_text(hjust = 0, face = 'italic', size = 13),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18),
    legend.text = element_text(size = 17),
    legend.title = element_text(size = 17)
  ) +
  scale_colour_brewer(palette = 'Set1') +
  scale_x_date(date_breaks = '1 month', date_labels = '%b', limits = c(as.Date('2020-03-01'), max(stats_by_region$date) + 10)) 

ggsave('nodic_mortality_comparison.png', height = 9, width = 12, units = 'in', dpi = 600, plot = mortality_plot)

economic_table_plot = ggplot(data.frame(x = 0:10, y = seq(0, 5, by = 0.5)), aes(x, y)) +
  geom_blank() +
  # theme_nothing() +
  annotation_custom(tableGrob(economic_table), xmin = 0, xmax = 10, ymin = 0.5, ymax = 2) +
  coord_cartesian(ylim = c(1, 1.5), clip = "on") +
  theme(
    plot.subtitle = element_text(size = 24),
    plot.caption = element_text(size = 14, face = 'italic', hjust = 0.5)
  ) 

tt2 = ttheme_default(
  core=list(bg_params = list(fill = c('white', 'lightgray')))
)

combined_tables_plot = ggplot(data.frame(x = 0:10, y = 0:10), aes(x, y)) +
  geom_blank() +
  theme_nothing() +
  annotate('text', x=0, y = 10, label = 'Demographics and COVID Outcomes', size = 5, fontface = 'italic', hjust = 0) +
  annotate('text', x=0, y = 5, label = 'Economic Characteristics and Outcomes', size = 5, fontface = 'italic', hjust = 0) +
  annotation_custom(tableGrob(demographics_table, theme = tt2), xmin = 0, xmax = 10, ymin = 5.5, ymax = 9.5) +
  annotation_custom(tableGrob(economic_table, theme = tt2), xmin = 0, xmax = 10, ymin = 0, ymax = 4.5) +
  theme(
    plot.subtitle = element_text(size = 24),
    plot.caption = element_text(size = 12, face = 'italic', hjust = 0.5)
  ) +
  labs(
    caption = 'Chart: Taylor G. White\nData: Johns Hopkins CSSE, IMF October Outlook, World Bank, Oxford Stringency Index'
  )


combined_mortality_comparison_plot = plot_grid(
  plotlist = list(mortality_plot, combined_tables_plot),
  nrow = 2,
  rel_heights = c(1, 0.85)
)


save_plot('nordic_mortality_comparison_with_tables.png', base_height = 9, base_width = 12, dpi = 600, plot = combined_mortality_comparison_plot)



nordic_table_data %>%
  write.csv('comparison_stats_nordics.csv', row.names = F)

##### case correlations ####

wide_high_income_cases = filter(covid_deaths_by_country_date_diffs, income == 'High income') %>%
  pivot_wider(
    names_from = 'country',
    id_cols = c('date'),
    values_from = c('roll_7_new_cases_per_100k')
  ) %>% na.omit()

cor(wide_high_income_cases %>% select(-date) %>% as.matrix()) %>% write.csv('high_income_countries_case_correlation.csv')
# 
# plot(wide_high_income_cases$Denmark, wide_high_income_cases$Germany)
# plot(wide_high_income_cases$Denmark, wide_high_income_cases$Sweden)
# ccf(wide_high_income_cases$Denmark, wide_high_income_cases$Sweden)
# ccf(wide_high_income_cases$Sweden, wide_high_income_cases$Denmark)
names(wide_high_income_cases) = str_replace_all(names(wide_high_income_cases), ' ', '_')

high_income_countries = names(wide_high_income_cases %>% select(-date))
country_combinations = combn(high_income_countries, 2) %>% t()

# for each country, run a granger causality test against all other countries. Record which countries are granger caused

granger_test_results = map(high_income_countries, function(the_country){
  the_country = 'United_Kingdom'
  other_countries = setdiff(high_income_countries, the_country)
  
  inner_results = map_dbl(other_countries, function(other_country){
    # other_country = other_countries[1]
    test_formula = paste(other_country, the_country, sep = ' ~ ') %>% as.formula()
    grangertest(test_formula, order = 7, data = wide_high_income_cases)$`Pr(>F)`[2]   
  })
  tibble(
    y_country = other_countries, 
    x_country = the_country, 
    p_x_predicts_y = inner_results
  )
  
}) %>%
  bind_rows()

granger_test_result_summary = group_by(granger_test_results, x_country) %>%
  summarize(
    n_countries_predicted = sum(p_x_predicts_y < 0.05)
  ) %>%
  arrange(-n_countries_predicted) %>%
  mutate(
    pct_predicted = n_countries_predicted / length(high_income_countries),
    x_country = str_replace_all(x_country, '_', ' ')
  )


shell('explorer .')
# a = ccf(wide_high_income_cases$Germany, wide_high_income_cases$Austria)
# ccf(wide_high_income_cases$Austria, wide_high_income_cases$Germany)
# ccf(wide_high_income_cases$Netherlands, wide_high_income_cases$France)
# ccf(wide_high_income_cases$Netherlands, wide_high_income_cases$Austria)
# ccf(wide_high_income_cases$Netherlands, wide_high_income_cases$Germany)
# ccf(wide_high_income_cases$Netherlands, wide_high_income_cases$Slovak_Republic)
# ccf(wide_high_income_cases$Netherlands, wide_high_income_cases$Slovenia)


europe_cropped_granger = left_join(europe_cropped, granger_test_result_summary, by = c('name' = 'x_country'))
ggplot(europe_cropped_granger) +
  geom_sf(aes(fill = pct_predicted)) +
  scale_fill_viridis_c(option = 'C')

