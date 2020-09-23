library(tidyverse)
library(WDI)
library(data.table)
library(countrycode)
library(zoo)
library(scales)
library(ggforce)
library(viridisLite)

# yearqtr('2020-1', format = '%Y-%q')
# a = as.yearqtr("2001 Q3")
# as.Date(a)

# countrycode(sourcevar = 'South Korea', destination = 'iso3c', origin = 'un.name.en')

##### Population data #####
wdi_indicators = c(
  'SP.POP.TOTL'
  # 'SP.POP.65UP.TO.ZS', 'SP.URB.TOTL.IN.ZS',
  #                  'SP.URB.MCTY.UR.ZS', 'SH.STA.ACCH.ZS',
  #                  'SH.MED.NURS.ZS', 'SH.STA.DIAB.ZS',
  #                  'SP.POP.65UP.TO.ZS', 'SP.POP.TOTL', 'SP.POP.LAND.ZS', 'SH.XPD.PCAP',
  #                  'SH.MED.CMHW.P3', 'SH.XPD.OOPC.CH.ZS', 'SH.XPD.CHEX.GD.ZS'
)
wdi_descriptions = map(wdi_indicators, function(x){
  
  a = WDIsearch(string = x, field = 'indicator', short = F) %>%
    as.data.frame() %>%
    mutate(
      orig_indicator = x
    )
  return(a)
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
  left_join(wdi_descriptions) %>% 
  select(-matches('V[0-9]'), -description, -contains('source'))

latest_country_pop = filter(wdi_data_stacked, indicator == 'SP.POP.TOTL', region != 'Aggregates') %>%
  group_by(country, income, region) %>%
  summarize(
    latest_year = max(year[!is.na(value)]),
    population = value[year == latest_year]
  ) %>%
  rename(
    year = latest_year
  ) %>%
  mutate(
    country = recode(country, `Korea, Rep.` = 'South Korea')
  )

##### Covid data #####

##### pull in data ####
johns_hopkins_cases = read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv') %>%
  pivot_longer(cols = matches('^([0-9])'), names_to = 'date', values_to = 'cases')

johns_hopkins_deaths = read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv') %>%
  pivot_longer(cols = matches('^([0-9])'), names_to = 'date', values_to = 'deaths')

# johns_hopkins_recovered = read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv') %>%
#   pivot_longer(cols = matches('^([0-9])'), names_to = 'date', values_to = 'recovered')

jh_joined = left_join(johns_hopkins_cases, johns_hopkins_deaths) %>%
  mutate(
    date_upd = as.Date(date, format = '%m/%d/%y')
  ) 

names(jh_joined) = names(jh_joined) %>% tolower() %>% str_replace('[\\/]', '_')


jh_with_pop = mutate(jh_joined, is_country = is.na(province_state)) %>%
  rename(country = country_region) %>%
  arrange(province_state, country, date_upd) %>%
  mutate(
    country = recode(country, US = 'United States', `Korea, South` = 'South Korea', )
  ) 

deaths_by_country_province = group_by(jh_with_pop, country, province_state) %>%
  summarize(
    last_deaths = tail(deaths, 1),
    max_deaths = max(deaths),
    last_date = max(date_upd)
  )

covid_deaths_by_country = group_by(deaths_by_country_province, country) %>%
  summarize(
    obs = n(),
    total_deaths = sum(max_deaths, na.rm = T),
    as_of_date = max(last_date)
  ) %>%
  arrange(-total_deaths) %>%
  full_join(
    latest_country_pop
  ) %>%
  mutate(
    mortality_rate = total_deaths / population
  ) %>% 
  select(-year, -obs) %>%
  arrange(-mortality_rate) %>%
  mutate(
    country_mort = factor(country, levels = country)
  ) %>%
  filter(
    population >= 1e5
  )



ggplot(covid_deaths_by_country %>% head(50), aes(country_mort, mortality_rate, fill = income)) +
  geom_bar(stat = 'identity') +
  coord_flip()


##### OECD Data #####

setwd("~/Public_Policy/Projects/COVID-19 Mismanagement/data/OECD")

## MEASURE -- PC_CHGPY -- SAME PERIOD PRIOR YEAR
## PC_CHGPP 
quarterly_gdp = fread('quarterly_gdp.csv') %>% 
  filter(FREQUENCY == 'Q', SUBJECT == 'TOT', MEASURE == 'PC_CHGPY') %>%
  mutate(
    Value_Pct = Value / 100,
    quarter = str_extract(TIME, 'Q[0-9]{1}') %>% str_remove('Q') %>% as.numeric(),
    year = str_extract(TIME, '[0-9]{4}') %>% as.numeric(),
    year_qtr = as.yearqtr(paste(year, quarter, sep = '-'), format = '%Y-%q'),
    country = countrycode(LOCATION, origin = 'iso3c', destination = 'country.name')
  )

table(quarterly_gdp$LOCATION)
filter(quarterly_gdp, LOCATION %in% c('USA', 'FIN', 'SWE', 'DNK', 'GBR', 'DEU', 'ITA', 'ESP'), year >= 2019) %>%
  ggplot(aes(year_qtr, Value, colour = LOCATION)) +
  geom_line(size = 1, show.legend  = F) +
  geom_text(data = filter(quarterly_gdp, year_qtr == max(year_qtr), LOCATION %in% c('USA', 'FIN', 'SWE', 'DNK', 'GBR', 'DEU', 'ITA', 'ESP')), aes(label = LOCATION), show.legend  = F)


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
View(last_month_by_country)

most_common_months_latest_data = 
  last_month_by_country %>%
  group_by(last_month) %>%
  summarize(obs = n()) %>%
  arrange(-obs)

selected_countries = filter(last_month_by_country, last_month >= most_common_months_latest_data$last_month[1])

monthly_2020_unemployment_rate_dt = filter(monthly_unemployment_rate, 
                                           year == 2020 & month_date <= most_common_months_latest_data$last_month[1] &
                                             country %in% selected_countries$country) %>%
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

ggplot(monthly_2020_unemployment_rate_dt_indexes, aes(month_date, val_index, colour = country)) +
  geom_line() +
  geom_point() +
  geom_text(data = filter(monthly_2020_unemployment_rate_dt_indexes, is_last_date), aes(label = country))


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
  arrange(-mean_index) 

## for each country, calculate UR index since December 2019


##### US Debt #####

# https://fiscaldata.treasury.gov/datasets/monthly-statement-public-debt/summary-of-treasury-securities-outstanding


latest_jh_data_with_growth = inner_join(
  latest_jh_data, 
  filter(quarterly_gdp, year_qtr == max(year_qtr)) %>% rename(qtr_gdp_change = Value_Pct) ) %>%
  inner_join(covid_ur_indexes)


ggplot(latest_jh_data_with_growth, aes(qtr_gdp_change, mortality_rate)) +
  geom_point(aes(size = mean_index-1, colour = mean_index-1)) +
  geom_text(aes(label = country)) +
  scale_color_viridis_c() +
  # geom_hline(aes(yintercept = 0)) +
  # geom_vline(aes(xintercept = 0)) +
  scale_size(labels = percent, range = c(1.5, 12)) +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(labels = percent) +
  labs(
    y = 'COVID-19 Mortality Rate\n(Deaths / Population)', 
    x = 'Q2 GDP Growth'
  )
?geom_hline
