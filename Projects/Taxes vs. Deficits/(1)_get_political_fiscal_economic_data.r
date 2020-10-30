
# https://ourcodingclub.github.io/2017/03/15/mixed-models.html
# https://rpsychologist.com/r-guide-longitudinal-lme-lmer
# https://stackoverflow.com/questions/49033016/plm-or-lme4-for-random-and-fixed-effects-model-on-panel-data
# https://www.princeton.edu/~otorres/Panel101R.pdf
# fredr_set_key('d0b9e64aba30b479343a06037a5a10c1')

library(rvest)
library(httr)
library(data.table)
library(tidyverse)
library(WDI)
library(countrycode)
library(lmtest)
library(tseries)
library(plm)
library(rvest)
library(httr)
library(quantmod)
library(fredr)
library(scales)
library(quantreg)

refresh_downloads = FALSE
setwd('~\Public_Policy\Projects\Taxes vs. Deficits\data')

##### Get political data #####

if (refresh_downloads) {
  the_url = 'https://www.presidency.ucsb.edu/statistics/data/house-and-senate-concurrence-with-presidents'
  concurrence_table = html_table(GET(the_url) %>% content(), fill = TRUE)
  concurrence_table[[1]] %>% write.csv('concurrence_with_president.csv')
}

concurrence_with_president_clean = read_csv('concurrence_with_president_clean.csv')

# MIT data lab 
house_elections = read.csv('1976-2018-house.csv') %>% 
  mutate(candidate_votes = as.numeric(candidatevotes)) %>%
  filter(stage == 'gen') %>% data.table()

senate_elections = read.csv('1976-2018-senate.csv') %>% filter(stage == 'gen') %>% data.table()
presidential_elections = read.csv('1976-2016-president.csv') %>% data.table()


##### Combine historical top tax rate data from Brookings and Current OECD Data #####
## Brookings source: 
# http://www.taxpolicycenter.org/sites/default/files/legacy/taxfacts/content/PDF/oecd_historical_toprate.pdf
## OECD source: 
# https://www.oecd.org/tax/tax-policy/tax-database/tax-database-update-note.pdf

# get TOP marginal tax rates

brookings_oecd_toptax = read_csv('oecd_historical_toprate_raw.csv', na = '--') %>%
  pivot_longer(cols = paste(c(1975:2013)), names_to = 'Year', values_to = 'TOP_TRATE')

current_oecd_toptax = read_csv('TABLE_I7_10022020181538921.csv') %>% 
  pivot_wider(id_cols = c('Country', 'Year'), names_from = 'TAX', values_from = 'Value')

# get tax revenues to GDP
all_oecd_downloads = list.files(pattern = 'DP_LIVE') %>% 
  unique() %>%
  map(read_csv) %>% 
  bind_rows() %>% 
  filter(LOCATION != 'OAVG', MEASURE == 'PC_GDP', SUBJECT == 'TOT')

# map country names
country_code_name_mappings = data.frame(
  LOCATION = unique(all_oecd_downloads$LOCATION)
) %>%
  mutate(
    Country = map_chr(LOCATION, function(the_country){
      countrycode(the_country, 'iso3c', 'country.name')
    }),
    Country = recode(Country, `South Korea` = 'Korea')
  ) 

stacked_extra_oecd_stats = inner_join(all_oecd_downloads, country_code_name_mappings)

# check col classes
map_chr(brookings_oecd_toptax, class)
map_chr(current_oecd_toptax, class)

# check if any countries aren't found in either dataset, may need to change names
current_oecd_toptax$Country[!current_oecd_toptax$Country %in% brookings_oecd_toptax$Country] %>% unique()
brookings_oecd_toptax$Country[!brookings_oecd_toptax$Country %in% current_oecd_toptax$Country]  %>% unique()

# check year ranges -- only keep non-overlapping
range(brookings_oecd_toptax$Year)
range(current_oecd_toptax$Year)

# recode columns
brookings_oecd_toptax_clean = mutate(
  brookings_oecd_toptax,
  Country = recode(Country, `Korea, Republic of` = 'Korea'),
  Year = as.numeric(Year), 
  TOP_TRATE = TOP_TRATE * 100 # to match OECD
) %>% 
  filter(
    Year < min(current_oecd_toptax$Year, na.rm = T)
  )
range(brookings_oecd_toptax_clean$Year)

# clear memory
rm(brookings_oecd_toptax)

# outer join on country and year, keeping the year overlaps from oecd
joined_oecd_brookings_toptax = bind_rows(
  brookings_oecd_toptax_clean,
  current_oecd_toptax
) %>%
  arrange(
    Country, Year
  )

# take a look at the data
ggplot(joined_oecd_brookings_toptax, aes(Year, TOP_TRATE, colour = Country)) +
  geom_line()

# lots of missing values in the middle of the dataset, let's interpolate those
joined_oecd_brookings_toptax_dt = data.table(joined_oecd_brookings_toptax)

joined_oecd_brookings_toptax_filled = joined_oecd_brookings_toptax_dt[, {
  # cat("\nCountry ==",.BY[[1]],"\n\n")
  
  non_na_years = Year[!is.na(TOP_TRATE)]
  na_years = Year[is.na(TOP_TRATE)]
  
  # find the most recent existing value
  filled_vals = map_dbl(na_years, function(this_year){
    years_to_check = non_na_years[non_na_years < this_year]
    if (length(years_to_check) == 0) {
      return(NA)
    } else {
      closest_year = max(years_to_check)
      return(TOP_TRATE[Year == closest_year])
    }
  })
  
  filled_df = data.frame(
    Year = na_years,
    TOP_TRATE = filled_vals
  )
  
  full_df = data.frame(
    Year, TOP_TRATE
  ) %>% 
    filter(Year %in% non_na_years)
  
  stacked_df = bind_rows(filled_df, full_df) %>% arrange(Year)
  
  list(
    Year = stacked_df$Year,
    TOP_TRATE = stacked_df$TOP_TRATE
  )
}, by = Country]

# add back other variables
joined_oecd_brookings_toptax_fin = inner_join(
  select(joined_oecd_brookings_toptax, -TOP_TRATE),
  joined_oecd_brookings_toptax_filled
)

rm(joined_oecd_brookings_toptax, joined_oecd_brookings_toptax_dt)


# looks much better now
ggplot(joined_oecd_brookings_toptax_filled, aes(Year, TOP_TRATE, colour = Country)) +
  geom_line()


##### Get fiscal data from the world bank #####
# WDIsearch('debt') %>% View()

start_year = min(joined_oecd_brookings_toptax_filled$Year, na.rm = T)
end_year = max(joined_oecd_brookings_toptax_filled$Year, na.rm = T)

wdi_indicators = c('NY.GDP.PCAP.KD.ZG', 'GC.TAX.TOTL.GD.ZS', 
                   'DT.DOD.DECT.GN.ZS', 'DT.INT.DECT.GN.ZS', 'GC.TAX.YPKG.RV.ZS', 'GC.XPN.INTP.RV.ZS',
                   'GC.REV.TOTL.CD', 'PAY.TAX.RK.DB19', 'NY.GDP.MKTP.KD.ZG', 'NY.GDP.PCAP.KD', 'GC.DOD.TOTL.GD.ZS')

wdi_names = map(wdi_indicators, function(ind){
  WDIsearch(ind, field = 'indicator') %>% t() %>% as.data.frame()
}) %>% 
  bind_rows()

if (refresh_downloads | !file.exists('wdi_download_long.csv')) {
  
  wdi_download = WDI(indicator = wdi_indicators, 
                     start = start_year, 
                     end = end_year, extra = T) %>% 
    filter(
      income == 'High income'
    ) 
  
  wdi_download_long = pivot_longer(wdi_download, cols = wdi_indicators, names_to = 'indicator') %>%
    inner_join(wdi_names) %>%
    mutate(
      country = recode(country, `Korea, Rep.` = 'Korea')
    )
  write.csv(wdi_download_long, 'wdi_download_long.csv', row.names = F)
  
} else {
  wdi_download_long = read_csv('wdi_download_long.csv')
}
# check country names
# wdi_download_long$country[!wdi_download_long$country %in% joined_oecd_brookings_toptax_filled$Country] %>% unique()
# joined_oecd_brookings_toptax_filled$Country[!joined_oecd_brookings_toptax_filled$Country %in% wdi_download_long$country] %>% unique()

##### Combine all world bank and OECD data together #####

stacked_oecd_wdi_data = bind_rows(
  joined_oecd_brookings_toptax_fin %>% mutate(Indicator = 'top_tax_rate') %>% select(Country, Year, Value = TOP_TRATE, Indicator),
  stacked_extra_oecd_stats %>% select(Country, Year = TIME, Value, Indicator = INDICATOR)
) %>%
  bind_rows(
    wdi_download_long %>% select(Country = country, Year = year, Indicator = indicator, Value = value)
  ) %>%
  arrange(
    Country, Indicator, Year
  ) %>%
  filter(!is.na(Country))

# test uniqueness of observations
unique_obs = select(stacked_oecd_wdi_data, Country, Year, Indicator) %>% unique()
stopifnot(nrow(unique_obs) == nrow(stacked_oecd_wdi_data))
stacked_oecd_wdi_data_dt = data.table(stacked_oecd_wdi_data)
duped_dt = stacked_oecd_wdi_data_dt[duplicated(stacked_oecd_wdi_data_dt, by = c('Country', 'Year', 'Indicator')),]
inner_join(stacked_oecd_wdi_data, duped_dt) %>% dim()

##### Compute new variables ##### 
# for each country and indicator, compute: 
# Lags 
# First differences
stacked_oecd_wdi_data_dt = data.table(stacked_oecd_wdi_data)
stacked_oecd_wdi_data_lags_diffs = stacked_oecd_wdi_data_dt[, {
  last_value = dplyr::lag(Value, 1)
  diff_value = Value - last_value
  lag_diff_value = dplyr::lag(diff_value, 1)
  
  pct_change = diff_value / last_value
  lag_pct_change = dplyr::lag(pct_change, 1)
  
  
  list(
    Year = Year,
    value = Value, # lowercase now
    last_value = last_value, 
    diff_value = diff_value, 
    pct_change = pct_change,
    lag_pct_change = lag_pct_change,
    lag_diff_value = lag_diff_value
  )
    
}, by = list(Country, Indicator)]

wide_oecd_wdi_data = pivot_wider(
  stacked_oecd_wdi_data_lags_diffs, 
                                 id_cols = c('Country', 'Year'),
  names_from = 'Indicator', 
  values_from = c('value', 'last_value', 'diff_value', 'pct_change', 'lag_pct_change', 'lag_diff_value')
  ) %>%
  arrange(
    Country, Year
  )

write.csv(stacked_oecd_wdi_data_lags_diffs, 'stacked_oecd_wdi_data_lags_diffs.csv', row.names = F)
write.csv(wide_oecd_wdi_data, 'wide_oecd_wdi_data.csv', row.names = F)

