
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
refresh_downloads = FALSE


# countrycode('AUS','iso3c', 'iso.name.en')

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
oecd_income_tax_gdp = read_csv('DP_LIVE_11022020161332254.csv') %>% 
  filter(LOCATION != 'OAVG', MEASURE == 'PC_GDP')

oecd_corp_tax_gdp = read_csv('DP_LIVE_11022020161427862.csv')  %>% 
  filter(LOCATION != 'OAVG', MEASURE == 'PC_GDP')

oecd_gen_gov_debt_gdp = read_csv('DP_LIVE_11022020211431425.csv') %>%
  filter(LOCATION != 'OAVG', MEASURE == 'PC_GDP')

oecd_gen_gov_deficit  = read_csv('DP_LIVE_11022020213043902.csv') %>%
  filter(LOCATION != 'OAVG', MEASURE == 'PC_GDP')

# map country names
country_code_name_mappings = data.frame(
  LOCATION = unique(oecd_income_tax_gdp$LOCATION)
) %>%
  mutate(
    Country = map_chr(LOCATION, function(the_country){
      countrycode(the_country, 'iso3c', 'country.name')
    }),
    Country = recode(Country, `South Korea` = 'Korea')
  ) 

oecd_income_tax_gdp_clean = inner_join(oecd_income_tax_gdp, country_code_name_mappings)
oecd_corp_tax_gdp_clean = inner_join(oecd_corp_tax_gdp, country_code_name_mappings)
oecd_gen_gov_debt_gdp_clean = inner_join(oecd_gen_gov_debt_gdp, country_code_name_mappings)
oecd_gen_gov_deficit_clean = inner_join(oecd_gen_gov_deficit, country_code_name_mappings)

stacked_extra_oecd_stats = bind_rows(oecd_income_tax_gdp_clean, 
                                     oecd_corp_tax_gdp_clean, oecd_gen_gov_debt_gdp_clean,
                                     oecd_gen_gov_deficit_clean)

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
WDIsearch('debt') %>% View()

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
wdi_download_long$country[!wdi_download_long$country %in% joined_oecd_brookings_toptax_filled$Country] %>% unique()
joined_oecd_brookings_toptax_filled$Country[!joined_oecd_brookings_toptax_filled$Country %in% wdi_download_long$country] %>% unique()

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
  )

# test uniqueness of observations
unique_obs = select(stacked_oecd_wdi_data, Country, Year, Indicator) %>% unique()
stopifnot(nrow(unique_obs) == nrow(stacked_oecd_wdi_data))

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

##### Simple panel data models ##### 
oecd_wdi_pdata = pdata.frame(wide_oecd_wdi_data, index = c('Country', 'Year'))

pooling_model = plm(diff_value_TAXINCOME ~ diff_value_NY.GDP.PCAP.KD.ZG, model = 'pooling', data = oecd_wdi_pdata)
fixed_model = plm(diff_value_TAXINCOME ~ diff_value_NY.GDP.PCAP.KD.ZG, model = 'within', data = oecd_wdi_pdata)
random_model = plm(diff_value_TAXINCOME ~ diff_value_NY.GDP.PCAP.KD.ZG, model = 'random', data = oecd_wdi_pdata)
fixed_time_model = plm(diff_value_TAXINCOME ~ diff_value_NY.GDP.PCAP.KD.ZG + factor(Year), model = 'within', data = oecd_wdi_pdata)


summary(fixed_model)
summary(random_model)

# check whether to use random effects. <0.05 then use fixed
phtest(fixed_model, random_model)

# check for time based effects -- there are significant time based effects
pFtest(fixed_time_model, fixed_model)
plmtest(fixed_model, c("time"), type=("bp"))

# check for panel effects  -- there are time based but not individual effects
plmtest(pooling_model, type=c("bp"))


# check for cross-sectional dependence
pcdtest(fixed_model, test = c("lm")) # there is cross sectional dependence
pcdtest(fixed_model, test = c("cd")) # there is cross sectional dependence

# test for serial correlation 
pbgtest(fixed_model) # there is serial correlation 

# test for unit roots
adf.test(filter(oecd_wdi_pdata, !is.na(diff_value_TAXINCOME))$diff_value_TAXINCOME, k=1) # none present

# check for heteroskedacity
bptest(diff_value_TAXINCOME ~ diff_value_NY.GDP.PCAP.KD.ZG, data = oecd_wdi_pdata, studentize=F)
coeftest(fixed_model, vcovHC(fixed_model, method = "arellano"))

##### Examine US data #####

us_real_gdp_per_capita = fredr('A939RX0Q048SBEA', aggregation_method = 'eop', frequency = 'a', units = 'pch') %>%
  rename(value_real_per_capita_gdp_growth = value) %>%
  mutate(
    Year = year(date),
    lag_value_real_per_capita_gdp_growth = dplyr::lag(value_real_per_capita_gdp_growth, 1)
  ) %>% 
select(-date, -series_id)

recession_years = fredr('JHDUSRGDPBR', aggregation_method = 'sum', frequency = 'a') %>%
  rename(
    n_recession_quarters = value
  ) %>% 
  mutate(
    Year = year(date),
    pct_of_year_in_recession = n_recession_quarters / 4,
    recession_year = n_recession_quarters > 0
  ) %>%
  select(-date, -series_id)


concurrence_with_president_clean = read_csv('concurrence_with_president_clean.csv')

US_wide = filter(wide_oecd_wdi_data, Country == 'United States') %>%
  left_join(concurrence_with_president_clean) %>%
  left_join(us_real_gdp_per_capita) %>%
  left_join(recession_years) %>%
  mutate(
    real_gdp_per_capita_z = (value_real_per_capita_gdp_growth - mean(value_real_per_capita_gdp_growth)) / sd(value_real_per_capita_gdp_growth)
  )

US_long = filter(stacked_oecd_wdi_data_lags_diffs, Country == 'United States')

US_sub = filter(US_long, Indicator %in% c('top_tax_rate', 'TAXINCOME', 'NY.GDP.PCAP.KD.ZG'))
ggplot(US_sub, aes(Year, value)) +
  facet_wrap(~Indicator, scales = 'free_y', ncol = 1) +
  geom_line()

ggplot(US_wide, aes(Year)) +
  geom_line(aes(y = diff_value_TAXINCOME), size = 1)  +
  geom_line(aes(y = value_real_per_capita_gdp_growth), colour = 'red') +
  geom_line(aes(y = diff_value_top_tax_rate), colour = 'blue') +
  geom_line(aes(y = diff_value_GGDEBT), colour = 'orange') +
  geom_line(aes(y = diff_value_GC.DOD.TOTL.GD.ZS), linetype = 'dashed', colour = 'purple4') +
  geom_line(aes(y = -value_GGNLEND), linetype = 'dotted', size = 1.5, colour = 'steelblue') +
  geom_line(aes(y = total_hs_concurrence), size = 2)

options(na.action = na.exclude)  
deficit_vs_growth_mod = lm(value_GGNLEND ~ value_NY.GDP.MKTP.KD.ZG, data = US_wide)
US_wide$deficit_residuals = residuals(deficit_vs_growth_mod)

ggplot(US_wide, aes(value_real_per_capita_gdp_growth, -diff_value_GGNLEND)) +
  geom_point(aes(shape = president_party)) +
  stat_smooth() +
  stat_smooth(method = 'lm') 

ggplot(US_wide, aes(value_real_per_capita_gdp_growth, -diff_value_GGNLEND)) +
  geom_point(aes(shape = president_party)) +
  stat_smooth() +
  stat_smooth(method = 'lm') 


ggplot(US_wide, aes(real_gdp_per_capita_z, -diff_value_GGNLEND, colour = recession_year)) +
  geom_point(aes(shape = president_party, size = pct_of_year_in_recession)) +
  stat_smooth(method = 'lm', se = F) 

ggplot(US_wide, aes(real_gdp_per_capita_z, -diff_value_GGNLEND, colour = president_party)) +
  geom_point(aes(shape = recession_year, size = pct_of_year_in_recession)) +
  stat_smooth(method = 'lm', se = F) +
  scale_colour_manual(
    name = "President's Party",
    values = c('DEM' = 'blue', 'REP' = 'red')
  ) +
  labs(
    y = 'Annual Deficit (% of GDP)',
    y = 'Real GDP Per Capita Growth\nStandard Deviations (Z Value)'
  )


ggplot(US_wide, aes(total_hs_concurrence, y = -deficit_residuals)) +
  geom_point(aes(colour = unified_government, shape = president_party)) +
  stat_smooth() +
  stat_smooth(method = 'lm')

ggplot(US_wide, aes(Year, y = n_recession_quarters)) +
  geom_bar(stat = 'identity')

ggplot(US_wide, aes(diff_value_NY.GDP.MKTP.KD.ZG, total_hs_concurrence)) +
  geom_point() +
  stat_smooth()


##### Get election results #####
house_elections = read.csv('1976-2018-house.csv') %>% 
  mutate(candidate_votes = as.numeric(candidatevotes)) %>%
  filter(stage == 'gen') %>% data.table()

senate_elections = read.csv('1976-2018-senate.csv') %>% filter(stage == 'gen') %>% data.table()
presidential_elections = read.csv('1976-2016-president.csv') %>% data.table()


# count up the members by party for each election to determine control 

head(concurrence_with_president_clean)



district_winners = house_elections[, {
  candidate_winner = candidate[candidate_votes == max(candidate_votes)]
  party_winner = party[candidate == candidate_winner]
  obs = length(party_winner)
  
  list(
    total_votes = totalvotes[1],
    candidate_winner = candidate_winner,
    party_winner = party_winner,
    obs = obs
  )
  
}, by = list(year, state, district)]
head(district_winners)

table(district_winners$obs)
filter(district_winners, obs > 3)
# 
# the_url = 'https://www.presidency.ucsb.edu/statistics/data/house-and-senate-concurrence-with-presidents'
# concurrence_table = html_table(GET(the_url) %>% content(), fill = TRUE)
# concurrence_table[[1]] %>% write.csv('concurrence_with_president.csv')
