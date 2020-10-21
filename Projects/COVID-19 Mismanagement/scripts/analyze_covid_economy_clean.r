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
library(RcppRoll)
library(gganimate)
library(gifski)
library(readxl)
library(sf)

library(RColorBrewer)
par(mar=c(3,4,2,2))
display.brewer.all()

#### use the imf projections for growth 


setwd("~/Public_Policy/Projects/COVID-19")
nordics = c('Sweden', 'Finland', 'Norway', 'Denmark')
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
  'NY.GDP.PCAP.CD'
  # 'SP.POP.65UP.TO.ZS', 'SP.URB.TOTL.IN.ZS',
  #                  'SP.URB.MCTY.UR.ZS', 'SH.STA.ACCH.ZS',
  #                  'SH.MED.NURS.ZS', 'SH.STA.DIAB.ZS',
  #                  'SP.POP.65UP.TO.ZS', 'SP.POP.TOTL', 'SP.POP.LAND.ZS', 'SH.XPD.PCAP',
  #                  'SH.MED.CMHW.P3', 'SH.XPD.OOPC.CH.ZS', 'SH.XPD.CHEX.GD.ZS'
)
wdi_descriptions = lapply(wdi_indicators, function(x){
  
  a = WDIsearch(string = x, field = 'indicator', short = T) %>%
    as.data.frame() %>%
    mutate(
      orig_indicator = x
    )
  class(a)
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

wdi_data_stacked = bind_rows(WDI_data_long) 
wdi_data_wide = pivot_wider(wdi_data_stacked, id_cols = c('country', 'year', 'income', 'region'), values_from = 'value', names_from = 'indicator') %>%
  arrange(country, year)


latest_country_pop = filter(wdi_data_wide) %>%
  group_by(country, income, region) %>%
  summarize(
    latest_pop_year = max(year[!is.na(SP.POP.TOTL)], na.rm = T),
    population = SP.POP.TOTL[year == latest_pop_year],
    trade_pct_gdp = tail(NE.TRD.GNFS.ZS[!is.na(NE.TRD.GNFS.ZS)], 1) / 100,
    gdp_per_capita_us = tail(NY.GDP.PCAP.CD[!is.na(NY.GDP.PCAP.CD)], 1),
    pop_pct_65_over = tail(SP.POP.65UP.TO.ZS[!is.na(SP.POP.65UP.TO.ZS)], 1) / 100
  ) %>%
  rename(
    year = latest_pop_year
  ) %>%
  left_join(world, by = c('country'= 'name')) %>%
  mutate(
    country = recode(country, `Korea, Rep.` = 'South Korea', `Russian Federation` = 'Russia')
  )

View(latest_country_pop)

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
head(country_stringency)

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
View(jh_joined)

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
  arrange(country, date) %>%
  mutate(
    Stringency_z = (StringencyIndex - mean(StringencyIndex, na.rm = T)) / sd(StringencyIndex, na.rm = T),
    Stringency_median_over_iqr = (StringencyIndex - median(StringencyIndex, na.rm = T)) / IQR(StringencyIndex ,na.rm = T)
  ) %>%
  data.table() 

covid_deaths_by_country_date_diffs = covid_deaths_by_country_date[, {
  new_deaths = c(NA, diff(cumulative_deaths))
  death_50_date = date[cumulative_deaths >= 50][1]
  days_since_death_50_date = as.numeric(date - death_50_date)
  
  
  list(
    days_since_death_50_date = days_since_death_50_date, 
    date = date, 
    new_cases = c(NA, diff(cumulative_cases)),
    new_deaths = new_deaths,
    new_deaths_pct_of_max = new_deaths / max(new_deaths, na.rm = T),
    roll_7_new_deaths = c(rep(NA, 6), roll_mean(new_deaths, 7)),
    cumulative_cases = cumulative_cases, 
    cumulative_deaths = cumulative_deaths,
    ContainmentHealthIndex  = ContainmentHealthIndex , 
    EconomicSupportIndex = EconomicSupportIndex ,
    GovernmentResponseIndex = GovernmentResponseIndex , 
    StringencyIndex = StringencyIndex ,
    one_week_stringency = c(rep(NA, 6), roll_mean(StringencyIndex, 7)),
    two_week_stringency = c(rep(NA, 13), roll_mean(StringencyIndex, 14)),
    Stringency_z = Stringency_z,
    Stringency_median_over_iqr = Stringency_median_over_iqr,
    StringencyIndex_percentile = cume_dist(StringencyIndex),
    change_StringencyIndex = c(NA, diff(StringencyIndex, 1)),
    mobility = mobility,
    avg_7_mobility = c(rep(NA, 6), roll_mean(mobility, 7)),
    daily_cumulative_deaths_percent_of_total = cumulative_cases / max(cumulative_cases)
  )
  
}, by = list(country)] %>%
  left_join(latest_country_pop) %>%
  mutate(
    new_deaths_per_100k = (new_deaths / population) * 1e5,
    roll_7_new_deaths_per_100k = (roll_7_new_deaths / population) * 1e5,
    new_cases_per_100k = (new_cases / population) * 1e5,
    mortality_rate = cumulative_deaths / population,
    mortality_per_100k = mortality_rate * 1e5
  ) 
# 
# 
# covid_deaths_by_country = group_by(deaths_by_country_province, country) %>%
#   summarize(
#     obs = n(),
#     total_deaths = sum(max_deaths, na.rm = T),
#     as_of_date = max(last_date)
#   ) %>%
#   arrange(-total_deaths) %>%
#   full_join(
#     latest_country_pop
#   ) %>%
#   mutate(
#     mortality_rate = total_deaths / population,
#     mortality_per_100k = mortality_rate * 1e5
#   ) %>% 
#   select(-year, -obs) %>%
#   arrange(-mortality_rate) %>%
#   mutate(
#     country_mort = factor(country, levels = country)
#   ) %>%
#   filter(
#     population >= 1e5
#   ) 

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

imf_real_gdp_projections = read_excel("IMF october projections data.xlsx", 'all countries projections') %>% 
  mutate_all(function(x){
    y = str_replace(x, '-', '-')
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


##### US Debt #####

# https://fiscaldata.treasury.gov/datasets/monthly-statement-public-debt/summary-of-treasury-securities-outstanding



##### analysis -- growth and mortality rankings #####
# 
# latest_jh_data_with_growth = left_join(
#   covid_deaths_by_country, 
#   filter(quarterly_gdp, year_qtr == max(year_qtr)) %>% rename(qtr_gdp_change = Value_Pct) ) %>%
#   left_join(covid_ur_indexes) %>%
#   filter(
#     !is.na(mortality_rate) & !is.na(qtr_gdp_change),
#     country != 'China'
#   ) %>%
#   mutate(
#     mortality_rank = cume_dist(-mortality_rate),
#     gdp_rank = cume_dist(qtr_gdp_change),
#     overall_rank = mortality_rank * gdp_rank
#   ) %>%
#   arrange(-overall_rank) %>%
#   mutate(
#     country_ranked_overall = factor(country, levels = rev(country))
#   ) %>%
#   arrange(
#     -mortality_rank
#   ) %>%
#   mutate(
#     country_ranked_mortality = factor(country, levels = rev(country))
#   ) %>%
#   arrange(
#     -gdp_rank
#   ) %>%
#   mutate(
#     country_ranked_gdp = factor(country, levels = rev(country))
#   )


covid_stats_by_country = 
  covid_deaths_by_country_date_diffs %>%
  group_by(country, income) %>%
  summarize(
    as_of_date = max(date, na.rm = T),
    total_deaths = max(cumulative_deaths, na.rm = T),
    mean_mobility = mean(mobility, na.rm = T),
    median_mobility = median(mobility, na.rm = T),
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
    country_factor = factor(country, levels = rev(country))
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
  filter(population > 5e6, projection_2020 >= -15) %>%
  mutate(
    three_year_avg_growth = (`2019` + `2018` + `2017` ) / 3,
    two_year_avg_growth = (`2019` + `2018`) / 2,
    last_year_growth = `2019`,
    
    diff_projection_avg = projection_2020 - three_year_avg_growth
  ) %>%
  arrange(-diff_projection_avg) %>%
  mutate(
    country_ranked_gdp = factor(country, levels = rev(country)),
  ) %>%
  arrange(-gdp_per_capita_us)

covid_stats_by_country$geometry = NULL

##### rank plots #####
setwd("~/Public_Policy/Projects/COVID-19 Mismanagement/output")

# us_comparator_countries = filter(covid_stats_by_country,  str_detect(economy, 'G7') | str_detect(economy, 'Emerging') | income == 'High income', !is.na(projection_2020))
us_comparator_countries = head(covid_stats_by_country, 30)


growth_sd = sd(us_comparator_countries$projection_2020)
growth_mean = mean(us_comparator_countries$projection_2020)
mortality_iqr = IQR(us_comparator_countries$mortality_per_100k)
mortality_median = median(us_comparator_countries$mortality_per_100k)

us_comparator_countries = mutate(us_comparator_countries, 
                                 growth_z = (projection_2020 - growth_mean) / growth_sd,
                                 mortality_iqr_over_median = (mortality_per_100k - mortality_median) / mortality_iqr 
                                 )
us_comparator_countries$growth_z = us_comparator_countries$projection_2020 


# selected_comparison_vars = select(covid_stats_by_country, three_year_avg_growth, gdp_per_capita_us, population, trade_pct_gdp, pop_pct_65_over) %>% 
#   mutate(gdp_per_capita_us = log(gdp_per_capita_us),
#          population = log(population)) %>% 
#   as.data.frame()
# 
# filter(covid_stats_by_country, str_detect(economy, 'G7') | str_detect(economy, 'Emerging'))$country
# 
# row.names(selected_comparison_vars) = covid_stats_by_country$country
# selected_comparison_vars_clean = na.omit(selected_comparison_vars)

# d <- dist(selected_comparison_vars_clean, method = "euclidean") # distance matrix
# fit <- hclust(d, method="ward")
# plot(fit) # display dendogram
# groups <- cutree(fit, k=3) # cut tree into 5 clusters

# cluster countries by: 
# income
# trade 
# population

# Ward Hierarchical Clustering



# draw dendogram with red borders around the 5 clusters
# rect.hclust(fit, k=3, border="red")

# larger european countries
# latin america
# japan, south korea

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

covid_stats_by_country = left_join(covid_stats_by_country, region_growth_stats_by_country)

min_mortality_not_zero = covid_stats_by_country$mortality_per_100k[covid_stats_by_country$mortality_per_100k > 0] %>% min()
covid_stats_by_country$mortality_per_100k_log = ifelse(covid_stats_by_country$mortality_per_100k == 0, min_mortality_not_zero, covid_stats_by_country$mortality_per_100k)

options(na.action = na.exclude)

ggplot(covid_stats_by_country, aes(log(mortality_per_100k_log), projection_2020)) +
  geom_point() +
  stat_smooth(method = 'lm')


simple_mod = lm(projection_2020 ~ log(mortality_per_100k_log) + last_year_growth, data = covid_stats_by_country)

simple_mod_region_deaths = lm(projection_2020 ~ log(mortality_per_100k_log) + log(region_deaths_per_100k) + last_year_growth, data = covid_stats_by_country)
simple_mod_region_proj = lm(projection_2020 ~ log(mortality_per_100k_log) + last_year_growth + mean_proj_for_region, data = covid_stats_by_country)
simple_mod_region = lm(projection_2020 ~ log(mortality_per_100k_log) + last_year_growth + region, data = covid_stats_by_country)
simple_mod_region_income = lm(projection_2020 ~ log(mortality_per_100k_log) + last_year_growth + region + income, data = covid_stats_by_country)
max_health_index = lm(projection_2020 ~ max_ContainmentHealthIndex + pop_pct_65_over + income + trade_pct_gdp + log(mortality_per_100k_log) + last_year_growth, data = covid_stats_by_country)
max_stringency = lm(projection_2020 ~ max_stringency + pop_pct_65_over + income + trade_pct_gdp + log(mortality_per_100k_log) + last_year_growth, data = covid_stats_by_country)
max_stringency_region_proj = lm(projection_2020 ~ max_stringency + pop_pct_65_over + income + trade_pct_gdp * mean_proj_for_region + log(mortality_per_100k_log) + last_year_growth, data = covid_stats_by_country)
median_health_index = lm(projection_2020 ~ median_ContainmentHealthIndex + pop_pct_65_over + income + trade_pct_gdp + log(mortality_per_100k_log) + last_year_growth, data = covid_stats_by_country)
median_stringency = lm(projection_2020 ~ median_stringency + pop_pct_65_over + income + trade_pct_gdp + log(mortality_per_100k_log) + last_year_growth, data = covid_stats_by_country)

# rank countries by growth as a percent of average 

summary(max_stringency)

anova(simple_mod, simple_mod_region_proj, max_health_index, max_stringency, max_stringency_region_proj, median_health_index, median_stringency)


median_econ_impact = median(us_comparator_countries$diff_projection_avg, na.rm = T)
median_mortality = median(us_comparator_countries$mortality_per_100k, na.rm = T)

us_data = filter(us_comparator_countries, country == 'United States')
us_calcs = us_data %>% select(mortality_per_100k, diff_projection_avg) %>% summarise_all(median)

us_calcs$mortality_per_100k / median_mortality
us_calcs$diff_projection_avg / median_econ_impact

superior_countries = filter(us_comparator_countries, mortality_per_100k <= us_calcs$mortality_per_100k & diff_projection_avg >= us_calcs$diff_projection_avg, country != 'United States')
length(superior_countries$country) / nrow(us_comparator_countries)

ggplot(covid_stats_by_country, aes(mortality_per_100k, diff_projection_avg)) +
  geom_point() +
  stat_smooth()


ggplot(covid_stats_by_country, aes(projection_2020)) +
  geom_histogram() + 
  geom_vline(aes(xintercept = us_data$projection_2020)) + 
  scale_x_continuous(limits = c(-15, 5))


mortality_rank_plot = ggplot(us_comparator_countries, aes(country_ranked_mortality, mortality_per_100k, fill = mortality_per_100k)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  geom_bar(data = filter(us_comparator_countries, country == 'United States'), fill = 'firebrick', stat = 'identity') +
  scale_y_continuous(labels = comma) +
  coord_flip() +
  theme_bw() +
  labs(
    title = 'COVID Mortality Rate',
    subtitle = 'High Income Countries, Minimum 5M Population',
    caption = 'Chart: Taylor G. White\nData: IMF October Economic Outlook, Johns Hopkins CSSE',
    x = '', y = '\nCOVID-19 Mortality Rate\n(Deaths / 100k Population)') +
  geom_hline(aes(yintercept = median_mortality), colour = 'darkslategray', size = 0.75) +
  theme(
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 17),
    plot.caption = element_text(size = 13, face = 'italic', hjust = 0),
    axis.text = element_text(size = 17),
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 14),
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
    title = 'Economic Impact of COVID',
    subtitle = 'High Income Countries, Minimum 5M Population',
    caption = '\n',
    x = '', y = '\nReal GDP Growth\nDifference from Three Year Average') +
  geom_hline(aes(yintercept = median_econ_impact/100), colour = 'darkslategray', size = 0.75) +
  theme(
    plot.title = element_text(size = 20),
    plot.caption = element_text(size = 13, face = 'italic', hjust = 0),
    axis.title = element_text(size = 17),
    axis.text = element_text(size = 17),
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.position = 'right', 
    panel.grid.minor = element_blank()
  ) 


# 
# overall_rank_plot = ggplot(latest_jh_data_with_growth, aes(country_ranked_overall, overall_rank, fill = overall_rank)) +
#   geom_bar(stat = 'identity') +
#   scale_y_continuous(labels = percent) +
#   scale_fill_viridis_c(option = 'A', direction = -1) +
#   coord_flip()


combined_plot = plot_grid(mortality_rank_plot, gdp_rank_plot)
save_plot('mortality_growth_comparison_oecd.png', base_height = 14, base_width = 18, 
          units = 'in', dpi = 600, plot = combined_plot)


covid_stats_by_country$country %>% table()
europe_map_data = left_join(europe_cropped, covid_stats_by_country, by = c('name' = 'country'))
filter(europe_map_data, is.na(mortality_per_100k))$name

selected_european_countries = c('Sweden', 'Denmark', 'Finland', 'Norway', 'Germany', 'Italy', 'Spain', 'France', 'United Kingdom', 'Ireland')
ggplot(europe_map_data) +
  geom_sf(aes(fill = mortality_per_100k)) +
  scale_fill_viridis_c(name = 'Deaths Per\n100k Pop.',option = 'A') +
  theme_map() +
  # theme_dark() +
  # theme_minimal() +
  geom_sf_label(data = filter(europe_map_data, name %in% selected_european_countries), aes(label = paste0(name, '\n', comma(mortality_per_100k, accuracy = 0.1))), size = 2.5) +
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
# dir.create('~/Public_Policy/Projects/COVID-19 Mismanagement/output')
ggsave('europe_mortality_rate_map.png', height = 9, width = 12, units = 'in', dpi = 600)

date_seq = seq.Date(min(europe_map_data_daily$date, na.rm = T), max(europe_map_data_daily$date, na.rm = T), by = 7)

europe_map_data_daily = left_join(europe_cropped, covid_deaths_by_country_date_diffs, by = c('name' = 'country')) 

animated_mortality_map = 
  ggplot(europe_map_data_daily) +
  geom_sf(aes(fill = mortality_per_100k)) +
  transition_time(date, range = as.Date(c('2020-02-01', '2020-08-01'))) +
  scale_fill_viridis_c(name = 'Deaths Per\n100k Pop.',option = 'A') +
  theme_map() +
  # theme_dark() +
  # theme_minimal() +
  # geom_sf_label(data = filter(europe_map_data, name %in% selected_countries), aes(label = paste0(name, '\n', comma(mortality_per_100k, accuracy = 0.1))), size = 2.5) +
  labs(
    x = '', y = '', 
    title = 'COVID-19 Mortality Rates in Selected European Countries',
    subtitle = sprintf('Data through {frame_time}'),
    caption = 'Chart: Taylor G. White\nData: Johns Hopkins CSSE, World Bank'
  ) +
  theme(
    # axis.text = element_blank(),
    plot.subtitle = element_text(face = 'italic'),
    plot.caption = element_text(hjust = 0, face = 'italic')
  )


# ?transition_reveal

animate(animated_mortality_map, 
        nframes = 450,
        renderer = gifski_renderer("europe_mortality_map.gif"),
        height = 8, width = 8, units = 'in',  type = 'cairo-png', res = 200)

##### analysis --- covid response and effectiveness #####
head(covid_deaths_by_country_date_diffs)


covid_deaths_by_country_date_diffs$country_factor = factor(covid_deaths_by_country_date_diffs$country, levels = covid_stats_by_country$country)

filter(covid_deaths_by_country_date_diffs, country %in% head(covid_stats_by_country, 9)$country) %>%
  ggplot(aes(date, roll_7_new_deaths_per_100k, fill = StringencyIndex)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~country_factor, nrow=3) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 3)) +
  scale_fill_viridis_c(option = 'A', name = 'Stringency\nIndex') + 
  theme(
    strip.text = element_text(face = 'bold', size = 15),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 16),
    plot.caption = element_text(size = 11, face = 'italic', hjust = 0),
    plot.subtitle = element_text(size = 11, face = 'italic'),
    strip.background = element_rect(fill = 'white'),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = 'black'),
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 12)
  ) +
  labs(
    x = '', y = '7 Day Average of Daily Mortality\nPer 100,000 Population\n',
    # subtitle = 'The stringency index shows the "strictness" or degree of government response to the COVID pandemic. A higher value means a more significant response, not necessarily a better response.',
    title = 'COVID Daily Mortality vs. Stringency of Government Response\nTop OECD Countries by Mortality Rate, Minimum 5M Population',
    caption = 'Chart: Taylor G. White\nData: Johns Hopkins CSSE, Oxford OxCGRT'
  )
ggsave('daily_mortality_vs_stringency.png', height = 10, width = 14, units = 'in', dpi = 600)





##### Stats by Region #####


peak_mortality_dates



stacked_daily_stats_selected_regions = bind_rows(
  us_daily_stats = filter(covid_deaths_by_country_date_diffs, country %in% c('United States', 'Canada')) %>% select(region = country, roll_7_new_deaths_per_100k, date),
  stats_by_region %>% filter(region %in% c('Europe & Central Asia', 'Latin America & Caribbean')) %>% select(region, roll_7_new_deaths_per_100k = total_new_death_100k_roll_7, date)
)

peak_mortality_dates = group_by(stacked_daily_stats_selected_regions, region) %>%
  summarize(
    peak_mortality = max(roll_7_new_deaths_per_100k, na.rm = T),
    peak_mortality_date = min(date[roll_7_new_deaths_per_100k == peak_mortality], na.rm = T)
  )

ggplot(stacked_daily_stats_selected_regions, aes(date, roll_7_new_deaths_per_100k, colour = region)) +
  geom_line(size = 1) + 
  labs(
    y = '7 Day Average of Daily Mortality\nPer 100k Population', 
    x = '',
    caption = 'Chart: Taylor G. White\nData: Johns Hopkins CSSE',
    title = 'COVID-19 Daily Mortality, by Region'
  ) +
  theme_bw() +
  # geom_point(data = peak_mortality_dates, aes(peak_mortality_date, peak_mortality, colour = region), size = 3.5, pch = 18) + 
  theme(
    plot.title = element_text(size = 18),
    plot.caption = element_text(hjust = 0, face = 'italic', size = 11),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 15),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 15)
  ) +
  scale_color_brewer(name = 'Region / Country', palette = 'Set1') +
  # scale_colour_hue() +
  scale_x_date(date_breaks = '1 month', date_labels = '%b', limits = c(as.Date('2020-03-01'), max(stats_by_region$date))) +
  guides(colour = guide_legend(override.aes = list(size = 2.5)))
ggsave('average_daily_mortality_by_region.png', height= 10, width = 12, units = 'in', dpi = 600)
