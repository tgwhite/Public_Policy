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


setwd("~/Public_Policy/Projects/COVID-19")
nordics = c('Sweden', 'Finland', 'Norway', 'Denmark')
top_europe = c('Spain', 'United Kingdom', 'Italy', 'France', 'Germany', 'Belgium')

##### get map data #####
world <- ne_countries(scale = "medium", returnclass = "sf")
europe = filter(world, continent == 'Europe') %>%
  mutate(
    name = recode(name, `Czech Rep.` = 'Czech Republic', 
                  `Slovakia` = 'Slovak Republic',
                  `Bosnia and Herz.` = 'Bosnia and Herzegovina')
  )
europe_cropped <- st_crop(europe, xmin = -24, xmax = 45,
                          ymin = 30, ymax = 73)


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
    country = recode(country, `Korea, Rep.` = 'South Korea', `Russian Federation` = 'Russia')
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
  ) 


deaths_by_country_province = group_by(jh_with_pop, country, province_state) %>%
  summarize(
    max_cases = max(cases),
    max_deaths = max(deaths),
    last_date = max(date_upd)
  )

covid_deaths_by_country_date = group_by(jh_with_pop, country, province_state, date_upd) %>%
  summarize(
    cumulative_cases = max(cases),
    cumulative_deaths = sum(deaths)
  ) %>%
  group_by(country, date_upd) %>%
  summarize(
    cumulative_cases = sum(cumulative_cases),
    cumulative_deaths = sum(cumulative_deaths)
  ) %>%
  arrange(country, date) %>%
  data.table()


covid_deaths_by_country_date_diffs = covid_deaths_by_country_date[, {
  
  list(
    date = date, 
    new_cases = c(NA, diff(total_cases)),
    new_deaths = c(NA, diff(total_deaths)),
    total_cases = total_cases, 
    total_deaths = total_deaths
  )
  
}, by = list(country)] %>%
  full_join(
    latest_country_pop
  ) %>%
  mutate(
    new_deaths_per_100k = (new_deaths / population) * 1e5,
    new_cases_per_100k = (new_cases / population) * 1e5,
    mortality_rate = total_deaths / population,
    mortality_per_100k = mortality_rate * 1e5
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
    mortality_rate = total_deaths / population,
    mortality_per_100k = mortality_rate * 1e5
  ) %>% 
  select(-year, -obs) %>%
  arrange(-mortality_rate) %>%
  mutate(
    country_mort = factor(country, levels = country)
  ) %>%
  filter(
    population >= 1e5
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

country_stringency = filter(oxford_stringency_index, stringency_geo_type == 'country')


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
  ) %>%
  left_join(
    country_stringency, by = c('entity_name', 'date')
  ) %>%
  mutate(
    entity_name = recode(entity_name, `Korea, South` = 'South Korea')
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
                                           year == 2020  
                                             # & month_date <= most_common_months_latest_data$last_month[1] &
                                             # country %in% selected_ur_countries$country
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
  arrange(-mean_index) 

## for each country, calculate UR index since December 2019


##### US Debt #####

# https://fiscaldata.treasury.gov/datasets/monthly-statement-public-debt/summary-of-treasury-securities-outstanding



##### analysis -- growth and mortality rankings #####

latest_jh_data_with_growth = left_join(
  covid_deaths_by_country, 
  filter(quarterly_gdp, year_qtr == max(year_qtr)) %>% rename(qtr_gdp_change = Value_Pct) ) %>%
  left_join(covid_ur_indexes) %>%
  filter(
    !is.na(mortality_rate) & !is.na(qtr_gdp_change),
    country != 'China'
  ) %>%
  mutate(
    mortality_rank = cume_dist(-mortality_rate),
    gdp_rank = cume_dist(qtr_gdp_change),
    overall_rank = mortality_rank * gdp_rank
  ) %>%
  arrange(-overall_rank) %>%
  mutate(
    country_ranked_overall = factor(country, levels = rev(country))
  ) %>%
  arrange(
    -mortality_rank
  ) %>%
  mutate(
    country_ranked_mortality = factor(country, levels = rev(country))
  ) %>%
  arrange(
    -gdp_rank
  ) %>%
  mutate(
    country_ranked_gdp = factor(country, levels = rev(country))
  )


setwd("~/Public_Policy/Projects/COVID-19 Mismanagement/output")

median_growth = median(latest_jh_data_with_growth$qtr_gdp_change)
median_mortality = median(latest_jh_data_with_growth$mortality_rate)


mortality_rank_plot = ggplot(latest_jh_data_with_growth, aes(country_ranked_mortality, mortality_rate, fill = mortality_rate)) +
  geom_bar(stat = 'identity', colour = 'gray') +
  scale_y_continuous(labels = comma) +
  scale_fill_viridis_c(option = 'A', labels = comma, name = 'Mortality Rate') +
  coord_flip() +
  theme_bw() +
  labs(x = '', y = '\nCOVID-19 Mortality Rate\n(Deaths / 100k Population)') +
  geom_hline(aes(yintercept = median_mortality*1e5), linetype = 'dashed', colour = 'gray', size = 0.75) +
  theme(legend.position = 'right', panel.grid.minor = element_blank()) +
  annotate('text', x = nrow(latest_jh_data_with_growth), y = median_mortality*1e5, label = paste('Median:', comma(median_mortality)), fontface = 'bold')
mortality_rank_plot

gdp_rank_plot = ggplot(latest_jh_data_with_growth, aes(country_ranked_gdp, qtr_gdp_change, fill = qtr_gdp_change)) +
  geom_bar(stat = 'identity', colour = 'gray') +
  coord_flip() +
  theme_bw() +
  scale_y_continuous(labels = percent) +
  labs(x = '', y = '\nQ2 GDP Change from Prior Period') +
  scale_fill_viridis_c(option = 'A', direction = -1, labels = function(x) {percent(x, accuracy = 0.1)}, name = 'Q2 GDP Change') +
  geom_hline(aes(yintercept = median_growth), linetype = 'dashed', colour = 'gray', size = 0.75) +
  theme(legend.position = 'right', panel.grid.minor = element_blank()) +
  annotate('text', x = nrow(latest_jh_data_with_growth), y = median_growth, label = paste('Median:', percent(median_growth, accuracy = 0.1)), fontface = 'bold')
gdp_rank_plot

overall_rank_plot = ggplot(latest_jh_data_with_growth, aes(country_ranked_overall, overall_rank, fill = overall_rank)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(labels = percent) +
  scale_fill_viridis_c(option = 'A', direction = -1) +
  coord_flip()


combined_plot = plot_grid(mortality_rank_plot, gdp_rank_plot)
save_plot('mortality_growth_comparison_oecd.png', base_height = 10, base_width = 18, 
          units = 'in', dpi = 600, plot = combined_plot)
shell('explorer .')


head(latest_jh_data_with_growth)
filter(latest_jh_data_with_growth, !is.na(mortality_rate)) %>% pull(country) %>% n_distinct()
filter(latest_jh_data_with_growth, !is.na(qtr_gdp_change) & !is.na(mortality_rate)) %>% pull(country) %>% n_distinct()
filter(latest_jh_data_with_growth, !is.na(qtr_gdp_change) & !is.na(mean_index)) %>% pull(country) %>% n_distinct()

View(latest_jh_data_with_growth)




ggplot(latest_jh_data_with_growth %>% filter(country != 'China'), aes(qtr_gdp_change, mortality_rate)) +
  # geom_point(aes(size = population)) +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  geom_text(aes(label = country, size = population), show.legend = F) +
  geom_hline(aes(yintercept = median_mortality), linetype = 'dashed', colour = 'firebrick', size = 0.75) +
  geom_vline(aes(xintercept = median_growth), linetype = 'dashed', colour = 'firebrick', size = 0.75) +
  scale_color_viridis_c() +
  # geom_hline(aes(yintercept = 0)) +
  # geom_vline(aes(xintercept = 0)) +
  scale_size( range = c(3, 12)) +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(labels = percent) +
  labs(
    y = 'COVID-19 Mortality Rate\n(Deaths / Population)', 
    x = 'Q2 GDP Growth'
  ) +
  stat_smooth(method = 'lm', se = F) +
  coord_cartesian(ylim = c(-.00025, 0.001))





nordics = c('Sweden', 'Finland', 'Norway', 'Denmark')
selected_countries = c('Sweden', 'Finland', 'Norway', 'Denmark', 'Germany', 'United Kingdom', 'Italy', 'Spain', 'France')

europe_map_data = left_join(europe_cropped, covid_deaths_by_country, by = c('name' = 'country'))
europe_cropped

nordics_quarterly_gdp = filter(quarterly_gdp, country %in% nordics, year >= 2018)
nordics_monthly_unemployment_rate  = filter(monthly_2020_unemployment_rate_dt_indexes, country %in% nordics)


ggplot(nordics_quarterly_gdp, aes(year_qtr, Value_Pct, colour = country)) +
  theme_bw() +
  geom_hline(aes(yintercept = 0)) +
  geom_line(size = 0.75) +
  geom_vline(aes(xintercept = as.yearqtr(paste(2020, 1, sep = '-'), format = '%Y-%q')), colour = 'firebrick', size = 0.75, linetype = 'dashed') +
  # geom_point() +
  labs(
    y = 'Quarterly GDP Change (% from prior period)\n',
    x = '\nYear - Quarter',
    title = 'Quarterly Economic Growth',
    subtitle = 'Selected European Countries, 2018-2020'
  ) + 
  scale_colour_hue(name = 'Country') +
  scale_x_yearqtr(breaks = unique(nordics_quarterly_gdp$year_qtr)) + 
  geom_text(data = filter(nordics_quarterly_gdp, year_qtr == max(year_qtr)), aes(label=country), size = 4, show.legend = F) +
  # geom_point(data = filter(nordics_quarterly_gdp, year == 2020)) +
  # geom_text(data = filter(nordics_quarterly_gdp, year == 2020), aes(label = percent(Value_Pct, accuracy = 0.1))) +
  scale_y_continuous(labels = percent)

ggsave('nordic_economic_growth_comparison.png', height = 8, width = 10, units = 'in', dpi=600)

ggplot(nordics_monthly_unemployment_rate, aes(month_date , val_index-1, colour = country)) +
  geom_line(size = 0.75) +
  geom_point() +
  labs(
    y = 'Percent Change in Unemployment Rate\n(Relative January 2020)\n',
    x = '\nMonth',
    title = ''
  ) +
  geom_text(data = filter(nordics_monthly_unemployment_rate, month_date == max(month_date)), aes(label=country), size = 4, show.legend = F) +
  scale_x_date(date_labels = '%b', date_breaks = '1 month') +
  scale_y_continuous(labels = percent)

ggplot(nordics_monthly_unemployment_rate, aes(month_date , Value_Pct, colour = country)) +
  geom_line(size = 0.75) +
  geom_point() +
  scale_y_continuous(labels = percent)
head(nordics_monthly_unemployment_rate)


head(europe_quarterly_gdp)


ggplot(europe_map_data) +
  geom_sf(aes(fill = mortality_per_100k)) +
  scale_fill_viridis_c(name = 'Deaths Per\n100k Pop.',option = 'A') +
  theme_map() +
  # theme_dark() +
  # theme_minimal() +
  geom_sf_label(data = filter(europe_map_data, name %in% selected_countries), aes(label = paste0(name, '\n', comma(mortality_per_100k, accuracy = 0.1))), size = 2.5) +
  labs(
    x = '', y = '', 
    title = 'COVID-19 Mortality Rates in Selected European Countries',
    subtitle = sprintf('Data through %s', max(covid_deaths_by_country$as_of_date, na.rm=T) %>% format('%b %d, %Y')),
    caption = 'Chart: Taylor G. White\nData: Johns Hopkins CSSE, World Bank'
  ) +
  theme(
    # axis.text = element_blank(),
    plot.subtitle = element_text(face = 'italic'),
    plot.caption = element_text(hjust = 0, face = 'italic')
  )
# dir.create('~/Public_Policy/Projects/COVID-19 Mismanagement/output')
ggsave('europe_mortality_rate_map.png', height = 9, width = 12, units = 'in', dpi = 600)
