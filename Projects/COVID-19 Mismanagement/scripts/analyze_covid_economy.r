(tidyverse)
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
sum(latest_country_pop$population)


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
  new_deaths = c(NA, diff(total_deaths))
  death_50_date = date[total_deaths >= 50][1]
  days_since_death_50_date = as.numeric(date - death_50_date)
  
  list(
    days_since_death_50_date = days_since_death_50_date, 
    date = date, 
    new_cases = c(NA, diff(total_cases)),
    new_deaths = new_deaths,
    new_deaths_pct_of_max = new_deaths / max(new_deaths, na.rm = T),
    roll_7_new_deaths = c(rep(NA, 6), roll_mean(new_deaths, 7)),
    total_cases = total_cases, 
    total_deaths = total_deaths,
    daily_cumulative_deaths_percent_of_total = total_cases / max(total_cases)
  )
  
}, by = list(country)] %>%
  full_join(
    latest_country_pop
  ) %>%
  mutate(
    new_deaths_per_100k = (new_deaths / population) * 1e5,
    roll_7_new_deaths_per_100k = (roll_7_new_deaths / population) * 1e5,
    new_cases_per_100k = (new_cases / population) * 1e5,
    mortality_rate = total_deaths / population,
    mortality_per_100k = mortality_rate * 1e5
  ) %>%
  left_join(country_stringency)




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
median_mortality = median(latest_jh_data_with_growth$mortality_rate) * 1e5


mortality_rank_plot = ggplot(latest_jh_data_with_growth, aes(country_ranked_mortality, mortality_per_100k, fill = mortality_per_100k)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  scale_y_continuous(labels = comma) +
  # scale_fill_viridis_c(option = 'A', labels = comma, name = 'Mortality Rate') +
  coord_flip() +
  theme_bw() +
  labs(x = '', y = '\nCOVID-19 Mortality Rate\n(Deaths / 100k Population)') +
  geom_hline(aes(yintercept = median_mortality), colour = 'darkslategray', size = 0.75) +
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 15),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13),
    legend.position = 'right', 
    panel.grid.minor = element_blank()
    ) +
  annotate('text', x = nrow(latest_jh_data_with_growth), y = median_mortality, label = paste('Median:', comma(median_mortality)), fontface = 'bold', size = 6)
mortality_rank_plot

gdp_rank_plot = ggplot(latest_jh_data_with_growth, aes(country_ranked_gdp, qtr_gdp_change, fill = qtr_gdp_change)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  coord_flip() +
  theme_bw() +
  scale_y_continuous(labels = percent) +
  labs(x = '', y = '\nQ2 Economic Growth\nChange from Prior Period') +
  # scale_fill_viridis_c(option = 'A', direction = 1, labels = function(x) {percent(x, accuracy = 0.1)}, name = 'Q2 GDP Change') +
  geom_hline(aes(yintercept = median_growth), colour = 'darkslategray', size = 0.75) +
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 15),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13),
    legend.position = 'right', 
    panel.grid.minor = element_blank()) +
  annotate('text', x = nrow(latest_jh_data_with_growth), y = median_growth, label = paste('Median:', percent(median_growth, accuracy = 0.1)), fontface = 'bold', size = 6)
gdp_rank_plot
# 
# overall_rank_plot = ggplot(latest_jh_data_with_growth, aes(country_ranked_overall, overall_rank, fill = overall_rank)) +
#   geom_bar(stat = 'identity') +
#   scale_y_continuous(labels = percent) +
#   scale_fill_viridis_c(option = 'A', direction = -1) +
#   coord_flip()


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


##### analysis --- covid response and effectiveness #####
covid_stats_by_country = 
  covid_deaths_by_country_date_diffs %>%
  filter(population > 10e6) %>%
  group_by(country, income) %>%
  summarize(
    max_stringency = max(StringencyIndex, na.rm = T),
    median_stringency = median(StringencyIndex, na.rm = T),
    mean_stringency = mean(StringencyIndex, na.rm = T),
    mean_new_deaths_pct_of_max = mean(new_deaths_pct_of_max, na.rm = T),
    mean_new_deaths_per_100k = mean(new_deaths_per_100k, na.rm = T),
    mortality_per_100k = max(mortality_per_100k)
  ) %>%
  arrange(-mortality_per_100k) %>%
  mutate(
    country_factor = factor(country, levels = rev(country))
  ) %>%
  filter(
    country %in% unique(latest_jh_data_with_growth$country)
  )

head(covid_stats_by_country)

covid_deaths_by_country_date_diffs$country_factor = factor(covid_deaths_by_country_date_diffs$country, levels = covid_stats_by_country$country)

filter(covid_deaths_by_country_date_diffs, country %in% head(covid_stats_by_country, 10)$country) %>%
  ggplot(aes(date, roll_7_new_deaths_per_100k, fill = StringencyIndex)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~country_factor) +
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
    subtitle = 'The stringency index shows the "strictness" or degree of government response to the COVID pandemic. A higher value means a more significant response, not necessarily a better response.',
    title = 'COVID Daily Mortality vs. Stringency of Government Response\nTop 10 OECD Countries by Mortality Rate, Minimum 5M Population',
    caption = 'Chart: Taylor G. White\nData: Johns Hopkins CSSE, Oxford OxCGRT'
  )
ggsave('daily_mortality_vs_stringency.png', height = 10, width = 14, units = 'in', dpi = 600)


ggplot(covid_deaths_by_country_date_diffs, aes(roll_7_new_deaths_per_100k, StringencyIndex)) +
  geom_point()
head(covid_deaths_by_country_date_diffs)

ggplot(covid_stats_by_country, aes(mean_new_deaths_pct_of_max, median_stringency, colour = income)) +
  geom_point(aes(size = mortality_per_100k))
             
ggplot(covid_stats_by_country, aes(mean_new_deaths_pct_of_max, mean_new_deaths_per_100k, colour = income)) +
  geom_point(aes(size = mortality_per_100k))

ggplot(covid_stats_by_country, aes(mean_new_deaths_pct_of_max, mortality_per_100k, colour = income)) +
  geom_point(aes())

ggplot(covid_stats_by_country %>% head(50), aes(country_factor, mean_new_deaths_per_100k)) +
  geom_bar(stat = 'identity') +
  coord_flip()

filter(covid_deaths_by_country_date_diffs, country %in% head(covid_stats_by_country$country, 10)) %>%
  ggplot(aes(date, roll_7_new_deaths_per_100k)) +
  facet_wrap(~country, ncol = 2) +
  geom_bar(stat = 'identity') +
  coord_cartesian(ylim = c(0, 3))

head(covid_deaths_by_country_date_diffs)

filter(covid_deaths_by_country_date_diffs, country %in% c('Italy', 'Germany', 'Sweden', 'United States', 'Japan', 'South Korea')) %>%
  ggplot(aes(date, daily_cumulative_deaths_percent_of_total, colour = new_deaths_per_100k)) +
  facet_wrap(~country, ncol = 2) +
  geom_line(size = 1) +
  scale_color_viridis_c(option = 'A') +
  geom_point() +
  scale_size(range = c(1, 10)) +
  geom_hline(aes(yintercept = 0.5))


filter(covid_deaths_by_country_date_diffs, country %in% c('Italy', 'Germany', 'Sweden', 'United States', 'Japan', 'South Korea', 'Finland', 'Norway'), total_deaths > 100) %>%
  ggplot(aes(date, daily_cumulative_deaths_percent_of_total, colour = new_deaths_pct_of_max)) +
  facet_wrap(~country, ncol = 2) +
  geom_line(size = 1) +
  scale_color_viridis_c(option = 'A') +
  geom_point() +
  scale_size(range = c(1, 10)) +
  geom_hline(aes(yintercept = 0.5)) +
  stat_smooth(method = 'lm')

filter(covid_deaths_by_country_date_diffs, country %in% c('Italy', 'Germany', 'Sweden', 'United States', 'Japan', 'South Korea')) %>%
  ggplot(aes(date, new_deaths_per_100k, fill = new_deaths_per_100k)) +
  facet_wrap(~country, ncol = 2) +
  geom_bar(stat = 'identity') +
  scale_fill_viridis_c(option = 'A')

head(covid_deaths_by_country_date_diffs )
filter(covid_deaths_by_country_date_diffs, country %in% c('Italy', 'Germany', 'Sweden', 'United States', 'Japan', 'South Korea')) %>%
  ggplot(aes(date, new_deaths_pct_of_max , fill = new_deaths_per_100k )) +
  facet_wrap(~country, ncol = 2) +
  geom_bar(stat = 'identity') +
  scale_fill_viridis_c(option = 'A')

filter(covid_deaths_by_country_date_diffs, country %in% c('Sweden', 'Denmark', 'Norway', 'Finland')) %>%
  ggplot(aes(date, new_deaths_per_100k , fill = new_deaths_per_100k )) +
  facet_wrap(~country, ncol = 2) +
  geom_bar(stat = 'identity') +
  scale_fill_viridis_c(option = 'A')


filter(covid_deaths_by_country_date_diffs, country %in% c('United States', 'Germany', 'Canada', 'United Kingdom', 'Italy', 'Sweden', 'Brazil', 'Spain', 'Mexico')) %>%
  ggplot(aes(date, new_deaths_per_100k , fill = new_deaths_per_100k )) +
  facet_wrap(~country, ncol = 2) +
  geom_bar(stat = 'identity') +
  scale_fill_viridis_c(option = 'A') +
  scale_y_continuous(limits = c(0, 2))



filter(covid_deaths_by_country_date_diffs, country %in% c('United States', 'Germany', 'Canada', 'United Kingdom', 'Italy', 'Sweden', 'Brazil', 'Spain', 'Mexico')) %>%
  ggplot(aes(date, roll_7_new_deaths_per_100k  , fill = roll_7_new_deaths_per_100k  )) +
  facet_wrap(~country, ncol = 2) +
  theme_bw() + 
  geom_bar(stat = 'identity') +
  scale_fill_viridis_c(option = 'A') +
  scale_y_continuous(limits = c(0, 2))
ggsave('test_plot.png', height = 8, width = 10, units = 'in', dpi = 600)


group_by(covid_deaths_by_country_date_diffs, country) %>%
  summarize(
    mean_new_deaths_pct_of_max = mean(new_deaths_pct_of_max, na.rm = T),
    mortality_rate = max(total_deaths, na.rm = T) / population[1]
  ) %>%
  arrange(-mean_new_deaths_pct_of_max) %>%
  ggplot(aes(mean_new_deaths_pct_of_max, mortality_rate)) +
  geom_point() +
  stat_smooth()


covid_deaths_by_country_date_diffs %>%
  filter(country %in% latest_jh_data_with_growth$country) %>%
ggplot(aes(days_since_death_50_date, roll_7_new_deaths_per_100k  , group = country )) +
  geom_line() + 
  geom_line(data = filter(covid_deaths_by_country_date_diffs, country %in%  c('United States', 'Sweden')), aes(colour = country), size = 1) +
  theme_bw() + 
  scale_y_continuous(limits = c(0, 3)) + 
  scale_x_continuous(limits = c(0, 250))


#### computing rolling seven counts by income ####

stats_by_income_excl_us = covid_deaths_by_country_date_diffs[country != 'United States' & !is.na(income), {
  the_countries = unique(country)
  latest_country_pop_sub = filter(latest_country_pop, country %in% the_countries)
  total_pop = sum(latest_country_pop_sub$population, na.rm = T)
  
  data.frame(roll_7_new_deaths, days_since_death_50_date, country) %>%
    group_by(days_since_death_50_date) %>%
    summarize(
      total_roll_7_new_deaths = sum(roll_7_new_deaths, na.rm = T)
    ) %>%
    mutate(
      n_countries = n_distinct(country),
      total_roll_7_new_deaths_100k = (total_roll_7_new_deaths / total_pop) * 1e5,
      total_pop = total_pop
    ) %>% 
    data.table()
  
}, by = income]

head(covid_deaths_by_country_date_diffs)

high_income_not_us = filter(latest_country_pop, income == 'High income', country != 'United States')
high_income_pop = sum(high_income_not_us$population)
head(covid_deaths_by_country_date_diffs)
daily_avg_mortality_high_income_not_us = filter(covid_deaths_by_country_date_diffs, country %in% high_income_not_us$country) %>%
  group_by(days_since_death_50_date) %>%
  summarize(
    roll_7_new_deaths_total = sum(roll_7_new_deaths, na.rm = T)
  ) %>%
  mutate(
    roll_7_new_deaths_total_100k = (roll_7_new_deaths_total / high_income_pop) * 1e5
  )

ggplot() +
  geom_line(data = stats_by_income_excl_us, aes(days_since_death_50_date, total_roll_7_new_deaths_100k, colour = income)) + 
  geom_line(data = covid_deaths_by_country_date_diffs %>% filter(country %in% c('United States', 'Brazil', 'Sweden', 'Germany')), aes(days_since_death_50_date, roll_7_new_deaths_per_100k, group = country), linetype = 'dashed') +
  scale_x_continuous(limits = c(0, 180))


  


# countries that have a "nagging cold" vs those that didn't
