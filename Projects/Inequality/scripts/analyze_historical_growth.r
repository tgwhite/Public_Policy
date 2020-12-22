library(tidyverse)
library(data.table)
library(readxl)
library(scales)
setwd("~/Public_Policy/Projects/Inequality/data")

polity_v = read_excel( "polity iv data.xls" )

us = filter(polity_v, country %in% c('United States', 'United Kingdom', 'Argentina', 'Botswana'))
ggplot(us, aes(year, polity2)) +
  facet_wrap(~country) +
  geom_ribbon(stat = 'identity', aes(fill = polity2)) +
  scale_fill_gradient2(low  ='red', high = 'blue', mid = 'gray')


historical_gdp_stats = read_excel("angus maddison historical gdp statistics.xlsx", 'Full data') %>% 
  arrange(countrycode, year) %>%
  data.table()


historical_gdp_stats_growth = historical_gdp_stats[, {
  
  the_dat = data.frame(year, cgdppc, rgdpnapc, pop)
  
  full_year_df = data.frame(
    year = min(year, na.rm = T):max(year, na.rm = T)
  ) %>%
    left_join(
      the_dat
    ) %>%
    mutate(
      real_gdp_cap_growth = (rgdpnapc - lag(rgdpnapc, 1))/ lag(rgdpnapc, 1)
    )
  
  full_year_df
  
}, by = list(countrycode, country)] %>%
  right_join(
    polity_v,
    by = c('country', 'year')
  ) %>%
  mutate(
    polity2 = ifelse(polity2 %in% c(-88, -66), NA, polity2)
  )



annual_gdp_cap_leaders = group_by(historical_gdp_stats_growth, year) %>%
  summarize(
    polity_median = median(polity2, na.rm = T),
    polity_90th_percentile = quantile(polity2, probs = 0.9, na.rm = T),
    annual_income_25th_percentile = quantile(cgdppc, probs = 0.25, na.rm = T),
    annual_income_median = quantile(cgdppc, probs = 0.5, na.rm = T),
    annual_income_75th_percentile = quantile(cgdppc, probs = 0.75, na.rm = T),
    annual_income_90th_percentile = quantile(cgdppc, probs = 0.9, na.rm = T)
  ) %>%
  mutate(
    top_median_ratio = annual_income_90th_percentile / annual_income_median
  )

ggplot(annual_gdp_cap_leaders, aes(polity_median, log(annual_income_median))) +
  geom_point() + 
  stat_smooth(method = 'lm')

ggplot(annual_gdp_cap_leaders, aes(polity_90th_percentile, log(annual_income_90th_percentile))) +
  geom_point() + 
  stat_smooth(method = 'lm')


ggplot(annual_gdp_cap_leaders, aes(year, top_median_ratio)) +
  geom_bar(stat = 'identity')


annual_gdp_cap_leaders %>% 
  pivot_longer(cols = c('annual_income_90th_percentile', 'annual_income_median', 'annual_income_75th_percentile', 'annual_income_25th_percentile')) %>%
ggplot(aes(year, colour = name)) +
  geom_line(
    aes(y = value)
  ) +
  scale_colour_brewer(palette = 'Set1', labels = c('annual_income_90th_percentile' = '90%', 'annual_income_median' = '50%', 'annual_income_75th_percentile' = '75%', 
                                  'annual_income_25th_percentile' = '25%')                      )
 
ggplot(annual_gdp_cap_leaders, aes(year)) +
  geom_line(
    aes(y = polity_90th_percentile)
  ) +
  geom_line(
    aes(y = polity_median), colour = 'blue'
  )


group_by(historical_gdp_stats_growth %>% filter(year >= 1800), country) %>%
  summarize(
    obs = n(),
   mean_real_gdp_cap_growth = mean(real_gdp_cap_growth, na.rm = T),
   sd_read_gdp_growth = sd(real_gdp_cap_growth, na.rm = T),
   n_recessions = sum(real_gdp_cap_growth < 0, na.rm = T)
  ) %>% 
  View()

table(historical_gdp_stats$country)
us = filter(historical_gdp_stats_growth, country %in% c('Argentina', 'United States', 'United Kingdom', 'Botswana', 'South Africa', 'Japan', 'Germany', 'West Germany'))
ggplot(us %>% filter(between(year, 1800, 2016)), aes(year, rgdpnapc)) +
  facet_wrap(~country) +
  geom_point(aes(colour = polity2 > 0))
table(historical_gdp_stats_growth$country)
