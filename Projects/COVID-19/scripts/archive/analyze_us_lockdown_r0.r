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

r0_window_size = 7
# questions to answer: 
# how have case / death rates changed recently?
# how have transmission rates changes over time?
# what do we know about mortality rates? 
# how many cases are missing?
# what is the geographic distribution of cases, and how does proximity affect transmission? 


setwd("~/Public_Policy/Projects/COVID-19")
all_covid_data_diffs_dates = read_csv('data/us_covid_data_by_state_with_calcs.csv')
all_covid_data_diffs_dates$cum_diff_value_total_tests

filter(all_covid_data_diffs_dates, location %in% c('LA', 'WA', 'NY', 'CA', 'MI')) %>% 
  ggplot(aes(date, cum_diff_value_total_deaths)) +
  facet_wrap(~location, scales = 'free_y') +
  geom_bar(stat = 'identity')

lm(cum_diff_value_tests_with_results ~ time, data = all_covid_data_diffs_dates) %>%
  summary()
filter(all_covid_data_diffs_dates, location %in% c('LA', 'WA', 'NY', 'CA', 'MI')) %>% 
  ggplot(aes(date, cum_diff_value_total_tests)) +
  facet_wrap(~location, scales = 'free_y') +
  geom_area(alpha = 0.5, colour = 'black') +
  geom_point() +
  geom_area(aes(date, cum_diff_value_total_cases), fill = 'red', alpha = 0.5) +
  geom_point(aes(date, cum_diff_value_total_cases), colour = 'red')
  
filter(all_covid_data_diffs_dates, location == 'LA') %>% 
  ggplot(aes(date, cum_diff_value_total_tests)) +
  geom_area() +
  geom_area(aes(date, cum_diff_value_total_cases), fill = 'red')

  

ggplot(all_covid_data_diffs_dates, aes(cum_diff_value_total_tests, cum_diff_value_total_cases)) +
  geom_point()

latest_state_data = filter(all_covid_data_diffs_dates, location != 'United States', date == max(date)) %>% 
  arrange(-value_total_cases)



ggplot(selected_early_states, aes(date, r0_rolling_lead_7)) +
  facet_wrap(~location_name) +
  # geom_line(aes(y = r0_rolling)) +
  geom_point(aes(colour = weekend_ind))  +
  # stat_smooth(method = 'gam', se = F) +
  geom_line() +
  # geom_line(aes(y = r0_rolling_lead_7), colour = 'red', linetype = 'dashed') +
  geom_hline(aes(yintercept = 2.5)) +
  geom_hline(aes(yintercept = 1)) +
  geom_vline(data = us_lockdown_dates %>% filter(location_name %in% unique(selected_early_states$location_name)), aes(xintercept = lockdown_start)) 


# how effective are lockdowns?
lockdown_r0_stats = group_by(all_covid_data_diffs_dates, lockdown_period) %>%
  summarize(
    obs = n(),
    obs_r0_rolling = length(r0_rolling[!is.na(r0_rolling)]),
    obs_r0_rolling_lead_7 = length(r0_rolling_lead_7[!is.na(r0_rolling_lead_7)]),
    obs_effective_r0_interpolated = length(effective_r0_interpolated[!is.na(effective_r0_interpolated)]),
    obs_effective_r0_interpolated_lead_7 = length(effective_r0_interpolated_lead_7[!is.na(effective_r0_interpolated_lead_7)]),
    mean_effective_r0 = mean(effective_r0_interpolated, na.rm = T),
    median_effective_r0 = median(effective_r0_interpolated, na.rm = T),
    mean_effective_r0_interpolated_lead_7 = mean(effective_r0_interpolated_lead_7, na.rm = T),
    median_effective_r0_interpolated_lead_7 = median(effective_r0_interpolated_lead_7, na.rm = T),
    median_r0_rolling = median(r0_rolling, na.rm = T),
    mean_r0_rolling = mean(r0_rolling, na.rm = T),
    median_r0_rolling_lead_7 = median(r0_rolling_lead_7, na.rm = T),
    mean_r0_rolling_lead_7 = mean(r0_rolling_lead_7, na.rm = T)
  )

# https://www.medrxiv.org/content/10.1101/2020.04.02.20051466v1.full.pdf
# https://www.reuters.com/article/us-health-coronavirus-britain-spread/preliminary-study-finds-uk-lockdown-is-slowing-spread-of-covid-19-idUSKBN21J56W



ggplot(all_covid_data_diffs_dates, aes(lockdown_period, r0_rolling_lead_7, fill = lockdown_period))+
  geom_boxplot(weight = 0, colour = 'gray') +
  scale_y_continuous(limits = c(0, 20)) +
  theme_bw() +
  labs(x = '', y = '7-Day R0', title = 'COVID-19 Rolling 7-Day R0 by Lockdown Status', 
       subtitle = sprintf('U.S. States, through %s', max(all_covid_data_diffs_dates$date) %>% format('%B %d'))
  ) +
  geom_text(data = lockdown_r0_stats, aes(y = median_r0_rolling_lead_7, label = round(median_r0_rolling_lead_7, 2)), size = 3.5, fontface = 'bold') +
  scale_fill_hue(guide = F) 
ggsave('output/effective_r0_by_lockdown_status.png', height = 6, width = 6, units = 'in', dpi = 800)  



latest_state_data$pre_lockdown_midpoint = with(latest_state_data, lockdown_start - as.numeric(lockdown_start - as.Date('2020-03-09'))/2)
latest_state_data$post_lockdown_midpoint = latest_state_data$lockdown_start + as.numeric(max(latest_state_data$date) - r0_window_size - r0_window_size + 3 - latest_state_data$lockdown_start)/2


# filter(all_covid_data_diffs_dates, location == 'CA') %>%
#   select(date, cum_diff_value_total_cases,r0_rolling, r0_rolling_lead_7) %>% View()

all_covid_data_diffs_dates %>% filter(location %in% c('NY', 'CA', 'WA'), !is.na(r0_rolling_lead_7), days_since_case_20 >= 5) %>%
  ggplot(aes(date, r0_rolling_lead_7)) +
  theme_bw() +
  facet_wrap(~location_name) +
  geom_vline(data = latest_state_data %>% filter(location %in% c('NY', 'CA', 'WA')), aes(xintercept = lockdown_start), size = 0.75, colour = 'gray50') +
  geom_bar(stat = 'identity', aes(alpha = cum_diff_value_total_cases, fill = weekend_ind)) +
  scale_alpha(name = 'Daily New Cases', range = c(0.4, 1)) +
  # scale_fill_viridis_d(name ='', option = 'D') +
  scale_fill_manual(name = '', values = c('Week Day' = 'steelblue', 'Weekend' = 'orange')) +
  # scale_fill_viridis(name = 'Daily New Cases', option = 'C') +
  geom_hline(aes(yintercept = 1), colour = 'red', size = 0.5) +
  geom_hline(aes(yintercept = 2.28), linetype = 'dashed', colour = 'black', size = 0.5) +
  scale_y_continuous(breaks = seq(0, 25, by = 1)) +
  # stat_smooth(method = 'gam') +
  labs(
    y = 'Estimated Seven-Day R0 of New Cases', x = '',
    title = 'COVID-19 Transmission Rates for Early U.S. States',
    subtitle = sprintf('Through %s. Dashed line shows average COVID-19 reproduction number (R0), red solid line shows R0=1, below which a pandemic slows.', max(all_covid_data_diffs_dates$date) %>% format('%B %d')) %>% str_wrap(100),
    caption = 'Chart: Taylor G. White\nData: covidtracking.com'
  ) +
  theme(
    legend.position = 'bottom',
    strip.text = element_text(face = 'bold', colour = 'white'),
    strip.background = element_rect(fill = 'black'),
    plot.subtitle = element_text(size = 11, face = 'italic'),
    plot.caption = element_text(hjust = 0, face = 'italic', size = 10),
    plot.title = element_text(size = 16),
    panel.grid.minor = element_blank()
    # plot.background = element_rect(fill = 'black'),
    # panel.background = element_rect(fill = 'black'),
    # panel.grid.minor = element_blank()
  ) +
  geom_text(data = latest_state_data %>% filter(location %in% c('NY', 'CA', 'WA')), aes(x = pre_lockdown_midpoint , y = 23, label = 'Pre-Lockdown'), fontface = 'italic', size = 3) + 
  geom_text(data = latest_state_data %>% filter(location %in% c('NY', 'CA', 'WA')), aes(x = post_lockdown_midpoint , y = 23, label = 'Post-Lockdown'), fontface = 'italic', size = 3, hjust = 0.5) + 
  geom_text(data = latest_state_data %>% filter(location %in% c('NY', 'CA', 'WA')), aes(x = lockdown_start, y = 15, label = format(lockdown_start, '%B %d')), fontface = 'bold', size = 3.5) 
ggsave('output/rolling_ro_by_early_states.png', height = 6, width = 9, units = 'in', dpi = 800)
