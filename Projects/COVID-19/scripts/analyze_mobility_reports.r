# https://datascience.blog.wzb.eu/2019/04/30/zooming-in-on-maps-with-sf-and-ggplot2/

library(tidyverse)
library(plotly)
library(data.table)
library(viridisLite)
library(scales)
library(ggrepel)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(cowplot)


setwd("~/Public_Policy/Projects/COVID-19")
nordics = c('Sweden', 'Finland', 'Norway', 'Denmark')
top_europe = c('Spain', 'United Kingdom', 'Italy', 'France', 'Germany', 'Belgium')

world <- ne_countries(scale = "medium", returnclass = "sf")
europe = filter(world, continent == 'Europe') %>%
  mutate(
    name = recode(name, `Czech Rep.` = 'Czechia', `Bosnia and Herz.` = 'Bosnia and Herzegovina', `Macedonia` = 'North Macedonia')
  )
europe_cropped <- st_crop(europe, xmin = -24, xmax = 45,
                          ymin = 30, ymax = 73)

# new york has a county for each borough, need to combine those into one 

combined_covid_dat = fread('data/countries_states_county_covid_calcs.csv') %>%
  mutate(
    date = as.Date(date),
    new_deaths_per_100k = cum_diff_value_total_deaths / pop_100k
  ) %>%
  arrange(
    location_key, location, date
  ) %>%
  rename(
    smoothed_mobility_baseline = smoothed_percent_of_predicted
  ) %>%
  filter(location_type == 'Country')

### the temperature data is missing, use a simpler smoothing approach
mobility_stringency_data = fread('data/mobility_stringency.csv') %>% 
  mutate(date = as.Date(date), cases_less_20 = ifelse(is.na(cases), T, cases < 20)) %>%
  rename(
    location = entity_name
  ) %>%
  filter(
    location %in% unique(combined_covid_dat$location)
  )


baseline_mean = filter(mobility_stringency_data, 
                       transportation_type == 
                         'walking', cases_less_20) %>%
  group_by(location, cases_less_20, transportation_type) %>%
  summarize(
    baseline_mobility = mean(value, na.rm = T)
  ) %>%
  ungroup() %>%
  select(-cases_less_20)

mobility_stringency_data_walking = 
  inner_join(mobility_stringency_data, baseline_mean) %>%
  mutate(
    mobility_pct_baseline = value / baseline_mobility
  ) 

options(na.action = na.exclude)
mobility_stringency_data_walking_smoothed = map(unique(mobility_stringency_data_walking$location), function(the_location){
  the_entity = filter(mobility_stringency_data_walking, location == the_location)
  print(nrow(the_entity))
  the_model = loess(mobility_pct_baseline ~ as.numeric(date), data = the_entity, span = 0.3)
  the_entity$simple_smoother = predict(the_model)
  the_entity
}) %>% bind_rows()

combined_covid_dat_upd_mobility_stringency = 
  select(combined_covid_dat, -StringencyIndex, -mobility) %>%
  left_join(mobility_stringency_data_walking_smoothed, by = c('location', 'date')) %>%
  rename(
    walking_mobility_simple_smoother = simple_smoother
  ) %>%
  arrange(location, date)


stats_by_country = filter(combined_covid_dat_upd_mobility_stringency) %>%
  group_by(location) %>%
  summarize(
    pop_100k = max(pop_100k, na.rm = T),
    max_death_rate_100k = max(new_deaths_per_100k, na.rm = T),
    mean_pct_of_max = mean(new_deaths_per_100k[cum_diff_value_total_deaths > 100]/max_death_rate_100k, na.rm = T),
    last_date = max(date),
    last_new_deaths_per_100k = tail(new_deaths_per_100k, 1),
    last_new_deaths_per_100k_pct_of_max = last_new_deaths_per_100k / max_death_rate_100k,
    total_deaths = sum(cum_diff_value_total_deaths, na.rm = T),
    total_deaths_100k = total_deaths / max(pop_100k, na.rm = T),
    mean_stringency = mean(StringencyIndex, na.rm = T),
    mean_mobility = mean(smoothed_mobility_baseline[cum_diff_value_total_cases > 0], na.rm = T)
  ) %>%
  ungroup() %>%
  arrange(-total_deaths_100k) %>%
  mutate(
    location_factor = factor(location, levels = location)
  )

##### mobility data vs. stringency #####

last_vals = filter(combined_covid_dat_upd_mobility_stringency, location %in% nordics) %>% 
  arrange(location, date) %>%
  group_by(location) %>%
  summarize(
    last_date = max(date),
    last_mobility = walking_mobility_simple_smoother[!is.na(walking_mobility_simple_smoother)] %>% last(),
    last_stringency = StringencyIndex[!is.na(StringencyIndex)] %>% last()
  )




##### map european crude mortality rates #####
europe_sf = left_join(europe_cropped, stats_by_country, by  = c('name' = 'location'))

ggplot(europe_sf) +
  geom_sf(aes(fill = total_deaths_100k)) +
  geom_sf_label(data = filter(europe_sf, 
                              name %in% c(nordics, top_europe)), 
                aes(label = paste0(name, '\n', comma(total_deaths_100k, accuracy = 0.1))), size = 2.5) +
  scale_fill_viridis_c(name = 'Deaths Per\n100k Pop.',option = 'C') +
  theme_map() +
  labs(
    title = 'COVID-19 Crude Mortality Rates in Selected European Countries',
    subtitle = sprintf('Data through %s', max(combined_covid_dat$date) %>% format('%b %d')),
    caption = 'Chart: Taylor G. White\nData: Johns Hopkins CSSE, World Bank'
  ) +
  theme(
    plot.subtitle = element_text(face = 'italic'),
    plot.caption = element_text(hjust = 0, face = 'italic')
  )
ggsave('output/europe_crude_mortality_rates.png', height = 8, width = 10, units = 'in', dpi = 600)



#### plot death rates, mobility, stringency ####
combined_covid_dat_upd = left_join(
  combined_covid_dat_upd_mobility_stringency,
  select(stats_by_country, -pop_100k)
) %>%
  mutate(
    percent_of_max_deaths = new_deaths_per_100k / max_death_rate_100k
  )


filter(combined_covid_dat_upd, 
       !is.na(StringencyIndex),
       location %in% c(nordics, top_europe)
       ) %>%
ggplot(aes(date, new_deaths_per_100k, colour = StringencyIndex)) +
  facet_wrap(~location_factor, ncol = 5) +
  scale_color_viridis_c(name = 'Stringency Index',direction = 1, option = 'C') +
  stat_smooth(span = 0.2, aes(colour = StringencyIndex), se = F, colour = 'white') +
  geom_point(size = 1) +
  theme_dark() +
  geom_hline(aes(yintercept = 0)) +
  scale_x_date(limits = c(as.Date('2020-02-15'), max(combined_covid_dat_upd$date))) +
  scale_y_continuous(limits = c(-.25, 4.5)) +
  theme(
    panel.grid.minor = element_blank(),
    plot.subtitle = element_text(face = 'italic'),
    plot.caption = element_text(hjust = 0, face = 'italic')
  ) +
  labs(
    x = '', y = 'Daily Mortality Per 100k Population',
    title = 'COVID-19 Daily Mortality Versus Stringency of Government Response',
    subtitle = 'Selected European Countries',
    caption = 'Chart: Taylor G. White\nData: Johns Hopkins CSSE, Oxford OxCGRT'
  )
ggsave('output/daily_deaths_vs_stringency_sel_europe.png', height = 8, width = 12, units = 'in', dpi = 800)

# 
# ggplot(stats_by_country, aes(mean_stringency, 1-mean_mobility)) +
#   # geom_point(aes(size = total_deaths_100k), alpha = 0) +
#   guides(size = guide_legend(override.aes = list(alpha = 1))) +
#   # geom_text_repel(aes(size = total_deaths_100k, label = location), segment.alpha = 0) +
#   geom_text_repel(aes(size = total_deaths_100k, label = location)) +
#   stat_smooth(method = 'lm') +
#   labs(
#     x = 'Avg. Stringency Index', y = 'Avg. Mobility Reduction'
#   ) +
#   scale_y_continuous(labels = percent) +
#   scale_size(name='Deaths Per 100k Pop.', range = c(2, 8)) +
#   theme(
#     
#   )

filter(combined_covid_dat_upd_mobility_stringency, location %in% c(nordics)) %>%
  ggplot(aes(date, walking_mobility_simple_smoother, group = location, colour = StringencyIndex, fill = StringencyIndex)) +
  geom_line(size = 1) +
  scale_color_viridis_c(name='Stringency Index',option = 'C') +
  scale_fill_viridis_c(name='Stringency Index',option = 'C') +
  geom_label(data = last_vals, 
             aes(x = last_date + 3, y = last_mobility, fill = last_stringency, label = location), 
             colour = 'white', fontface = 'bold', size = 2.5) +
  theme_dark() +
  labs(y = 'Percent of Baseline Mobility', x = '', 
       title = 'Mobility Index vs. Stringency of Government Response', 
       caption = 'Chart: Taylor G. White\nData: Johns Hopkins CSSE, Oxford OxCGRT',
       subtitle = 'Selected Nordic Countries. The mobility index measures the rate at which people ask for walking directions via Apple Maps.') +
  scale_y_continuous(labels = percent, breaks = seq(0.4, 1.1, by = 0.1), limits = c(0.4, 1.1)) +
  theme(
    panel.grid.minor.y = element_blank(),
    plot.subtitle = element_text(face = 'italic'),
    plot.caption = element_text(hjust = 0, face = 'italic')
  )
ggsave('output/mobility_vs_stringency_nordics.png', height = 8, width = 12, units = 'in', dpi = 800)
