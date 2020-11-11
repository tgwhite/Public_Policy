

library(tidyverse)
library(data.table)
library(USAboundaries)
library(USAboundariesData)
library(tigris)
library(albersusa)
library(scales)
library(viridisLite)

large_text_theme = theme(
  plot.title = element_text(size = 24),
  plot.subtitle = element_text(size = 18, face = 'italic'),
  plot.caption = element_text(size = 13, face = 'italic', hjust = 0),
  axis.text = element_text(size = 16),
  axis.title = element_text(size = 18)
) 
library(cowplot)

setwd("~/Public_Policy/Projects/Affordable Care Act/data")
list.files()
medicaid_expansion_data = read_csv('medicaid_expansion_states.csv')
national_uninsured_stats_files = list.files("national characteristics of uninsured", pattern = '_data.*.csv', full.names = T)
county_uninsured_stats_files = list.files("county characteristics of uninsured, 5-year", pattern = '_data.*.csv', full.names = T)   
state_uninsured_stats_files = list.files("state characteristics of uninsured, 5-year", pattern = '_data.*.csv', full.names = T)


get_stacked_acs_data = function(filenames) {
  
  all_stacked_uninsured_data = map(filenames, function(the_file){
    print(the_file)
    
    headers = read_csv(the_file, n_max = 1)
    # t(headers) %>% View()
    uninsured_data = read_csv(the_file, col_names = F, skip = 2, na = c('', 'NA', 'null'))
    names(uninsured_data) = names(headers)  
    
    uninsured_data = rename(uninsured_data, total_population = S2702_C01_001E, uninsured = S2702_C02_001E) %>%
      mutate(
        total_population = as.numeric(total_population), 
        uninsured = as.numeric(uninsured),
        year = str_extract(the_file, 'ACSST[0-9+]{1}Y[0-9]{4}') %>% str_remove('ACSST[0-9]{1}Y') %>% as.numeric()
      ) %>%
      select(GEO_ID, NAME, year, total_population, uninsured)
  }) %>% 
    bind_rows() %>%
    mutate(
      percent_uninsured = uninsured / total_population,
      fips = str_extract(GEO_ID, 'US[0-9]+') %>% str_remove('US')
    )
  return(all_stacked_uninsured_data)
}

national_uninsured_data = get_stacked_acs_data(national_uninsured_stats_files)
state_uninsured_data = get_stacked_acs_data(state_uninsured_stats_files) %>% filter(!is.na(total_population))
county_uninsured_data = get_stacked_acs_data(county_uninsured_stats_files) %>% filter(!is.na(total_population))

# View(state_uninsured_data)
# View(county_uninsured_data)

##### Plot overall uninsured stats #####

ggplot(national_uninsured_data, aes(year, percent_uninsured)) +
  theme_bw() +
  large_text_theme + 
  theme(panel.grid.minor = element_blank()) +
  geom_bar(stat = 'identity', fill = 'steelblue', colour = 'black', width = 0.75) +
  geom_text(
    aes(label = paste0(comma(uninsured/1e6, accuracy = 0.1), 'M')),
    vjust = -0.5, size = 5
  ) +
  labs(
    x = '', y = 'Uninsured Population (%)', 
    title = 'Trend in the U.S. Uninsured Population',
    subtitle = 'The Affordable Care Act (Obamacare) became law in 2010 and went into effect in 2014.',
    caption = 'Chart: Taylor G. White\nData: Census ACS 1-Year Estimates'
  ) + 
  scale_y_continuous(labels = percent, breaks = seq(0, 0.15, by = 0.025)) +
  scale_x_continuous(breaks = unique(stacked_uninsured_data$year))

setwd("~/Public_Policy/Projects/Affordable Care Act/output")

ggsave('total_effects_uninsured.png', height = 9, width = 12, units = 'in', dpi = 600)


## get shapefile data ## 
# us_counties_shp = us_counties()
# us_states_shp = us_states()
# us_map = USAboundaries::us_boundaries()
# us_states_tigris = tigris::states()
# us_counties_tigris = tigris::counties()

us_sf <- usa_sf("laea")
# plot(us_sf)
cty_sf <- counties_sf("aeqd")

View(state_uninsured_data)

state_uninsured_stats = group_by(state_uninsured_data, GEO_ID, fips, NAME         ) %>%
  summarize(
    obs = n(),
    latest_year = max(year),
    delta_uninsured_pct = percent_uninsured[year == max(year)] - percent_uninsured[year == min(year)],
    delta_uninsured = uninsured [year == max(year)] - uninsured [year == min(year)],
  ) %>%
  ungroup() %>%
  left_join(medicaid_expansion_data, by = c('NAME' = 'Name')) %>%
  arrange(delta_uninsured_pct) %>%
  mutate(
    state_name_factor = factor(NAME, levels = NAME)
  )
state_uninsured_stats$NAME

county_uninsured_stats = group_by(county_uninsured_data, GEO_ID, fips) %>%
  summarize(
    obs = n(),
    latest_year = max(year),
    delta_uninsured_pct = percent_uninsured[year == max(year)] - percent_uninsured[year == min(year)],
    delta_uninsured = uninsured [year == max(year)] - uninsured [year == min(year)],
  ) %>%
  ungroup() 

View(county_uninsured_data)

county_map_data = left_join(cty_sf, county_uninsured_stats)
state_map_data = left_join(us_sf, state_uninsured_stats, by = c('fips_state' = 'fips')) 


ggplot(state_uninsured_stats, aes(NAME, delta_uninsured_pct)) +
  geom_bar(stat = 'identity')

ggplot(state_map_data) +
  large_text_theme +
  theme_map() +
  theme(
    plot.title = element_text(size = 24),
    plot.subtitle = element_text(size = 18, face = 'italic'),
    plot.caption = element_text(size = 13, face = 'italic', hjust = 0),
  ) +
  geom_sf(aes(fill = delta_uninsured_pct)) +
  geom_sf_text(data = filter(state_map_data, Status == 'Not Adopted'), aes(label = 'No Exp.'), fontface = 'bold', size = 3.5) + 
  scale_fill_viridis_c(direction = -1, option = 'D', labels = percent, name = 'Change in Uninsured\nPopulation') +
  labs(
    title = 'Percent Change in Uninsured Population, 2013-2018',
    subtitle = 'States that did not expand Medicaid are marked "No Exp."',
    caption = 'Chart: Taylor G. White\nData: Census ACS 5-Year Estimates, Kaiser Family Foundation'
  )
ggsave('change_in_uninsured_2013_2018_map.png', height = 9, width = 12, units = 'in', dpi = 600)

  
ggplot(county_map_data) +
  geom_sf(aes(fill = delta_uninsured_pct)) +
  scale_fill_viridis_c()


  


##### create map for all 50 states #####
US_state_data = left_join(us_sf, latest_state_data, by = c('iso_3166_2' = 'location')) %>% left_join(state_geo_center, by = c('iso_3166_2' = 'state_abbr'))

ggplot() +
  geom_sf(data = US_state_data, aes(fill = log(cases_per_100k)), alpha = 0.75, size = 0.25) +
  scale_fill_viridis(guide = F, option = 'C') +
  geom_sf_text(data = US_state_data, aes(long, lat, label = comma(cases_per_100k, accuracy = 1)),
               colour = 'black', fontface='bold', size = 2) +
  theme_map() +
  labs(x = '', y = '',
       caption = 'Chart: Taylor G. White\nData: covidtracking.com',
       title = 'U.S. COVID-19 Cases by State, Per 100k Population', subtitle = sprintf('As of %s',
                                                                                       unique(format(latest_state_data$date, '%B %d')))) +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    title = element_text(size = 16),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(hjust = 0, face = 'italic', size = 10)
  )
