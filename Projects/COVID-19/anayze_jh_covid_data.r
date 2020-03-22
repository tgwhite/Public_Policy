# https://github.com/thomasp85/gganimate/wiki/Temperature-time-series
# https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
# https://covidtracking.com/api/
# https://covid.ourworldindata.org
install.packages("ggrepel")

library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(data.table)
library(countrycode)
library(viridisLite)
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
library(readxl)
library(transformr)
library(ggrepel)
setwd("~/Public_Policy/Projects/COVID-19")

us_states_shp = us_states()
us_map = USAboundaries::us_boundaries()

us_cities_shp = us_cities() %>%
  mutate(
    province_state_city = paste0(city, ', ', state_abbr)
  )


us_counties_shp = us_counties() %>%
  mutate(
    province_state_city = ifelse(state_abbr == 'LA', paste0(name, ' Parish, ', state_abbr), paste0(name, ' County, ', state_abbr)),
    county_state = paste0(name, ', ', state_abbr)
  )

county_shp = tigris::counties()

##### pull in data ####
johns_hopkins_cases = read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv') %>%
  pivot_longer(cols = matches('^([0-9])'), names_to = 'date', values_to = 'cases')

johns_hopkins_deaths = read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv') %>%
  pivot_longer(cols = matches('^([0-9])'), names_to = 'date', values_to = 'deaths')

johns_hopkins_recovered = read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv') %>%
  pivot_longer(cols = matches('^([0-9])'), names_to = 'date', values_to = 'recovered')

jh_joined = left_join(johns_hopkins_cases, johns_hopkins_deaths) %>% left_join(johns_hopkins_recovered) %>%
  mutate(
    date_upd = as.Date(date, format = '%m/%d/%y')
  )

names(jh_joined) = names(jh_joined) %>% tolower() %>% str_replace('[\\/]', '_')

#### classify whether city/state/county #### 

jh_joined = mutate(jh_joined, 
                   is_country = is.na(province_state),
                   US_state = country_region == 'US' & province_state %in% us_states_shp$name,
                   US_county = country_region == 'US' & province_state %in% us_counties_shp$province_state_city,
                   US_city = country_region == 'US' & !US_state & !US_county & province_state %in% us_cities_shp$province_state_city,
                   other_county = country_region == 'US' & !US_state & !US_county & !US_city & province_state %in% us_counties_shp$county_state,
                   other_county_name = ifelse(other_county, str_replace(province_state, ',', '%s,'), province_state) %>%
                     sprintf(' County'),
                   US_county = US_county | other_county,
                   province_state_fin = ifelse(other_county & str_detect(province_state, ', LA'), str_replace(other_county_name, ' County', ' Parish'), other_county_name),
                   US_other_geo_entity = country_region == 'US' & US_state+US_county+US_city==0,
                   location_key = paste(province_state, country_region, sep = '|')
                     )

entity_types = select(jh_joined, country_region, province_state_fin, is_country, US_state, 
                      US_county, US_city, US_other_geo_entity, long, lat) %>% unique()

jh_last_data_updates = group_by(jh_joined, country_region, province_state_fin) %>%
  summarize(
    last_update = max(date_upd),
    last_cases = cases[date_upd == max(date_upd)],
    last_deaths = deaths[date_upd == max(date_upd)],
    last_recovered = recovered[date_upd == max(date_upd)]
  ) %>%
  left_join(entity_types) %>%
  arrange(-last_deaths)

US_state_jh = left_join(us_states_shp, jh_last_data_updates, by = c('name' = 'province_state_fin'))
lower_48 = state.name[!state.name %in% c('Hawaii', 'Alaska')]
latest_state_data = filter(US_state_jh, name %in% lower_48)

ggplot() +
  geom_sf(data = latest_state_data, aes(fill = log(last_cases)), show.legend = F) +
  scale_fill_viridis_b() +
  geom_text(data = latest_state_data, aes(long, lat, label = comma(last_cases)), colour = 'white', fontface='bold', size = 2.5) +
  labs(x = '', y = '', 
       caption = 'Chart: Taylor G. White\nData: Johns Hopkins Coronavirus Resource Center',
       title = 'COVID-19 Cases by State', subtitle = sprintf('As of: %s', unique(jh_last_data_updates$last_update))) +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    title = element_text(size = 16),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(hjust = 0)
  )

ggsave('latest_cases_by_state.png', height = 6, width = 8, units = 'in', dpi=800)

##### plot country comparison #####

comparator_countries = filter(jh_joined, country_region %in% c('Korea, South', 'Italy')) 

US_china_total = filter(jh_joined, country_region %in% c('US', 'China')) %>%
  group_by(country_region, date_upd) %>%
  summarize(
    total_cases = sum(cases), total_deaths = sum(deaths)
  ) %>%
  filter(total_cases > 0) %>%
  rename(
    cases = total_cases, 
    deaths = total_deaths
  ) 

US_country_comparison = bind_rows(comparator_countries, US_china_total)

dates_case_100 = group_by(US_country_comparison, country_region) %>%
  summarize(
    case_100_date = min(date_upd[cases >= 100])
  )

us_states_vs_countries_dates = left_join(US_country_comparison, dates_case_100) %>%
  mutate(
    days_since_case_100 = as.numeric(date_upd - case_100_date)
  ) %>% 
  pivot_longer(cols = c('deaths', 'cases'), names_to = 'measure')

key_dates = tibble(
  date_upd = c('2020-01-23', '2020-03-07', '2020-03-19') %>% as.Date(),
  action = c('Hubei Lockdown', 'Lombardy Lockdown', 'California Lockdown'),
  country_region = c('China', 'Italy', 'US')
) %>%
  left_join(dates_case_100) %>%
  mutate(
    days_since_case_100 = as.numeric(date_upd - case_100_date),
  )

us_states_vs_countries_dates %>%
  filter(date_upd >= case_100_date) %>%
ggplot(aes(date_upd, value, colour = country_region)) +
  theme_bw() +
  facet_wrap(~str_to_title(measure), ncol = 1, scales = 'free_y') +
  geom_vline(data = key_dates, 
             aes(xintercept = date_upd, colour = country_region), size = 0.5, linetype = 'dashed', show.legend = F) +
  geom_line(size = 1) +
  scale_y_continuous(labels = comma)  +
  theme(
    strip.background = element_rect(fill = 'darkgray'),
    strip.text = element_text(face = 'bold'),
    text = element_text(colour = 'white'),
    axis.text = element_text(colour = 'white'),
    title = element_text(size = 16, colour = 'white'),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    plot.caption = element_text(size = 10, hjust = 0),
    legend.title = element_text(size = 14),
    plot.subtitle = element_text(size = 11, face = 'italic'),
    plot.background = element_rect(fill = 'black'),
    legend.background = element_rect(fill = 'black'),
    panel.background = element_rect(fill = 'black'),
    panel.grid.minor  = element_blank(),
    panel.grid.major = element_line(size = 0.25)
  ) +
  labs(
    x = '\nDate of 100th Case', 
    y = '', title = 'COVID-19 Country Comparison',
    caption = 'Chart: Taylor G. White\nData: Johns Hopkins Coronavirus Resource Center',
    subtitle = str_wrap('Vertical lines show date of first regional lockdown. China locked down Hubei a day after their 100th case. Italy and the U.S. acted more slowly, with Lombardy locked down 13 days after their 100th case and California, 16 days later. South Korea opted for a mass-testing and targeted quarantine strategy instead of locking down entire regions.', 100)
  ) +
  scale_colour_hue(name = 'Country', labels = c('Korea, South' = 'South Korea'))

ggsave('covid_country_comparison.png', height = 9, width = 9, units = 'in', dpi = 800)

#### show case growth with Trump quotes superimposed ####

coronavirus_quotes = read_excel('coronavirus quotes.xlsx') %>% mutate(Date = as.Date(Date)) %>%
  filter(`Person/Organization` == 'Donald Trump', Show == 1) %>%
  mutate(
    odd_row = 1:length(Date) %% 2,
    row_yval = ifelse(odd_row, .25, -0.25),
    measure = c('timeline'),
    country_region = 'US'
  ) %>%
  rename(date_upd = Date)

us_cases = filter(us_states_vs_countries_dates, !str_detect(country_region, 'Korea'), measure == 'cases')
write.csv(us_cases, 'us_cases_comparison.csv', row.names = F)

anim = ggplot(us_cases, aes(date_upd, value, colour = country_region)) + 
  geom_line(size = 1) + 
  geom_segment(aes(xend = max(date_upd) + 1, yend = value, group = country_region), linetype = 2, colour = 'grey') + 
  geom_point(size = 2) + 
  geom_text_repel(aes(x = max(date_upd) + 1, label = comma(value)), hjust = 0, size = 3, 
                  vjust = -0.5,
            show.legend = F) + 
  geom_text(data = coronavirus_quotes, aes(x = as.Date('2020-03-01'), y = 30000, 
                                           label = paste0(str_wrap(Quote, 24), '\n', format(date_upd, '%B %d')) 
  ), colour = 'black', show.legend=F, size = 4.5) +
  
  transition_reveal(date_upd) + 
  coord_cartesian(clip = 'off') + 
  labs(
    title = paste0('COVID-19 Cases by Day, Through ', format(max(us_cases$date_upd), '%B %d')), 
       y = 'Case Count\n', x = '', subtitle = 'Quotes from President Trump superimposed.',
       caption = 'Chart: Taylor G. White\nData: Johns Hopkins Coronavirus Resource Center\nQuotes: David Leonhardt, NYT'
    ) + 
  theme_minimal() + 
  scale_y_continuous(labels = comma) +
  scale_x_date(breaks = seq(as.Date('2020-01-22'), max(us_cases$date_upd), by = 7), date_labels = "%b %d") +
  theme(
    legend.position = 'bottom',
    plot.caption = element_text(size = 10, hjust = 0),
    plot.margin = margin(5.5, 10, 5.5, 5.5), plot.subtitle = element_text(size=11, face = 'italic')) +
  scale_colour_hue(name = 'Country')

animate(anim, nframes = 300,
        renderer = gifski_renderer("us_covid_cases_by_day.gif"), 
        height = 6, width = 6, units = 'in',  type = 'cairo-png', res = 200)

####  show case growth by day since case 100 ####        
growth_comparison_dat = filter(us_states_vs_countries_dates, measure == 'cases', 
                  country_region %in%  c('US', 'Italy', 'China', 'Korea, South'))

anim = 
growth_comparison_dat %>% filter(days_since_case_100 >=0) %>%
ggplot(aes(days_since_case_100, value, colour = country_region)) + 
  geom_line(size = 1) + 
  geom_point(size = 2) + 
  transition_reveal(days_since_case_100) + 
  coord_cartesian(clip = 'off') + 
  labs(
    title = paste0('COVID-19 Cases by Day, Through ', format(max(growth_comparison_dat$date_upd), '%B %d')), 
    y = 'Case Count\n', 
       x = '\nDays Since Case 100', 
       subtitle = 'Diverging paths illustrate the varied effectiveness of public health responses.',
       caption = 'Chart: Taylor G. White\nData: Johns Hopkins CSSE') + 
  theme_minimal() + 
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = seq(0, 60, by = 10)) +
  theme(
    plot.caption = element_text(size = 10, hjust = 0),
    legend.position = 'bottom'
  ) +
  theme(plot.margin = margin(5.5, 10, 5.5, 5.5), plot.subtitle = element_text(size=11, face = 'italic')) +
  scale_colour_hue(name = 'Country', labels = c('Korea, South' = 'South Korea')) +
  geom_segment(aes(xend = max(days_since_case_100) + 1, yend = value, group = country_region), linetype = 2, colour = 'grey') + 
  geom_point(size = 2) + 
  geom_text_repel(aes(x = max(days_since_case_100) + 1, label = comma(value)), hjust = 0, size = 3, 
                  vjust = -0.5,
                  show.legend = F) +
  geom_text_repel(data = key_dates, aes(x =days_since_case_100, y = c(60000, 70000, 80000), label = action), hjust = 0, size = 3, 
                  vjust = -0.5,
                  show.legend = F) +
  geom_vline(data = key_dates, 
             aes(xintercept = days_since_case_100, colour = country_region), size = 0.5, linetype = 'dashed', show.legend = F) 

animate(anim, nframes = 300,
        renderer = gifski_renderer("covid_case_growth_comparison.gif"), 
        height = 6, width = 6, units = 'in',  type = 'cairo-png', res = 200)
