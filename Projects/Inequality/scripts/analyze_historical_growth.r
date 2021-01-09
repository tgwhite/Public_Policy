library(tidyverse)
library(data.table)
library(readxl)
library(scales)
library(ggrepel)
library(ggforce)
library(gganimate)
library(gifski)
library(xts)

setwd("~/Public_Policy_Upd/Projects/Inequality")

##### get polity data #####
polity_political_violence = read_excel('data/MEPVv2018.xls')
polity_coups = read_excel('data/CSPCoupsAnnualv2018 (1).xls')
polity_v = read_excel( "data/polity iv data.xls" ) 


polity_v_fin = 
  left_join(polity_v, polity_coups) %>%
  left_join(polity_political_violence) %>%
  mutate(
    country = recode(country, 
                     UAE = 'United Arab Emirates',
                     Bosnia =  "Bosnia and Herzegovina",
                     `Congo Brazzaville` = 'Congo-Brazzaville',
                     `Korea South` = 'South Korea',
                     `Korea North` = 'North Korea',
                     `USSR` = 'Former USSR')
  )


#### get historical gdp stats and join everything #####
historical_gdp_stats = read_excel("data/angus maddison historical gdp statistics.xlsx", 'Full data') %>% 
  mutate(
    country = recode(country, 
                     `Bolivia (Plurinational State of)` = 'Bolivia',
                     `Russian Federation` = 'Russia',
                     `Viet Nam` = 'Vietnam',
                     `D.P.R. of Korea` = 'North Korea',
                     `Republic of Korea` = 'South Korea',
                     `Syrian Arab Republic` = 'Syria',
                     `Iran (Islamic Republic of)` = 'Iran',
                     `Lao People's DR` = 'Laos',
                     # `Former USSR` = 'USSR',
                     `Myanmar` = 'Myanmar (Burma)',
                     `Slovakia` = 'Slovak Republic',
                     `Venezuela (Bolivarian Republic of)` = 'Venezuela',
                     `Taiwan, Province of China` = 'Taiwan',
                     `TFYR of Macedonia` = 'Macedonia',
                     `D.R. of the Congo`='Congo Kinshasa',
                     `Congo` = 'Congo-Brazzaville',
                     `Republic of Moldova` = 'Moldova',
                     `Dominica` = 'Dominican Republic',
                     `U.R. of Tanzania: Mainland` = 'Tanzania'
                     )
  ) %>%
  arrange(countrycode, year) %>%
  data.table()



historical_gdp_stats_growth = historical_gdp_stats[, {
  # china = filter(historical_gdp_stats, country == 'China')
  # attach(china)
  # detach(china)
  # 
  the_dat = data.frame(year, cgdppc, rgdpnapc, pop)
  
  full_year_df = data.frame(
    year = min(year, na.rm = T):max(year, na.rm = T)
  ) %>%
    left_join(
      the_dat
    ) %>%
    mutate(
      rgdpnapc_interpolated = na.approx(rgdpnapc, na.rm = F),
      pop_interpolated = na.approx(pop, na.rm = F),
      real_gdp_cap_growth = (rgdpnapc - lag(rgdpnapc, 1))/ lag(rgdpnapc, 1),
      real_gdp_cap_growth_interp = (rgdpnapc_interpolated - lag(rgdpnapc_interpolated, 1))/ lag(rgdpnapc_interpolated, 1),
      pop_growth = (pop - lag(pop, 1)) / lag(pop, 1),
      pop_growth_interp = (pop_interpolated - lag(pop_interpolated, 1)) / lag(pop_interpolated, 1),
      lagged_real_gdp_cap_growth = lag(real_gdp_cap_growth, 1),
      lagged_real_gdp_cap_growth_interp = lag(real_gdp_cap_growth_interp, 1)
    )
  
  full_year_df
  
}, by = list(countrycode, country)] %>%
  full_join(
    polity_v_fin,
    by = c('country', 'year')
  ) %>%
  mutate(
    polity2 = ifelse(polity2 %in% c(-88, -66), NA, polity2),
    polity_desc = ifelse(between(polity2, -5, 5), 'Anocracy (mixed)', ifelse(polity2 > 5, 'Democracy', 'Autocracy')),
    polity_desc = factor(polity_desc, levels = c('Autocracy', 'Anocracy (mixed)', 'Democracy')),
    autocratic_shift = change < 0 & sign(polity2) != sign(prior) & is.na(interim),
    n_coups = scoup1 + atcoup2,
    had_civil_war = civwar > 0
  ) %>%
  arrange(country, year)

get_period_growth = function(start, end, periods) {
  period_avg_growth = (end / start)^(1/periods) - 1
  return(period_avg_growth)
}

stats_by_country = group_by(historical_gdp_stats_growth, country) %>%
  summarize(
    anocracy_years = sum(between(polity2, -5, 5), na.rm = T),
    democracy_years = sum(polity2 > 5, na.rm = T),
    autocracy_years = sum(polity2 < -5, na.rm = T),
    avg_polity2 = mean(polity2, na.rm = T),
    max_polity2 = max(polity2, na.rm = T),
    min_polity2 = min(polity2, na.rm = T),
    transition_periods = sum(is.na(polity2)),
    n_coup_attempts = sum(atcoup2, na.rm = T),
    n_successful_coups = sum(scoup1, na.rm = T),
    n_civil_wars = sum(had_civil_war, na.rm = T),
    pop_1850 = pop_interpolated[year == 1850],
    pop_2015 = pop_interpolated[year == 2015],
    income_1850 = rgdpnapc_interpolated[year == 1850],
    income_1900 = rgdpnapc_interpolated[year == 1900],
    income_1950 = rgdpnapc_interpolated[year == 1950],
    income_2015 = rgdpnapc_interpolated[year == 2015],
    pop_2015 = pop_interpolated[year == 2015]
  ) %>%
  mutate(
    percent_democracy_years = democracy_years / (anocracy_years + democracy_years + autocracy_years),
    coup_war_score = (n_civil_wars > 0) + (n_coup_attempts > 0 | n_successful_coups > 0),
    growth_1850_2015 = get_period_growth(income_1850, income_2015, 2015 - 1850 + 1),
    growth_1900_2015 = get_period_growth(income_1900, income_2015, 2015 - 1900 + 1),
    growth_1950_2015 = get_period_growth(income_1950, income_2015, 2015 - 1950 + 1)
  )


filter(historical_gdp_stats_growth, country %in% c('United States', 'United Kingdom', 'Argentina'), between(year, 1890, 1900) | between(year, 1930, 1940)) %>%
  select(country, year, rgdpnapc) %>% pivot_wider(names_from = "country", values_from = 'rgdpnapc') %>% View()


setwd('output')


##### US vs. Argentina growth, polity #####

us_argentine_comparison = filter(historical_gdp_stats_growth %>% filter(between(year, 1850, 2015)), country %in% c('United States', 'Argentina'), !is.na(rgdpnapc)) %>%
  mutate(label = ifelse(year == max(year), country, NA))

argentina_greater_us = us_argentine_comparison %>% 
  select(country, year, rgdpnapc) %>%
  pivot_wider(id_cols = c('country', 'year'), values_from  = 'rgdpnapc', names_from = 'country') %>%
  filter(Argentina > `United States`)

argentina_rect = data.frame(
  xmin = min(argentina_greater_us$year),
  xmax = max(argentina_greater_us$year)
)

argentine_coups_regime_changes = filter(us_argentine_comparison, n_coups > 0 | autocratic_shift ) %>% 
  as.data.frame() %>%
  mutate(
    label = ifelse(scoup1 > 0, "Success", ifelse(n_coups > 0, "Failure", NA)),
    coup_label = ifelse(scoup1 > 0, "Coup", ifelse(n_coups > 0, "Attempted Coup", NA)),
    coup_label = ifelse(scoup1 > 1, 'Multiple\nSuccessful Coups', coup_label)
    ) 

argentina_rects = tribble(
  ~xmin, ~xmax,
  min(argentina_greater_us$year), max(argentina_greater_us$year),
  min(argentine_coups_regime_changes$year), max(argentine_coups_regime_changes$year)
)

n_coups = filter(argentine_coups_regime_changes, n_coups > 0) %>% pull(n_coups) %>% sum()
n_successful_coups = filter(argentine_coups_regime_changes, n_coups > 0) %>% pull(scoup1) %>% sum()
n_autocratic_shifts = filter(argentine_coups_regime_changes, autocratic_shift) %>% nrow()

# 
# anim = 
#   growth_comparison_dat %>% filter(days_since_case_100 >=0) %>%
#   ggplot(aes(days_since_case_100, value, colour = country_region)) + 
#   geom_line(size = 1) + 
#   geom_point(size = 2) + 
#   transition_reveal(days_since_case_100) + 
#   coord_cartesian(clip = 'off') + 
#   labs(
#     title = paste0('COVID-19 Cases by Day, Through ', format(max(growth_comparison_dat$date_upd), '%B %d')), 
#     y = 'Case Count\n', 
#     x = '\nDays Since Case 100', 
#     subtitle = 'Diverging paths illustrate the varied effectiveness of public health responses.',
#     caption = 'Chart: Taylor G. White\nData: Johns Hopkins CSSE') + 
#   theme_minimal() + 
#   scale_y_continuous(labels = comma) +
#   scale_x_continuous(breaks = seq(0, 60, by = 10)) +
#   theme(
#     plot.caption = element_text(size = 10, hjust = 0),
#     legend.position = 'bottom'
#   ) +
#   theme(plot.margin = margin(5.5, 10, 5.5, 5.5), plot.subtitle = element_text(size=11, face = 'italic')) +
#   scale_colour_hue(name = 'Country', labels = c('Korea, South' = 'South Korea')) +
#   geom_segment(aes(xend = max(days_since_case_100) + 1, yend = value, group = country_region), linetype = 2, colour = 'grey') + 
#   geom_point(size = 2) + 
#   geom_text_repel(aes(x = max(days_since_case_100) + 1, label = comma(value)), hjust = 0, size = 3, 
#                   vjust = -0.5,
#                   show.legend = F) +
#   geom_text_repel(data = key_dates, aes(x =days_since_case_100, y = c(60000, 70000, 80000), label = action), hjust = 0, size = 3, 
#                   vjust = -0.5,
#                   show.legend = F) +
#   geom_vline(data = key_dates, 
#              aes(xintercept = days_since_case_100, colour = country_region), size = 0.5, linetype = 'dashed', show.legend = F) 
# 
# animate(anim, nframes = 300,
#         renderer = gifski_renderer("output/covid_case_growth_comparison.gif"), 
#         height = 6, width = 6, units = 'in',  type = 'cairo-png', res = 200)

argentina_rects_anim = tibble(
  year = argentina_rects[2,]$xmin:argentina_rects[2,]$xmax %>% as.numeric(),
  start_year = argentina_rects[2,]$xmin
)

animated_us_argentina = us_argentine_comparison %>%
  ggplot(aes(group = country)) +
  theme_bw() +
  transition_reveal(year) +
  geom_rect(data = argentina_rects_anim, aes(xmin = min(year), xmax = year, ymin = 0, group = seq_along(year),
                                             ymax = 55000), fill = 'gray90') +
  geom_line(aes(year, rgdpnapc)) +
  geom_text(data = argentina_greater_us, aes(x = year, y = Argentina * 2, label = "Argentina's income briefly passes the U.S.", group = NA)) +
  geom_text(data = argentina_rects_anim, aes(x = mean(argentina_rects_anim$year), y = 55000, label = sprintf("Argentina:\n%s autocratic shifts, %s coups d'etat (%s attempted)", n_autocratic_shifts, n_successful_coups, n_coups), group = seq_along(year))) +
  
  geom_segment(data = argentina_greater_us, aes(x = year, xend = year, y = Argentina, yend = Argentina * 2, group = NA)) +
  
  # geom_point(data = argentine_coups_regime_changes %>% filter(!is.na(label)), aes(year, rgdpnapc, shape = label), size = 4) +
  geom_point(aes(year, rgdpnapc, colour = polity_desc, group = seq_along(year)), size = 1.5) +
  
  scale_shape_manual(name = "Coups d'Etat",values = c("Success" = 8, "Failure" = 5)) +
  # facet_zoom(xlim = c(1890, 1910), ylim = c(3000, 7000), horizontal = FALSE) +
  scale_alpha(guide = F) +
  scale_colour_manual(name = "Type of Government", values = c('Autocracy' = '#e61938', 'Anocracy (mixed)' = 'darkgray', 'Democracy' = '#0045a1')) +
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  # annotate('text', x = mean(argentina_greater_us$year), y = 55000, label = "Argentina's income surpasses U.S., 1894-1896", angle = 0, vjust = -0.5) +
  # annotate('text', x = mean(argentine_coups_regime_changes$year), y = 55000, label = sprintf("%s autocratic shifts, %s coups d'etat (%s attempted)", n_autocratic_shifts, n_successful_coups, n_coups), angle = 0, vjust = -0.5) +
  # scale_colour_gradient2(name = 'Polity V Score\n>0 is Democratic',low = '#e61938', high = '#0045a1', mid = 'gray', midpoint = 0) +
  # scale_colour_hue(name = 'Democratic', labels = c('TRUE' = 'Yes', 'FALSE' = 'No')) +
  geom_label_repel(aes(year, rgdpnapc, label = country), nudge_x = 1, na.rm = T, nudge_y = 3) +
  scale_y_continuous(labels = dollar, breaks = seq(0, 50000, by = 10000)) +
  scale_x_continuous(breaks = seq(1850, 2015, by = 10)) +
  labs(
    y = 'Real GDP Per Capita (2011 USD)\n', x = '',
    title = 'Democracy and Economic Prosperity',
    subtitle = 'Comparing divergent political and economic paths of the U.S. and Argentina',
    caption = 'Chart: Taylor G. White (@t_g_white)\nData: Polity Project, Maddison Project'
  ) +
  theme_minimal() +
  theme(
    legend.background = element_rect(fill = 'white'),
    legend.position = c(0.15, 0.9),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 14),
    plot.subtitle = element_text(size = 15),
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 10),
    plot.caption = element_text(size = 11, face = 'italic', hjust = 0),
    title = element_text(size = 18)
  )
# 
# animate(animated_us_argentina, nframes = 200,
#         renderer = gifski_renderer("argentina_vs_us_income_comparison.gif"),
#         height = 8, width = 8, units = 'in',  type = 'cairo-png', res = 150, start_pause = 4, end_pause = 20)
# 

argentina_greater_us

us_argentine_comparison %>%
  ggplot() +
  theme_bw() +
  geom_label_repel(aes(year, rgdpnapc, label = label), nudge_x = 1, na.rm = T, nudge_y = 3e3, segment.colour = 'orange') +
  geom_rect(data = argentina_rects[2,], aes(xmin = xmin, xmax = xmax, ymin = 0, 
                                        ymax = 55000), alpha = 0.25, fill = 'gray90', colour = 'black', linetype = 'dashed') +  
  geom_text_repel(data = argentina_greater_us %>% head(1), aes(x = year, y = Argentina), label = "Argentina's income\nbriefly surpassed the U.S.", nudge_y = 10e3) +
  geom_line(aes(year, rgdpnapc, group = country)) +
  # geom_point(data = argentine_coups_regime_changes %>% filter(!is.na(label)), aes(year, rgdpnapc, shape = label), size = 4) +
  geom_text_repel(data = argentine_coups_regime_changes %>% filter(scoup1 > 0), aes(year, rgdpnapc, label = coup_label), 
                  size = 3, segment.colour = 'orange', nudge_y = -4e3) +
  geom_point(aes(year, rgdpnapc, colour = polity_desc), size = 1.5) +
  
  scale_shape_manual(name = "Coups d'Etat",values = c("Success" = 8, "Failure" = 5)) +
  # facet_zoom(xlim = c(1890, 1910), ylim = c(3000, 7000), horizontal = FALSE) +
  scale_alpha(guide = F) +
  scale_colour_manual(name = "Type of Government", values = c('Autocracy' = '#e61938', 'Anocracy (mixed)' = 'gray', 'Democracy' = '#0045a1')) +
  # annotate('text', x = mean(argentina_greater_us$year), y = 55000, label = "Argentina's income surpasses U.S., 1894-1896", angle = 0, vjust = -0.5) +
  annotate('text', x = mean(argentine_coups_regime_changes$year), y = 55000, label = sprintf("%s autocratic shifts, %s coups d'etat (%s attempted)", n_autocratic_shifts, n_successful_coups, n_coups), angle = 0, vjust = -0.5) +
  # scale_colour_gradient2(name = 'Polity V Score\n>0 is Democratic',low = '#e61938', high = '#0045a1', mid = 'gray', midpoint = 0) +
  # scale_colour_hue(name = 'Democratic', labels = c('TRUE' = 'Yes', 'FALSE' = 'No')) +
  scale_y_continuous(labels = dollar, breaks = seq(0, 50000, by = 10000)) +
  scale_x_continuous(breaks = seq(1850, 2015, by = 10)) +
  labs(
    y = 'Real GDP Per Capita (2011 USD)\n', x = '',
    title = 'Political Power and Stability vs. Economic Prosperity',
    subtitle = 'Comparing divergent political and economic paths of the U.S. and Argentina, 1850-2015',
    caption = 'Chart: Taylor G. White (@t_g_white)\nData: Polity Project, Maddison Project'
  ) +
  theme_minimal() +
  theme(
    legend.background = element_rect(fill = 'white'),
    legend.position = c(0.10, 0.90),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16),
    plot.subtitle = element_text(size = 18),
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 12),
    plot.caption = element_text(size = 13, face = 'italic', hjust = 0),
    title = element_text(size = 26)
  )
ggsave('us vs argentina paths.png', height = 9, width = 12, units = 'in', dpi = 600)


##### South Korea vs North Korea #####
filter(historical_gdp_stats_growth %>% filter(between(year, 1950, 2015)), country %in% c('South Korea', 'North Korea'), !is.na(rgdpnapc)) %>%
  mutate(label = ifelse(year == max(year), country, NA)) %>%
  ggplot(aes(year, rgdpnapc)) +
  theme_bw() +
  geom_line(aes(group = country)) +
  geom_point(aes(colour = polity_desc), size = 1.5) +
  scale_alpha(guide = F) +
  scale_colour_manual(name = "Type of Government", values = c('Autocracy' = '#e61938', 'Anocracy (mixed)' = 'gray', 'Democracy' = '#0045a1')) +
  # scale_colour_gradient2(name = 'Polity V Score\n>0 is Democratic',low = '#e61938', high = '#0045a1', mid = 'gray', midpoint = 0) +
  # scale_colour_hue(name = 'Democratic', labels = c('TRUE' = 'Yes', 'FALSE' = 'No')) +
  geom_label_repel(aes(label = label), nudge_x = 1, na.rm = T, nudge_y = 2) +
  scale_y_continuous(labels = dollar) +
  scale_x_continuous(breaks = seq(1950, 2015, by = 5)) +
  labs(
    y = 'Real GDP Per Capita (2011 USD)\n', x = '',
    title = 'Political Power and Stability vs. Economic Prosperity',
    subtitle = 'Comparing diverging political and economic paths of North and South Korea, 1950-2015',
    caption = 'Chart: Taylor G. White\nData: Polity Project, Maddison Project'
  ) +
  theme_minimal() +
  theme(
    legend.background = element_rect(fill = 'white'),
    legend.position = c(0.10, 0.875),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16),
    plot.subtitle = element_text(size = 18),
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 12),
    plot.caption = element_text(size = 13, face = 'italic', hjust = 0),
    title = element_text(size = 26)
  )
ggsave('north vs. south korea paths.png', height = 9, width = 12, units = 'in', dpi = 600)

##### USA vs USSR #####


##### South Korea vs North Korea #####
filter(historical_gdp_stats_growth %>% filter(between(year, 1850, 2015)), country %in% c('USSR', 'Russia', 'United States'), !is.na(rgdpnapc)) %>%
  mutate(label = ifelse(year == max(year), country, NA)) %>%
  ggplot(aes(year, rgdpnapc)) +
  theme_bw() +
  geom_line(aes(group = country)) +
  geom_point(aes(colour = polity_desc), size = 1.5) +
  scale_alpha(guide = F) +
  scale_colour_manual(name = "Type of Government", values = c('Autocracy' = '#e61938', 'Anocracy (mixed)' = 'gray', 'Democracy' = '#0045a1')) +
  # scale_colour_gradient2(name = 'Polity V Score\n>0 is Democratic',low = '#e61938', high = '#0045a1', mid = 'gray', midpoint = 0) +
  # scale_colour_hue(name = 'Democratic', labels = c('TRUE' = 'Yes', 'FALSE' = 'No')) +
  geom_label_repel(aes(label = label), nudge_x = 1, na.rm = T, nudge_y = 3) +
  scale_y_continuous(labels = dollar) +
  scale_x_continuous(breaks = seq(1850, 2015, by = 10)) +
  labs(
    y = 'Real GDP Per Capita (2011 USD)\n', x = '',
    title = 'Political Power and Stability vs. Economic Prosperity',
    subtitle = 'Comparing divergent political and economic paths of the U.S. and Russia/USSR, 1850-2015',
    caption = 'Chart: Taylor G. White\nData: Polity Project, Maddison Project'
  ) +
  theme_minimal() +
  theme(
    legend.background = element_rect(fill = 'white'),
    legend.position = c(0.10, 0.875),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16),
    plot.subtitle = element_text(size = 18),
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 12),
    plot.caption = element_text(size = 13, face = 'italic', hjust = 0),
    title = element_text(size = 26)
  )
ggsave('russia+ussr versus US.png', height = 9, width = 12, units = 'in', dpi = 600)

##### UK versus Australia #####
filter(historical_gdp_stats_growth %>% filter(between(year, 1850, 2015)), country %in% c('Australia', 'United Kingdom'), !is.na(rgdpnapc)) %>%
  mutate(label = ifelse(year == max(year), country, NA)) %>%
  ggplot(aes(year, rgdpnapc)) +
  theme_bw() +
  geom_line(aes(colour = country)) +
  geom_point(aes(size = polity2, shape = polity_desc, colour = country)) +
  scale_size(range = c(0, 2.55)) + 
  # scale_alpha(guide = F) +
  # scale_colour_manual(name = "Type of Government", values = c('Autocracy' = '#e61938', 'Anocracy (mixed)' = 'gray', 'Democracy' = '#0045a1')) +
  # scale_colour_gradient2(name = 'Polity V Score\n>0 is Democratic',low = '#e61938', high = '#0045a1', mid = 'gray', midpoint = 0) +
  # scale_colour_hue(name = 'Democratic', labels = c('TRUE' = 'Yes', 'FALSE' = 'No')) +
  geom_label_repel(aes(label = label), nudge_x = 1, na.rm = T, nudge_y = 3) +
  scale_y_continuous(labels = dollar) +
  scale_x_continuous(breaks = seq(1850, 2015, by = 10)) +
  labs(
    y = 'Real GDP Per Capita (2011 USD)\n', x = '',
    title = 'Political Power and Stability vs. Economic Prosperity',
    subtitle = 'Comparing divergent political and economic paths of the U.S. and Russia/USSR, 1850-2015',
    caption = 'Chart: Taylor G. White\nData: Polity Project, Maddison Project'
  ) +
  theme_minimal() +
  theme(
    legend.background = element_rect(fill = 'white'),
    legend.position = c(0.10, 0.875),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16),
    plot.subtitle = element_text(size = 18),
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 12),
    plot.caption = element_text(size = 13, face = 'italic', hjust = 0),
    title = element_text(size = 26)
  )
ggsave('russia+ussr versus US.png', height = 9, width = 12, units = 'in', dpi = 600)


##### median income by polity type #####

filter(historical_gdp_stats_growth, is.na(polity_desc)) %>% select(country, year, polity2, interim, prior, regtrans) %>% filter(year > 1800) %>%View()
growth_by_polity_type = group_by(historical_gdp_stats_growth, year, polity_desc) %>% 
  filter(year >= 1800) %>%
  summarize(
    countries_with_coups = n_distinct(country[n_coups > 0]),
    median_rgdpnapc = median(rgdpnapc, na.rm = T),
    median_growth = median(real_gdp_cap_growth, na.rm = T),
    obs = length(!is.na(rgdpnapc))
  )

ggplot(growth_by_polity_type, aes(year, median_rgdpnapc )) +
  geom_line(aes(colour = polity_desc)) +
  geom_point(aes(colour = polity_desc, alpha = obs, size = obs))


top_incomes_by_year = group_by(year) %>%
  top_n(20, rgdpnapc)

top_n(historical_gdp_stats_growth, 10, rgdpnapc)


historical_gdp_stats_growth %>% filter(year >= 1800, pop >= 1000) %>%
ggplot(aes(factor(cyl), mpg)) +
  geom_boxplot() +
  # Here comes the gganimate code
  transition_states(
    gear,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() +
  exit_shrink() +
  ease_aes('sine-in-out')


##### global trends, democracy and growth #####

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
  ) +
  geom_vline(aes(xintercept = 1989))


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



##### coups, civil wars, and population/income impacts #####

##### how likely are coups to cluster in time? #####

# for each year with a coup, find how many coups preceded and followed? 
e = exp(1)
ggplot(stats_by_country, aes(income_1850, income_2015, size = pop_2015)) +
  geom_point(aes(colour = factor(coup_war_score))) + 
  scale_y_continuous(labels = dollar) +
  scale_x_continuous(labels = dollar) +
  stat_smooth( se = F, span = 1) 
  # scale_y_continuous(trans = log_trans(),
  #                    # labels = dollar,
  #                         breaks = trans_breaks("log", function(x) e^x),
  #                         labels = trans_format("log", math_format(e^.x))) +
  # scale_x_continuous(trans = log_trans(),
  #                    # labels = dollar,
  #                    breaks = trans_breaks("log", function(x) e^x),
  #                    labels = trans_format("log", math_format(e)))


ggplot(stats_by_country, aes(pop_avg_growth, period_avg_growth )) +
  geom_point() 

names(stats_by_country)
ggplot(stats_by_country, aes(income_1850, income_2015)) +
  geom_point(aes(fill = percent_democracy_years ), pch = 21, size = 5) +
  scale_fill_viridis_c()

ggplot(stats_by_country, aes(income_1850, income_2015)) +
  geom_point(aes(fill = avg_polity2  ), pch = 21, size = 5) +
  geom_text_repel(aes(label = country)) +
  # scale_fill_gradient2(low = 'red', mid = 'gray', high= 'blue', midpoint = 5) +
  scale_fill_viridis_c(option = 'A') +
    scale_colour_viridis_c() 

ggplot(stats_by_country %>% filter(!is.na(avg_polity2)), aes(income_1900, income_2015)) +
  theme_bw() +
  labs(
    x = 'Real Per Capita GDP in 1900',
    y = 'Real Per Capita GDP in 2015'
  ) +
  scale_x_continuous(labels = dollar) +
  scale_y_continuous(labels = dollar) +
  geom_point(aes(fill = avg_polity2  ), pch = 21, size = 5) +
  geom_text_repel(aes(label = country)) +
  # scale_fill_gradient2(low = 'red', mid = 'gray', high= 'blue', midpoint = 5) +
  scale_fill_viridis_c(option = 'A', name = 'Avg. Polity Score') +
  scale_colour_viridis_c() 

names(historical_gdp_stats_growth)
simple_growth_model = lm(real_gdp_cap_growth ~ year + autocratic_shift + polity_desc + lagged_real_gdp_cap_growth, data = historical_gdp_stats_growth)
summary(simple_growth_model)
