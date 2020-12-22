library(tidyverse)
library(data.table)
library(readxl)
library(scales)
library(ggrepel)
library(ggforce)

setwd("~/Public_Policy_Upd/Projects/Inequality")

polity_coups = read_excel('data/CSPCoupsAnnualv2018 (1).xls')
View(polity_coups)
polity_v = read_excel( "data/polity iv data.xls" ) 


polity_v_fin = 
  left_join(polity_v, polity_coups) %>%
  mutate(
    country = recode(country, 
                     UAE = 'United Arab Emirates',
                     Bosnia =  "Bosnia and Herzegovina",
                     `Congo Brazzaville` = 'Congo-Brazzaville',
                     `Korea South` = 'South Korea',
                     `Korea North` = 'North Korea')
  )
View(polity_v_fin)


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
                     `Former USSR` = 'USSR',
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
  full_join(
    polity_v_fin,
    by = c('country', 'year')
  ) %>%
  mutate(
    polity2 = ifelse(polity2 %in% c(-88, -66), NA, polity2),
    polity_desc = ifelse(between(polity2, -5, 5), 'Anocracy (mixed)', ifelse(polity2 > 5, 'Democracy', 'Autocracy')),
    polity_desc = factor(polity_desc, levels = c('Autocracy', 'Anocracy (mixed)', 'Democracy')),
    autocratic_shift = change < 0 & sign(polity2) != sign(prior) & is.na(interim),
    n_coups = scoup1 + atcoup2
  )


setwd('output')


##### US vs. Argentina #####

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
filter(us_argentine_comparison, country == 'Argentina', year %in% 1942:1948) %>% select(year, polity2, prior, durable, interim)

argentine_coups_regime_changes = filter(us_argentine_comparison, n_coups > 0 | autocratic_shift ) %>% 
  mutate(label = ifelse(scoup1 > 0, "Success", ifelse(n_coups > 0, "Failure", NA)))

argentina_rects = tribble(
  ~xmin, ~xmax,
  min(argentina_greater_us$year), max(argentina_greater_us$year),
  min(argentine_coups_regime_changes$year), max(argentine_coups_regime_changes$year)
)
filter(argentine_coups_regime_changes, autocratic_shift)

argentine_coups_regime_changes %>% select(country, year, prior, polity2, durable, regtrans, change, n_coups) %>% View()
n_coups = filter(argentine_coups_regime_changes, n_coups > 0) %>% pull(n_coups) %>% sum()
n_successful_coups = filter(argentine_coups_regime_changes, n_coups > 0) %>% pull(scoup1) %>% sum()
n_autocratic_shifts = filter(argentine_coups_regime_changes, autocratic_shift) %>% nrow()


us_argentine_comparison %>%
  ggplot() +
  theme_bw() +
  geom_rect(data = argentina_rects, aes(xmin = xmin, xmax = xmax, ymin = 0, 
                                        ymax = 55000), alpha = 0.25, fill = NA, colour = 'black', linetype = 'dashed') +  
  geom_line(aes(year, rgdpnapc, group = country)) +
  geom_point(data = argentine_coups_regime_changes %>% filter(!is.na(label)), aes(year, rgdpnapc, shape = label), size = 4) +
  geom_point(aes(year, rgdpnapc, colour = polity_desc), size = 1.5) +
  
  scale_shape_manual(name = "Coups d'Etat",values = c("Success" = 8, "Failure" = 5)) +
  # facet_zoom(xlim = c(1890, 1910), ylim = c(3000, 7000), horizontal = FALSE) +
  scale_alpha(guide = F) +
  scale_colour_manual(name = "Type of Government", values = c('Autocracy' = '#e61938', 'Anocracy (mixed)' = 'gray', 'Democracy' = '#0045a1')) +
  annotate('text', x = mean(argentina_greater_us$year), y = 55000, label = "Argentina's income surpasses U.S., 1894-1896", angle = 0, vjust = -0.5) +
  annotate('text', x = mean(argentine_coups_regime_changes$year), y = 55000, label = sprintf("%s autocratic shifts, %s coups d'etat (%s attempted)", n_autocratic_shifts, n_successful_coups, n_coups), angle = 0, vjust = -0.5) +
  # scale_colour_gradient2(name = 'Polity V Score\n>0 is Democratic',low = '#e61938', high = '#0045a1', mid = 'gray', midpoint = 0) +
  # scale_colour_hue(name = 'Democratic', labels = c('TRUE' = 'Yes', 'FALSE' = 'No')) +
  geom_label_repel(aes(year, rgdpnapc, label = label), nudge_x = 1, na.rm = T, nudge_y = 3) +
  scale_y_continuous(labels = dollar, breaks = seq(0, 50000, by = 10000)) +
  scale_x_continuous(breaks = seq(1850, 2015, by = 10)) +
  labs(
    y = 'Real GDP Per Capita (2011 USD)\n', x = '',
    title = 'Political Power and Stability vs. Economic Prosperity',
    subtitle = 'Comparing divergent political and economic paths of the U.S. and Argentina, 1850-2015',
    caption = 'Chart: Taylor G. White\nData: Polity Project, Maddison Project'
  ) +
  theme_minimal() +
  theme(
    legend.background = element_rect(fill = 'white'),
    legend.position = c(0.10, 0.75),
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
historical_gdp_stats_growth$real_gdp_cap_growth
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
