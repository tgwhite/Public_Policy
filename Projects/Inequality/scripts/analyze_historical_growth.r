library(tidyverse)
library(data.table)
library(readxl)
library(scales)
library(ggrepel)
library(ggforce)
library(gganimate)
library(gifski)
library(xts)
library(roll)
library(rnaturalearth)
library(rnaturalearthdata)
library(countrycode)
library(cowplot)
options(na.action = na.exclude)

setwd("~/Public_Policy_Upd/Projects/Inequality")

get_period_growth = function(start, end, periods) {
  period_avg_growth = (end / start)^(1/periods) - 1
  return(period_avg_growth)
}


##### get polity data #####
polity_political_violence = read_excel('data/MEPVv2018.xls')
polity_coups = read_excel('data/CSPCoupsAnnualv2018 (1).xls')
polity_v = read_excel( "data/polity iv data.xls" ) 

filter(polity_political_violence, str_detect(country %>% tolower(), 'vietnam')) %>% pull(country) %>% unique()
filter(polity_political_violence, str_detect(country %>% tolower(), 'cambodia')) %>% pull(country) %>% unique()
filter(polity_political_violence, str_detect(country %>% tolower(), 'laos')) %>% pull(country) %>% unique()

polity_v_fin = 
  left_join(polity_v, polity_coups) %>%
  left_join(polity_political_violence) %>%
  mutate(
    country = recode(country, 
                     UAE = 'United Arab Emirates',
                     Bosnia =  "Bosnia and Herzegovina",
                     `Congo Brazzaville` = 'Congo',
                     `Congo Kinshasa` = 'D.R. of the Congo',
                     `Korea South` = 'South Korea',
                     `Korea North` = 'North Korea',
                     `USSR` = 'Former USSR')
  )



world_map <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  mutate(
    name = recode(name, 
                  `Dem. Rep. Korea` = 'South Korea', 
                  `Czech Rep.` = 'Czech Republic', 
                  `Slovakia` = 'Slovak Republic',
                  `Bosnia and Herz.` = 'Bosnia and Herzegovina',
                  `Macedonia` = 'North Macedonia',
                  `Dem. Rep. Congo` = "D.R. of the Congo",
                  `S. Sudan` = 'South Sudan',
                  `Eq. Guinea`="Equatorial Guinea",
                  `Central African Rep.` = "Central African Republic",
                  `North Macedonia` = 'Macedonia',
                  `Myanmar` = 'Myanmar (Burma)',
                  `N. Cyprus` = 'Cyprus',
                  `Lao PDR` = 'Laos'
                  )
  )
world_map$name = ifelse(str_detect(world_map$name, 'Ivoire'), "Cote D'Ivoire", world_map$name)





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
                     `Sudan (Former)` = 'Sudan',
                     `Myanmar` = 'Myanmar (Burma)',
                     `Slovakia` = 'Slovak Republic',
                     `Venezuela (Bolivarian Republic of)` = 'Venezuela',
                     `Taiwan, Province of China` = 'Taiwan',
                     `TFYR of Macedonia` = 'Macedonia',
                     # `D.R. of the Congo`='Congo Kinshasa',
                     # `Congo` = 'Congo-Brazzaville',
                     `Republic of Moldova` = 'Moldova',
                     `Dominica` = 'Dominican Republic',
                     `U.R. of Tanzania: Mainland` = 'Tanzania',
                     `China, Hong Kong SAR` = 'Hong Kong'
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
      lag_1_rgdpnapc = lag(rgdpnapc, 1),
      lag_2_rgdpnapc = lag(rgdpnapc, 2),
      lag_5_rgdpnapc = lag(rgdpnapc, 5),
      lag_10_rgdpnapc = lag(rgdpnapc, 10),
      lag_10_rgdpnapc_interpolated = lag(rgdpnapc_interpolated, 10),
      pop_interpolated = na.approx(pop, na.rm = F),
      real_gdp_cap_growth = (rgdpnapc - lag(rgdpnapc, 1))/ lag(rgdpnapc, 1),
      real_gdp_cap_growth_interp = (rgdpnapc_interpolated - lag(rgdpnapc_interpolated, 1))/ lag(rgdpnapc_interpolated, 1),
      roll_5_real_gdp_cap_growth_interp = roll_mean(real_gdp_cap_growth_interp, 5),
      lag_5_roll_5_real_gdp_cap_growth_interp = lag(roll_5_real_gdp_cap_growth_interp, 5),
      pop_growth = (pop - lag(pop, 1)) / lag(pop, 1),
      pop_growth_interp = (pop_interpolated - lag(pop_interpolated, 1)) / lag(pop_interpolated, 1),
      lagged_real_gdp_cap_growth = lag(real_gdp_cap_growth, 1),
      lagged_real_gdp_cap_growth_interp = lag(real_gdp_cap_growth_interp, 1)
    )
  
  full_year_df
  
}, by = list(country)] %>%
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


historical_gdp_stats_growth_rollstats = historical_gdp_stats_growth[, {
  lagged_polity2 = lag(polity2, 1)
  mean_real_gdp_cap_growth_interp_10 = roll_mean(real_gdp_cap_growth_interp, 10) 
  
  list(
    year = year, 
    mean_real_gdp_cap_growth_interp_10  = mean_real_gdp_cap_growth_interp_10,
    lagged_mean_real_gdp_cap_growth_interp_10 = lag(mean_real_gdp_cap_growth_interp_10, 1),  
    n_polity_changes_last_20_years = roll_sum(polity2 - lagged_polity2 != 0, 20),
    lagged_polity2 = lagged_polity2,
    n_autocratic_shifts_last_10_years = roll_sum(autocratic_shift, 10),
    n_coup_attempts_last_10_years = roll_sum(atcoup2, 10),
    n_coups_last_10_years = roll_sum(scoup1, 10),
    n_civil_war_years_last_10_years = roll_sum(civwar > 0, 10)  
  )
}, by = list(country)]


stats_by_country = historical_gdp_stats_growth[,{
  list(
    anocracy_years = sum(between(polity2, -5, 5), na.rm = T),
    democracy_years = sum(polity2 > 5, na.rm = T),
    autocracy_years = sum(polity2 < -5, na.rm = T),
    civ_war_denom = sum(!is.na(civwar)),
    n_civil_war_years = sum(civwar > 0, na.rm = T),
    avg_polity2 = mean(polity2, na.rm = T),
    max_polity2 = max(polity2, na.rm = T),
    min_polity2 = min(polity2, na.rm = T),
    transition_periods = sum(is.na(polity2)),
    n_coup_attempts = sum(atcoup2, na.rm = T),
    n_successful_coups = sum(scoup1, na.rm = T)
  )
}, by = list(country)] 

polity_income_years = c(1850, 1900, seq(1950, 2010, by = 10), 2015)
wide_income_polity_stats = 
  historical_gdp_stats_growth %>% filter(year %in% polity_income_years) %>%
  select(-polity) %>%
  rename(income = rgdpnapc_interpolated, polity = polity2) %>%
  pivot_wider(id_cols = c('country'), 
            values_from = c('polity', 'income', 'pop'), names_from = 'year', values_fn = mean)

stats_by_country = left_join(
  stats_by_country, 
  wide_income_polity_stats
) %>%
  dplyr::mutate(
    pct_civil_war_years = n_civil_war_years / civ_war_denom,
    percent_democracy_years = democracy_years / (anocracy_years + democracy_years + autocracy_years),
    coup_war_score = (n_civil_war_years > 0) + (n_coup_attempts > 0 | n_successful_coups > 0),
    growth_1850_2015 = get_period_growth(income_1850, income_2015, 2015 - 1850 + 1),
    growth_1900_2015 = get_period_growth(income_1900, income_2015, 2015 - 1900 + 1),
    growth_1950_2015 = get_period_growth(income_1950, income_2015, 2015 - 1950 + 1),
    percentile_1900 = cume_dist(income_1900),
    percentile_2015 = cume_dist(income_2015)
  )



stats_by_year = group_by(historical_gdp_stats_growth, year) %>%
  summarize(
    annual_income_95 = quantile(rgdpnapc, probs = 0.95, na.rm = T),
    annual_income_max = max(rgdpnapc, na.rm = T),
    annual_income_median = quantile(rgdpnapc, probs = 0.5, na.rm = T),
    avg_world_gdp_growth = mean(real_gdp_cap_growth, na.rm = T)
  )

historical_gdp_stats_growth_fin = left_join(
  historical_gdp_stats_growth, 
  stats_by_country
) %>%
  left_join(
    stats_by_year
  ) %>%
  left_join(
    historical_gdp_stats_growth_rollstats
  ) %>%
  mutate(
    percent_of_max_income = rgdpnapc_interpolated / annual_income_max,
    had_attempted_coup = atcoup2 > 0,
    had_successful_coup = scoup1 > 0,
    civwar_factor = factor(civwar),
    log_lag_10_rgdpnapc_interpolated = log(lag_10_rgdpnapc_interpolated),
    log_rgdpnapc_interpolated = log(rgdpnapc_interpolated),
    percent_polity_changes_20 = n_polity_changes_last_20_years / 20
  )


##### quick model for per capita gdp #### 

filter(historical_gdp_stats_growth_fin, country == 'Argentina', between(year, 1940, 1980)) %>% select(year, scoup1, atcoup2 )

names(historical_gdp_stats_growth_fin)

full_mod = lm(log_rgdpnapc_interpolated ~ log_lag_10_rgdpnapc_interpolated + avg_world_gdp_growth + lag_5_roll_5_real_gdp_cap_growth_interp +
                percent_of_max_income + year + polity2 + had_attempted_coup + 
                civwar_factor + had_successful_coup + percent_polity_changes_20, 
             data = historical_gdp_stats_growth_fin)
summary(full_mod)
plot(full_mod)

polity_with_coups_wars = lm(log_rgdpnapc_interpolated ~ log_lag_10_rgdpnapc_interpolated + percent_of_max_income * polity2 + 
                              had_attempted_coup + had_successful_coup + I(civwar > 0) + 
                              lag_5_roll_5_real_gdp_cap_growth_interp + avg_world_gdp_growth + year, 
                     data = historical_gdp_stats_growth_fin %>% filter(year <= 2005))

summary(polity_with_coups_wars)

pure_polity_mod = lm(log_rgdpnapc_interpolated ~ log_lag_10_rgdpnapc_interpolated + percent_of_max_income + polity_desc*polity2 + percent_polity_changes_20 + 
                       lag_5_roll_5_real_gdp_cap_growth_interp + avg_world_gdp_growth + year, 
                     data = historical_gdp_stats_growth_fin %>% filter(year <= 2005))

summary(pure_polity_mod)


predict(pure_polity_mod, newdata = overall_means %>% 
          mutate(
            log_lag_10_rgdpnapc_interpolated = log(6211.999),
            avg_world_gdp_growth = 0.015,
            year = 2015,
            lagged_mean_real_gdp_cap_growth_interp_10 = 0.06,
            polity2 = -7
            )
        ) %>% exp()


anova(pure_polity_mod, polity_desc_mod)

roll_mod = lm(log(rgdpnapc) ~ log(lag_10_rgdpnapc) + avg_world_gdp_growth + I(rgdpnapc / annual_income_max) + year + polity2 + 
                I(n_coup_attempts_last_10_years > 0) + I(n_civil_war_years_last_10_years > 0) + I(n_coups_last_10_years > 0), 
             data = historical_gdp_stats_growth_fin)

levels(historical_gdp_stats_growth_fin$polity_desc)
historical_gdp_stats_growth_fin$predicted_income_polity_mod = predict(pure_polity_mod, 
                                                                      newdata = historical_gdp_stats_growth_fin %>%
                                                                        mutate(
                                                                          # polity2 = ifelse(country == 'Argentina', 10, polity2),
                                                                          # polity_desc = ifelse(country == 'Argentina', levels(historical_gdp_stats_growth_fin$polity_desc)[3], polity_desc),
                                                                          # polity_desc = factor(polity_desc)
                                                                        )
                                                                      )

ggplot(historical_gdp_stats_growth_fin, aes(predicted_income_polity_mod %>% exp(), rgdpnapc)) +
  facet_wrap(~year <= 2005) + 
  geom_point()

historical_gdp_stats_growth_fin %>% filter(country %in% c('Argentina', 'United States', 'Botswana', 'Japan', 'South Korea'), year > 1850) %>%
ggplot(aes(year)) +
  geom_line(aes(y = predicted_income_full_mod %>% exp(), colour = country), linetype = 'dashed') +
  geom_line(aes(y = rgdpnapc, colour = country)) 

historical_gdp_stats_growth_fin %>% filter(country %in% c('Argentina', 'United States', 'Botswana', 'Japan', 'South Korea', 'China'), year > 1850) %>%
  ggplot(aes(year)) +
  facet_wrap(~country) +
  geom_line(aes(y = predicted_income_polity_mod %>% exp(), colour = country), linetype = 'dashed') +
  geom_line(aes(y = rgdpnapc_interpolated, colour = country)) 
filter(historical_gdp_stats_growth_fin, country == 'China') %>% View()




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

argentina_stats = filter(stats_by_country, country == 'Argentina') %>% as.data.frame()

dollar_label_func = function(x) {
  dollar(round(x, 0))
}
ggplot(stats_by_country, aes(income_1900, income_2015)) +
  theme_bw() +
  # geom_vline(aes(xintercept = ifelse(country == 'Argentina', income_1900, NA)), na.rm = T, linetype = 'dashed') +
  # geom_hline(aes(yintercept = ifelse(country == 'Argentina', income_2015, NA)), na.rm = T, linetype = 'dashed') +
  geom_segment(data = argentina_stats, aes(x = income_1900, xend = 8000, y = income_2015, yend = 15000), colour = 'orange') +
  geom_label(data = argentina_stats, aes(x = 8000, y = 15000, 
                                         label = sprintf("Argentina's Income was in the %sth percentile in 1900, but fell to the %sth percentile by 2015.", 
                                                         round(percentile_1900*100), round(percentile_2015 * 100)) %>% str_wrap(32)
                                         )) +
  
# annotate('text', x = 8000, y = 15000, label = sprintf("Argentina's Income was in the %sth percentile in 1900", round(argentina_stats$percentile_1900*100)) %>% str_wrap(32)) +
  
  # geom_segment(aes(x = 0, xend = ifelse(country == 'Argentina', income_1900, NA), y = ifelse(country == 'Argentina', income_2015, NA), yend = ifelse(country == 'Argentina', income_2015, NA)), na.rm = T, linetype = 'dashed') +
  # geom_segment(aes(y = 0, yend = ifelse(country == 'Argentina', income_2015, NA), x = ifelse(country == 'Argentina', income_1900, NA), xend = ifelse(country == 'Argentina', income_1900, NA)), na.rm = T, linetype = 'dashed') +
  labs(
    x = 'Log Real Per Capita GDP in 1900\n(2011 USD)',
    y = 'Log Real Per Capita GDP in 2015\n(2011 USD)',
    title = 'Per Capita Income in 1900 vs. 2015'
  ) +
  
  # geom_label_repel(aes(label = ifelse(country %in% c('Argentina', 'United States'), country, NA)), na.rm = T) +
  scale_x_continuous(labels = dollar_label_func, trans = 'log') +
  scale_y_continuous(labels = dollar_label_func, trans = 'log') +
  stat_smooth(method = 'lm', se = F) +
  geom_point() 
  




us_argentine_comparison %>%
  ggplot() +
  theme_bw() +
  # annotation_custom(grob = ggplotGrob(historical_income_plot), xmin = 1850, xmax = 1900, ymin = 20e3, ymax = 55e3) +
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
    # legend.background = element_rect(fill = 'white'),
    # legend.position = c(0.10, 0.90),
    legend.position = 'bottom',
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

filter(stats_by_country, !is.na(income_1900) & !is.na(income_2015)) %>%
  ungroup() %>%
  summarize(
    the_cor = cor(income_1900, income_2015),
    log_cor = cor(log(income_1900), log(income_2015))
  )

cor(stats_by_country$income_1900, stats_by_country$income_2015)


names(historical_gdp_stats_growth)

names(stats_by_country)

filter(stats_by_country, coup_war_score > 0)
simple_growth_model = lm(real_gdp_cap_growth ~ year + autocratic_shift + polity_desc + lagged_real_gdp_cap_growth, data = historical_gdp_stats_growth)
summary(simple_growth_model)

ggplot(historical_gdp_stats_growth, aes(lagged_real_gdp_cap_growth, real_gdp_cap_growth, colour = year > 1965)) +
  geom_point(alpha = 0.5) + stat_smooth(se = F, method = 'lm') +
  facet_wrap(~year > 1965)

ggplot(historical_gdp_stats_growth, aes(lagged_real_gdp_cap_growth, real_gdp_cap_growth)) +
  geom_point(alpha = 0.5, aes(colour = polity2)) + 
  scale_colour_gradient2(low = 'red', mid = 'gray', high= 'blue', midpoint = 5) +
  stat_smooth(se = F, method = 'lm') +
  facet_wrap(~year > 1975)

historical_gdp_stats_growth$polity2


##### map coups, civil wars #####

polity_income_vars = expand.grid(
  var = c('income', 'polity', 'pop'),
  year = c(seq(1960, 2010, by = 10), 2015)
) %>% 
  arrange(var, year) %>%
  mutate(
    var_combo = paste(var, year, sep = '_')
  )
stats_by_country$pop_1960

stats_by_country_long = pivot_longer(
  stats_by_country %>% as.data.frame(),
  cols = polity_income_vars$var_combo, names_to = 'variable', values_to = 'value'
)



stats_by_country_map = left_join(world_map, stats_by_country, by = c('name' = 'country'))
ggplot(stats_by_country_map) +
  geom_sf(aes(fill = n_civil_war_years > 0)) +
  geom_sf_label(aes(label = ifelse(is.na(n_civil_war_years), name, NA)), na.rm = T) +
  scale_fill_viridis_d()
names(stats_by_country_map)
ggplot(stats_by_country_map) +
  geom_sf(aes(fill = n_coup_attempts + n_successful_coups + n_civil_war_years > 0)) +
  # geom_sf_label(aes(label = ifelse(is.na(n_civil_war_years), name, NA)), na.rm = T) +
  scale_fill_viridis_d()

ggplot(stats_by_country_map) +
  geom_sf(aes(fill = income_2015 %>% log())) +
  # geom_sf_label(aes(label = ifelse(is.na(n_civil_war_years), name, NA)), na.rm = T) +
  scale_fill_viridis_c()



stats_by_country_map_long = left_join(world_map, stats_by_country_long, by = c('name' = 'country'))


filter(stats_by_country_map_long %>% filter(str_detect(variable, 'income')), continent == 'Africa') %>%
  ggplot() + 
  facet_wrap(~str_replace(variable, '_', " ") %>% str_to_title()) +
  geom_sf(aes(fill = value)) +
  scale_fill_viridis_c() 

filter(stats_by_country_map_long %>% filter(str_detect(variable, 'polity')), continent == 'Africa') %>%
  ggplot() + 
  facet_wrap(~str_replace(variable, '_', " ") %>% str_to_title()) +
  geom_sf(aes(fill = value)) +
  scale_fill_gradient2(low = 'red', high = 'blue', mid = 'gray', midpoint = 0)


maps_by_continent = lapply(unique(stats_by_country_map$continent), function(the_continent){
  # the_continent = 'North America'
  cont_sub = filter(stats_by_country_map, continent == the_continent)
  the_plot = ggplot(cont_sub) + 
    geom_sf(aes(fill = avg_polity2)) +
    scale_fill_viridis_c() +
    geom_sf_label(aes(label = ifelse(is.na(avg_polity2), name, NA)), na.rm = T)
  return(the_plot)
})
cowplot::plot_grid(plotlist = maps_by_continent)

View(polity_v_fin)
filter(polity_v_fin, country == 'Kazakhstan') %>% pull(polity2)
View(stats_by_country)
