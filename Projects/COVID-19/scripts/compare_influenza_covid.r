

library(tidyverse)
library(plotly)
library(readxl)
library(data.table)
library(htmltab)
library(incidence)
library(cowplot)
library(scales)
library(gganimate)
library(gifski)
library(gridExtra)

# start_week = 31
# end_week = 26

# goldstein uses 40-20, Rosano uses 42-17, CDC uses a very wide season
# https://journals.plos.org/plosmedicine/article?id=10.1371/journal.pmed.1001051
start_week = 40
end_week = 20
last_week = 53

italy_start_week = 30

# get excess italian deaths
# https://www.sciencedirect.com/science/article/pii/S1201971219303285#bib0025


italian_excess_deaths = tibble(
  season = c(2013:2016),
  deaths = c(7027, 20259, 15801, 24981),
  population = c(345168/0.00572, 375579/.00618, 354513/0.00584, 381578/.00628)
)

setwd("~/Public_Policy/Projects/COVID-19")

# read in extracted plot data from Rosano 2019 using https://apps.automeris.io/wpd/
rosano_webplot_extracts = read_csv("literature/wpd_datasets.csv", skip = 1) %>%
  mutate(
    excess_deaths_date = as.Date(X, format = '%m/%d/%Y'),
    baseline_date = as.Date(X_1, format = '%m/%d/%Y'),
    diff_excess_deaths_date = c(NA, diff(excess_deaths_date)),
    diff_baseline_date = c(NA, diff(baseline_date))
  ) %>% 
  rename(
    excess_deaths_val = Y,
    baseline_deaths_val = Y_1
  ) 

# use loess smoother for full date range for both plots 
full_date_range = c(rosano_webplot_extracts$excess_deaths_date, 
                    rosano_webplot_extracts$baseline_date) %>%
  range(na.rm = T)

date_df = data.frame(
  the_date = seq(full_date_range[1], full_date_range[2], by = 1)
)


# plot initial data
ggplot(rosano_webplot_extracts) +
  stat_smooth(aes(excess_deaths_date, excess_deaths_val), span= 0.1, se = F) +
  stat_smooth(aes(baseline_date, baseline_deaths_val), span= 0.1, colour = 'black', se = F) 

# fit smoothers across indvidual dates
excess_deaths_loess = loess(excess_deaths_val ~ as.numeric(excess_deaths_date), data = rosano_webplot_extracts %>% filter(), span = 0.1)
background_deaths_loess = loess(baseline_deaths_val ~ as.numeric(baseline_date), data = rosano_webplot_extracts %>% filter(), span = 0.1)

date_df$excess_deaths_smoothed = predict(excess_deaths_loess, 
                                         newdata = mutate(date_df, excess_deaths_date = the_date))

date_df$background_deaths_smoothed = predict(background_deaths_loess, 
                                         newdata = mutate(date_df, baseline_date = the_date))

date_df = mutate(date_df, 
                 excess_deaths_smoothed = pmax(excess_deaths_smoothed, background_deaths_smoothed),
                 year_week_orig = format(the_date, '%Y-%U'),
                 the_week = format(the_date, '%U') %>% as.numeric(), 
                 the_year = format(the_date, '%Y') %>% as.numeric(),
                 year_alt = ifelse(the_week == 0, the_year - 1, the_year),
                 week_alt = ifelse(the_week == 0, 52, the_week),
                 year_week = paste0(year_alt, '-', week_alt)
                 )

all_date_data_long = pivot_longer(date_df, cols = c('excess_deaths_smoothed', 'background_deaths_smoothed'))

## plot the smoothed data
ggplot(all_date_data_long, aes(the_date, value, colour = name)) +
  geom_line()

## compute weekly statistics
weekly_vals = group_by(all_date_data_long, year_week, name) %>%
  summarize(
    start_date = min(the_date),
    value = head(value, 1)
  )

# compute excess over baseline
wide_vals = pivot_wider(weekly_vals, id_cols = c('year_week', 'start_date'),
                        values_from = 'value', names_from = 'name') %>%
  mutate(
    excess_deaths_over_baseline = excess_deaths_smoothed - background_deaths_smoothed,
    year = year(start_date),
    week = format(start_date, '%U') %>% as.numeric(),
    season = ifelse(week>= italy_start_week, year, ifelse(week <= end_week, year-1, NA))
  ) %>%
  left_join(italian_excess_deaths %>% select(season, population)) %>%
  mutate(
    count_excess_deaths_over_baseline = excess_deaths_over_baseline * ( population/1e5)
  )

# total by season
totals_by_season = group_by(wide_vals, season) %>%
  summarize(
    sum_count_excess_deaths_over_baseline = sum(count_excess_deaths_over_baseline, na.rm = T),
    mean_weekly_excess_deaths_over_baseline = mean(excess_deaths_over_baseline, na.rm = T),
    obs = n()
  ) %>%
  left_join(italian_excess_deaths %>% select(-population)) %>%
  mutate(
    ratio = sum_count_excess_deaths_over_baseline / deaths
  )

# the rendering may have been off due to low pixellation in original graph, adjust to the ratio by season
wide_vals_with_ratios = left_join(
  wide_vals, totals_by_season, by = 'season'
) %>%
  mutate(
    count_excess_deaths_over_baseline_adj = count_excess_deaths_over_baseline / ratio
  )

season_starts = group_by(wide_vals_with_ratios, season) %>%
  summarize(
    season_start = min(start_date[count_excess_deaths_over_baseline_adj > 0])
  )

# final dataset -- get weeks since first death
italy_flu_season_deaths_fin = left_join(wide_vals_with_ratios, season_starts, by = 'season') %>%
  mutate(
    weeks_since_first_death = as.numeric(start_date - season_start) %/% 7,
    excess_deaths_per_100k = (count_excess_deaths_over_baseline_adj / population) * 1e5
  ) %>%
  rename(
    excess_deaths = count_excess_deaths_over_baseline_adj
  ) %>%
  select(
    start_date, weeks_since_first_death, year_week, season, excess_deaths, excess_deaths_per_100k
  )

# get 2015_season
italy_season_2015 = filter(italy_flu_season_deaths_fin, season == 2015) 

# plot results -- looks right! 
ggplot(italy_season_2015, aes(start_date, excess_deaths)) +
  geom_bar(stat = 'identity')

ggplot(italy_season_2015, aes(weeks_since_first_death, excess_deaths)) +
  geom_bar(stat = 'identity')



# as.Date('2013-04-28') %>% format('%U')
# as.Date('2017-04-23') %>% format('%U')

##### get US flu burden data, compute flu season shapes #####
historical_us_flu_burden = htmltab('https://www.cdc.gov/flu/about/burden/past-seasons.html') %>%
  rename(death_estimate = `Deaths >> Estimate`) %>%
  filter(death_estimate != 'Estimate') %>%
  mutate(
    death_estimate = death_estimate %>% str_remove(',') %>% as.numeric(),
    season = str_extract(Season, '[0-9]{4}') %>% as.numeric()
  )


# read in latest john's hopkins covid case / death data 
jh_joined_it_us = read_csv('data/jh_joined.csv') %>%
  filter(
    country_region %in% c('Italy', 'US')
  ) %>%
  data.table()

jh_joined_it_us_stats = jh_joined_it_us[, {
  new_deaths = deaths - lag(deaths, 1)
  new_deaths = ifelse(is.na(new_deaths), 0, new_deaths)
  first_death_date = min(date_upd[new_deaths > 0])
  days_since_first_death = as.numeric(date_upd - first_death_date)
  weeks_since_first_death = days_since_first_death %/% 7
  
  list(
    date_upd = date_upd,
    days_since_first_death = days_since_first_death, 
    first_death_date = first_death_date, 
    weeks_since_first_death = weeks_since_first_death,
    new_deaths = new_deaths
  )
}, by = list(country_region)]



# get weekly deaths for the U.S. and Italy 
weekly_deaths = group_by(jh_joined_it_us_stats, country_region, weeks_since_first_death) %>%
  summarize(
    total_deaths = sum(new_deaths),
    obs = n()
  )

us_weekly_deaths = filter(weekly_deaths, country_region == 'US', weeks_since_first_death >= 0) %>%
  mutate(
    Virus = 'COVID-19',
    projected_deaths = total_deaths / (obs/7),
    projected_minus_actual = projected_deaths - total_deaths
  )

italy_weekly_deaths = filter(weekly_deaths, country_region == 'Italy', weeks_since_first_death >= 0) %>%
  mutate(
    Virus = 'COVID-19',
    projected_deaths = total_deaths / (obs/7),
    projected_minus_actual = projected_deaths - total_deaths
  )

first_death_dates = filter(jh_joined_it_us_stats, days_since_first_death == 0)

#### compute flu seasons using U.S. season data 
#### read in excess flu / pneumonia deaths ####
# us_NCHSData12 = read_excel('literature/Italy Influenza vs. COVID.xlsx', 'NCHSData12')
us_NCHSData12 = read_csv('https://www.cdc.gov/flu/weekly/weeklyarchives2019-2020/data/NCHSData12.csv')
names(us_NCHSData12) = str_replace_all(names(us_NCHSData12), '[ ]', '_') %>% str_to_lower()

# last_week = 52
us_NCHSData12 = arrange(us_NCHSData12, year, week) %>%
  mutate(
    order = 1:length(year),
    season = ifelse(week>= start_week, year, ifelse(week <= end_week, year-1, NA)),
    pn_in_deaths = pneumonia_deaths + influenza_deaths
  ) %>%
  arrange(season, order) %>%
  data.table()

# us_NCHSData12 = left_join(us_NCHSData12, week_of_season) %>% filter(!is.na(season))

# calculate season peaks/shapes
season_diffs_calcs = us_NCHSData12[, {
  peak_val = max(percent_of_deaths_due_to_pneumonia_and_influenza)[1]
  peak_val_week = min(week[percent_of_deaths_due_to_pneumonia_and_influenza == peak_val])
  last_val = lag(percent_of_deaths_due_to_pneumonia_and_influenza, 1)
  delta_val = percent_of_deaths_due_to_pneumonia_and_influenza - last_val
  
  list(
    week = week,
    week_of_season = 1:length(order),
    order,
    year = year,
    all_deaths = all_deaths, 
    pn_in_deaths = pn_in_deaths, 
    pct_of_peak = percent_of_deaths_due_to_pneumonia_and_influenza / peak_val,
    last_val = last_val, 
    this_val = percent_of_deaths_due_to_pneumonia_and_influenza,
    delta_val = delta_val
  )
  
}, by = season]

# compare disease burden estimates to flu / pneumonia deaths and back out 
# what the baseline deaths would have to be to get the excess death numbers
get_baseline_value = function(pars, the_season) {
  est_burden = filter(historical_us_flu_burden, season == the_season) %>% pull(death_estimate)
  vals = filter(season_diffs_calcs, season == the_season) %>% pull(pn_in_deaths)
  burden_minus_vals = est_burden - sum(vals - pars[1])
  return(burden_minus_vals^2)
}

seasons_to_check = intersect(historical_us_flu_burden$season, season_diffs_calcs$season)
implied_baselines = map_dbl(seasons_to_check, function(the_season){
  the_solution = optim(fn = get_baseline_value, par = c(3000), method = 'Brent', lower = 2500, upper=4000, 
                       the_season = the_season)
  return(the_solution$par)
}) 
us_flu_baseline_deaths = tibble(season = seasons_to_check, baseline_deaths = implied_baselines)
historical_us_flu_burden_fin = left_join(historical_us_flu_burden, us_flu_baseline_deaths)

# add back the
season_diffs_calcs = left_join(season_diffs_calcs, historical_us_flu_burden_fin) %>% 
  data.table()


### compute excess deaths using this mean baseline number ###
season_diffs_calcs_pct_of_excess = season_diffs_calcs[season %in% seasons_to_check, {
  
  # take deaths from pneumonia and influenza and subtract baseline deaths
  excess_deaths = pmax(pn_in_deaths - baseline_deaths, 0)
  total_excess = sum(excess_deaths)
  first_death_week = min(week_of_season[excess_deaths > 0])
  
  list(
    weeks_since_first_death = week_of_season - first_death_week,
    # covid_week = week_of_season * (r0_flu_covid_ratio),
    week_of_season = week_of_season,
    excess_deaths = excess_deaths, 
    pct_of_excess = excess_deaths / total_excess
  )
}, by = season] %>%
  mutate(
    Virus = 'Influenza'
  )



# check the calculations. Most are spot on, but some are *slightly* off. There may
#  be rounding somewhere in the CDC weekly death data 
calculated_excess = filter(season_diffs_calcs_pct_of_excess, season == 2013)$excess_deaths %>% sum()
listed_excess = filter(historical_us_flu_burden, season == 2013)$death_estimate
#stopifnot(calculated_excess == listed_excess)

cat('!!!calculated_excess - listed_excess is', calculated_excess-listed_excess, '\n')

pcts_by_week = group_by(season_diffs_calcs_pct_of_excess, week_of_season) %>%
  summarize(
    avg_pct = mean(pct_of_excess)
  )

ggplot(season_diffs_calcs_pct_of_excess, aes(weeks_since_first_death, excess_deaths)) +
  facet_wrap(~season) +
  geom_bar(stat = 'identity')

#### show excess deaths and season shapes ####
the_plot = ggplot(season_diffs_calcs_pct_of_excess, aes(week_of_season, pct_of_excess)) +
  geom_line(aes(colour = factor(season))) +
  geom_point(aes(colour = factor(season))) +
  geom_line(data = pcts_by_week, aes(week_of_season, avg_pct), size = 1.25)


# us_incidence = filter(jh_joined_it_us_stats, new_deaths > 0, country_region == 'US')
# 
# incidence_dates = map(us_incidence$date_upd, function(this_date){
#   date_vals = filter(us_incidence, date_upd == this_date)
#   rep(this_date, each = date_vals$new_deaths)
# })
# 
# us_incidence_obj = as.incidence(x = us_incidence$new_deaths, dates = us_incidence$date_upd)
# a = fit(us_incidence_obj)

add_extra_bars = function(a_plot, weekly_stats, the_hjust = 0.5, the_angle = 0, projected_label = ' Projected') {
  the_sub = filter(weekly_stats, weeks_since_first_death >=0, obs < 7)
  if (nrow(the_sub) == 0) {
    return(a_plot + geom_blank())
  } else {
    comb_plot = a_plot + 
      geom_bar(data = the_sub, 
               aes(weeks_since_first_death, projected_deaths, fill = Virus), alpha = 0.3, stat = 'identity') +
      geom_text(data = the_sub, 
                aes(weeks_since_first_death, projected_deaths, 
                    label = projected_label), hjust = the_hjust, size = 2.5, angle = the_angle) 
    return(comb_plot)
  }
}

us_max_scale = max(us_weekly_deaths$total_deaths) * 1.05
us_main_plot = season_diffs_calcs_pct_of_excess %>% 
  filter(weeks_since_first_death >= 0,
         excess_deaths > 0, season == 2016) %>%
  ggplot(aes(weeks_since_first_death, excess_deaths)) +
  theme_bw() +
  labs(
    x = '\nWeeks Since First Death',
    y = 'Excess Deaths\n',
    title = 'U.S. COVID-19 Deaths vs. Typical Flu Season',
    subtitle = sprintf('2016 Flu Season: 38,000 Excess Deaths. COVID-19: %s deaths through %s.', 
                       comma(sum(us_weekly_deaths$total_deaths)), format(max(jh_joined_it_us_stats$date_upd), '%B %d')),
    caption = "Chart: Taylor G. White\nData: Johns Hopkins CSSE, CDC"
  ) +
  theme(
    plot.caption = element_text(hjust = 0, size = 10),
    plot.subtitle = element_text(size = 11, face='italic'),
    title = element_text(size = 16),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 12)
  ) +
  geom_bar(aes(fill = Virus), alpha = 0.3, stat = 'identity', position = 'identity') +
  geom_bar(data = us_weekly_deaths %>%
             filter(weeks_since_first_death >=0), aes(weeks_since_first_death, total_deaths, fill = Virus, alpha = obs),
           stat = 'identity', colour = 'black', size = 0.75) +
  scale_x_continuous(breaks = seq(0, 30, by = 5))  +
  scale_alpha(guide = F, range = c(0.2, 0.4)) 
  
us_main_plot_with_scale = us_main_plot + 
  scale_fill_manual(name = '', values = c('Influenza' = 'black', 'COVID-19' = 'red')) +
  scale_y_continuous(labels = comma, breaks = seq(0, us_max_scale, by = 2000))
fin_us_plot_scale = add_extra_bars(us_main_plot_with_scale, us_weekly_deaths, the_angle = 90, projected_label = paste0('Projected', paste(rep(' ', 8), collapse = '')))
ggsave('output/U.S. covid_19 vs. 2016 flu season deaths.png', height = 6, width = 8, units = 'in', dpi = 800, plot = fin_us_plot_scale)

us_main_plot_noscale = us_main_plot + 
  scale_fill_manual(guide = F, name = '', values = c('Influenza' = 'black', 'COVID-19' = 'red')) +
  scale_y_continuous(labels = comma, breaks = seq(0, us_max_scale, by = 2000), limits = c(0, us_max_scale))   


us_main_plot_scale = us_main_plot + 
  scale_fill_manual(name = '', values = c('Influenza' = 'black', 'COVID-19' = 'red')) +
  scale_y_continuous(labels = comma, breaks = seq(0, us_max_scale, by = 2000), limits = c(0, us_max_scale))   +
  theme(legend.position = 'bottom')



fin_us_plot_noscale = add_extra_bars(us_main_plot_noscale, us_weekly_deaths, the_angle = 90, projected_label = paste0('Projected', paste(rep(' ', 30), collapse = '')))

#### animate us plot ####

season_diffs_calcs_pct_of_excess_covid = 
  left_join(season_diffs_calcs_pct_of_excess, us_weekly_deaths, by = c('weeks_since_first_death')) %>%
  mutate(
    flu_virus = 'Influenza',
    covid_virus = 'COVID-19',
    flu_obs = 7,
    Projected = 'Projected'
  )

min_obs = min(season_diffs_calcs_pct_of_excess_covid$obs, na.rm = T)
alpha_vec = c("7" = 0.5)
alpha_vec[paste(min_obs)] = 0.3

flu_burden_table = filter(historical_us_flu_burden, season %in% season_diffs_calcs_pct_of_excess_covid$season) %>%
  mutate(death_estimate_pretty = comma(death_estimate)) %>%
  select(
    Season = season, 
    `Flu Deaths` = death_estimate_pretty
  )

the_anim = ggplot(season_diffs_calcs_pct_of_excess_covid %>% filter(season < 2019), aes(weeks_since_first_death)) + 
  theme_bw() +
  geom_bar(aes(y = excess_deaths, alpha = paste0(flu_obs), fill = flu_virus), stat = 'identity') +
  geom_bar(aes(y = projected_deaths, alpha = paste0(obs), fill = covid_virus),
           stat = 'identity') +
  geom_bar(aes(y = total_deaths, alpha = paste0(obs), fill = covid_virus),
           stat = 'identity', colour = 'black') +
  annotate("text", x = max(us_weekly_deaths$weeks_since_first_death), y = 12500, label = "Projected", angle = 90, size = 2.75) +
  scale_fill_manual(name = '', values = c('Influenza' = 'black', 'COVID-19' = 'red')) +
  annotation_custom(tableGrob(flu_burden_table %>% filter(Season < 2019), rows = NULL), xmin=20, xmax=30, ymin=12000, ymax=12000) +
  transition_states(
     season,
     transition_length = 2,
     state_length = 1
   ) +
  enter_fade() + 
  exit_shrink() +
  labs(
    title = 'Deaths from COVID-19 vs. Recent Flu Seasons in the U.S.',
    subtitle = sprintf('COVID-19: %s deaths through %s\nFlu season: {closest_state}',
                       comma(sum(us_weekly_deaths$total_deaths)), format(max(jh_joined_it_us_stats$date_upd), '%B %d')), 
       x = 'Weeks Since First Death', y = 'Weekly Deaths',
    caption = 'Chart: Taylor G. White\nData: CDC, Johns Hopkins CSSE') +
  ease_aes('sine-in-out') +
  theme(
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 11, face = 'italic'),
    plot.caption = element_text(size = 10, face = 'italic', hjust = 0)
  ) +
  scale_alpha_manual(guide = F, values = alpha_vec, labels = c('Actual', 'Projected')) +
  scale_y_continuous(labels = comma, breaks = seq(0, us_max_scale, by = 2000))  +
  scale_x_continuous(breaks = seq(0, 33, by = 5))

animate(the_anim, nframes = 100,
        renderer = gifski_renderer("output/us_flu_vs_covid_deaths.gif"),
        height = 6, width = 8, units = 'in',  type = 'cairo-png', res = 200)

# use shape of US 2016 flu season as proxy for Italian 2014-2015 season
mean_italian_flu_season = mean(italian_excess_deaths$deaths)

season_comparison = filter(season_diffs_calcs_pct_of_excess,
                           season == 2016) %>%
  mutate(
    weeks_since_first_death_covid = floor(weeks_since_first_death * 0.5),
    weeks_since_first_death_covid_rnd = floor(weeks_since_first_death_covid),
    projected_excess_deaths = pct_of_excess * mean_italian_flu_season,
    pct_of_excess_pretty = percent(pct_of_excess, accuracy = 0.01)
  ) %>%
  group_by(weeks_since_first_death) %>%
  summarize(
    total_excess = sum(projected_excess_deaths)
  ) %>%
  ungroup() %>%
  filter(weeks_since_first_death >=0) %>%
  mutate(
    Virus = 'Influenza'
  )


italy_lockdown = filter(jh_joined_it_us_stats, country_region == 'Italy', date_upd == as.Date('2020-03-09')) 
main_plot = italy_season_2015 %>% 
  filter(weeks_since_first_death >=0) %>%
  mutate(Virus = 'Influenza') %>%
  ggplot(aes(weeks_since_first_death, excess_deaths)) +
  theme_bw() +
  # geom_vline(data = italy_lockdown, aes(xintercept = weeks_since_first_death), linetype = 'dashed') +
  geom_bar(aes(fill = Virus), stat = 'identity', alpha = 0.3) +
  geom_bar(data = italy_weekly_deaths, aes(weeks_since_first_death, total_deaths, fill = Virus, alpha = obs), 
            stat = 'identity', colour = 'black', size = 0.75) +
  scale_alpha(guide = F, range = c(0.2, 0.4)) +
  labs(
    x = '\nWeeks Since First Death',
    y = 'Excess Deaths\n',
    title = 'Italy COVID-19 Deaths vs. Typical Flu Season',
    subtitle = sprintf('2015 Flu Season: 15,800 excess deaths. COVID-19: %s deaths through %s.', 
                       comma(sum(italy_weekly_deaths$total_deaths)), format(max(jh_joined_it_us_stats$date_upd), '%B %d')),
    caption = "Chart: Taylor G. White\nData: Rosano et. al. 2019, Johns Hopkins CSSE"
  ) +
  theme(
    plot.caption = element_text(hjust = 0, size = 10),
    plot.subtitle = element_text(size = 11, face='italic'),
    title = element_text(size = 16),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    legend.position = c(0.85, 0.85)
  ) +
  scale_x_continuous(breaks = seq(0, 27.5, by = 5), limits = c(0, 27.5)) +
  scale_fill_manual(name = '', values = c('Influenza' = 'black', 'COVID-19' = 'red')) 

italy_own_scale = main_plot + scale_y_continuous(labels = comma, breaks = seq(0, 6000, by = 1000)) 
italy_us_scale = main_plot + 
  scale_y_continuous(labels = comma, breaks = seq(0, us_max_scale, by = 2000), limits = c(0, us_max_scale))   

# add_extra_bars(main_plot, italy_weekly_deaths, the_hjust = 1, the_angle = 90, projected_label = 'Projected      ')

fin_italy_plot_ownscale = add_extra_bars(italy_own_scale, italy_weekly_deaths, the_hjust = 0.5, the_angle = 90, 
                                         projected_label = paste0('Projected', paste(rep(' ', 16), collapse = '')))
fin_italy_plot = add_extra_bars(italy_us_scale, italy_weekly_deaths, the_hjust = 0.5, the_angle = 90, 
                                projected_label = paste0('Projected', paste(rep(' ', 20), collapse = '')))

ggsave('output/average_italian_flu_deaths.png', height = 6, width = 8, units = 'in', dpi = 800, plot = fin_italy_plot_ownscale)  

combined_plot = plot_grid(fin_us_plot_noscale, fin_italy_plot_ownscale)
combined_plot_samescale = plot_grid(fin_us_plot_noscale, fin_italy_plot)
save_plot('output/combined_us_italy_flu_comparison.png', base_height = 10, base_width = 16, units = 'in', dpi = 600, plot = combined_plot)
save_plot('output/combined_us_italy_flu_comparison_same_scale.png', base_height = 10, base_width = 16, units = 'in', dpi = 600, plot = combined_plot_samescale)
