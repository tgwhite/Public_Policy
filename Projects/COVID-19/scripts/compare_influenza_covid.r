
library(tidyverse)
library(plotly)
library(readxl)
library(data.table)
library(htmltab)
library(incidence)

# parameters
# start_week = 31
# end_week = 26
start_week = 40
end_week = 20
last_week = 52

# the real ratio is 0.52 but this helps keep things simple coding-wise
r0_flu_covid_ratio = 0.5 

##### get US flu burden data, compute flu season shapes #####
historical_us_flu_burden = htmltab('https://www.cdc.gov/flu/about/burden/past-seasons.html') %>%
  rename(death_estimate = `Deaths >> Estimate`) %>%
  filter(death_estimate != 'Estimate') %>%
  mutate(
    death_estimate = death_estimate %>% str_remove(',') %>% as.numeric(),
    season = str_extract(Season, '[0-9]{4}') %>% as.numeric()
  )

setwd("~/Public_Policy/Projects/COVID-19")

#### read in excess flu / pneumonia deaths ####
# us_NCHSData12 = read_excel('literature/Italy Influenza vs. COVID.xlsx', 'NCHSData12')
us_NCHSData12 = read_csv('https://www.cdc.gov/flu/weekly/weeklyarchives2019-2020/data/NCHSData12.csv')
names(us_NCHSData12) = str_replace_all(names(us_NCHSData12), '[ ]', '_') %>% str_to_lower()

# get excess italian deaths
# https://www.sciencedirect.com/science/article/pii/S1201971219303285#bib0025

italian_excess_deaths = tibble(
  season = c(2013:2016),
  deaths = c(7027, 20259, 15801, 24981)
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
us_NCHSData12 = arrange(us_NCHSData12, year, week) %>%
  mutate(
    order = 1:length(year),
    season = ifelse(week>= start_week, year, ifelse(week <= end_week, year-1, NA)),
    week_of_season = ifelse(week>= start_week, week-start_week + 1, (last_week - start_week) + week + 1),
    pn_in_deaths = pneumonia_deaths + influenza_deaths
  ) %>%
  data.table()

# calculate season peaks/shapes
season_diffs_calcs = us_NCHSData12[, {
  peak_val = max(percent_of_deaths_due_to_pneumonia_and_influenza)[1]
  peak_val_week = min(week[percent_of_deaths_due_to_pneumonia_and_influenza == peak_val])
  last_val = lag(percent_of_deaths_due_to_pneumonia_and_influenza, 1)
  delta_val = percent_of_deaths_due_to_pneumonia_and_influenza - last_val
  
  list(
    week = week,
    week_of_season = week_of_season,
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
    covid_week = week_of_season * (r0_flu_covid_ratio), 
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
stopifnot(calculated_excess == listed_excess)

pcts_by_week = group_by(season_diffs_calcs_pct_of_excess, week_of_season) %>%
  summarize(
    avg_pct = mean(pct_of_excess)
  )
sum(pcts_by_week$avg_pct)

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

season_diffs_calcs_pct_of_excess %>% 
  filter(weeks_since_first_death >= 0,
         excess_deaths > 0, season == 2016) %>%
  ggplot(aes(weeks_since_first_death, excess_deaths)) +
  theme_bw() +
  labs(
    x = '\nWeeks Since First Death',
    y = 'Excess Deaths\n',
    title = 'Deaths Caused by COVID-19 vs. Typical Flu Season',
    subtitle = sprintf('U.S. 2016 Flu Season: 38,000 Excess Deaths. COVID-19: %s deaths through %s.', 
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
  scale_fill_manual(name = '', values = c('Influenza' = 'black', 'COVID-19' = 'red')) +
  geom_bar(aes(fill = Virus), alpha = 0.3, stat = 'identity', position = 'identity') +
  geom_bar(data = us_weekly_deaths %>%
             filter(weeks_since_first_death >=0), aes(weeks_since_first_death, total_deaths, fill = Virus),
           stat = 'identity', alpha = 0.3, colour = 'black', size = 0.75) +
  geom_text(data = us_weekly_deaths %>%
             filter(weeks_since_first_death >=0, obs < 7), 
            aes(weeks_since_first_death, total_deaths, label = paste0(' ', obs, ' out of 7 days in bar')), angle = 90, hjust = 0, size = 2.5) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = seq(0, 30, by = 5)) 
  
ggsave('output/U.S. covid_19 vs. 2016 flu season deaths.png', height = 6, width = 8, units = 'in', dpi = 800)

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

add_extra_bars = function(a_plot) {
  the_sub = filter(italy_weekly_deaths, weeks_since_first_death >=0, obs < 7)
  if (nrow(the_sub) == 0) {
    return(a_plot + geom_blank())
  } else {
    comb_plot = a_plot + 
    geom_bar(data = the_sub, 
             aes(weeks_since_first_death, projected_deaths, fill = Virus), alpha = 0.3, stat = 'identity') +
      geom_text(data = the_sub, 
                aes(weeks_since_first_death, total_deaths, label = paste0(' Projected')), angle = 90, hjust = 0, size = 2.5) 
    return(comb_plot)
  }
}

italy_lockdown = filter(jh_joined_it_us_stats, country_region == 'Italy', date_upd == as.Date('2020-03-09')) 
main_plot = season_comparison %>%  
  ggplot(aes(weeks_since_first_death, total_excess)) +
  theme_bw() +
  geom_vline(data = italy_lockdown, aes(xintercept = weeks_since_first_death), linetype = 'dashed') +
  geom_bar(aes(fill = Virus), stat = 'identity', alpha = 0.3) +
  geom_bar(data = italy_weekly_deaths, aes(weeks_since_first_death, total_deaths, fill = Virus, alpha = obs), 
            stat = 'identity', colour = 'black', size = 0.75) +
  scale_alpha(guide = F, range = c(0.2, 0.4)) +
  labs(
    x = '\nWeeks Since First Death',
    y = 'Excess Deaths\n',
    title = 'Deaths Caused by COVID-19 vs. Typical Flu Season in Italy',
    subtitle = sprintf('Average Flu Season in Italy: 17,000 excess deaths. COVID-19: %s deaths through %s.\nVertical line shows date of country-wide lockdown.', 
                       comma(sum(italy_weekly_deaths$total_deaths)), format(max(jh_joined_it_us_stats$date_upd), '%B %d')),
    caption = "Chart: Taylor G. White\nData: Rosano et. al. 2019, Johns Hopkins CSSE, CDC"
  ) +
  theme(
    plot.caption = element_text(hjust = 0, size = 10),
    plot.subtitle = element_text(size = 11, face='italic'),
    title = element_text(size = 16),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 12)
  ) +
  scale_x_continuous(breaks = seq(0, 30, by = 5)) +
  scale_fill_manual(name = '', values = c('Influenza' = 'black', 'COVID-19' = 'red')) +
  scale_y_continuous(labels = comma, breaks = seq(0, 6000, by = 1000)) 

add_extra_bars(main_plot)

ggsave('output/average_italian_flu_deaths.png', height = 6, width = 8, units = 'in', dpi = 800)  

