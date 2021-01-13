library(tidyverse)
library(data.table)
library(albersusa)
library(quantmod)
library(gganimate)
library(ggforce)
library(ggrepel)
library(extrafont)
library(uuid)
library(scales)

loadfonts(device = "win")
font_family = 'Calibri'


us_pop = getSymbols('POP', src = 'FRED', auto.assign = F)
us_pop_df = 
  tibble(
    population = as.numeric(us_pop$POP * 1000),
    date = index(us_pop),
    year = year(date)
  ) %>%
  arrange(date) %>%
  group_by(year) %>%
  summarize(
    year_end_pop = tail(population, 1)
  )

us_sf <- usa_sf("laea")

setwd("~/Public_Policy_Upd/Projects/Voting/data")

heritage_voterfraud_database = read_csv("heritage_voterfraud_database.csv")
fraud_stats_by_year = group_by(heritage_voterfraud_database, Year) %>%
  summarize(
    count = n()
  ) 


##### Get political data #####


# MIT data lab 

get_margins_by_race = function(grouped_df) {

  margins_by_race = grouped_df %>%
    summarize(
      total_votes = totalvotes[1],
      n_candidates = n_distinct(candidate[writein == FALSE]),
      top_vote_count = candidate_votes[1],
      second_vote_count = ifelse(n_candidates > 1, candidate_votes[2], 0)
    ) %>%
    ungroup() %>%
    mutate(
      uncontested = total_votes <= 1 | n_candidates <= 1,
      winning_margin = top_vote_count - second_vote_count,
      winning_margin_pct = winning_margin / total_votes
    )
  
  return(margins_by_race)  
}

house_elections = read.csv('1976-2018-house.csv') %>% 
  mutate(candidate_votes = as.numeric(candidatevotes %>% str_remove(','))) %>%
  filter(stage == 'gen') %>% 
  arrange(state, year, district, -candidate_votes )


margins_by_house_race = get_margins_by_race(house_elections %>% 
                                              group_by(state, year, district)) %>%
  mutate(
    race_id = paste(state, year, district, sep = '_')
  )

house_totals_by_state_year = group_by(margins_by_house_race, state, year) %>%
  summarize(
    house_total_votes = sum(total_votes, na.rm = T)
  )

group_by(margins_by_house_race, year) %>%
  summarize(
    n_races = n_distinct(race_id)
  )



# get winning margins for house, senate, and president
# get total votes for each race as well

senate_elections = read.csv('1976-2018-senate.csv') %>% 
  mutate(candidate_votes = as.numeric(candidatevotes %>% str_remove(','))) %>%
  filter(stage == 'gen') %>% 
  arrange(state, year, district, -candidate_votes ) 

margins_by_senate_race = get_margins_by_race(senate_elections %>% group_by(state, year, district))

senate_totals_by_state_year = group_by(margins_by_senate_race, state, year) %>%
  summarize(
    senate_total_votes = sum(total_votes, na.rm = T)
  )

presidential_elections = read.csv('1976-2016-president.csv') %>% 
  mutate(candidate_votes = as.numeric(candidatevotes %>% str_remove(','))) %>%
  arrange(state, year, -candidate_votes ) 

margins_by_presidential_race = get_margins_by_race(presidential_elections %>% group_by(state, year))
president_totals_by_state_year = group_by(margins_by_presidential_race, state, year) %>%
  summarize(
    president_total_votes = sum(total_votes, na.rm = T)
  ) 

combined_margin_analysis = bind_rows(
  margins_by_house_race %>% mutate(race = 'House', district = as.character(district)) %>% filter(!uncontested) %>% select(state, year, district, race, winning_margin, winning_margin_pct),
  margins_by_senate_race %>% mutate(race = 'Senate') %>% filter(!uncontested) %>% select(state, year, district, race, winning_margin, winning_margin_pct),
  margins_by_presidential_race %>% mutate(race = 'President', district = 'statewide') %>% select(state, year, district, race, winning_margin, winning_margin_pct)
) %>%
  mutate(
    race_id = 1:length(year),
    plot_order = 7
  )
races_less_1k_margin = filter(combined_margin_analysis, winning_margin <= 1000) %>% nrow()
percent_races_less_1k_margin = races_less_1k_margin / nrow(combined_margin_analysis)



total_votes_by_year_state = full_join(
  house_totals_by_state_year,
  senate_totals_by_state_year, 
  by = c('state', 'year')
) %>%
  full_join(
    president_totals_by_state_year, by = c('state', 'year')
  ) %>%
  mutate(
    annual_total_votes = pmax(
      house_total_votes,  senate_total_votes, president_total_votes, 
      na.rm = T
    )
  )

total_votes_by_year = group_by(total_votes_by_year_state, year) %>%
  summarize(
    total_votes = sum(annual_total_votes, na.rm = T)
  ) %>%
  bind_rows(
    data.frame(
      year = 2020, 
      total_votes = 81283077 + 74222965
    )
  ) %>%
  left_join(us_pop_df)


# https://www.weather.gov/safety/lightning-odds

yearly_odds_lightning = 1/1222000
not_struck = 1-yearly_odds_lightning
odds_struck_in_50_years = 1-(not_struck^80)
odds_struck_in_50_years


voter_fraud_counts_2009_2018 = filter(fraud_stats_by_year, Year >= 2009, Year <= 2018)
total_fraud_cases = sum(voter_fraud_counts_2009_2018$count)
annual_avg_fraud_cases = total_cases / length(2018:2009)
lighning_strikes_per_year = 270
lightning_strikes_over_period = lighning_strikes_per_year * length(2018:2009)

total_votes_2009_2018 = filter(total_votes_by_year, between(year, 2009, 2018)) %>% pull(total_votes) %>% sum()
ratio_lightning_fraud = lighning_strikes_per_year / annual_avg_fraud_cases


get_circle_radius = function(area) {
  (area / pi)^(1/2)
}

# title, circle 1, circle 1:2, title, circle 1:3
# People are more likely to be struck by lightning\nthan commit voter fraud.

circle_areas = tibble(
  counts = c(total_fraud_cases, lightning_strikes_over_period, total_votes_2009_2018),
  circle_alphas = c(.9, 0.75, 0.65),
  desc = c('proven cases of voter fraud', 
           'lightning strikes on people', 'votes in federal elections'),
  source = c('Heritage Foundation\nVoter Fraud Database', 'Weather.gov','MIT Election Lab'),
  circle_r = get_circle_radius(counts),
  circle_desc = sprintf('There were %s %s\nbetween 2009 and 2018', comma(counts), desc),
  voter_fraud_label = c(NA, 'Voter Fraud', 'Voter fraud is represented\nby this pixel'),
  voter_fraud_label_y = c(0, min(circle_r) * .25, min(circle_r) * 230),
  circle_label = c('Voter Fraud', 'Lightning Strikes on People', 'Votes in Federal Elections'),
  circle_label_y = c(0, -get_circle_radius(lightning_strikes_over_period) * .85, -get_circle_radius(total_votes_2009_2018) * .85),
  source_label = sprintf('Source: %s', source),
  order = 1:length(counts)
) %>%
  mutate(
    circle_desc = ifelse(circle_alphas == 0, desc, circle_desc),
    source_label = ifelse(circle_alphas == 0, source, source_label),
    desc_position = ifelse(circle_alphas == 0, 0, circle_r * 1.2),
    desc_size = c(10, 10, 10)
  )

stacked_circle_dat = bind_rows(
  tibble(
    plot_order = 1,
    circle_desc = 'Americans are more likely to be\nstruck by lightning\nthan commit voter fraud.',
    desc = circle_desc, 
    source_label = 'Chart: Taylor G. White (@t_g_white)',
    desc_position = 0, desc_size = 20, 
    voter_fraud_label = NA, 
    circle_label = NA, 
    circle_label_y = 0,
    voter_fraud_label_y = 0,
    circle_alphas = 0,
    circle_r = min(circle_areas$circle_r)
  ),
  filter(circle_areas, order == 1) %>% mutate(plot_order = 2),
  
  filter(circle_areas, order %in% 1:2) %>% 
    mutate(plot_order = 3, 
           voter_fraud_label = ifelse(order < 2, NA, voter_fraud_label),
           circle_label = ifelse(order < 2, NA, circle_label),
           source_label = ifelse(order < 2, NA, source_label),
           circle_desc = ifelse(order < 2, NA, circle_desc),
    ),
  tibble(
    plot_order = 4,
    circle_desc = 'Voter fraud represents a small proportion of all votes cast.' %>% str_wrap(20),
    desc = "fraud in federal elections desc", 
    source_label = '',
    desc_position = 0, desc_size = 20, 
    voter_fraud_label = NA, 
    circle_label = NA, 
    circle_label_y = 0,
    voter_fraud_label_y = 0,
    circle_alphas = 0,
    circle_r = min(circle_areas$circle_r)
  ),
  filter(circle_areas, order %in% 1:3) %>% 
    mutate(plot_order = 5, 
           voter_fraud_label = ifelse(order < 3, NA, voter_fraud_label),
           circle_label = ifelse(order < 3, NA, circle_label),
           source_label = ifelse(order < 3, NA, source_label),
           circle_desc = ifelse(order < 3, NA, circle_desc),
    ),
  tibble(
    plot_order = 6,
    circle_desc = "Voter fraud is rare because it doesn't pay off." %>% str_wrap(24),
    desc = "doesn't pay off", 
    source_label = '',
    desc_position = 0, desc_size = 20, 
    voter_fraud_label = NA, 
    circle_label = NA, 
    circle_label_y = 0,
    voter_fraud_label_y = 0,
    circle_alphas = 0,
    circle_r = min(circle_areas$circle_r)
  ),
  tibble(
    plot_order = 7,
    # circle_desc = NA,
    circle_desc = 'Few federal elections are close enough to be impacted by fraud.' %>% str_wrap(32),
    desc = "close race desc", 
    source_label = '',
    desc_position = 0, 
    desc_size = 20, 
    voter_fraud_label = NA, 
    circle_label = NA, 
    circle_label_y = 0,
    voter_fraud_label_y = 0,
    circle_alphas = 0,
    circle_r = 0.0001
  ),
  tibble(
    plot_order = 8,
    # circle_desc = NA,
    circle_desc = sprintf('Only %s percent of the %s contested federal elections between 1976-2018 were decided by a thousand votes or less.', percent(percent_races_less_1k_margin, accuracy = 0.01), comma(nrow(combined_margin_analysis))) %>% str_wrap(32),
    desc = "close race desc", 
    source_label = '',
    desc_position = 0, 
    desc_size = 20, 
    voter_fraud_label = NA, 
    circle_label = NA, 
    circle_label_y = 0,
    voter_fraud_label_y = 0,
    circle_alphas = 0,
    circle_r = 0.0001
  ),
  tibble(
    plot_order = 9,
    # circle_desc = NA,
    circle_desc = 'Large-scale fraud carries a strong punishment and can be detected easily, which serves as a deterrent.' %>% str_wrap(28),
    desc = "last panel desc", 
    source_label = '',
    desc_position = 0, 
    desc_size = 20, 
    voter_fraud_label = NA, 
    circle_label = NA, 
    circle_label_y = 0,
    voter_fraud_label_y = 0,
    circle_alphas = 0,
    circle_r = 0.0001
  )
) %>%
  mutate(
    circle_x = 0,
    circle_y = 0
  )



plot_colors = c('white', '#e41a1c', 'yellow', 'white','#386cb0', 'white', 'white', 'white')
names(plot_colors) = c('People are more likely to be\nstruck by lightning\nthan commit voter fraud.', 
                       "proven cases of voter fraud", "lightning strikes on people", 
                       "fraud in federal elections desc",
                       "votes in federal elections", "doesn't pay off", 'close race desc', "last panel desc")


combined_margin_analysis_counts = group_by(combined_margin_analysis, year, winning_margin <= 1000) %>%
  summarize(
    obs = n()
  ) %>%
  ungroup() %>%
  rename(
    close_margin = `winning_margin <= 1000`
  ) %>%
  mutate(
    plot_order = 7
  )
  

seg_dat = data.frame(plot_order = 5, x = 0, y = min(circle_areas$circle_r) * 130)

fraud_anim = ggplot(stacked_circle_dat) +
  geom_circle(aes(x0 = circle_x, y0 = circle_y, r = circle_r, fill = desc, alpha = circle_alphas, group = plot_order), colour = NA, na.rm = T) +
  # geom_bar(data = combined_margin_analysis_counts, aes(year, obs, fill = close_margin, group = plot_order), show.legend = F, stat = 'identity') +
  coord_equal() +
  scale_size(guide = F, range = c(6.5, 9)) +
  scale_alpha(guide = F, range = c(0, .75)) +
  theme_bw() +
  theme_void() +
  scale_fill_manual(guide = F, values = plot_colors) +
  geom_text(aes(x = circle_x, y = desc_position, label = circle_desc, size = desc_size), fontface = 'bold', family = font_family, show.legend = F, na.rm = T) +
  geom_text(aes(x = circle_x, y = -circle_r * 1.2, label = source_label), family = font_family, size = 6, fontface = 'italic') +
  geom_text(aes(x = circle_x, y = voter_fraud_label_y, label = voter_fraud_label), na.rm = T, family = font_family, size = 6) +
  geom_text(aes(x = circle_x, y = circle_label_y, label = circle_label), na.rm = T, family = font_family, size = 6) +
  geom_segment(data = seg_dat, aes(x = 0, xend = 0, y = 0, yend = y, group = plot_order), size = 0.25) +
  transition_states(
    plot_order,
    transition_length = 2,
    state_length = 8
  ) +
  # shadow_mark(exclude_layer = c(2, 3, 4, 5)) +
  view_zoom(pause_length = 40, step_length = 10, nsteps = max(stacked_circle_dat$plot_order), ease = 'sine-in-out')

setwd('~/Public_Policy_Upd/Projects/Voting/output')

animate(fraud_anim,
         renderer = gifski_renderer("fraud_frequency_comparison_upd.gif"),
          nframes = 300,
         height = 8, width = 8, units = 'in',  type = 'cairo-png', res = 200)



# consider creating barchart, saving that down separately, and then reading that in as a set of pixels
# plot the pixels and view_zoom will show it perfectly. 
# http://mfviz.com/r-image-art/

