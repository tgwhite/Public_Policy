library(tidyverse)
library(scales)
library(readxl)
library(ggforce)
setwd("~/Public_Policy/Projects/Voter Turnout Analysis")

turnout_dat = read_excel("data/voter turnout by age and ideology.xlsx", 'clean table')

##### get voter turnout / vote share by #####
reg_vote_dat = read_excel("data/voter turnout by age and ideology.xlsx", '2018 nov midterm reg turnout', skip = 3) %>%
  filter(
    !age %in% c('80-84', '85 and over')
  ) %>%
  mutate(
    age_int = ifelse(age == '80+', 80, as.integer(age)),
    age_group = cut(age_int, breaks = c(18, 29, 44, 64, 80), include.lowest = T, right = T, ordered_result = T)
  )
names(reg_vote_dat) = str_replace_all(names(reg_vote_dat), '[ ]', '_')

total_voted = sum(reg_vote_dat$citizen_voted)
total_citizen_pop = sum(reg_vote_dat$citizen_pop)

reg_vote_dat = mutate(reg_vote_dat, 
                      share_of_citizen_pop = citizen_pop / total_citizen_pop,
                      share_of_vote = citizen_voted / total_voted,
                      influence_ratio = share_of_vote / share_of_citizen_pop
                      )

long_reg_vote_dat = pivot_longer(reg_vote_dat, cols = c('share_of_citizen_pop', 'share_of_vote')) %>%
  filter(age_int != 80)

influence_polynomial = lm(influence_ratio ~ age_int + I(age_int^2) + I(age_int^3) + I(age_int^4), data = reg_vote_dat)
reg_vote_dat$influence_polynomial_pred = predict(influence_polynomial)

summary(influence_polynomial)


breakeven_influence = optim(par = c(18), lower=18, upper = 79, fn = function(par){
  pred_dat = data.frame(age_int = par[1])
  the_prediction = predict(influence_polynomial, newdata= pred_dat)
  return((the_prediction - 1)^2)
}, method = 'Brent')

ggplot(reg_vote_dat, aes(age_int, influence_ratio)) +
  theme_bw() +
  geom_point() +
  geom_line(aes(y = influence_polynomial_pred), size = 1, colour = 'blue') +
  geom_hline(aes(yintercept = 1), size =  0.75) +
  geom_vline(aes(xintercept = breakeven_influence$par), size = .75) +
  scale_x_continuous(breaks = seq(20, 80, by = 5)) +
  labs(
    y = 'Influence Ratio\n(vote share / population share)\n', x = '\nAge',
    title = 'Voter Influence by Age, Nov. 2018 Midterm Election', 
    subtitle = str_wrap("An influence ratio of 1 means an age group's share of the vote is equal to their share of the population (one person, one vote).", 90)
  ) +
  theme(
    legend.text = element_text(size = 12),
    title = element_text(size = 16), 
    text = element_text(size = 14),
    plot.subtitle = element_text(size = 12)
  ) 
ggsave('output/voter_influence_by_age.png', height = 8, width = 8, units = 'in', dpi = 600)


ggplot(long_reg_vote_dat, aes(age_int, value, colour = name)) +
  theme_bw() +
  geom_point() +
  stat_smooth(se = F, span = 0.35, size = 1.25) +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(breaks = seq(20, 80, by = 5)) +
  scale_colour_manual(
    name = '',
    labels = c('share_of_citizen_pop' = 'Share of Population', 'share_of_vote' = 'Share of Vote'),
    values = c('share_of_citizen_pop' = 'steelblue', 'share_of_vote' = 'orange')
  ) +
  labs(
    y = 'Percent Share\n', x = '\nAge'
  ) 
ggsave('output/share_of_vote_vs_pop_share.png', height = 8, width = 9, units = 'in', dpi = 600)



stats_by_age_group = group_by(reg_vote_dat, age_group) %>%
  summarize(
    tot_citizen_pop = sum(citizen_pop),
    tot_citizen_registered = sum(citizen_registered),
    tot_citizen_voted = sum(citizen_voted)
  ) %>%
  mutate(
    share_of_vote = tot_citizen_voted / total_voted
  )



ggplot(reg_vote_dat, aes(age_int, citizen_pct_voted)) +
  geom_point() +
  stat_smooth()

ggplot(reg_vote_dat, aes(age_int, pct_citizen_registered)) +
  geom_point() +
  stat_smooth()



##### get partisan leanings by age group #####
age_pop_by_year = read_excel("data/voter turnout by age and ideology.xlsx", 'age by indv years') %>%
  pivot_longer(-Age, names_to = 'year') %>%
  mutate(
    age_int = ifelse(Age == '100+', 100, as.integer(Age)),
    age_group = cut(age_int, breaks = c(18, 35, 51, 70, 88), include.lowest = T, right = T, ordered_result = T)
         ) %>%
  filter(year == 2018, age_int >= 18, !is.na(age_group)) 


counts_by_age_group = 
  group_by(age_pop_by_year, age_group) %>%
  summarize(
    total_pop = sum(value),
    n_ages = n_distinct(age_int),
    min_age = min(age_int), 
    max_age = max(age_int)
  )
partisan_lean_dat = read_excel("voter turnout by age and ideology.xlsx", 'lean_data_clean') %>%
  filter(age_group != 'Total')

counts_by_age_group_partisanship = inner_join(counts_by_age_group, partisan_lean_dat) %>%
  pivot_longer(
    cols = c('lib_dem', 'mod_dem', 'mod_rep', 'con_rep'),
    values_to = 'proportion', 
    names_to = 'partisan_lean'
  ) %>%
  mutate(
    value_vs_pop = proportion * total_pop,
    party_lean = ifelse(str_detect(partisan_lean, 'dem'), 'DEM', 'REP'),
    age_group = factor(age_group, levels = levels(age_pop_by_year$age_group))
  )

counts_by_party = group_by(counts_by_age_group_partisanship, party_lean) %>%
  summarize(
    party_total = sum(value_vs_pop)
  ) %>%
  mutate(
    pct_of_total = party_total / sum(party_total)
  )

counts_by_party_age = group_by(counts_by_age_group_partisanship, age_group, party_lean) %>%
  summarize(
    party_age_total = sum(value_vs_pop)
  ) %>%
  inner_join(counts_by_party) %>%
  mutate(
    pct_of_party_total = party_age_total / party_total
  )
ggplot(counts_by_party_age, aes(age_group, party_age_total, fill = party_lean)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_y_continuous(labels = comma)

bar_width = 0.75
ggplot(counts_by_party_age, aes(age_group, pct_of_party_total, fill = party_lean)) +
  theme_bw() +
  geom_bar(stat = 'identity', position = 'dodge', colour = 'black', width = bar_width) +
  geom_text(aes(label = percent(pct_of_party_total, accuracy = 1)), position = position_dodge(width = bar_width), vjust = -0.5) +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(
    name = 'Party Lean',
    values = c('DEM' = '#00aef3', 'REP' = '#d8171e')    
  ) +
  labs(
    y = 'Percent of Party Lean\n', x = '\nAge Group',
    caption = 'Chart: Taylor G. White\nData: U.S. Census, Pew Research',
    title = 'Partisan Leanings by Age Group, 2016-2018',
    subtitle = 'Partisan leanings extracted from Pew Research and multipled by population estimates'
  ) +
  theme(
    legend.text = element_text(size = 12),
    title = element_text(size = 16), 
    text = element_text(size = 14),
    plot.subtitle = element_text(size = 12)
  )
ggsave('output/partisan_leanings_by_age_group.png', height = 8, width = 9, units = 'in', dpi = 600)

##### get turnout by age group #####
voter_pref_dat = 
  turnout_dat %>% 
  pivot_longer(
    cols = c('TX Biden Share', 'TX Sanders Share', 'CA Biden Share', 'CA Sanders Share'),
    values_to = 'vote_share', names_to = 'state_candidate_preference'
  ) %>% 
  rename(
    age_group = `Age Group`
  ) %>% 
  filter(age_group != 'Total') %>%
  select(age_group, state_candidate_preference, vote_share) %>%
  mutate(
    state = ifelse(str_detect(state_candidate_preference, '^(CA)'), 'CA', 'TX'),
    candidate = ifelse(str_detect(state_candidate_preference, 'Biden'), 'Biden', 'Sanders')
  )

ggplot(voter_pref_dat, aes(age_group, vote_share, fill = candidate)) +
  theme_bw() +
  facet_wrap(~state, ncol = 2) +
  geom_bar(stat = 'identity', colour = 'black', position = 'dodge', width = bar_width) +
  geom_text(aes(label = percent(vote_share, accuracy = 1)), 
            position = position_dodge(width = bar_width), vjust = -0.5, size = 3) +
  
  scale_y_continuous(labels = percent) +
  scale_fill_manual(
    name = '',
    values = c('Biden' = 'steelblue', 'Sanders' = 'orange')
  ) +
  labs(
    y = 'Estimated Vote Share\n', x = '\nAge Group',
    caption = 'Chart: Taylor G. White\nData: Washington Post Exit Polls',
    title = 'Estimated Vote Share by Age Group',
    subtitle = 'Democratic Primary, 2020'
  ) +
  theme(
    title = element_text(size = 16), 
    text = element_text(size = 14),
    legend.text = element_text(size = 12),
    plot.subtitle = element_text(size = 12),
    strip.text = element_text(size = 14, face = 'bold')
  )
ggsave('output/vote_share_by_age_group.png', height = 8, width = 9, units = 'in', dpi = 600)

#### get share of electorate ####
  electorate_shares = pivot_longer(
    turnout_dat, 
    cols = c("Share of Voting Age Pop", "Super Tuesday Avg")
  ) %>%
    rename(
      age_group = `Age Group`
    ) %>%
    filter(age_group != 'Total') %>%
    mutate(
      name = factor(name, levels = c('Share of Voting Age Pop', 'Super Tuesday Avg'))
    )
names(turnout_dat)

bar_width = 0.75
ggplot(electorate_shares, aes(age_group, value, fill = name)) +
  theme_bw() +
  geom_bar(stat = 'identity', position = 'dodge', colour = 'black', width = bar_width) +
  scale_y_continuous(labels = percent) +
  geom_text(aes(label = percent(value, accuracy = 1)), position = position_dodge(width = bar_width), vjust = -0.5) +
  labs(
    x = '\bAge Group', y = ''
  ) +
  theme(
    legend.position = 'right'
  ) +
  labs(
    y = 'Share of Electorate\n', x = '\nAge Group',
    caption = 'Chart: Taylor G. White\nData: U.S. Census, Washington Post Exit Polls',
    title = 'Estimated Share of Electorate by Age Group',
    subtitle = 'Democratic Primary, 2020'
  ) +
  theme(
    title = element_text(size = 16), 
    text = element_text(size = 14),
    legend.text = element_text(size = 12),
    plot.subtitle = element_text(size = 12),
    strip.text = element_text(size = 14, face = 'bold')
  ) +
  scale_fill_manual(name = '', values = c('Share of Voting Age Pop' = 'darkorange', 'Super Tuesday Avg' = 'purple4')) 

ggsave('output/share_of_electorate_by_age_group.png', height = 8, width = 9, units = 'in', dpi=600)


##### show loss of influence #####
turnout_dat_fin = 
  rename(turnout_dat, age_group = `Age Group`, pop_loss = `Implied Loss of Population`) %>%
  filter(age_group != 'Total')

dem_count = filter(counts_by_party, party_lean == 'DEM')$party_total
total_pop = filter(turnout_dat, `Age Group` == 'Total')$Population
dem_pct_of_voting_pop = dem_count / total_pop
lab_pos = dem_pct_of_voting_pop * 0.5

ggplot(turnout_dat_fin, aes(age_group, pop_loss * dem_pct_of_voting_pop)) +
  theme_bw() +
  geom_hline(aes(yintercept = 0)) +
  geom_bar(stat = 'identity', width = bar_width, colour = 'black', fill = 'steelblue') +
  geom_text(aes(label = comma(pop_loss * dem_pct_of_voting_pop), y = (pop_loss * lab_pos))) +
  scale_y_continuous(labels = comma) +
  labs(
    x  = '\nAge Group', y = 'Implied Population Change\n',
    title = 'Estimated Change in Influence from\nDisproportionate Turnout\nSuper Tuesday Dem. Primary 2020',
    subtitle = str_wrap('Younger voters represent less of the Democratic primary electorate because of low registration and turnout rates, given them a smaller influence on elections than their population size would suggest.', 90),
    caption = 'Chart: Taylor G. White\nData: U.S. Census, Washington Post Exit Polls, Pew Research'
  ) +
  theme(
    title = element_text(size = 16), 
    text = element_text(size = 14),
    legend.text = element_text(size = 12),
    plot.subtitle = element_text(size = 12),
    strip.text = element_text(size = 14, face = 'bold')
  )
ggsave('output/influence_change_disproportionate_turnout.png', height = 8, width = 8, units = 'in', dpi = 600)
