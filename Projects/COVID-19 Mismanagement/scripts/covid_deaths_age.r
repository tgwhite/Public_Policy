library(tidyverse)
library(data.table)
library(scales)
library(ggforce)
library(htmltab)
library(gganimate)

##### get ssa life table data #####
ssa_life_table = htmltab('https://www.ssa.gov/oact/STATS/table4c6.html')
names(ssa_life_table) = c('age', 'male_death_probability', 'males_alive', 'male_life_expectancy', 
                          'female_death_probability', 'females_alive', 'female_life_expectancy')

ssa_life_table = 
  mutate_all(ssa_life_table, 
             function(.){
               str_remove_all(., '[,\\+]') %>%
                 as.numeric()
             }) %>%
  filter(!is.na(age)) %>%
  mutate(
    male_expected_age = age + male_life_expectancy,
    female_expected_age = age + female_life_expectancy
  )

ssa_life_table %>%
  pivot_longer(cols = c('male_expected_age', 'female_expected_age')) %>%
  filter(between(age, 18, 90)) %>%
  ggplot(aes(age, value, colour = name)) +
  geom_point()

ssa_life_table %>%
  pivot_longer(cols = c('male_life_expectancy', 'female_life_expectancy')) %>%
  filter(between(age, 18, 90)) %>%
  ggplot(aes(age, value, colour = name)) +
  geom_point()


##### get covid deaths by age and week #####

covid_deaths_by_age_week = read_csv('https://data.cdc.gov/resource/vsak-wrfu.csv') %>% 
  mutate(
    week_end = as.Date(week_ending_date)
  )

age_midpoint_85p = filter(ssa_life_table, age >= 85)$age %>% mean()

age_groups_df = tibble(
  age_group = unique(covid_deaths_by_age_week$age_group),
) %>%
  filter(age_group != 'All Ages') %>%
  mutate(
    age_group_start = str_extract(age_group, '[0-9]{1,2}') %>% as.numeric(),
    age_group_end = str_extract(age_group, '-[0-9]{1,2}') %>% str_remove('-') %>% as.numeric(),
    age_group_midpoint = (age_group_end + age_group_start) / 2,
    age_group_midpoint = ifelse(is.na(age_group_end) & age_group_start == 1, 0, age_group_midpoint),
    age_group_midpoint = ifelse(is.na(age_group_end) & age_group_start == 85, age_midpoint_85p, age_group_midpoint),
    age_group_end = ifelse(age_group_start == 85 & is.na(age_group_end), max(ssa_life_table$age), age_group_end),
    age_group_end = ifelse(age_group_start == 1 & is.na(age_group_end), 0, age_group_end),
    age_group_start = ifelse(age_group_start == 1 & age_group_end == 0, 0, age_group_start),
  ) 
  

for (it in 1:nrow(age_groups_df)) {
  # it = 11
  the_row = age_groups_df[it,]
  ssa_sub = filter(ssa_life_table, between(age, the_row$age_group_start, the_row$age_group_end)) %>%
    mutate(
      row_total_denom = males_alive + females_alive,
      overall_life_expectancy = (males_alive / row_total_denom) * male_life_expectancy  + (females_alive / row_total_denom) * female_life_expectancy 
    ) %>%
    filter(
      row_total_denom > 0
    )
  
  overall_total_denom = sum(ssa_sub$row_total_denom)
  group_avg_expectancy = sum(
    (ssa_sub$row_total_denom / overall_total_denom) * ssa_sub$overall_life_expectancy
  )
  age_groups_df[it,'group_avg_expectancy'] = group_avg_expectancy
  # get rowwise weighted means and then get columnwise weighted means
  
}

total_deaths_by_age_group = group_by(covid_deaths_by_age_week, age_group) %>%
  summarize(
    total_deaths = sum(covid_19_deaths)
  ) %>%
  inner_join(
    age_groups_df
  ) %>%
  arrange(age_group_start) %>%
  mutate(
    age_group_factor = factor(age_group, levels = age_group),
    years_lost = total_deaths * group_avg_expectancy
  ) 
  


ggplot(mtcars, aes(factor(cyl), mpg)) +
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


total_deaths_by_age_group %>% pivot_longer(cols = c('total_deaths', 'years_lost')) %>% 
  mutate(
    name = factor(name, levels = c('total_deaths', 'years_lost')) 
  ) %>%
ggplot(aes(age_group_factor, value)) +
  transition_states(
    name,
    transition_length = 2,
    state_length = 5
  ) +
  
  # enter_fade() +
  # exit_shrink() +
  # ease_aes('sine-in-out') +
  geom_bar(stat = 'identity') +
  # shadow_mark(past = TRUE, future = TRUE, colour = 'grey') +
  scale_y_continuous(labels = comma) +
  view_zoom(pause_length = 3, step_length = 2, nsteps = 2, ease = 'linear')

anim <- ggplot(iris, aes(Petal.Length, Petal.Width, colour = Species)) +
  geom_point() +
  transition_states(Species, transition_length = 2, state_length = 1) +
  shadow_mark(past = TRUE, future = TRUE, colour = 'grey') +
  view_zoom(pause_length = 1, step_length = 2, nsteps = 3)


ggplot(total_deaths_by_age_group, aes(age_group_factor, group_avg_expectancy )) +
  geom_bar(stat = 'identity')

ggplot(total_deaths_by_age_group, aes(age_group_factor, total_deaths )) +
  geom_bar(stat = 'identity')

