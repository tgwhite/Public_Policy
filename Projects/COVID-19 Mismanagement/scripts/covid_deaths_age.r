library(tidyverse)
library(data.table)
library(scales)
library(ggforce)
library(htmltab)
library(gganimate)
library(extrafont)
library(ggrepel)

loadfonts(device = "win")
windowsFonts()
font_family = 'Calibri'


setwd("~/Public_Policy_Upd/Projects/COVID-19 Mismanagement/output")


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
    female_expected_age = age + female_life_expectancy,
    row_total_denom = males_alive + females_alive,
    prop_alive = row_total_denom / max(row_total_denom, na.rm = T),
    overall_life_expectancy = (males_alive / row_total_denom) * male_life_expectancy + (females_alive / row_total_denom) * female_life_expectancy,
    overall_expected_age = age + overall_life_expectancy
  ) 

selected_expectancies = filter(ssa_life_table, age %in% c(0, 65, 75, 85)) %>% select(age, overall_life_expectancy, overall_expected_age) %>%
  mutate(
    label = sprintf('Life Exp. at %s:\n%s Years', ifelse(age == 0, 'Birth', age), round(overall_life_expectancy))
  )
selected_expectancies_vec = selected_expectancies$overall_life_expectancy
names(selected_expectancies_vec) = selected_expectancies$age


life_table_plot_dat = 
ssa_life_table %>%
  mutate(
    age_x = age
  ) %>%
  pivot_longer(cols = c('age', 'overall_life_expectancy')) %>%
  mutate(
    name_factor = factor(name, levels = c('age', 'overall_life_expectancy') %>% rev()),
    bar_labels = ifelse(name == 'overall_life_expectancy' & age_x %in% selected_expectancies$age, 
                        paste('Life Expectancy of', selected_expectancies_vec[as.character(age_x)] %>% round(), 'Years at Age', age_x), NA)
  ) 

ggplot(life_table_plot_dat, aes(age_x, value)) + 
  theme_bw() +
  theme(
    legend.position = 'bottom',
    text = element_text(family = font_family),
    axis.title = element_text(size = 15),
    legend.text = element_text(size = 14),
    panel.grid.major.x = element_blank(),
    axis.text = element_text(size = 14),
    plot.title = element_text(size = 26),
    plot.subtitle = element_text(size = 15, face = 'italic'),
    plot.caption = element_text(size = 11, hjust = 0, face = 'italic'),
    strip.background = element_rect(fill = 'black'),
    strip.text = element_text(colour = 'white', face = 'bold', size = 16)
  ) +
  scale_alpha(range = c(0.3, 1), guide = F) +
  scale_y_continuous(breaks = seq(0, 120, by = 10)) +
  scale_x_continuous(breaks = seq(0, 120, by = 10)) +
  labs(
    x = '', y = 'Life Expectancy',
    title = 'U.S. Life Expectancy by Age',
    caption = 'Chart: Taylor G. White (@t_g_white)\nData: Social Security Administration Life Tables',
    subtitle = 'Bars are shaded according to the probability of reaching that age. While life expectancy is 78 years at birth, the expected age someone will reach increases given that they already survived to old age.' %>%
      str_wrap(140)
  ) +
  geom_label_repel(data = selected_expectancies, aes(x = age, y = overall_expected_age, label = label), nudge_y = 20) +
  # annotate('text', x = 65, y = 120, label = sprintf('Life Expectancy of %s Years at 65', round(selected_expectancies_vec['65']))) +
  # geom_segment(data = selected_expectancies, aes(x = age, xend = age, y = 120, yend = overall_expected_age)) +
  geom_bar(aes(alpha = prop_alive, fill = name_factor), stat = 'identity') + 
  geom_hline(aes(yintercept = filter(selected_expectancies, age == 0)$overall_life_expectancy),  colour = 'black', linetype = 'dashed') +
  # geom_text(aes(label = bar_labels), size = 3, position = position_stack(vjust = 0.5), angle = 90, family = font_family, hjust = 0.25) +
  scale_fill_manual(name = '', labels = c('age' = 'Current Age', 'overall_life_expectancy' = 'Years Remaining'), values = c('age' = 'steelblue', 'overall_life_expectancy' = 'orange'))
ggsave('life_expectancy_by_age.png', height = 9, width = 12, units = 'in', dpi = 600)


##### get covid deaths by age and week #####

covid_deaths_by_age_week = read_csv('https://data.cdc.gov/resource/vsak-wrfu.csv') %>% 
  mutate(
    week_end = as.Date(week_ending_date),
    as_of_date = as.Date(data_as_of)
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
    age_group_pretty = age_group %>% str_replace("85 years and over", ">85") %>% str_replace("Under 1 year", "<1") %>% str_remove(' years'),
    age_group_factor = factor(age_group_pretty, levels = age_group_pretty),
    person_years_lost = total_deaths * group_avg_expectancy
  ) 

  
long_dat =   total_deaths_by_age_group %>% pivot_longer(cols = c('total_deaths', 'person_years_lost')) %>% 
  mutate(
    name = factor(name, levels = c('total_deaths', 'person_years_lost')),
    desc = str_replace_all(name, '_', ' ') %>% str_to_title()
  ) 


labels_df = group_by(long_dat, name, desc) %>%
  summarize(
    max_val = max(value, na.rm = T), 
    age_group_factor = levels(age_group_factor)[1],
    total_val = sum(value, na.rm = T)
  ) %>%
  arrange(
    total_val
  ) %>%
  mutate(
    num_label = sprintf(paste(desc, '(%s)'), comma(total_val)),
    num_label = factor(num_label, levels = num_label)
  )

long_dat = left_join(long_dat, labels_df %>% select(name, num_label), by = c('name'))

years_lost_anim = ggplot(long_dat, aes()) +
  transition_states(
    name,
    transition_length = 2,
    state_length = 5
  ) +
  labs(
    x = '', y = '',
    title = 'Comparing U.S. COVID Mortality and Person-Years Lost by Age Group',
    caption = sprintf('Chart: Taylor G. White (@t_g_white)\nData: CDC COVID Data (as of %s), SSA Life Tables', covid_deaths_by_age_week$as_of_date[1] %>% format('%b %d, %Y')),
    subtitle = sprintf('If someone was expected to live another 10 years based on their age but died early due to COVID, they lost 10 person-years. On average, someone in the 65-74 age group is expected to live another 16 years.') %>% str_wrap(120)
  ) +
  theme_bw() +
  theme(
    text = element_text(family = font_family),
    axis.text.x = element_text(),
    panel.grid.major.x = element_blank(),
    axis.text = element_text(size = 14),
    plot.title = element_text(size = 26),
    plot.subtitle = element_text(size = 15, face = 'italic'),
    plot.caption = element_text(size = 11, hjust = 0, face = 'italic')
  ) +
  # enter_fade() +
  # exit_shrink() +
  # ease_aes('sine-in-out') +
  geom_bar(aes(age_group_factor, value), stat = 'identity', fill = 'steelblue') +
  geom_text(aes(x = age_group_factor, y = value, label = comma(value)), family = font_family, size = 3, vjust = -1) +
  geom_text(data = labels_df, aes(x = age_group_factor, y = max_val, label = desc, group = name), family = font_family, size = 6) +
  # shadow_mark(past = TRUE, future = TRUE, colour = 'grey') +
  scale_y_continuous(labels = comma) +
  # view_step(pause_length = 3, step_length = 2, nsteps = 2, ease = 'linear')
  view_zoom(pause_length = 25, step_length = 8, nsteps = 2, ease = 'sine-in-out')


# animate(years_lost_anim, 
#          renderer = gifski_renderer("deaths_years_lost.gif"),
#         nframes = 100,
#          height = 9, width = 12, units = 'in',  type = 'cairo-png', res = 150)


ggplot(long_dat, aes(age_group_factor, value)) +
  facet_wrap(~num_label, scales = 'free_y', ncol = 1) +
  labs(
    x = '', y = '',
    title = 'Comparing U.S. COVID Mortality and Person-Years Lost by Age Group',
    caption = sprintf('Chart: Taylor G. White (@t_g_white)\nData: CDC COVID Data (as of %s), SSA Life Tables', covid_deaths_by_age_week$as_of_date[1] %>% format('%b %d, %Y')),
    subtitle = sprintf('If someone was expected to live another 10 years based on their age but died early due to COVID, they lost 10 person-years. On average, someone in the 65-74 age group is expected to live another 16 years.') %>% str_wrap(120)
  ) +
  theme_bw() +
  theme(
    text = element_text(family = font_family),
    axis.text.x = element_text(),
    panel.grid.major.x = element_blank(),
    axis.text = element_text(size = 14),
    plot.title = element_text(size = 26),
    plot.subtitle = element_text(size = 15, face = 'italic'),
    plot.caption = element_text(size = 11, hjust = 0, face = 'italic'),
    strip.background = element_rect(fill = 'black'),
    strip.text = element_text(colour = 'white', face = 'bold', size = 16)
  ) +
  geom_bar(aes(), stat = 'identity', fill = 'firebrick', width = 0.75) +
  geom_text(aes(x = age_group_factor, y = value, label = comma(value)), family = font_family, size = 4, vjust = -1) +
  scale_y_continuous(labels = comma) 
ggsave('total_deaths_years_lost_age_group.png', height = 14, width = 12, units = 'in', dpi = 600)
shell('explorer .')


head(total_deaths_by_age_group)
