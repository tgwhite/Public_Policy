library(tidyverse)
library(data.table)
library(scales)
library(ggforce)
library(htmltab)
library(gganimate)
library(extrafont)
library(ggrepel)
library(gridExtra)
library(packcircles)

loadfonts(device = "win")
windowsFonts()
font_family = 'Calibri'

setwd("~/Public_Policy_Upd/Projects/COVID-19 Mismanagement/output")

##### get comparative flu stats #####
flu_links = c('https://www.cdc.gov/flu/about/burden/2019-2020.html',
              'https://www.cdc.gov/flu/about/burden/2018-2019.html', 'https://www.cdc.gov/flu/about/burden/2017-2018.htm', 
              'https://www.cdc.gov/flu/about/burden/2016-2017.html', 'https://www.cdc.gov/flu/about/burden/2015-2016.html', 
              'https://www.cdc.gov/flu/about/burden/2014-2015.html', 'https://www.cdc.gov/flu/about/burden/2013-2014.html',
              'https://www.cdc.gov/flu/about/burden/2012-2013.html', 'https://www.cdc.gov/flu/about/burden/2011-2012.html',
              'https://www.cdc.gov/flu/about/burden/2010-2011.html')

flu_burden_tables = lapply(flu_links, function(the_link){
  the_season = str_extract(the_link, '[0-9]{4}-[0-9]{4}')
  the_df = htmltab(the_link) %>% 
    mutate(
      season = the_season
    )
  names(the_df) = c('age_group', 'syptomatic_illnesses', 'syptomatic_illnesses_ci', 
                    'medical_visits', 'medical_visits_ci', 'hosptitalizations', 'hosptitalizations_ci',
                    'deaths', 'deaths_ci', 'season')
  the_df
}) %>% 
  bind_rows() %>%
  mutate(
    deaths = str_remove(deaths, ',') %>% as.numeric() 
  )




##### get JH data and create circle plots #####


johns_hopkins_deaths = read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv') %>%
  pivot_longer(cols = matches('^([0-9])'), names_to = 'date', values_to = 'deaths') %>% 
  mutate(
    date_upd = as.Date(date, format = '%m/%d/%y')
  ) %>%
  select(-date) %>%
  rename(
    date = date_upd,
    country = `Country/Region`
  ) %>%
  filter(country == 'US')

flu_season_total_deaths = filter(flu_burden_tables, age_group == 'All ages') %>% select(age_group, season, deaths) %>% mutate(illness = 'Influenza') %>%
  head(9)
# flu_season_total_deaths = bind_rows(
#   data.frame(season = '2019-2020', deaths = 22000),
#   flu_season_total_deaths
# )
covid_deaths = filter(johns_hopkins_deaths, date == as.Date('2020-12-31')) %>% mutate(illness = 'COVID-19', season = '2020')
stacked_flu_covid_total_deaths = bind_rows(covid_deaths, flu_season_total_deaths) %>% select(season, illness, deaths) %>% mutate(id = 1:length(deaths))

packing <- circleProgressiveLayout(stacked_flu_covid_total_deaths, sizecol = 'deaths') %>% bind_cols(stacked_flu_covid_total_deaths)

dat.gg <- circleLayoutVertices(packing) %>% left_join(stacked_flu_covid_total_deaths)

ggplot(data = dat.gg) +
  geom_polygon(aes(x, y, group = id, fill = illness), 
               colour = "black",
               show.legend = T) +
  theme_minimal() +
  theme(
    text = element_text(family = font_family),
    legend.position = 'bottom',
    legend.text = element_text(size = 16),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 28),
    plot.subtitle = element_text(size = 15, face = 'italic'),
    plot.caption = element_text(size = 13, face = 'italic', hjust = 0)
    # plot.margin = unit(c(1.5, 0, 1.5, 0), 'cm')
  ) +
  labs(
    x = '\n\n',
    title = 'COVID-19 vs. Influenza Mortality in the U.S.',
    caption = 'Chart: Taylor G. White (@t_g_white)\nData: CDC Flu Disease Burden, Johns Hopkins CSSE',
    subtitle = sprintf('COVID-19 killed more Americans in one year (%s) than did the flu over the last nine seasons (%s).', 
                       covid_deaths$deaths %>% comma(), sum(flu_season_total_deaths$deaths) %>% comma()) %>%
      str_wrap(110)
  ) +
  geom_text(data = packing, aes(x, y, label = paste0(season %>% str_extract('[0-9]{4}'), ':\n',comma(deaths)), size = radius), family = font_family) +
  scale_size(range = c(3, 8), guide = F) +
  scale_fill_manual(name = '', values = c('COVID-19' = 'orange', 'Influenza' = 'steelblue')) +
  coord_equal()
ggsave('deaths_by_influenza_covid.png', height = 10, width = 10, units = 'in', dpi = 600)

ggplot(stacked_flu_covid_total_deaths, aes(illness, size = deaths)) +
  geom_point(data = filter(stacked_flu_covid_total_deaths, illness == 'COVID-19'), aes(y = 10), pch = 21) +
  geom_point(data = filter(stacked_flu_covid_total_deaths, illness == 'Influenza'), aes(y = 10), position = position_jitter(width = .2, height = 1), pch = 21) +
  scale_size(range = c(2, 20)) +
  scale_y_continuous(limits = c(8, 12))
  
  ?position_jitter


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
      str_wrap(130)
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

##### deaths by age group #####



flu_stats_by_age_group = group_by(flu_burden_tables, age_group) %>%
  summarize(
    `Total Flu Deaths\nLast 9 Seasons` = sum(deaths, na.rm = T) %>% comma(),
    `Avg. Flu Deaths\nPer Season` = mean(deaths, na.rm = T) %>% comma()
  ) %>%
  mutate(
    `Age Group` = factor(age_group, levels = c('0-4 yrs', '5-17 yrs', '18-49 yrs','50-64 yrs', '65+ yrs', 'All ages'))
  ) %>%
  arrange(`Age Group`) %>%
  select(`Age Group`, `Total Flu Deaths\nLast 9 Seasons`, `Avg. Flu Deaths\nPer Season`)

flu_stats_by_age_group_pretty = select(flu_stats_by_age_group, contains('Flu')) %>% as.data.frame()
row.names(flu_stats_by_age_group_pretty) = flu_stats_by_age_group$`Age Group`


tt2 = ttheme_default(
  core=list(bg_params = list(fill = c('white', 'lightgray')))
)


deaths_55_64 = filter(total_deaths_by_age_group, between(age_group_start, 55, 64)) %>% pull(total_deaths) %>% sum()
deaths_65plus = filter(total_deaths_by_age_group, age_group_start >= 65) %>% pull(total_deaths) %>% sum()
total_covid_cdc = sum(total_deaths_by_age_group$total_deaths) %>% comma()

ggplot(total_deaths_by_age_group, aes(age_group_factor, total_deaths)) +
  labs(
    x = '', y = '',
    title = 'U.S. COVID Mortality by Age Group',
    caption = sprintf('Chart: Taylor G. White (@t_g_white)\nData: CDC COVID Data (as of %s)', covid_deaths_by_age_week$as_of_date[1] %>% format('%b %d, %Y'))
  ) +
  theme_bw() +
  # annotation_custom(tableGrob(flu_stats_by_age_group_pretty, theme=tt2), 
  #                   xmin = levels(total_deaths_by_age_group$age_group_factor)[1], xmax = levels(total_deaths_by_age_group$age_group_factor)[4], 
  #                   ymin = 75e3, ymax = 100e3
  #                   )  +
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
  geom_text(aes(x = age_group_factor, y = total_deaths, label = comma(total_deaths)), family = font_family, size = 4, vjust = -1) +
  scale_y_continuous(labels = comma) 
ggsave('covid_mortality_by_age.png', height = 9, width = 12, units = 'in', dpi = 600)

ggplot(total_deaths_by_age_group, aes(age_group_factor, total_deaths)) +
  labs(
    x = '', y = '',
    title = 'COVID vs. Flu Mortality in the U.S., by Age Group',
    caption = sprintf('Chart: Taylor G. White (@t_g_white)\nData: CDC COVID Data (as of %s)', covid_deaths_by_age_week$as_of_date[1] %>% format('%b %d, %Y'))
  ) +
  theme_bw() +
  annotation_custom(tableGrob(flu_stats_by_age_group_pretty, theme=tt2),
                    xmin = levels(total_deaths_by_age_group$age_group_factor)[1], xmax = levels(total_deaths_by_age_group$age_group_factor)[4],
                    ymin = 75e3, ymax = 100e3
                    )  +
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
  geom_text(aes(x = age_group_factor, y = total_deaths, label = comma(total_deaths)), family = font_family, size = 4, vjust = -1) +
  scale_y_continuous(labels = comma) 
ggsave('covid_flu_mortality_by_age.png', height = 9, width = 12, units = 'in', dpi = 600)
