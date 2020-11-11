
library(rvest)
library(httr)
library(data.table)
library(tidyverse)
library(WDI)
library(countrycode)
library(lmtest)
library(tseries)
library(plm)
library(rvest)
library(httr)
library(quantmod)
library(fredr)
library(scales)
library(quantreg)
setwd('~\\Public_Policy\\Projects\\Taxes vs. Deficits\\data')


#### Load OECD revenue, expense, and deficit data ####
all_oecd_downloads =   c("oecd_net_lending.csv"   ,         "oecd_revenue.csv"   ,             "oecd_spending.csv" ) %>%
  map(read_csv) %>% 
  bind_rows() 



USA_revenue_expense_deficits = filter(all_oecd_downloads, 
                                      LOCATION == 'USA',
                                      INDICATOR %in% c('GGEXP', 'GGREV', 'GGNLEND'),
                                      MEASURE == 'PC_GDP') %>%
data.table()

us_defense_only = filter(USA_revenue_expense_deficits, INDICATOR == 'GGEXP', SUBJECT == 'DEF')

ggplot(us_defense_only, aes(TIME, Value)) +
  geom_line()

# compute YOY differences, push wide 
USA_revenue_expense_deficits_diffs = USA_revenue_expense_deficits[, {
  lag_value = dplyr::lag(Value, 1)
  diff_value = Value - lag_value
  pct_change = diff_value / lag_value
  list(
    value = Value,
    lag_value = lag_value,
    diff_value = diff_value,
    pct_change = pct_change,
    year = TIME
  )
}, by = list(INDICATOR, SUBJECT)] %>%
  mutate(
    series_name = paste(INDICATOR, SUBJECT, sep = '_')
  ) %>%
  pivot_wider(
    id_cols = c('year'),
    names_from = c('series_name'),
    values_from = c('value', 'diff_value')
  )

# load in existing wide US data
load('US_political_economic_data.rdata')


# combine OECD data and wide US data (keep only political variables and growth)
US_wide_selection = 
  select(US_wide, 
         year = Year,
         contains('pcap'),
         contains('president'),
         contains('recession')) %>%
  left_join(USA_revenue_expense_deficits_diffs) %>%
  filter(!is.na(value_NY.GDP.PCAP.KD.ZG)) %>%
  arrange(year)

iqr_growth = quantile(US_wide_selection$value_NY.GDP.PCAP.KD.ZG, probs = c(0.25, 0.75))

US_wide_selection = mutate(
  US_wide_selection,
  growth_desc = ifelse(dplyr::between(value_NY.GDP.PCAP.KD.ZG, iqr_growth[1], iqr_growth[2]), 
                       'Middle 50%', ifelse(value_NY.GDP.PCAP.KD.ZG < iqr_growth[1], 
                                            'Bottom 25%', 'Top 25%'))
)
group_by(US_wide_selection, growth_desc) %>%
  summarize(
    obs = n(),
    mean_growth = mean(value_NY.GDP.PCAP.KD.ZG)
  )

descriptions = c(
  'TOT' = 'Total Revenue',
  'EDU' = 'Education',
  'DEF' = 'Defense',
  'HEALTH' = 'Health',
  'SOCPROT' = 'Social Protection',
  'RECULTREL' = 'Recreation, Culture, and Religion',
  'GRALPUBSER' = 'General Public Services',
  "ENVPROT" = 'Environmental Protection',
  "PUBORD" = 'Public Order and Safety',
  'ECOAFF' = 'Economic Affairs', 
  'HOUCOMM' = 'Housing and Community Amenities'
)
desc_df = tibble(
  desc = names(descriptions),
  full_description = descriptions
)
View(long_rev_expense_components)

# pull out only revenues and expense components
long_rev_expense_components = 
  US_wide_selection %>%
  select(year, contains('diff_value_GGREV'), contains('diff_value_GGEXP'), -diff_value_GGEXP_TOT) %>%
  pivot_longer(
  -year
) %>%
  mutate(
    value = ifelse(str_detect(name, 'GGEXP'), value * -1, value),
    type = ifelse(str_detect(name, 'GGEXP'), 'Expense', 'Revenue'),
    desc = ifelse(type == 'Expense', str_remove(name, 'diff_value_GGEXP_'), str_remove(name, 'diff_value_GGREV_'))
  ) %>%
  left_join(desc_df) %>%
  mutate(
    collapsed_desc = recode(full_description,
                            `Education` = 'Other',
                            `Economic Affairs` = 'Other',
                            `Recreation, Culture, and Religion` = 'Other',
                            `General Public Services` = 'Other',
                            `Environmental Protection` = 'Other',
                            `Public Order and Safety` = 'Other'
                            ),
    defense_vs_non_defense = ifelse(desc == 'DEF', 'Defense Exp.', ifelse(type == 'Revenue', 'Revenue', 'Non-Defense Exp.'))
  ) %>%
  filter( 
    !(type == 'Expense' & desc == 'TOT')
  )

# sum up expense components that have been collapsed
summarized_by_year = group_by(long_rev_expense_components, defense_vs_non_defense, year) %>%
  summarize(
    total_difference = sum(value, na.rm = T)
  )

# get president start/stop years
president_starts_stops = group_by(US_wide_selection, President, president_party) %>%
  summarize(
    start_year = min(year), end_year = max(year)
  ) %>%
  filter(start_year >= min(long_rev_expense_components$year)) %>%
  mutate(
    midpoint = (start_year + end_year)/2,
    pres_last_name = str_extract(President, '([ ]{1}[A-Za-z]+)$') %>% str_trim()
  )


ggplot(summarized_by_year, aes(year, total_difference, fill = defense_vs_non_defense)) +
  geom_bar(stat = 'identity')
filter(summarized_by_year, year == 2019)

# plot everything
ggplot(summarized_by_year, aes(year, total_difference)) +
  geom_rect(data = president_starts_stops, aes(xmin = start_year, xmax = end_year, 
                                               x = NULL,  y = NULL, ymin = -6, ymax = 4, 
                                               colour = president_party),
            fill = NA,
            show.legend = F,
            size = 0.75,
            stat = 'identity', alpha = 0.3) +
  theme_bw() +
  scale_fill_manual(
    name = '',
    values = c('Defense Exp.' = 'darkblue',
               'Non-Defense Exp.' = 'steelblue',
               'Revenue' = 'orange')
  ) +
  geom_bar(aes(fill= defense_vs_non_defense), stat = 'identity', colour = 'black') +
  # geom_point(data = reg_dat, aes(Year, diff_value_GGNLEND), size = 2.5, shape = 18) +
  labs(
    y = 'Change from Prior Year (% of GDP)\n',
    x = '',
    title = 'Contributions to Changes in Budget Deficits\nU.S. 1975-2018',
    caption = 'Chart: Taylor G. White\nData: OECD, FRED, WDI'
  ) +
  scale_colour_manual(guide = F, values = c('DEM'='#00aef3', 'REP' = '#d8171e')) +
  scale_x_continuous(breaks = seq(1977, max(summarized_by_year$year), by = 4)) +
  geom_text(data = president_starts_stops, 
            aes(y = 4.5, x = midpoint, label = pres_last_name, colour = president_party), hjust = 0.5, size = 4.5) +
  # geom_segment(data = president_starts_stops, aes(y = 4, yend = 4, x = start_year, xend = end_year)) +
  
  theme(
    legend.position = 'top',
    axis.text.x = element_text(angle = 0),
    title = element_text(size = 20),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18),
    legend.text = element_text(size = 14)
  ) +
  
  geom_segment(
    aes(x = 1974, xend = 1974, y = 1.5, yend = 3), 
    lineend = 'butt', linejoin = 'mitre',
    size = 1, arrow = arrow(length = unit(0.1, "inches"))
  ) +
  geom_segment(
    aes(x = 1974, xend = 1974, y = -1.5, yend = -3), 
    lineend = 'butt', linejoin = 'mitre',
    size = 1, arrow = arrow(length = unit(0.1, "inches"))
  ) +
  geom_text(
    aes(x = 1973, y = 2.5, label = 'Decreases Deficit'), angle = 90, hjust = 0.5, size = 4.5
  ) +
  geom_text(
    aes(x = 1973, y = -2.5, label = 'Increases Deficit'), angle = 90, hjust = 0.5, size = 4.5
  )
ggsave('output/contributions_to_deficits_upd.png', height = 8, width = 10, units = 'in', dpi = 600)

# create the same chart but colored by economic growth
ggplot(US_wide_selection, aes(year, diff_value_GGNLEND_TOT, fill = growth_desc)) +
  geom_rect(data = president_starts_stops, aes(xmin = start_year, xmax = end_year, 
                                               x = NULL,  y = NULL, ymin = -6, ymax = 4 
                                               ),
            fill = NA,
            colour = 'black', linetype = 'dotted',
            show.legend = F,
            size = 0.25,
            stat = 'identity', alpha = 0.3) +
  theme_bw() +
  geom_bar(stat = 'identity') +
  geom_segment(
    aes(x = 1974, xend = 1974, y = 1.5, yend = 3), 
    lineend = 'butt', linejoin = 'mitre',
    size = 1, arrow = arrow(length = unit(0.1, "inches"))
  ) +
  geom_segment(
    aes(x = 1974, xend = 1974, y = -1.5, yend = -3), 
    lineend = 'butt', linejoin = 'mitre',
    size = 1, arrow = arrow(length = unit(0.1, "inches"))
  ) +
  geom_text(data = president_starts_stops,
            aes(y = 4.5, x = midpoint, label = pres_last_name, colour = president_party, fill = NA), hjust = 0.5, size = 4.5) +
  geom_text(
    aes(x = 1973, y = 2.5, label = 'Decreased Deficit'), angle = 90, hjust = 0.5, size = 4.5
  ) +
  geom_text(
    aes(x = 1973, y = -2.5, label = 'Increased Deficit'), angle = 90, hjust = 0.5, size = 4.5
  ) +
  scale_fill_hue(
    name = 'Per-Capita Economic Growth'
  ) +
  theme(
    legend.position = 'top',
    axis.text.x = element_text(angle = 0),
    title = element_text(size = 20),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 15)
  ) +
  labs(
    y = 'Deficit Change from Prior Year (% of GDP)\n',
    x = '',
    title = 'Changes in Budget Deficits vs. Economic Growth\nU.S. 1975-2018',
    caption = 'Chart: Taylor G. White\nData: OECD, FRED, WDI'
  ) +
  scale_x_continuous(breaks = seq(1977, 2018, by = 4)) +
  scale_colour_manual(guide = F, values = c('DEM'='#00aef3', 'REP' = '#d8171e')) 
ggsave('output/contributions_to_deficits_by_growth.png', height = 8, width = 10, units = 'in', dpi = 600)
