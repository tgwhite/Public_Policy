
library(tidyverse)
library(data.table)
# library(bit64)
library(scales)
library(readxl)
# Extracted revenue and expenditure data is found here: 
# https://bythenumbers.sco.ca.gov/Finance-Application/City-Revenues/rrtv-rsj9
# https://bythenumbers.sco.ca.gov/Finance-Application/City-Expenditures/ju3w-4gxp

setwd('~/../Downloads')

pop_by_city = read_excel('SUB-IP-EST2019-ANNRNK.xlsx', 'pop_raw') %>%
  pivot_longer(
    cols = matches('[0-9]{4}')
  ) %>%
  rename(
    Entity_Name_Orig = `Geographic Area`,
    Fiscal_Year = name,
    popluation = value
  ) %>%
  mutate(
    Fiscal_Year = as.numeric(Fiscal_Year),
    state = str_extract(Entity_Name_Orig, ', .+') %>% str_remove(', ') %>% str_trim(),
    Entity_Name = str_remove(Entity_Name_Orig, ' city.+') %>% str_trim() %>% str_remove('\\(.+')
  ) %>%
  filter(
    state == 'California'
  )

View(pop_by_city)

City_Revenues = read_csv('City_Revenues.csv')
names(City_Revenues) = names(City_Revenues) %>% str_replace_all('[ ]+', '_')

City_Revenues = mutate(City_Revenues,
                       Estimated_Population_int = as.integer(Estimated_Population),
                           value_per_capita = Value / Estimated_Population_int) %>%
  data.table()

total_revenues_by_fiscal_year = City_Revenues[, {
  list(
    category_total = sum(Value, na.rm = T)
  )
}, by = list(Entity_Name, Fiscal_Year, Category)] %>%
  group_by(Entity_Name, Fiscal_Year) %>%
  summarize(
    total_revenues = sum(category_total, na.rm=T)
  ) %>%
  ungroup()

City_Expenditures = read_csv('City_Expenditures.csv')
names(City_Expenditures) = names(City_Expenditures) %>% str_replace_all('[ ]+', '_')

City_Expenditures = mutate(City_Expenditures,
                           value_per_capita = Value / Estimated_Population) %>%
  data.table()

total_expenditures_by_fiscal_year = City_Expenditures[, {
  list(
    category_total = sum(Value, na.rm = T)
  )
}, by = list(Entity_Name, Fiscal_Year, Category)] %>%
  group_by(Entity_Name, Fiscal_Year) %>%
  summarize(
    total_expenditures = sum(category_total, na.rm=T)
  ) %>%
  ungroup()


police_current_expenditures = filter(City_Expenditures, 
                                     Field_Name  %in% c('EXP_POLICE_PUB', 'CURR_EXP_POLICE')) %>%
  arrange(
    Entity_Name, -Fiscal_Year
  )

total_exp_rev_police = 
  left_join(
    pop_by_city , 
    total_expenditures_by_fiscal_year
  ) %>%
  left_join(
    total_revenues_by_fiscal_year 
  ) %>%
  left_join(
    police_current_expenditures %>% select(Entity_Name, Fiscal_Year, current_police_exp = Value)
  ) %>%
  filter(Fiscal_Year %in% unique(total_expenditures_by_fiscal_year$Fiscal_Year)) %>%
  arrange(
    Entity_Name, Fiscal_Year
  )


total_exp_rev_police$rev_per_capita = total_exp_rev_police$total_revenues / total_exp_rev_police$popluation
total_exp_rev_police$police_exp_per_capita = total_exp_rev_police$current_police_exp / total_exp_rev_police$popluation
total_exp_rev_police$police_pct_revenue = total_exp_rev_police$current_police_exp / total_exp_rev_police$total_revenues

ggplot(total_exp_rev_police, aes(rev_per_capita, police_exp_per_capita)) +
  geom_point(alpha = 0.3) +
  geom_point(
    data = filter(total_exp_rev_police, Entity_Name %in% c('Santa Monica')), 
    aes(colour = Entity_Name)
  ) +
  scale_colour_brewer(name='', palette = "Set1") +
  # scale_color_viridis_d(option = 'D') +
  stat_smooth(method = 'lm', se = F) +
  scale_x_continuous(labels = dollar) +
  scale_y_continuous(labels = dollar) +
  labs(
    x = 'Revenue Per Capita', y = 'Police Expenditure Per Capita', 
    title = 'Overall City Revenues vs. Police Expenditures',
    subtitle = 'California Cities, 2010-2018',
    caption = 'Data: CA State Controller, Census'
  ) + 
  theme_bw() +
  theme(
    plot.caption = element_text(face = 'italic', hjust = 0),
    plot.subtitle = element_text(face = 'italic')
  )

setwd("C:/Users/Owner/Dropbox/Politics")
ggsave('city_rev_vs_police_exp_ca.png', height = 8, width = 10, units = 'in', dpi = 800)

mean_police_exp = group_by(total_exp_rev_police, Entity_Name) %>%
  summarize(
    mean_police_per_cap = mean(police_exp_per_capita)
  ) %>%
  arrange(-mean_police_per_cap) %>%
  head(20) %>%
  mutate(
    Entity_Name_Factor = factor(Entity_Name, levels = rev(Entity_Name))
  )

ggplot(mean_police_exp, aes(Entity_Name_Factor, mean_police_per_cap)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  geom_text(aes(label = dollar(mean_police_per_cap)), hjust = 1, size = 3) +
  coord_flip() +
  theme_bw() + 
  labs(y = 'Average Per-Capita Police Expenditure', x = '', 
       title = 'Top 20 CA Cities by Average Police Expenditure Per Capita',
       subtitle = '2010-2018',
       caption = 'Data: CA State Controller, Census'
       ) +
  theme(
    plot.caption = element_text(face = 'italic', hjust = 0),
    plot.subtitle = element_text(face = 'italic')
  )
ggsave('top_20_cities_exp.png', height = 8, width = 8, units = 'in', dpi = 800)
View(total_exp_rev_police)
