library(tidyverse)
library(USAboundaries)
library(tigris)
library(htmltab)
library(viridisLite)
library(scales)
library(data.table)
library(cowplot)
library(treemapify)
library(ggrepel)


large_text_theme = theme(
  plot.title = element_text(size = 28),
  plot.subtitle = element_text(size = 16, face = 'italic'),
  plot.caption = element_text(size = 13, face = 'italic', hjust = 0),
  axis.text = element_text(size = 16),
  axis.title = element_text(size = 18),
  legend.title = element_text(size = 18),
  legend.text = element_text(size = 16)
) 

us_nation = nation()

us_states_current = us_states()

us_cities_1790 = us_cities(map_date = '1790-01-01', states = NULL)

# treemap and packcircles


setwd("~/Public_Policy/Projects/History - union, polarization, power/data/historical census/historical population stats/nhgis0012_csv")
state_pop_series = read_csv('nhgis0012_ts_nominal_state.csv') %>% 
  pivot_longer(
    cols = starts_with('A00AA'),
    values_to = 'population'
  ) %>%
  mutate(
    year = str_extract(name, '([0-9]{4})$') %>% as.numeric()
  ) %>%
  select(-name) 

pop_by_year = group_by(state_pop_series, year) %>%
  summarize(
    total_pop = sum(population, na.rm = T)
  ) %>%
  ungroup() 

state_pop_series = left_join(
  state_pop_series, 
  pop_by_year
) %>%
  mutate(
    pct_of_total_pop = population / total_pop
  )


race_stats_table = htmltab('https://faculty.weber.edu/kmackay/statistics_on_slavery.htm')

setwd("~/Public_Policy/Projects/History - union, polarization, power/data/historical census/historical race stats/nhgis0018_csv")

file_list = list.files()
stat_files = list.files(pattern = '.csv')
codebook_files = list.files(pattern = '.txt')
file_years = str_extract(file_list, '_[0-9]{4}_state') %>% str_extract('[0-9]{4}') %>% unique()


sorted_file_years = file_years %>% sort() %>% as.character()
race_stat_files = map(sorted_file_years , function(the_year){
  # the_year = 1790 %>% as.character()
  # head(race_stats)
  total_pop_year = filter(state_pop_series, year == as.numeric(the_year))
  the_stat_file = stat_files[str_detect(stat_files, the_year)]
  the_codebook_file = codebook_files[str_detect(codebook_files, the_year)]
  
  race_stats = read_csv(the_stat_file) %>% left_join(total_pop_year)
  val_cols = names(race_stats)[str_detect(names(race_stats), '[A-Z0-9]{2,3}[0-9]+')]
  
  
  code_mappings = read_file(the_codebook_file) %>% str_split('\r\n') %>% unlist() %>% str_trim()
  # race_stats$total = rowSums(race_stats[,val_cols])
  
  for(the_col in val_cols) {
    col_mapping = code_mappings[str_detect(code_mappings, the_col)] %>% str_remove(the_col) %>% str_remove(':') %>% str_trim() %>%
      str_replace(':', ',')
    names(race_stats)[names(race_stats) == the_col] = col_mapping
  }
  white_col = names(race_stats)[names(race_stats) %>% tolower() == 'white']
  
  # if (length(white_col) > 0) {
  #   race_stats$white_total = race_stats[,white_col]
  #   race_stats$nonwhite_total = race_stats$population  - race_stats$white_total  
  # } else {
  #   cat('no white col', the_year, '\n')
  # }
  race_stats$STATEA = NULL
  
  return(race_stats)
  
})
names(race_stat_files) = sorted_file_years

# race_stat_files[['1800']]
# race_stat_files[['1820']] %>% names()



race_stat_files[['1790']]$black_pop = race_stat_files[['1790']]$`Non-White, Free` + race_stat_files[['1790']]$`Non-White, Slave`
race_stat_files[['1800']]$black_pop = race_stat_files[['1800']]$`Nonwhite`
race_stat_files[['1810']]$black_pop = race_stat_files[['1810']]$`Nonwhite`
race_stat_files[['1820']]$black_pop = race_stat_files[['1820']]$`Slaves and free colored`
race_stat_files[['1830']]$black_pop = race_stat_files[['1830']]$`Non-white`
race_stat_files[['1840']]$black_pop = race_stat_files[['1840']]$`Nonwhite, Free` + race_stat_files[['1840']]$`Nonwhite, Slave`
race_stat_files[['1850']]$black_pop = race_stat_files[['1850']]$`Nonwhite, Free` + race_stat_files[['1850']]$`Nonwhite, Slave`

race_stat_files[['1860']]$black_pop = race_stat_files[['1860']]$`Free colored` + race_stat_files[['1860']]$Slave
race_stat_files[['1870']]$black_pop = race_stat_files[['1870']]$Colored
race_stat_files[['1880']]$black_pop = race_stat_files[['1880']]$Colored
race_stat_files[['1890']]$black_pop = race_stat_files[['1890']]$`Colored >> Male` + race_stat_files[['1890']]$`Colored >> Female`
race_stat_files[['1900']]$black_pop = race_stat_files[['1900']]$`Negro >> Male` + race_stat_files[['1900']]$`Negro >> Female`
race_stat_files[['1910']]$black_pop = race_stat_files[['1910']]$`Negro >> Male` + race_stat_files[['1910']]$`Negro >> Female`
race_stat_files[['1920']]$black_pop = race_stat_files[['1920']]$`Negro >> Male` + race_stat_files[['1920']]$`Negro >> Female`
race_stat_files[['1930']]$black_pop = race_stat_files[['1930']]$`Negro >> Male` + race_stat_files[['1930']]$`Negro >> Female`
race_stat_files[['1940']]$black_pop = race_stat_files[['1940']]$Negro
race_stat_files[['1950']]$black_pop = race_stat_files[['1950']]$`Male >> Negro` + race_stat_files[['1950']]$`Female >> Negro`
race_stat_files[['1960']]$black_pop = race_stat_files[['1960']]$`Male >> Negro` + race_stat_files[['1960']]$`Female >> Negro`

stats_since_1970 = read_csv('nhgis0018_ts_nominal_state.csv') %>%
  mutate(
    black_pop = B18AB
  )
stats_since_1970$population = rowSums(
  select(stats_since_1970, starts_with('B1')),
  na.rm = T
)

stacked_pops_since_1790_dt = race_stat_files %>% 
  bind_rows() %>%
  select(GISJOIN, YEAR, STATE, black_pop, population) %>%
  bind_rows(
    stats_since_1970 %>% 
      select(GISJOIN, YEAR, STATE, black_pop, population) 
  ) %>%
  mutate(
    black_pct = black_pop / population
  ) %>%
  arrange(STATE, YEAR) %>%
  data.table()

stacked_pops_since_1790 = stacked_pops_since_1790_dt[, {
  
  list(
    YEAR = YEAR, 
    black_pct_change = black_pct - lag(black_pct, 1),
    black_pop_change = black_pop - lag(black_pop, 1),
    black_pct = black_pct, 
    population = population, 
    black_pop = black_pop
  )
}, by = list(STATE, GISJOIN)] %>% as.data.frame()


filter(stacked_pops_since_1790, STATE == 'Louisiana', YEAR %in% c(1860, 1870, 1880))

ggplot(stacked_pops_since_1790 %>% filter(YEAR == 1790), aes(area = population, fill = black_pct, label = STATE)) +
  geom_treemap() +
  geom_treemap_text(fontface = "italic", colour = "gray", place = "centre",
                    grow = TRUE) +
  scale_fill_viridis_c(option = 'A', labels = percent)

ggplot(stacked_pops_since_1790 %>% filter(YEAR == 1870), aes(area = population, fill = black_pct, label = STATE)) +
  geom_treemap() +
  geom_treemap_text(fontface = "italic", colour = "gray", place = "centre",
                    grow = TRUE) +
  scale_fill_viridis_c(option = 'A', labels = percent)

stacked_pops_since_1790 %>% filter(YEAR == 1870) %>% arrange(-black_pct)


state_shapefiles = seq(1790, 2000, by = 10) %>% paste0('-01-01') %>% as.Date() %>% map(function(the_date){
  us_states(map_date = the_date) %>% mutate(map_date = the_date)
}) %>% 
  bind_rows() %>%
  bind_rows(
    us_states(map_date = '2000-01-01') %>% mutate(map_date = '2010-01-01' %>% as.Date())
  ) %>% 
  mutate(
    map_year = year(map_date)
  ) %>%
  filter(!str_detect(name, 'Alaska|Hawaii'))

joined_shapefiles = left_join(state_shapefiles, stacked_pops_since_1790, by = c('name' = 'STATE', 'map_year'='YEAR'))

ggplot(joined_shapefiles) +
  facet_wrap(~map_year) + 
  theme_map() +
  geom_sf(aes(fill = black_pct)) + 
  scale_fill_viridis_c(option = 'A', name = 'Black % of Population', labels = percent)

ggplot(joined_shapefiles) +
  facet_wrap(~map_year) + 
  theme_map() +
  geom_sf(aes(fill = black_pct_change < -0.02))  
  # scale_fill_gradient2(midpoint = 0, mid = '#ffffbf', high = '#fc8d59', low = '#91bfdb') 
  # scale_fill_viridis_c(option = 'A', name = 'Black % of Population', labels = percent)

setwd("~/Public_Policy/Projects/History - union, polarization, power/output")
ggsave('black_pct_change_pop_map_hist.png', height = 14, width = 18, units = 'in', dpi = 600)



filter(stacked_pops_since_1790, YEAR %in% c(1880), black_pct >= .40) %>%
  ggplot(aes(STATE, black_pct, fill = factor(YEAR))) +
  geom_bar(stat = 'identity', position = 'dodge') + 
  geom_hline(aes(yintercept = 0.5))


national_black_pop_total = group_by(stacked_pops_since_1790, YEAR) %>%
  summarize(
    total_black_pop = sum(black_pop, na.rm = T),
    total_pop = sum(population, na.rm = T)
  ) %>%
  bind_rows(
    # add in 1 year ACS estimates
    tibble(
      YEAR = 2019,
      total_pop = 328239523,
      total_black_pop = 41989671
    )
  ) %>%
  mutate(
    total_black_pct = total_black_pop / total_pop
  ) 

ggplot(national_black_pop_total %>% filter(between(YEAR, 1790, 2019)), aes(YEAR, (total_pop - lag(total_pop))/lag(total_pop))) +
  geom_bar(stat = 'identity') + 
  scale_y_continuous(labels = percent) + 
  scale_x_continuous(breaks = seq(1800, 2019, by = 10))

ggplot(national_black_pop_total , aes(YEAR, (total_black_pop - lag(total_black_pop))/lag(total_black_pop))) +
  geom_bar(stat = 'identity') + 
  scale_y_continuous(labels = percent) + 
  scale_x_continuous(breaks = seq(1800, 2019, by = 10))




# subset code_mappings to 


us_states_1790 = us_states(map_date = '1790-01-01') %>% 
  mutate(
    name = recode(name, `Vermont Republic` = 'Vermont')
  ) %>% 
  left_join(race_stat_files[['1790']], by = c('name' = 'STATE')) %>%
  mutate(
    slave_pct = `Non-White, Slave` / population,
    slave_state = ifelse(slave_pct >= 0.1, 'Slave State', 'Non-Slave State')
  ) %>%
  arrange(
    -population
  ) %>%
  mutate(
    state_name_pop = factor(name, levels = rev(name))
  )


us_states_1790 = us_states(map_date = '1810-01-01') %>% 
  mutate(
    name = recode(name, `Vermont Republic` = 'Vermont')
  ) %>% 
  left_join(race_stat_files[['1790']], by = c('name' = 'STATE')) %>%
  mutate(
    slave_pct = `Non-White, Slave` / population,
    slave_state = ifelse(slave_pct >= 0.1, 'Slave State', 'Non-Slave State')
  ) %>%
  arrange(
    -population
  ) %>%
  mutate(
    state_name_pop = factor(name, levels = rev(name))
  )


ggplot() + 
  geom_sf(data = us_states_current %>% filter(state_name %in% state.name[!state.name %in% c('Hawaii', 'Alaska')]), colour = NA, fill = 'gray60') +
  geom_sf(data = us_states_1790 %>% filter(), aes(fill = population)) + 
  # geom_sf_label(data = us_states_1790, aes(label = state_name )) +
  # geom_sf(data = us_cities_1790, aes(size = population)) +
  scale_fill_viridis_c()

ggplot() + 
  geom_sf(data = us_states_current %>% filter(state_name %in% state.name[!state.name %in% c('Hawaii', 'Alaska')]), colour = NA, fill = 'gray60') +
  geom_sf(data = us_states_1790 %>% filter(), aes(fill = slave_pct)) + 
  scale_fill_viridis_c(labels = percent, option = 'A')

ggplot(us_states_1790 %>% filter(!is.na(population)), aes(state_name_pop, population, fill = slave_pct)) +
  labs(y = 'Population, 1790 Census', x = '') +
  theme_bw() +
  theme(legend.position = 'right') +
  geom_bar(stat = 'identity', colour = 'black') +
  scale_y_continuous(labels = comma) +
  coord_flip() +
  geom_text(aes(label = paste0(round(population/1000, 0), 'k (', percent(pct_of_total_pop, accuracy = 1), ')')), hjust = -0.1) +
  scale_fill_viridis_c(option = 'A', labels = percent, name = 'Percent of Pop.\nEnslaved') 
  
group_by(us_states_1790, slave_state) %>%
  summarize(
    tot_population  = sum(population )
  )

setwd("~/Public_Policy/Projects/History - union, polarization, power/output")
ggsave('state_pop_1790_slavery.png', height = 9, width = 15, units = 'in', dpi = 600)


setwd("~/Public_Policy/Projects/History - union, polarization, power/data")
list.files(pattern = '.csv')
total_congressional_membership = read_csv("congress by party historical.csv") %>%
  mutate(
    Year = str_extract(Years, '[0-9]{4}') %>% as.numeric()
  ) %>%
  filter(PartyStatus == 'All')

african_americans_in_congress = read_csv("african americans in congress brookings.csv", na = c("", "NA", ".")) %>%
  group_by(Congress, Year, Chamber) %>%
  summarize(
    total_black_members = sum(Members, na.rm = T),
    dem_members = Members[Party == 'D']
  ) %>%
  ungroup() %>%
  bind_rows(
    tibble(
      Year = seq(1789, 1867, by = 2),
      Congress = 1:length(Year),
      Chamber = 'House',
      total_black_members = 0,
      dem_members = 0
    )
  ) %>%
  left_join(
    total_congressional_membership
  ) %>%
  mutate(
    total_seats = as.numeric(Seats),
    black_member_pct = ifelse(is.na(total_seats), 0, total_black_members / total_seats)
  ) %>%
  arrange(Congress, Chamber)




combined_black_representation_stats = bind_rows(
  african_americans_in_congress %>% mutate(variable = '% of Congress (House)', value = black_member_pct) %>% filter(Chamber == 'House'),
  national_black_pop_total %>% mutate(variable = '% of Population', value = total_black_pct ) %>% rename(Year = YEAR)
)
filter(combined_black_representation_stats, Year == 1881)

date_df = tribble(
  ~start_year, ~end_year, ~desc, ~color,
  1787, 1865, '1619-1865: Legalized Slavery', "black",
  1865, 1877, 'Post-Civil War\nReconstruction', 'lightgray',
  1877, 1965, 'Jim Crow Era', "black",
  1965, 2020, 'Post-Voting Rights Act', "lightgray"
) %>%
  mutate(
    desc_factor = factor(desc, levels = desc)
  )
fill_colors = date_df$color
names(fill_colors) = date_df$desc


ggplot(combined_black_representation_stats) +
  theme_bw() +
  labs(
    x = '', y = '',
    title = 'Timeline of Political Rights for Black Americans',
    subtitle = 'The number of black members of Congress is a measure of political rights and participation. After the Civil War, several black Americans were elected to Congress. This progress was halted by the Compromise of 1877, which removed federal troops from the South. By 1881, blacks comprised over 40% of the population in seven states, with majorities in three, yet only had two representatives in Congress.' %>% str_wrap(130),
    caption = 'Chart: Taylor G. White\nData: NHGIS/U.S. Census/ACS, Brookings'
  ) +
  theme(
    panel.grid.minor.x = element_blank()
  ) +
  large_text_theme +
  # geom_hline(aes(yintercept = 0)) +
  geom_rect(data = date_df, aes(
    xmin = start_year, xmax = end_year, ymin = 0, ymax = 0.20,
    fill = desc_factor
  ), alpha = 0.25, show.legend = F) + 
  geom_text(
    data = filter(date_df, desc == 'Post-Civil War\nReconstruction'),
    aes(x = (start_year + end_year)/2, y = 0.2, label = desc), 
    angle = 90, hjust = 1.1, size = 5
  ) +
  geom_text(
    data = filter(date_df, desc != 'Post-Civil War\nReconstruction'),
    aes(x = (start_year + end_year)/2, y = 0.2, label = desc),
    vjust = -1, size = 5
  ) +
  geom_step(aes(Year, value, linetype = variable), size = 1.25) +
  scale_x_continuous(breaks = seq(1790, 2020, by = 20)) +
  scale_fill_manual(
    values = fill_colors
  ) +
  # scale_fill_brewer(palette = 'Set1', name = '') +
  scale_linetype(name = 'Black American %', guide = F) + 
  # geom_vline(aes(xintercept = 1964)) +
  scale_y_continuous( labels = percent) +
  geom_text(data = tibble(variable = c('% of Congress', '% of Population'), value = c(0.075, 0.135), Year = 2010), 
            aes(Year, value, label = variable), size = 5)
  # geom_text_repel(data = combined_black_representation_stats %>% filter(Year %in% c(1930, 1931)), aes(Year, value, label = variable))


setwd("~/Public_Policy/Projects/History - union, polarization, power/output")
ggsave('black_political_rights_timeline.png', height = 10, width = 14, units = 'in', dpi = 600)

