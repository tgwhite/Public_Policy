library(tidyverse)
library(albersusa)
library(jsonlite)
library(viridisLite)
library(scales)
library(cowplot)
library(data.table)
library(USAboundaries)
library(fuzzyjoin)
options(stringsAsFactors = F)

cty_sf <- counties_sf("aeqd") %>%
  mutate(
    name = ifelse(state == 'Missouri' & name == 'St. Louis' & lsad == 'County', 'St. Louis County', as.character(name)),
    name = ifelse(state == 'Missouri' & name == 'St. Louis' & lsad == 'city', 'St. Louis City', as.character(name))
  ) 

us_sf <- usa_sf("laea")
us_states_sf = us_states()
us_counties_sf = us_counties()



setwd("~/Public_Policy/Projects/2020 Election/data")
historical_results = read_csv( "countypres_2000-2016.csv")
results_2016 = filter(historical_results, year == 2016) %>% pivot_wider(id_cols = c('year', 'state', 'county', 'FIPS', 'totalvotes'), names_from = 'candidate', values_from = 'candidatevotes') %>%
  rename(
    clinton_2016 = `Hillary Clinton`,
    trump_2016 = `Donald Trump`
  ) %>%
  mutate(
    clinton_2016_pct = clinton_2016 / (clinton_2016 + trump_2016 ),
    clinton_margin = (clinton_2016 - trump_2016),
    clinton_margin_pct = clinton_margin  / (clinton_2016 + trump_2016 )
  )

file_list = list.files("2020", full.names = T)

election_results_by_state_county_fin = map(file_list, function(filename){
  read_file(filename) %>% fromJSON()
}) %>%
  bind_rows() %>%
  rename(
    trump = trump_votes,
    biden = biden_votes
  ) %>%
  mutate(
    orig_county = county,
    lsad = ifelse(str_detect(orig_county, 'city'), 'city', 'County'),
    new_county = str_remove_all(county, '(County)|(Parish)') %>% str_trim() %>% str_remove_all("(city)") %>% str_trim(),
    county = ifelse(state_name == 'Missouri' & new_county == 'St. Louis', orig_county %>% str_to_title(), new_county),
    county = ifelse(state_name == 'Louisiana' & county == 'LaSalle', 'La Salle', county),
    trump_clean = str_remove_all(trump, ',') %>% as.numeric(),
    biden_clean = str_remove_all(biden, ',') %>% as.numeric(),
    total_votes = trump_clean + biden_clean,
    biden_margin = biden_clean - trump_clean,
    biden_margin_pct = biden_margin / total_votes,
    biden_pct = biden_clean / total_votes
  )


historical_results_wide = pivot_wider(historical_results %>% filter(totalvotes > 0), 
                                      id_cols = c('year', 'state', 'county','FIPS', 'totalvotes'), names_from = 'party', values_from = 'candidatevotes') %>%
  mutate(
    dem_rep_total = (democrat + republican),
    dem_pct = democrat / dem_rep_total,
    rep_pct = 1 - dem_pct
  ) %>%
  bind_rows(
    select(election_results_by_state_county_fin, democrat = biden_clean, republican = trump_clean, county, state = state_name, dem_pct = biden_pct) %>% 
      mutate(rep_pct = 1-dem_pct, year = 2020, dem_rep_total = democrat + republican) 
  ) %>%
  mutate(
    dem_margin_pct = dem_pct - rep_pct,
    county = recode(
      county,
      `Desoto` = 'DeSoto',
      `Lac Qui Parle` = 'Lac qui Parle',
      `Dona Ana` = 'Doña Ana',
      `Dewitt` = 'DeWitt',
      `Saint Louis` = 'St. Louis'
    )
  ) %>%
  arrange(state, county, year) %>%
  data.table() 



historical_results_fin = historical_results_wide[, {
  list(
    year = year, 
    dem_rep_total = dem_rep_total,
    democrat = democrat,
    last_dem = lag(democrat, 1),
    rep_pct = rep_pct, 
    dem_margin_pct = dem_margin_pct, 
    dem_pct = dem_pct, 
    last_dem_pct = lag(dem_pct, 1),
    last_dem_margin = lag(dem_margin_pct, 1),
    margin_change = c(NA, diff(dem_margin_pct))
  )
}, by = list(state, county)] %>%
  filter(!is.na(dem_rep_total)) %>%
  data.table() 


# filter(historical_results_fin, year == 2016, state == 'Louisiana')
# filter(historical_results_fin, year == 2020, is.na(margin_change) & !is.na(dem_rep_total))

# anti_join(cty_sf, historical_results_fin %>% select(state, county, year) %>% filter(year == 2020) %>% unique(), by = c('state' = 'state', 'name' = 'county') ) %>%
  # View()
# 
# 
# matchfun = function(a, b) {
#   stringdist::stringdist(a, b) <= 2
# }
# joined_counties = fuzzy_left_join(county_mismatches_2020, counties_2016, by = c('state'= 'state', 'county' = 'county'), match_fun = list(`==`, `matchfun`))
# joined_counties$dist = stringdist::stringdist(joined_counties$county.x, joined_counties$county.y, method = 'lcs')
# joined_counties = arrange(joined_counties, -dist)
# joined_counties %>%
#   select(state.x, contains('county'), contains('year'))
# joined_counties$county.y == joined_counties$county.x
# joined_counties$county.x


county_vote_total_percentiles = historical_results_fin[, {
  list(
    county = county, 
    state = state, 
    margin_change_cut = cut_number(margin_change, 5),
    margin_change_cut_width = cut_width(margin_change, width = 0.005, center = 0),
    dem_rep_total_cut = cut(dem_rep_total, breaks = quantile(dem_rep_total, probs = seq(0, 1, by = 0.1), na.rm = T), 
                            ordered_result = T, include.lowest = T) %>% as.integer()
  )
}, by = list(year)] %>% 
  mutate(
    margin_change_int = as.integer(margin_change_cut)
  )


historical_results_fin = left_join(historical_results_fin, county_vote_total_percentiles) %>% tibble()
setwd("~/Public_Policy/Projects/2020 Election/output")


summary(historical_results_fin$margin_change)
sd(historical_results_fin$margin_change, na.rm = T)

ggplot(historical_results_fin, aes(margin_change)) +
  facet_wrap(~factor(dem_rep_total_cut)) +
  stat_density(position = 'identity')



margin_stats_by_year_county_size = group_by(historical_results_fin, dem_rep_total_cut, year) %>%
  summarize(
    median_dem_pct = median(dem_pct, na.rm = T), 
    q25 = quantile(dem_pct, probs = 0.25, na.rm = T), 
    q75 = quantile(dem_pct, probs = 0.75, na.rm = T),
    pct_dem_winners = mean(dem_pct > 0.5, na.rm = T),
    obs = n()
  ) 

group_by(historical_results_fin, year) %>% 
  summarize(
    median_margin = median(dem_margin_pct, na.rm = T),
    median_change = median(margin_change, na.rm = T),
    avg_change = mean(margin_change, na.rm = T),
    dem_win_pct = mean(dem_pct > 0.5, na.rm = T),
    obs = n(),
    total_votes = sum(dem_rep_total , na.rm = T),
    dem_votes = sum(democrat, na.rm = T)
  )

  
ggplot(margin_stats_by_year_county_size) + 
  geom_hline(aes(yintercept = 0.5)) +
  scale_y_continuous(labels = percent) +
  geom_ribbon(aes(x = year, ymin = q25, ymax = q75), alpha = 0.5) +
  geom_line(aes(year, median_dem_pct), colour = 'orange', size = 1) +
  geom_point(aes(year, median_dem_pct), colour = 'orange') +
  facet_wrap(~dem_rep_total_cut)


ggplot(margin_stats_by_year_county_size, aes(year, median_dem_pct, colour = factor(dem_rep_total_cut))) + 
  geom_hline(aes(yintercept = 0.5)) +
  scale_y_continuous(labels = percent) +
  geom_line(size = 1) + 
  geom_point()

ggplot(historical_results_fin, aes(dem_rep_total_cut %>% factor(), margin_change)) +
  geom_boxplot()


selected_states = c('Pennsylvania', 'Georgia', 'Michigan', 'Wisconsin', 'Minnesota', 'Florida', 'North Carolina', 'Arizona', 'Nevada')

historical_results_2020 = left_join(us_counties_sf, historical_results_fin, by = c('name' = 'county', 'state_name' = 'state')) %>% 
  filter(year == 2020)

historical_results_2020_albers = left_join(cty_sf, historical_results_fin %>% filter(year == 2020), 
                                                 by = c('name' = 'county', 'state' = 'state')) %>% 
  mutate(
    margin_change_desc = ifelse(margin_change > 0, 'Biden Improvement', 'Trump Improvement') %>% factor(levels = c('Trump Improvement', 'Biden Improvement'))
  )

View(historical_results_2020_albers)
filter(historical_results_2020_albers, is.na(dem_rep_total ), state != 'Alaska')
pct_biden_improvement = sum(historical_results_2020_albers$margin_change_desc == 'Biden Improvement', na.rm = T) / sum(table(historical_results_2020_albers$margin_change_desc))
median_change = median(historical_results_2020_albers$margin_change, na.rm = T)

ggplot() + 
  # theme_map() +
  theme_minimal() +
  labs(
    x = '', y = '',
    caption = 'Chart: Taylor G. White\nData: Politico / AP, MIT Election Lab\nCounty data unavailable for Alaska',
    title = 'Change in U.S. Presidential Vote Margins from 2016-2020',
    subtitle = str_wrap("Counties are marked blue if Biden's vote margin improved over Clinton. Biden's margins improved in %s of counties, with a median of %s improvement per county."
                        %>%
                          sprintf(
                            percent(pct_biden_improvement, accuracy = 1),
                            percent(median_change, accuracy = 1)
                          ), 100)
  ) +
  theme(legend.position = 'bottom', 
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.subtitle = element_text(face = 'italic', size = 16),
        plot.title = element_text(size = 28),
        legend.text = element_text(size = 16),
        plot.caption = element_text(size = 11, face = 'italic', hjust = 0)
        ) +
  geom_sf(data = historical_results_2020_albers, aes(fill = margin_change_desc), size = 0.25) + 
  # scale_fill_brewer( palette = 'RdBu') +
  scale_fill_brewer(name = '', palette = 'Set1', na.translate = T, na.value = 'gray', breaks = c('Biden Improvement', 'Trump Improvement')) +
  # scale_fill_manual(na.translate = F, values = c('Biden Improvement' = 'steelblue', 'Trump Improvement' = 'firebrick')) +
  geom_sf(data = us_sf, fill = NA, colour = 'black', size = 0.5) 

ggsave('vote_margin_improvement_map.png', height = 9, width = 12, units = 'in', dpi = 650)
historical_results_fin$margin_change_int

plot_list = map(selected_states %>% sort(), function(the_state){
  county_sub = filter(historical_results_2020, state_name == the_state)
  state_sub = filter(us_states_sf, state_name == the_state)
  ggplot() + 
    theme_map() +
    labs(
      title = the_state
    ) +
    geom_sf(data = county_sub, aes(fill = margin_change_cut)) +
    geom_sf(data = state_sub, fill = NA) +
    scale_fill_brewer(na.translate = F, palette = 'RdBu')
})

plot_grid(plotlist = plot_list)

filter(historical_results_shape, state_name %in% selected_states) %>%
  ggplot() +
  facet_grid(state_name ~ year) +
  geom_sf(aes(fill = dem_margin_pct)) + 
  scale_fill_viridis_c()


pcts_by_county = left_join(cty_sf, election_results_by_state_county_fin, by = c('name' = 'county', 'state' = 'state_name')) %>%
  left_join(
    results_2016, 
    by = c('name' = 'county', 'state' = 'state')
  ) %>%
  mutate(
    change_2020_2016 = biden_margin_pct - clinton_margin_pct,
    # chane_bin = cut(change_2020_2016, breaks = c(-.01, -.005, 0, 0.005, 0.01)),
    change_bin = ifelse(change_2020_2016 >= 0.005, 'Biden Increase', ifelse(change_2020_2016 >= 0, 'Slight Biden Gain', 'Trump Gain')),
    change_bins = cut(change_2020_2016, breaks = seq(0, 0.03, by = 0.005), right = T, include.lowest = T, ordered_result = T),
    change_z = (change_2020_2016 - mean(change_2020_2016, na.rm = T)) / sd(change_2020_2016, na.rm = T)
  )




filter(historical_results_fin, margin_change <= -0.10, year == 2020, state == 'New York')

ggplot() + 
  theme_map() +
  scale_fill_hue(na.translate= F) +
  # scale_fill_brewer(palette = 'Set1', na.translate= F, na.value = 'gray', name = 'Biden Margin Improvement > 1/2 %') +
  geom_sf(data = pcts_by_county, aes(fill = change_bin), colour = NA) +
  geom_sf(data = us_sf, fill = NA, colour = 'black', size = 0.75) 


n_counties_by_state = group_by(cty_sf, state) %>% summarize(obs = n()) %>% ungroup()
n_counties_by_state_2020_data = group_by(election_results_by_state_county_fin, state = state_name) %>% summarize(n_counties = n_distinct(county))

combined_county_data = left_join(n_counties_by_state, n_counties_by_state_2020_data) %>% 
  mutate(
    diff = obs -n_counties
  ) %>%
  arrange(-abs(diff))

combined_county_data$geometry = NULL
matching_counties = filter(combined_county_data, obs == n_counties)


summary(pcts_by_county$change_2020_2016)

ggplot(pcts_by_county, aes(clinton_margin_pct, change_2020_2016)) +
  geom_point()

ggplot() + 
  theme_map() +
  scale_fill_brewer(palette = 'Set1', na.translate= F, na.value = 'gray', name = 'Biden Margin Improvement > 1/2 %') +
  geom_sf(data = pcts_by_county, aes(fill = change_2020_2016 >= 0.005), colour = NA) +
  geom_sf(data = us_sf, fill = NA, colour = 'black', size = 0.75) 
  # scale_fill_gradient2(midpoint = 0, low = 'red', high = 'blue', mid = 'white')

plot_list = map(selected_states, function(the_state){
  ggplot(pcts_by_county %>% filter(state == the_state)) + 
    # geom_sf(data = us_sf, fill = 'gray', size = 1) +
    labs(title = the_state) +
    theme_map() +
    geom_sf(aes(fill = change_bin)) +
    scale_fill_viridis_d()  
})
cowplot::plot_grid(plotlist = plot_list)
?plot_grid


historical_results_fin %>% head()
head(historical_results_fin)
filter(historical_results_fin, state == 'Nevada', year == 2020)
ggplot(historical_results_fin %>% filter(state  %in% selected_states, year == 2020), aes(last_dem_pct , dem_pct, size = democrat - last_dem)) +
  theme_bw() + 
  labs(
    x = 'Clinton 2016 Vote Share', y = 'Biden 2020 Vote Share',
    caption = 'Chart: Taylor G. White\nData: Politico / AP, MIT Election Lab',
    title = 'Comparing Democratic Presidential Vote Shares in Swing States',
    subtitle = '2016 and 2020 Elections, by County. Counties with more Democratic votes in 2020 are shown in blue and those with fewer are shown in yellow.' %>% str_wrap(120)
  ) + 
  geom_vline(aes(xintercept = 0.5), colour = 'gray') +
  geom_hline(aes(yintercept = 0.5), colour = 'gray') +
  scale_size(range = c(0.25, 12), labels = comma, name = 'Vote Gain / Loss') +
  facet_wrap(~state, scales = 'free') + 
  scale_fill_manual(guide = F, values = c("orange", "steelblue")) +
  # scale_color_viridis_d() +
  geom_point(aes(fill = dem_pct > last_dem_pct), pch = 21, colour = 'black') +
  geom_abline(
    slope = 1,
    intercept = 0,
    colour = 'firebrick', size = 0.5
  ) +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(labels = percent) +
# scale_y_continuous(labels = percent, limits = c(0, 1)) +
  # scale_x_continuous(labels = percent, limits = c(0, 1)) +
  theme(
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    plot.title = element_text(size = 28),
    plot.subtitle = element_text(size = 16, face = 'italic'),
    plot.caption = element_text(size = 12, face = 'italic', hjust = 0),
    strip.background = element_rect(fill = 'black'),
    strip.text = element_text(face = 'bold', colour = 'white', size = 16),
    legend.position = 'bottom', panel.grid = element_line(linetype = 'dashed')
    ) 
  

ggsave('vote_share_comparison.png', height = 12, width = 14, units = 'in', dpi = 600)
summary(pcts_by_county$change_2020_2016)
sd(pcts_by_county$change_2020_2016, na.rm = T)

ggplot(pcts_by_county) + 
  geom_sf(aes(fill = voted_1pct_more_for_biden)) 

ggplot(pcts_by_county %>% filter(state %in% matching_counties$state)) + 
  geom_sf(data = us_sf, fill = 'gray', size = 1) +
  geom_sf(aes(fill = change_z)) +
  scale_fill_viridis_c()  



change_z


options(na.option = na.exclude)
model_by_state = lm(biden_pct ~ clinton_2016_pct * state, data = pcts_by_county)

summary(model_by_state)
hist(residuals(model_by_state))

ggplot(pcts_by_county) + 
  geom_sf(aes(fill = biden_pct)) +
  scale_fill_gradient2(low = 'red', high = 'blue', mid = 'purple', midpoint = 0.5)
?scale_fill_gradient2
