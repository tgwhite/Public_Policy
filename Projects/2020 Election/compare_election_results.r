library(tidyverse)
library(albersusa)
library(jsonlite)
library(viridisLite)
library(scales)

cty_sf <- counties_sf("aeqd")
us_sf <- usa_sf("laea")

View(cty_sf)

setwd("~/Public_Policy/Projects/2020 Election/data")
historical_results = read_csv( "countypres_2000-2016.csv")
results_2016 = filter(historical_results, year == 2016) %>% pivot_wider(id_cols = c('year', 'state', 'county', 'FIPS', 'totalvotes'), names_from = 'candidate', values_from = 'candidatevotes') %>%
  rename(
    clinton_2016 = `Hillary Clinton`,
    trump_2016 = `Donald Trump`
  ) %>%
  mutate(
    clinton_2016_pct = clinton_2016 / (clinton_2016 + trump_2016 ),
    clinton_margin_pct = (clinton_2016 - trump_2016) / (clinton_2016 + trump_2016 )
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
    county = str_remove(county, ' (County)|(Parish)') %>% str_trim(),
    trump_clean = str_remove_all(trump, ',') %>% as.numeric(),
    biden_clean = str_remove_all(biden, ',') %>% as.numeric(),
    total_votes = trump_clean + biden_clean,
    biden_margin = biden_clean - trump_clean,
    biden_margin_pct = biden_margin / total_votes,
    biden_pct = biden_clean / total_votes
  )
election_results_by_state_county_fin$county
View(election_results_by_state_county_fin)

head(cty_sf)

selected_states = c('Pennsylvania', 'Georgia', 'Michigan', 'Wisconsin', 'Minnesota', 'Texas', 'North Carolina', 'Arizona', 'Nevada')

n_counties_by_state = group_by(cty_sf, state) %>% summarize(obs = n()) %>% ungroup()
n_counties_by_state_2020_data = group_by(election_results_by_state_county_fin, state = state_name) %>% summarize(n_counties = n_distinct(county))
combined_county_data = left_join(n_counties_by_state, n_counties_by_state_2020_data) %>% 
  mutate(
    diff = obs -n_counties
  ) %>%
  arrange(-abs(diff))

combined_county_data$geometry = NULL
matching_counties = filter(combined_county_data, obs == n_counties)
View(combined_county_data)

View(cty_sf)
cty_sf$name
pcts_by_county = left_join(cty_sf, election_results_by_state_county_fin, by = c('name' = 'county', 'state' = 'state_name')) %>%
  left_join(
    results_2016, 
    by = c('name' = 'county', 'state' = 'state')
  ) %>%
  mutate(
    change_2020_2016 = biden_margin_pct - clinton_margin_pct,
    change_z = (change_2020_2016 - mean(change_2020_2016, na.rm = T)) / sd(change_2020_2016, na.rm = T)
  )

ggplot(pcts_by_county) + 
  geom_sf(aes(fill = change_2020_2016 > .005)) 
summary(pcts_by_county$change_2020_2016)


ggplot(pcts_by_county %>% filter(state  %in% selected_states), aes(clinton_2016_pct, biden_pct)) +
  facet_wrap(~state, scales = 'free') + 
  stat_smooth(method = 'lm', se = F) +
  geom_point() +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(labels = percent)


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
