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
    clinton_2016_pct = clinton_2016 / totalvotes
  )


file_list = list.files("2020", full.names = T)
list.files("2020", full.names = T)[1] %>% fromJSON(read_file())

election_results_by_state_county = map(file_list, function(filename){
  read_file(filename) %>% fromJSON()
}) %>%
  bind_rows() %>%
  rename(
    trump = `INCUMBENT\nDONALD TRUMP`,
    jorgensen = `JO JORGENSEN`,
    biden = `JOE BIDEN`,
    county = name
  ) %>%
  mutate(
    trump_clean = str_remove_all(trump, ',') %>% as.numeric(),
    biden_clean = str_remove_all(biden, ',') %>% as.numeric(),
    jorgensen_clean = str_remove_all(jorgensen, ',') %>% as.numeric(),
    percent_reporting_est = str_extract(percent_reporting, '.*%') %>% str_remove('%') %>% as.numeric()
  )

counts_by_county = group_by(election_results_by_state_county, state_name, county) %>%
  summarize(
    total_votes = trump_clean + biden_clean + jorgensen_clean
  ) %>%
  ungroup()


election_results_by_state_county_fin = left_join(election_results_by_state_county, counts_by_county) %>%
  mutate(
    biden_pct = biden_clean / total_votes,
    trump_pct = trump_clean / total_votes
  )

head(cty_sf)

selected_states = c('Pennsylvania', 'Georgia', 'Michigan', 'Wisconsin', 'Minnesota', 'Texas', 'North Carolina', 'Arizona', 'Nevada')

pcts_by_county = left_join(cty_sf, election_results_by_state_county_fin, by = c('name' = 'county', 'state' = 'state_name')) %>%
  left_join(
    results_2016, 
    by = c('name' = 'county', 'state' = 'state')
  ) %>%
  mutate(
    change_2020_2016 = biden_pct - clinton_2016_pct,
    change_z = (change_2020_2016 - mean(change_2020_2016, na.rm = T)) / sd(change_2020_2016, na.rm = T),
    voted_1pct_more_for_biden = change_2020_2016 >= .005 
  )



summary(pcts_by_county$change_2020_2016)
sd(pcts_by_county$change_2020_2016, na.rm = T)

ggplot(pcts_by_county) + 
  geom_sf(aes(fill = voted_1pct_more_for_biden)) 

ggplot(pcts_by_county) + 
  # geom_sf(data = us_sf, aes(fill = NA)) +
  geom_sf(aes(fill = change_z)) +
  scale_fill_viridis_c()  

ggplot(pcts_by_county) + 
  geom_sf(aes(fill = change_2020_2016)) +
  scale_fill_viridis_c(option = 'A')  


change_z


ggplot(pcts_by_county %>% filter(state  %in% selected_states), aes(clinton_2016_pct, biden_pct)) +
  facet_wrap(~state, scales = 'free') + 
  stat_smooth(method = 'lm', se = F) +
  geom_point() +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(labels = percent)

options(na.option = na.exclude)
model_by_state = lm(biden_pct ~ clinton_2016_pct * state, data = pcts_by_county)

summary(model_by_state)
hist(residuals(model_by_state))

ggplot(pcts_by_county) + 
  geom_sf(aes(fill = biden_pct)) +
  scale_fill_gradient2(low = 'red', high = 'blue', mid = 'purple', midpoint = 0.5)
?scale_fill_gradient2
