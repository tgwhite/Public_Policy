library(tidyverse)
library(albersusa)
library(data.table)
library(ggforce)
library(plotly)
library(RcppRoll)

setwd("~/Public_Policy/Projects/COVID-19")

### get covid data and lockdown info ###
us_covid_data_by_state_with_calcs = read_csv('data/us_covid_data_by_state_with_calcs.csv')

lockdowns_by_state = select(us_covid_data_by_state_with_calcs, 
                            state_name = location_name,
                            state_abbr = location,
                            lockdown_start, lockdown_end
) %>% unique()

#### get census and FRED data #####
state_census_data = read_csv('data/ACSDP5Y2018.DP03_data_with_overlays_2020-04-13T221826.csv',
                             skip = 2, na = c('', 'NA', '(X)'),
                             col_names = F)
state_census_data_names = read_csv('data/ACSDP5Y2018.DP03_data_with_overlays_2020-04-13T221826.csv',
                              n_max = 1, na = c('', 'NA', '(X)'),
                             col_names = T) 

names(state_census_data) = names(state_census_data_names)
state_census_data_names_desc = pivot_longer(state_census_data_names, everything()) %>%
  as.data.frame() 


industry_vals = paste0('DP03_00', 33:45, 'PE')
insurance_coverage = paste0('DP03_00', 98:99, 'PE')

selected_state_census_data = select(state_census_data, GEO_ID, state_name = NAME, 
                                    all_of(industry_vals), all_of(insurance_coverage)) %>%
  mutate_if(is.numeric, function(x){x/100})

### get state and US population data  ###
fred_sqlite = dbConnect(SQLite(), dbname= "data/fred_sqlite.sqlite")

state_economic_data = dbGetQuery(fred_sqlite, 'select * from state_economic_data') %>%
  mutate(
    date = as.Date(date, origin = '1970-01-01'),
    title_clean = str_extract(title, '(.* in )|(.* for )') %>% str_replace('( in )|( for )', ''),
    title_for_col = paste0("x_", str_replace_all(title_clean, '[ \\-%,]', '_'))
  ) %>% 
  arrange(state_name, title_clean, date) %>%
  data.table() %>%
  # accidentally inserted duplicate records for Alabama, dedup here (need to index this db later)
  unique(by = c('state_name', 'title_clean', 'date'))


annual_population_by_state = filter(state_economic_data, title_clean == 'Resident Population') %>%
  mutate(
    year = year(date)
  ) 

population_by_state = filter(annual_population_by_state, date == max(date))
us_population = sum(population_by_state$value)


if (max(annual_population_by_state$year) < year(Sys.Date())) {
  annual_population_by_state_fin = 
    bind_rows(
      annual_population_by_state,
      filter(annual_population_by_state, year == max(year)) %>%
        mutate(
          year = year(Sys.Date())
        )
    )
}

initial_claims_by_state = filter(state_economic_data, title_clean == 'Initial Claims') %>%
  mutate(
    year = year(date)
  ) %>%
  left_join(annual_population_by_state_fin %>% select(state_name, year, population = value ), 
            by = c('state_name', 'year')) %>%
  rename(initial_claims = value) %>%
  mutate(
    population_total = population * 1000,
    claims_pct_pop = initial_claims / population_total
  ) %>%
  arrange(state_name, date) %>%
  data.table()

initial_claims_by_state_calcs = initial_claims_by_state[, {
  
  rolling_monthly_claims = c(rep(NA, 3), roll_sum(initial_claims, 4))
  rolling_monthly_claims_pop = rolling_monthly_claims / population_total
  
  lag_claims = lag(initial_claims, 1)
  delta_claims = initial_claims - lag_claims
  pct_change_claims = delta_claims / lag_claims
  
  lag_claims_pct_pop = lag(claims_pct_pop, 1)
  delta_claims_pct_pop = claims_pct_pop - lag_claims_pct_pop
  
  list(
    date = date,
    rolling_monthly_claims = rolling_monthly_claims,
    rolling_monthly_claims_pop = rolling_monthly_claims_pop,
    pct_change_claims = pct_change_claims,
    delta_claims_pct_pop = delta_claims_pct_pop
  )
}, by = list(state_name)]

initial_claims_by_state_fin = 
  select(initial_claims_by_state, contains('state'), date, year, initial_claims, 
         population_total, claims_pct_pop) %>%
  left_join(initial_claims_by_state_calcs) %>%
  # get weekly covid counts 
  inner_join(us_covid_data_by_state_with_calcs %>% 
               select(state_name, date, value_total_cases, cases_per_100k)) %>%
  left_join(selected_state_census_data) %>%
  inner_join(lockdowns_by_state) %>%
  mutate(
    days_since_lockdown_start = as.numeric(date - lockdown_start),
    lockdown_status = ifelse(days_since_lockdown_start >= 0, 'Lockdown Period', 'Pre-Lockdown')
  )

pivot_longer(initial_claims_by_state_fin, cols = contains('DP03')) %>%
  group_by(name) %>%
  summarize(
    correlation = cor(rolling_monthly_claims_pop, value),
    correlation2 = correlation^2
  ) %>%
  arrange(-correlation2)

latest_initial_claims_by_state_fin = filter(initial_claims_by_state_fin, 
                                            date == max(date))



filter(initial_claims_by_state_fin, 
       # date >= as.Date('2020-01-01'),
       # state_abbr %in% c('NY', 'CA', 'AL', 'GA'), 
       year==2020, month(date) == 3) %>%
ggplot(aes(days_since_lockdown_start, group = state_name))+
  # facet_wrap(~state_name) +
  geom_vline(aes(xintercept = 0), colour = 'red', linetype = 'dashed', size = 1) +
  geom_line(aes(y = rolling_monthly_claims_pop), size= 0.75, alpha = 0.4) +
  # geom_bezier0(aes(y = claims_pct_pop)) +
  geom_point(aes(y = rolling_monthly_claims_pop)) +
  labs(
    x = '\nDays Until State Lockdown', 
    y = 'Initial Claims as a Percent of State Population\n',
    title = 'COVID-19 State Lockdowns vs. Initial Unemployment Claims'
  ) + 
  # theme_dark() +
  scale_y_continuous(labels = percent)
  # theme_classic() +
  
ggsave('output/initial_claims_vs_lockdowns.png', height = 6, width = 8, units = 'in', dpi = 800)


a = ggplot(initial_claims_by_state_fin, aes(log(cases_per_100k), claims_pct_pop, colour = state_name)) +
  geom_path() +
  geom_point(aes(size = value_total_cases)) +
  # scale_size(range = c(1, 7)) +
  scale_colour_hue(guide = F)
  # stat_smooth(method = 'loess', span = 1.5) 
  
 ggplotly(a)
 