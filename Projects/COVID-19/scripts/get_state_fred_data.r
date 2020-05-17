library(tidyverse)
# library(fredr)
library(sqldf)
install.packages('fredr')

continue_download = F
clean_existing_data = T

setwd("~/Public_Policy/Projects/COVID-19/data")

fred_sqlite = dbConnect(SQLite(), dbname= "fred_sqlite.sqlite")

fredr_set_key('0437e7baffc7066bacb86efa56cc37c9')

states_category = 27281


state_categories = fredr_category_children(states_category) %>% 
  filter(! name %in% c('Puerto Rico', 'District of Columbia', 'Virgin Islands'))

if (continue_download) {
  states_already_downloaded = dbGetQuery(fred_sqlite, 'select distinct state_name from state_economic_data')
  state_categories = filter(state_categories, !name %in% states_already_downloaded$state_name)
}

if (clean_existing_data) {
  dbGetQuery(fred_sqlite, 'drop table state_economic_data')
}


all_state_data_downloaded = lapply(1:nrow(state_categories), function(state_it){
  Sys.sleep(1)
  the_state = state_categories[state_it,]
  cat('working on ', the_state$name, '...\n')
  
  state_id = the_state$id
  
  category_tags = fredr_category_tags(state_id)
  
  # inflation_deflators = fredr_category_series(state_id, order_by = 'popularity', tag_names = 'implicit price deflator')
  # taxes = fredr_category_series(state_id, order_by = 'popularity', exclude_tag_names = 'income', tag_names = 'tax', sort_order = 'desc') %>% head(10)
  # vacancy_rates = fredr_category_series(state_id, order_by = 'popularity', tag_names = 'vacancy', sort_order = 'desc')
  # housing_categories = fredr_category_series(state_id, order_by = 'popularity', tag_names = 'housing', exclude_tag_names = 'vacancy', sort_order = 'desc')
  population = fredr_category_series(state_id, order_by = 'popularity', tag_names = 'population', sort_order = 'desc')
  Sys.sleep(0.5)
  
  # unemployment = fredr_category_series(state_id, order_by = 'popularity', tag_names = 'unemployment;rate', sort_order = 'desc')
  initial_claims = fredr_category_series(state_id, order_by = 'popularity', tag_names = 'initial claims', sort_order = 'desc')
  # state_product = fredr_category_series(state_id, order_by = 'popularity', tag_names = 'gsp', sort_order = 'desc') %>% head(10)
  # wages = fredr_category_series(state_id, order_by = 'popularity', tag_names = 'wages', sort_order = 'desc') %>% head(10)
  # personal_income = fredr_category_series(state_id, order_by = 'popularity', tag_names = 'personal income;per capita', sort_order = 'desc')
  
  all_combined_categories = bind_rows(
    population, initial_claims
    # taxes, vacancy_rates, housing_categories, population, unemployment,
    # state_product, wages, personal_income, initial_claims
  ) %>% 
    unique() %>% 
    filter(
      seasonal_adjustment_short == 'NSA'
    )
  
  download_list = list()
  for (it in 1:nrow(all_combined_categories)) {
    
    selected_series = all_combined_categories[it,]
    print(it/nrow(all_combined_categories))
    the_frequency = ifelse(str_detect(selected_series$title, 'Initial Claims'), 'w', 'a')
    download_list[[it]] = fredr_series_observations(selected_series$id, frequency = the_frequency, aggregation_method = 'eop') %>%
      left_join(selected_series, by = c('series_id' = 'id')) %>%
      mutate(
        state_name = the_state$name,
        state_id = the_state$id
      )
    Sys.sleep(0.75)
  }
  
  all_state_data_stacked = bind_rows(download_list) %>% unique()
  cat('downloaded ', nrow(all_state_data_stacked), 'rows\n')
  dbWriteTable(fred_sqlite, 'state_economic_data', all_state_data_stacked, append = T)
  return(all_state_data_stacked)
})
