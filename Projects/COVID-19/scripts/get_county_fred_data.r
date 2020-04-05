# to do -- use sqlite as storage to work around download/rate limit issues

library(tidyverse)
library(fredr)
library(sqldf)

continue_download = T
clean_existing_data = F

setwd("~/Public_Policy/Projects/COVID-19/data")

fred_sqlite = dbConnect(SQLite(), dbname= "fred_sqlite.sqlite")

fredr_set_key('0437e7baffc7066bacb86efa56cc37c9')

states_category = 27281


desired_title_substrings = c("Per Capita Personal Income","Real Per Capita Personal Income",   
                             "All-Transactions House Price Index", 
                             "All Employees: Total Nonfarm", 
                             "Average Weekly Wages for Employees in Private Establishments", 
                             "Total Gross Domestic Product", "Total Per Capita Real Gross Domestic Product")


state_categories = fredr_category_children(states_category)

all_counties_MSAs = lapply(state_categories$id, function(state_id){
  
  state_children = fredr_category_children(state_id)
  msa_counties = filter(state_children, name %in% c('Counties', 'MSAs', 'Parishes'))
}) %>% 
  bind_rows() %>%
  left_join(
    state_categories %>% select(-parent_id) %>% rename(state = name), 
    by = c('parent_id' = 'id')
  ) %>% 
  rename(
    state_id = parent_id
  )

## all county level data for the US

county_substrings = c(
  'Resident Population',
  'Mean Commuting Time',
  'Unemployment Rate', 'Civilian Labor Force', 'All-Transactions House Price Index',
  'New Private Housing Structures Authorized by Building Permits',
  'Per Capita Personal Income', 
  'Net Migration Flow', 'Estimate of Median Household Income', 'Homeownership Rate',
  'Burdened Households'
)

# no counties in puerto rico
counties_categories = filter(all_counties_MSAs, name %in% c('Counties', 'Parishes'), state != 'Puerto Rico')

if (continue_download) {
  loaded_states = dbGetQuery(fred_sqlite, "select distinct state_name from us_county_indicators")
  counties_categories = filter(counties_categories, !state %in% loaded_states$state_name)  
}

if (clean_existing_data) {
  dbGetQuery(fred_sqlite, 'drop table us_county_indicators')
}

# dbSendQuery(fred_sqlite, 'drop table us_county_indicators')

all_states_counties_downloaded = map(1:nrow(counties_categories), function(state_it){
  
  this_state = counties_categories[state_it,]
  state_name = this_state$state
  cat('working on', state_name, '...\n')
  
  state_id = this_state$state_id
  state_counties_category = this_state$id
  
  the_counties = fredr_category_children(state_counties_category) 
  
  n_counties = nrow(the_counties)
  progress_bar <- txtProgressBar(min = 0, max = n_counties, style = 3)
  
  all_counties_downloaded = lapply(1:n_counties, function(county_it){
    
    this_county = the_counties[county_it,]
    county_id = this_county$id
    county_name = this_county$name
    
    county_series = fredr_category_series(county_id) %>% 
      filter(seasonal_adjustment_short == 'NSA', frequency_short == 'A')
    
    matched_county_series = map(county_substrings, function(substring){
      county_series$title[str_detect(county_series$title, substring)]
    }) %>% 
      unlist() %>% 
      unique()
    
    selected_county_series = filter(county_series, title %in% matched_county_series)
    
    all_series_downloaded = map(selected_county_series$id, function(x){
      the_download = fredr_series_observations(x, frequency = 'a', aggregation_method = 'eop')
      Sys.sleep(0.75) # the fred api complains about rate limit constraints 
      return(the_download)
    }) %>%
      bind_rows() %>%
      left_join(
        dplyr::select(selected_county_series, id, title), 
        by = c('series_id' = 'id')
      )
    
    series_download_clean = mutate(all_series_downloaded, 
                                   title_clean = str_replace(title, county_name, '') %>%
                                     str_replace(pattern = '( [a-z]{2,4} )$', ''),
                                   county_name = county_name,
                                   county_id = county_id
    ) %>%
      left_join(
        county_series %>% dplyr::select(id, frequency_short, units, seasonal_adjustment_short), 
        by = c('series_id' = 'id')
      )
    
    setTxtProgressBar(progress_bar, county_it)
    
    return(series_download_clean)
  }) %>%
    bind_rows() %>% 
    mutate(
      state_name = state_name, 
      state_id = state_id
    )
  
  dbWriteTable(fred_sqlite, 'us_county_indicators', all_counties_downloaded, append = T)
  
  close(progress_bar)
  
  return(all_counties_downloaded)
}) 

dbDisconnect(fred_sqlite)