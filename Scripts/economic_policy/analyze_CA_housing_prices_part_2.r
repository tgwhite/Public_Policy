# this script 

library(RColorBrewer)
library(plyr)
library(dplyr)
library(reshape2)
library(dtplyr)
library(data.table)
library(ggplot2)
library(ggmap)
library(stringr)
library(zipcode)
library(scales)
library(animation)
library(magick)
library(httr)
library(jsonlite)
library(quantmod)
library(plm)

options(stringsAsFactors = F)

### get the names of series associated with the given tags from california counties
tag_search = c('housing', 'population', 'migration', 'income', 'commuting time')

api_base = 'https://api.stlouisfed.org/fred/category/series'
children_api = 'https://api.stlouisfed.org/fred/category/children'

ca_counties_category = 27521
ca_msas_category = 30560

## get all children of counties and msas and then search for tags
ca_counties_children = GET(children_api, query = list(
  category_id = ca_counties_category, 
  api_key = fred_api_key,
  file_type = 'json'
))
# 
# ca_msas_children = GET(children_api, query = list(
#   category_id = ca_msas_category, 
#   api_key = fred_api_key,
#   file_type = 'json'
# ))

# get category ids for each county in California
county_category_info = lapply(content(ca_counties_children)$categories, function(x) data.frame(id = x$id, county = x$name)) %>% rbindlist()
# msa_category_info = lapply(content(ca_msas_children)$categories, function(x) data.frame(id = x$id, msa = x$name)) %>% rbindlist()

# function to search a category for given tags
search_fred_series = function(category_id, tag_names){
  
  search_params = list(
    category_id = category_id,
    api_key = fred_api_key,
    tag_names = tag_names,
    limit = 1000,
    offset = 0,
    file_type = 'json'
  )
  the_query = GET(api_base, query = search_params)
  
  if (status_code(the_query) != 200) return(NULL)
  
  search_results = content(the_query)$seriess %>%
    lapply(as.data.frame) %>%
    rbind.fill()
  
  return(search_results)
}

### get all series ids/info associated with a given tag and county
county_housing_searches = lapply(county_category_info$id, function(the_id){
  inner_search_results = lapply(tag_search, function(the_tag){
    search_fred_series(the_id, the_tag) 
  }) %>%
    rbindlist()
  inner_search_results$category_id = the_id
  return(inner_search_results)
}) %>%
  rbindlist()

## download all series using quantmod
series_download = lapply(1:nrow(county_housing_searches), function(it){

  df = county_housing_searches[it,]
  
  the_download = getSymbols(df$id, auto.assign = F, src = 'FRED')
  
  out_df = as.data.frame(the_download)
  out_df$date = index(the_download) 
  out_df$series_id = names(the_download)
  out_df$title = df$title  
  out_df$frequency = df$frequency
  out_df$units = df$units
  out_df$category_id = df$category_id
  
  out_df$value = out_df[[ names(the_download)]]
  out_df[[ names(the_download)]] = NULL
  
  return(out_df)
}) %>%
  rbindlist()

# remove idiosyncratic identifiers from titles, e.g. Personal income in San Francisco
series_download$clean_title = str_split(series_download$title, '( for)|( in)') %>% sapply(function(x) x[1])

# merge the download with county names and subset down to selected series
selected_vars = c("Per Capita Personal Income", "Resident Population", "All-Transactions House Price Index", "New Private Housing Structures Authorized by Building Permits")

series_with_county_names = inner_join(series_download %>% as.data.frame(), county_category_info %>% as.data.frame(), by = c('category_id' = 'id')) %>%
  unique() %>%
  filter(clean_title %in% selected_vars)

# be careful not to create a panel data frame with varying time frequencies!
stopifnot(n_distinct(series_with_county_names$frequency) == 1)

series_with_county_names_panel = 
  dcast(series_with_county_names, county + date  ~ clean_title, value.var = 'value') %>%
  pdata.frame(index = c('county', 'date'))

# add on lagged values and percent changes
series_with_county_names_panel_fin = series_with_county_names_panel[,selected_vars]
names(series_with_county_names_panel_fin) = c('per_cap_income', 'population', 'house_prices', 'units_authorized')

## compute year on year change and lags
for (var in names(series_with_county_names_panel_fin)) {
  change_name = paste(var, 'yoy_change', sep = '_')
  series_with_county_names_panel_fin[[change_name]] = diff(series_with_county_names_panel_fin[[var]], 1) / lag(series_with_county_names_panel_fin[[var]], 1)
  series_with_county_names_panel_fin[[change_name]][is.infinite(series_with_county_names_panel_fin[[change_name]]) | is.nan(series_with_county_names_panel_fin[[change_name]])] = NA
  series_with_county_names_panel_fin[[change_name]] = series_with_county_names_panel_fin[[change_name]] * 100
  for (lag_it in 1:5) {
    lag_name = paste(var, 'yoy_change_lag', lag_it, sep = '_')
    series_with_county_names_panel_fin[[lag_name]] = lag(series_with_county_names_panel_fin[[change_name]], lag_it)
  }
}

# re-add county and date indexes
series_with_county_names_panel_fin$county = index(series_with_county_names_panel_fin, which = 'county')
series_with_county_names_panel_fin$date = index(series_with_county_names_panel_fin, which = 'date') %>% as.character() %>% as.Date(format = '%Y-%m-%d')

## create a scatterplot matrix of all vars versus house price change 
scatterplot_df = 
  select(series_with_county_names_panel_fin, county, date, contains('yoy')) %>%
  melt( 
    id = c('county', 'date', 
         'per_cap_income_yoy_change',
         'population_yoy_change',
         'house_prices_yoy_change', 
         'units_authorized_yoy_change'
         )
    ) %>%
  mutate(
    var_group = str_replace(variable, '_yoy_change_lag_[0-9]{1}', ''),
    lag_number = str_extract(variable, 'lag_[0-9]{1}')
  )

ggplot(scatterplot_df, aes(value, house_prices_yoy_change, colour = lag_number)) +
  facet_wrap(~var_group, ncol = 1, scales = 'free') +
  geom_point() +
  stat_smooth(method = 'lm', se = F)
ggsave('housing_scatterplot_matrix.png', height = 10, width = 10, units = 'in', dpi = 350)


# overall_model = lm(house_prices_yoy_change ~ population_yoy_change_lag_1 + units_authorized_yoy_change_lag_3 + per_cap_income_yoy_change_lag_1, data = series_with_county_names_panel_fin)
# summary(overall_model)
