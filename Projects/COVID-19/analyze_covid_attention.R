library(readxl)
library(httr)
library(jsonlite)
library(tidyverse)
library(reshape2)
library(plotly)
library(scales)

setwd("~/Public_Policy/Projects/COVID-19")
reset_queries = F

#### query reddit for coronavirus activity ####
if (reset_queries | !file.exists('all_general_queries_clean.rds')) {
  
  day_counts = (365 * 1)
  periods_size = 365 / 52
  repeats = day_counts %/% periods_size
  starts = round(day_counts - (periods_size * 0:repeats))
  ends = lag(starts, 1)
  start_ends = data.frame(starts, ends) %>% na.omit()
  
  api_url = 'https://api.pushshift.io/reddit/search/comment/'
  
  queries = c(
    'coronavirus', 'virus', 'covid', 'hubei',
    'thank you', 'think', 'thanks', 'cat', 'dog', 
    'Biden', 'Bernie', 'Trump',
    'funny', 'kitten', 'puppy', 'happy', 'sad', 'please')
  
  all_queries_stacked = map(1:nrow(start_ends), function(it){
    print(it/nrow(start_ends))
    
    row = start_ends[it,]
    
    queries_stacked = map(queries, function(query){
      tryCatch({
        query_results = GET(api_url, query = list(before = paste0(row$starts, 'd'), 
                                                  after = paste0(row$ends, 'd'), 
                                                  q = query, aggs = 'subreddit', size = 0)) %>% 
          as.character() %>% fromJSON()
        
        Sys.sleep(0.5)
        mutate(query_results$aggs$subreddit, query = query) %>% return()
      }, error = function(e){
        cat('error with', query, '\n')
        print(query_results)
        print(e)
        return(NULL)
      })
      
    }) %>% 
      bind_rows() %>%
      mutate(
        start = row$starts, 
        end = row$ends, 
        period = paste(end, start, sep = '-', 'days ago')
      )
    
    return(queries_stacked)
  }) %>% bind_rows()
  
  all_queries_clean = mutate(all_queries_stacked, 
                             start_date = Sys.Date() - end, end_date = Sys.Date() - start,
                             query = str_to_title(query)
  )
  saveRDS(all_queries_clean, 'all_general_queries_clean.rds')
} else {
  all_queries_clean = readRDS('all_general_queries_clean.rds')
}

#### stack everything and summarize across subreddits #####
query_summary = group_by(all_queries_clean, end_date, query) %>% 
  summarize(
    Total = sum(doc_count)
  )

# think is the most common word used in comment threads and its very stable. Use this as a baseline for
# reddit usage
think_queries = filter(query_summary, query == 'Think') %>% rename(think_query = Total) %>% mutate(query = NULL)
query_summary_fin = left_join(query_summary, think_queries) %>%
  mutate(
    pct_of_think = Total / think_query,
    data_source = 'Reddit Comments'
  )


# extract only virus queries
virus_queries = filter(query_summary_fin, query %in% c('Coronavirus')) %>%
  arrange(query, end_date)
virus_queries_last = group_by(virus_queries, query) %>% 
  summarize(
    max_value = max(Total),
    max_pct = max(pct_of_think)
  ) %>%
  ungroup()

# index to the max query volumn 
virus_queries_fin = inner_join(virus_queries, virus_queries_last) %>%
  mutate(
    pct_of_think_index = pct_of_think/max_pct
  )


g_trends_corona_weather = read_csv('g_trends_corona_weather.csv', 
                                   col_names = c('date', 'coronavirus', 'weather'), skip=1) %>%
  mutate_all(function(x){
    ifelse(x == '<1', 0, x) 
  }) %>%
  mutate(
    data_source = 'Google Trends',
    coronavirus = as.numeric(coronavirus),
    dates = as.Date(date, format = '%m/%d/%Y'),
    coronavirus_index = coronavirus / max(coronavirus), 
    query = 'Coronavirus'
  ) %>%
  filter(
    dates >= as.Date('2020-01-20')
  )
g_trends_coronavirus_hoax = read_csv('g_trends_coronavirus_hoax.csv', 
                                     col_names = c('dates', 
                                                   'is_coronavirus_real', 
                                                   'coronavirus_hoax',
                                                   'is_coronavirus_a_hoax'), skip = 1) %>%
  mutate_all(function(x){
    ifelse(x == '<1', 0, x) 
  }) %>%
  mutate(
    dates = as.Date(dates, format = '%m/%d/%Y'),
    is_coronavirus_a_hoax = as.numeric(is_coronavirus_a_hoax),
    coronavirus_hoax_value = pmax(is_coronavirus_real, coronavirus_hoax, is_coronavirus_a_hoax),
    coronavirus_hoax_index = coronavirus_hoax_value / max(coronavirus_hoax_value)
    ) 

coronavirus_quotes = read_excel('coronavirus quotes.xlsx') %>% mutate(end_date = as.Date(Date)) %>%
  filter(`Person/Organization` == 'Donald Trump', Show == 1) %>%
  mutate(
    odd_row = 1:length(Date) %% 2,
    row_yval = ifelse(odd_row, .25, -0.25),
    measure = c('timeline'),
    country_region = 'US'
  ) %>%
  rename(date_upd = Date)

us_cases_comparison = read_csv('us_cases_comparison.csv')

#### show online interest vs. Trump quotes/actions


head(g_trends_corona_weather)
ggplot() +
  geom_line(data = g_trends_coronavirus_hoax, aes(dates, coronavirus_hoax_index)) +
  geom_line(data = g_trends_corona_weather, aes(dates, coronavirus_index), colour = 'blue')
