
library(httr)
library(jsonlite)
library(tidyverse)
library(reshape2)
library(plotly)
library(scales)

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

query_summary = group_by(all_queries_clean, end_date, query) %>% 
  summarize(
    Total = sum(doc_count)
  )


a = ggplot(query_summary, aes(end_date, Total, colour = query)) +
  geom_line()

ggplotly(a)
