library(tidyverse)
library(httr)
library(rvest)

the_url = "https://www.heritage.org/voterfraud/search?combine=&state=All&year=&case_type=All&fraud_type=All&page=%s"

continue_loop = T
all_results_list = list()
page_num = 0

while(continue_loop) {
  
  cat('page:', page_num, '\n')
  
  results = sprintf(the_url, page_num) %>% 
    xml2::read_html()
  
  if (length(results) == 0) break
  
  
  # get the rows and then extract text from each component 
  the_rows = results %>% html_nodes('.views-row') 
  
  row_catcher = list()
  for (row in the_rows) {
    
    # row = the_rows[2]
    row_class = html_attr(row, 'class')
    if (str_detect(row_class, "header")) {
      next
    }
    row_values = html_children(row) %>% html_node('.field-content') %>% map_chr(html_text)
    row_labels = html_children(row) %>% html_node('.views-label') %>% map_chr(html_text)
    child_text = map_chr(row_children, html_text)
    length(row_values)
    length(row_labels)
    names(row_values) = row_labels
    out_data = as.data.frame(row_values) %>% t() %>% as.data.frame()
    row_catcher[[length(row_catcher) + 1]] = out_data
  }
  
  stacked_results = bind_rows(row_catcher)
  
  
  if (nrow(stacked_results) == 0 | ! "Name" %in% names(stacked_results)) {
    cat('last iteration at ', page_num, '\n')
    continue_loop = F
  } else {
    all_results_list[[length(all_results_list) + 1]] = stacked_results %>% filter(!is.na(Name))
    page_num = page_num + 1
    Sys.sleep(0.5)
  }
}

setwd("~/Public_Policy/Projects/Voting/data")

all_results_stacked = bind_rows(all_results_list)

row.names(all_results_stacked) = NULL
write.csv(all_results_stacked, 'heritage_voterfraud_database.csv', row.names = F)
all_results_stacked$`Fraud Type`
group_by(all_results_stacked, State, `Fraud Type`) %>% summarize(obs = n()) %>% View()
table(all_results_stacked$`Fraud Type`) %>% sort(decreasing = T)
