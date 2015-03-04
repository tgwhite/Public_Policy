
get_FRED_regional_data <- function(){
  
}

library(stringr)
library(RCurl)
library(XML)
library(plyr)
library(dplyr)
library(data.table)

options(stringsAsFactors = F)

# Parameters --------------------------------------------------------------
region_type = c("county", "MSA")

fred_base_url = "http://research.stlouisfed.org/"
fred_state_main_url = getURL("http://research.stlouisfed.org/fred2/categories/27281")

# get parent URLs by state 
state_url_matches <- lapply(state.name, function(state){
  
  ticker_matched_results <- mutate(data.frame(
    split = unlist(strsplit(fred_state_main_url, split = "href=", fixed = T))),
    match = str_match(split, pattern = state)  
  ) %>% 
    subset(!(is.na(match))) # subset to matched entries      
  
  state_fred_string <-
    data.frame(fred_string = unlist(str_split(ticker_matched_results$split[1], pattern = '\"'))[2]) %>%    
    mutate(State = rep(state))
      
  return(state_fred_string)
}) %>%
  rbindlist()

setkey(state_url_matches, State)


state_specific_fred_html <- lapply(state.name, function(state){
  
  selected_state_URL <- paste0(fred_base_url, state_url_matches[J(state)]$fred_string) %>%
    getURL()
    
  ticker_matched_results <- mutate(data.frame(
    split = unlist(strsplit(selected_state_URL, split = "href=", fixed = T))),
    match = str_match(split, pattern = state)  
  ) %>% 
    subset(!(is.na(match))) # subset to matched entries      
  
  state_fred_string <-
    data.frame(fred_string = unlist(str_split(ticker_matched_results$split[1], pattern = '\"'))[2]) %>%    
    mutate(State = rep(state))  
  
})