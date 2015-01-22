
##### Libraries and Options #####
library(quantmod)
library(stringr)
library(RCurl)
library(XML)
library(plyr)
library(data.table)
options(stringsAsFactors = F)

##### Helper paramters #####
first_county = 27522
last_county = 27579
base_URL = "http://research.stlouisfed.org/fred2/categories/"

##### Extract FRED tickers for each California county and download UR data ##### 
CA_county_UR = rbindlist(lapply(first_county:last_county, function(county_num){
  url = getURL(paste0(base_URL, county_num))
  
  # get ticker string
  ticker_url_split_match = mutate(data.frame(
    split = unlist(strsplit(url, split = "href=", fixed = T))),
    match = str_match(split, pattern = "URN")  
  )
  
  # subset to matched entries
  ticker_matched_results = subset(ticker_url_split_match, is.na(match) == F)
  
  # find and extract ticker description
  ticker_search_index = regexpr("fred2", ticker_matched_results$split[1])
  urn_search_index = regexpr("URN", ticker_matched_results$split[1])
  
  ticker_string = substr(ticker_matched_results$split[1], start = ticker_search_index, stop = (urn_search_index + 2))
  cleaned_FRED_ticker = unlist(strsplit(ticker_string, split = "/series/"))[2]
  
  # find and extract ticker description
  name_start_search_index = regexpr("Unemployment", ticker_matched_results$split[1])
  name_end_search_index = ticker_search_index = regexpr(", CA", ticker_matched_results$split[1])
  name_string = substr(ticker_matched_results$split[1], start = name_start_search_index, stop = (name_end_search_index + 3))  
    
  ## download data from FRED ## 
  ticker = cleaned_FRED_ticker
  description = name_string
  
  download = getSymbols(ticker, from = "1990-01-01", to = Sys.Date(), src = "FRED", auto.assign = F)
  
  output_df = mutate(as.data.frame(download), 
                     Date = index(download), 
                     Ticker = rep(ticker), 
                     Description = rep(description),                             
                     County = gsub(gsub(Description, pattern = "Unemployment Rate in ", replacement = "", fixed = T), 
                                   pattern = " County, CA", replacement = "", fixed = T))
  
  setnames(output_df, ticker, "Value")
  row.names(output_df) = NULL
  
  cat("\nData extracted for: ", name_string, "\n")  
  return(output_df)  
}))
