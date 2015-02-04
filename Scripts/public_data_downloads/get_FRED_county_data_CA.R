

get_FRED_county_data_CA = function(variable = c("population", "unemployment_rate", 
                                                "per_capita_personal_income", "civilian_labor_force")){
  
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
  
  if (variable == "population") {
    ticker_search_pattern = "POP"
    description_search_pattern = "Resident"  
    
  } else if (variable == "unemployment_rate") {
    ticker_search_pattern = "URN"
    description_search_pattern = "Unemployment"      
    
  } else if (variable == "per_capita_personal_income") {
    ticker_search_pattern = "PCPI"
    description_search_pattern = "Per Capita"      
    
  } else if (variable == "civilian_labor_force") {
    ticker_search_pattern = "LFN"
    description_search_pattern = "Civilian Labor"      
  
  } 
  
  ##### Extract FRED tickers for each California county and download UR data ##### 
  CA_county_data = rbindlist(lapply(first_county:last_county, function(county_num){    
    
    url = getURL(paste0(base_URL, county_num))
    
    # get ticker string
    ticker_matched_results = mutate(data.frame(
      split = unlist(strsplit(url, split = "href=", fixed = T))),
      match = str_match(split, pattern = ticker_search_pattern)  
    ) %>% 
      subset(!(is.na(match))) # subset to matched entries      
    
    # find and extract ticker description
    search_results_split_string = unlist(str_split(ticker_matched_results$split[1], pattern = '\"'))[2]
    
    cleaned_fred_ticker <- 
      search_results_split_string %>%
      str_replace_all(pattern = "/fred2/series/", replacement = "")      
    
    # find and extract ticker description
    name_start_search_index = regexpr(description_search_pattern, ticker_matched_results$split[1])
    name_end_search_index = ticker_search_index = regexpr(", CA", ticker_matched_results$split[1])
    name_string = substr(ticker_matched_results$split[1], 
                         start = name_start_search_index, 
                         stop = (name_end_search_index + 3))  
    
    ## download data from FRED ## 
    ticker = cleaned_fred_ticker
    description = name_string
    
    download = getSymbols(ticker, from = "1990-01-01", to = Sys.Date(), src = "FRED", auto.assign = F)
    
    output_df = mutate(as.data.frame(download), 
                       Date = index(download), 
                       Ticker = rep(ticker), 
                       Description = rep(description))
    
    setnames(output_df, ticker, "Value")
    row.names(output_df) = NULL
    
    cat("\nData extracted for: ", name_string, "\n")  
    return(output_df)  
  }))  
  
  return(CA_county_data)  
}
