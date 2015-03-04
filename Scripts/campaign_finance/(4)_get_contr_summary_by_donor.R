
# To do: 
# In a loop over filing year: import data, calculate total contributions by donor and receipt year/month, 
# output donor/year/month files, restack files by filing year, calculate totals by receipt year/month, 
# compute summary statistics on donors and produce data visualizations


# Libraries and options ---------------------------------------------------

library(ggplot2)
library(plyr)
library(dplyr)
library(stringr)
library(microbenchmark)
library(foreach)
library(parallel)
library(doSNOW)

options(stringsAsFactors = F)


# Find analysis file directory --------------------------------------------

setwd("C:/Users/taylor/Dropbox/WildPolicy/Data/Campaign finance/CA")

most_recent_download_folder <- 
  list.files() %>%
  as.Date(format = "%d%b%Y") %>%
  max(na.rm = T) %>%
  format("%d%b%Y") %>%
  as.character() %>% 
  toupper()

input_directory = paste0("C:/Users/taylor/Dropbox/WildPolicy/Data/Campaign finance/CA/", 
                         most_recent_download_folder, 
                         "/extracted_files/CalAccess/DATA/CLEANED_CSV_FILES/Analysis_Files/contributions_by_year")
setwd(input_directory)


# Define helper function to get contributions by donor --------------------

get_stats_by_donor <- function(file_year){
  
  library(dplyr)
  library(data.table)      
  library(stringr)
  
  # read in data 
  contributions_import <- 
    lapply(list.files(), function(filename){
      if (str_detect(filename, file_year)) {
        fread(filename) %>%
          mutate(
            chunk_source = rep(filename)
          )
      }      
    }) %>%
    rbindlist() %>%
    mutate(
      filing_date_r = as.Date(filing_date_r), 
      receipt_date_r = as.Date(receipt_date_r), 
      receipt_year_month_date = paste0(format(receipt_date_r, "%Y-%m"), "-01") %>% as.Date(), 
      AMOUNT = as.numeric(AMOUNT)
    )  
    
  # get summary stats  
  stats_by_donor_year <- contributions_import[, {  
    not_na_amount = na.omit(AMOUNT)
    
    list(
      n_unique_recipients = n_distinct(FILER_ID), 
      n_contributions = length(not_na_amount), 
      sum_contributions = sum(not_na_amount), 
      mean_contribution = mean(not_na_amount), 
      q10_contribution = quantile(not_na_amount, 0.10), 
      q25_contribution = quantile(not_na_amount, 0.25), 
      q50_contribution = quantile(not_na_amount, 0.50), 
      q75_contribution = quantile(not_na_amount, 0.75), 
      q90_contribution = quantile(not_na_amount, 0.90)
    )
    
  }, by = list(full_donor_name_city_state, receipt_year)] 
  
  # output each set of files 
  dir.create("C:/Users/taylor/Dropbox/WildPolicy/Data/Campaign finance/CA/21FEB2015/extracted_files/CalAccess/DATA/CLEANED_CSV_FILES/Analysis_Files/contributions_by_donor")
  out_dir = "C:/Users/taylor/Dropbox/WildPolicy/Data/Campaign finance/CA/21FEB2015/extracted_files/CalAccess/DATA/CLEANED_CSV_FILES/Analysis_Files/contributions_by_donor"

  write.csv(stats_by_donor_year, paste0(out_dir, "/contributions_by_donor_", file_year, ".csv"), row.names = F)
  
  # clean memory
  rm(stats_by_donor_year, contributions_import)  
  gc()  
}

# Run the summary stats function in parallel 
par_cluster <- makeCluster(3, type = "SOCK")
registerDoSNOW(par_cluster)

foreach(file_year = as.character(start_year:end_year)) %dopar% get_stats_by_donor(file_year)
stopCluster(par_cluster)
