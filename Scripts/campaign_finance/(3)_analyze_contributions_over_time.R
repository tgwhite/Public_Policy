
# Libraries and options ---------------------------------------------------

library(plyr)
library(dplyr)
library(ggplot2)
library(data.table)
library(reshape2)
library(stringr)

options(stringsAsFactors = F)

# Helper parameters -------------------------------------------------------

start_year = 2000
end_year = year(Sys.Date())

# switches
export_contributions_by_year = F
      
# Find latest download directory ----------------------------------------------

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
                        "/extracted_files/CalAccess/DATA/CLEANED_CSV_FILES")
setwd(input_directory)

# create output directory
output_directory = paste0(input_directory, "/Analysis_Files/")
dir.create(output_directory, recursive = T)

if (export_contributions_by_year) {
  # Prep filing info files for merge with receipt filings -------------------
  
  FILER_FILINGS_CD <- lapply(list.files(), function(filename){
    
    if (str_detect(filename, "FILER_FILINGS_CD")) {    
      file_import <-      
        fread(filename, colClasses = "character") %>%      
        return()      
    } 
    
  }) %>%
    rbindlist() %>%
    mutate(
      filing_date_r = as.Date(FILING_DATE, format = "%m/%d/%Y %r"), 
      filing_year = year(filing_date_r)
    )
  
  # check the distribution of filing years
  FILER_FILINGS_CD$filing_year %>% table()
  
  # create 'finder' file and subset to believable year information # 
  
  selected_filings_info <- 
    FILER_FILINGS_CD[, list(
      FILER_ID, FILING_ID, FORM_ID, filing_date_r, filing_year
    )] %>%
    unique() %>%
    filter(
      filing_year >= start_year & filing_year <= end_year
    )
  
  setkey(selected_filings_info, filing_year)
  
  # clean memory
  rm(FILER_FILINGS_CD)
  
  # for each filing year, merge with receipt filings chunks, stack for a given year, and export 
  
  dir.create(paste0(output_directory, "contributions_by_year"), recursive = T)        
  
  
  # Loop to export contribution filings by year -----------------------------
  
  # This loop is memory efficient (for home machines) but isn't super fast. Best would be simply to stack all receipts files and merge
  # but receipts files are too large to read into memory AND perform other operations on such as a merge.
  
  lapply(start_year:end_year, function(file_year){  
    
    # data.table subset to file year 
    file_year_subset = selected_filings_info[J(file_year)]
    
    contributions_for_a_given_year <- lapply(list.files(), function(filename){
      
      if (str_detect(filename, "RCPT_CD")) {      
        
        contributions_import <- 
          fread(filename, colClasses = "character") %>%
          mutate(
            receipt_date_r = as.Date(RCPT_DATE, format = "%m/%d/%Y %r"), 
            receipt_year = year(receipt_date_r), 
            full_donor_name = paste(str_trim(CTRIB_NAML), str_trim(CTRIB_NAMF), sep = "|") %>% toupper(),
            
            full_donor_name_city_state = paste(str_trim(CTRIB_NAML), str_trim(CTRIB_NAMF), 
                                               str_trim(CTRIB_CITY), str_trim(CTRIB_ST), sep = "|") %>% toupper(), 
            
            full_donor_name_city_state_zip = paste(str_trim(CTRIB_NAML), str_trim(CTRIB_NAMF), 
                                                   str_trim(CTRIB_CITY), str_trim(CTRIB_ST), str_trim(CTRIB_ZIP4), sep = "|") %>% toupper(), 
            source_file = rep(filename)
          )
        
        # merge receipts file and filings info 
        contributions_filing_info_merged <- 
          inner_join(file_year_subset, contributions_import) %>%
          return()
      } 
    }) %>%
      rbindlist()
    
    # export contributions by year  
    
    outfile_name = paste0(output_directory, "contributions_by_year/contributions_for_", 
                          file_year, ".csv")
    
    write.csv(contributions_for_a_given_year, outfile_name)
    
    # clean memory
    rm(contributions_for_a_given_year, file_year_subset)
  })  
} # export_contributions_by_year



# Perform analyses by filing year -----------------------------------------

# setwd(paste0(output_directory, "contributions_by_year"))
# test = fread("contributions_for_2014.csv")

# test$receipt_year %>% table()
# dim(test)
