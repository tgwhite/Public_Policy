
# Libraries and options ---------------------------------------------------

library(plyr)
library(dplyr)
library(ggplot2)
library(data.table)
library(reshape2)
library(stringr)
library(scales)

options(stringsAsFactors = F)
      
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

setkey(FILER_FILINGS_CD, FORM_ID, filing_year)

### These steps effectively runs a "final action" process on form 460 filings by only retaining the amended versions.  ###

form_460_sub <- 
  FILER_FILINGS_CD[J("F460", c(start_year:end_year))] %>%    
  mutate(
    FILING_SEQUENCE = as.numeric(FILING_SEQUENCE)
    )  

setkey(form_460_sub, FILING_ID)    
  
# quickly get counts of filings' frequencies to reduce load on selecting amendments 
filing_counts <- 
  table(form_460_sub$FILING_ID) %>% 
  as.data.frame() %>%
  mutate(
    FILING_ID = Var1, 
    amended_filing = ifelse(Freq > 1, T, F)
    )
    
filings_without_amendments <- 
  form_460_sub[J(filing_counts$FILING_ID[!(filing_counts$amended_filing)])]

# extract files with amendments using data.table subset and arrange by filing sequence (last is the latest filing)
filings_with_amendments <-
  form_460_sub[J(filing_counts$FILING_ID[filing_counts$amended_filing])] %>%
  arrange(
    FILING_ID, FILING_SEQUENCE
    )
    
final_amendments <-       
  filings_with_amendments[, {
    
      data.frame(
        FILER_ID, FILING_ID, 
        FORM_ID, filing_date_r, filing_year, FILING_SEQUENCE
        ) %>%
      tail(1)
    
    }, by = FILING_ID] 


# stack final amendments and original filings  

selected_filings_info <- 
  rbind.fill(filings_without_amendments, final_amendments) %>%
  mutate(
    FILING_ID_NUM = round(as.numeric(FILING_ID), 0) # there are zero warnings about character conversion #
    ) %>%
  data.table(key = "FILING_ID") 

# check to make sure duplicates have been removed 
nrow(selected_filings_info) == nrow(unique(selected_filings_info[, list(FILER_ID, FILING_ID)]))

# chek to make sure the data was stacked properly
nrow(selected_filings_info) == nrow(filing_counts)

# clean memory
rm(FILER_FILINGS_CD)

# for each filing year, merge with receipt filings chunks, stack for a given year, and export 

dir.create(paste0(output_directory, "contributions_by_year"), recursive = T)        


# Loop to export contribution filings by year -----------------------------

# This loop is memory efficient (for home machines) but isn't super fast. Best would be simply to stack all receipts files and merge
# but receipts files are too large to read into memory AND perform other operations on such as a merge.

lapply(list.files()[str_detect(list.files(), "RCPT_CD")], function(filename){    
      
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
      source_file = rep(filename), 
      FILING_ID = round(as.numeric(FILING_ID, 0)) %>% as.character()
    ) 
      
  # merge receipts file and filings info 
  contributions_filing_info_merged <- 
    inner_join(contributions_import, selected_filings_info)          
  
  setkey(contributions_filing_info_merged, filing_year)
  
  chunk_number <- 
    filename %>%
    str_replace_all(pattern = "RCPT_CD_chunk_", replacement = "") %>%
    str_replace_all(pattern = fixed(".csv"), replacement = "") %>%
    as.integer()    
  
  lapply(min(contributions_filing_info_merged$filing_year):max(contributions_filing_info_merged$filing_year), function(file_year){
    outfile_name = paste0(output_directory, "contributions_by_year/contributions_for_", 
                          file_year, "_", chunk_number, ".csv")
    write.csv(contributions_filing_info_merged[J(file_year)], outfile_name, row.names = F)
  })
      
}) 
