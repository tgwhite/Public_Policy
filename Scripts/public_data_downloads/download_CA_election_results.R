

download_CA_election_results = function(election_year = seq(2002, 2014, by = 2), 
                                        election_type = c("general", "primary"), 
                                        output_directory){
  
  
  
  # Libraries and options ---------------------------------------------------
  
  library(stringr)
  library(dplyr)
  library(foreign)
    
  # Helper parameters --------------------------- ----------------------------
  
  selected_year = election_year %>% as.character() %>% substr(3, 4)    
  
  base_url = "http://statewidedatabase.org/"
  election_prefix = substr(election_type, 1, 1)  
  
  download_link = paste0(base_url, "pub/data/", toupper(election_prefix), selected_year, 
                         "/state/state_", election_prefix, selected_year, "_sov_data_by_", election_prefix, selected_year, "_srprec.zip")
  
  download_filename = str_extract(download_link, pattern = "state/[[:graph:]]+") %>% 
    str_replace_all(pattern = fixed("state/"), replacement = "")
  
  download_filename_clean = str_replace_all(download_filename, pattern = fixed(".zip"), replacement = "")
  
    
  download.file(download_link, destfile = paste(output_directory, download_filename, sep = "/"))
  
  unzip(zipfile = paste(output_directory, download_filename, sep = "/"), 
        exdir = paste(output_directory, download_filename_clean, sep = "/"))
  
  
  election_results = read.dbf(paste(output_directory, download_filename_clean, paste0(download_filename_clean, ".dbf"), sep = "/"))
  return(election_results)
}
