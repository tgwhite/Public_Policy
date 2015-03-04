#
# Download, clean, and analyze California campaign finance data 
# Taylor G. White 
# 02/26/15
#

# Helper parameters -------------------------------------------------------

download_SOS_zip = F
run_python_cleaning = F
get_clean_annual_contributions = F
get_contr_summary_by_donor = F


# Download CA SOS finance database ----------------------------------------

download_file_path = paste0("C:/Users/taylor/Dropbox/WildPolicy/Data/Campaign finance/CA/", 
                            Sys.Date() %>% format("%d%b%Y") %>% toupper())


if (download_SOS_zip) {

  dir.create(download_file_path, recursive = T)
  setwd(download_file_path)
  
  download.file("http:/campaignfinance.cdn.sos.ca.gov/dbwebexport.zip", 
                destfile = "dbwebexport.zip")
  
  # unzip files
  unzip("dbwebexport.zip", exdir = "extracted_files")
  
}

# Run python import and cleaning script -----------------------------------

if (run_python_cleaning) {
  
  dir.create(paste0("C:/Users/taylor/Dropbox/WildPolicy/Public/Scripts/campaign_finance/logs/", format(Sys.Date(), "%d%b%Y") %>% toupper()), 
             recursive = T)
  setwd(paste0("C:/Users/taylor/Dropbox/WildPolicy/Public/Scripts/campaign_finance/logs/", format(Sys.Date(), "%d%b%Y") %>% toupper()))  
  
  python_program_path = 'C:/Users/taylor/Dropbox/WildPolicy/Public/Scripts/campaign_finance/(2)_import_clean_CA_campaign_finance.py'
  python_command = paste0("python ", python_program_path)
  
  python_log = system(python_command, intern = T)
  
  # output the log file 
  sink(file = paste0("python_import_log.txt"))
  lapply(python_log, function(string){
    cat(string, "\n\n")    
  })
  sink()
  
}

# Run annual contributions cleaning and export ----------------------------

if (get_clean_annual_contributions) {
  
  start_year = 2000
  end_year = year(Sys.Date())  
  
  system.time({
    source("C:/Users/taylor/Dropbox/WildPolicy/Public/Scripts/campaign_finance/(3)_get_clean_annual_contributions.R")
  })  
}


# Get stats by donor ------------------------------------------------------

if (get_contr_summary_by_donor) {
    
  start_year = 2000
  end_year = 2015
  
  system.time({
    source("C:/Users/taylor/Dropbox/WildPolicy/Public/Scripts/campaign_finance/(4)_get_contr_summary_by_donor.R")
  })  

}

