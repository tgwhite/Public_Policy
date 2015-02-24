
# Helper parameters -------------------------------------------------------

download_file_path = paste0("C:/Users/taylor/Dropbox/WildPolicy/Data/Campaign finance/CA/", 
                            Sys.Date() %>% format("%d%b%Y") %>% toupper())

download_SOS_zip = F
run_python_cleaning = F

# Download CA SOS finance database ----------------------------------------

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

# Run analysis programs in R ----------------------------------------------


