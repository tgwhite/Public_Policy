

download_CA_election_results = function(election_year = seq(2002, 2014, by = 2), 
                                        election_type = c("general", "primary"), 
                                        output_directory){

  
  
  # Libraries and options ---------------------------------------------------
  
  library(stringr)
  library(dplyr)
  library(RCurl)
  library(XML)
  library(foreign)
  
  # Helper parameters --------------------------- ----------------------------

  selected_year = election_year %>% as.character() %>% substr(3, 4)
  
  base_url = "http://statewidedatabase.org/"
  election_prefix = substr(election_type, 1, 1)
  district_year = ifelse(election_year <= 2010, "d00", "d10")
  
  download_page_source = 
    paste0(base_url, district_year, "/", election_prefix, selected_year, ".html") %>%
    getURL()
  
  
  download_link_partial = paste0("pub/data/", toupper(election_prefix), selected_year, 
                                 "/state/state_", election_prefix, selected_year, 
                                 "_sov_data_by_", election_prefix, selected_year, 
                                 "_srprec.zip")
  
  download_link_full = paste0(base_url, download_link_partial)
  
  # if the election results file is found, then download
  
  if (!(str_detect(download_page_source, download_link_partial))) {
    
    cat("Election data for", selected_year, election_type, "not found, skipping download\n")
    return(NULL)
    
  } else {
        
    cat("Downloading data for the", selected_year, election_type, "election...")
    
    # download and unzip
    download.file(download_link_full, 
                  destfile = paste0(output_directory, "/election_results_", selected_year, "_", election_type, ".zip"))
    
    unzip(zipfile = paste0(output_directory, "/election_results_", selected_year, "_", election_type, ".zip"), 
          exdir = paste0(output_directory, "/election_results_", selected_year, "_", election_type))
    
    # filename for the downloaded results
    election_results_filename = paste0(output_directory, "/election_results_", selected_year, "_", election_type, 
                                       "/state_", election_prefix, selected_year, "_sov_data_by_", election_prefix, selected_year, "_",
                                       "srprec")
    
    # import .DBF file 
    if (!(file.exists(paste0(election_results_filename, ".DBF")))) {
      cat(paste0(election_results_filename, ".DBF"), "not found, skipping import!")
      cat("here is the file list: \n")
      print(list.files(path = paste0(output_directory, "/election_results_", selected_year, "_", election_type)))
      
      return(NULL)
      
    } else {
      
      imported_election_results = read.dbf(paste0(election_results_filename, ".DBF"))         
      write.csv(imported_election_results, paste0(election_results_filename, ".csv"), row.names = F)
      cat("done\n\n")
      return(imported_election_results)
      
    }            
    
  }

}


# Test Run ----------------------------------------------------------------

out_dir = "C:/Users/taylor/Dropbox/WildPolicy/Data/Voting/Berkeley Statewide Data/election_results"

general_election_results <- 
  lapply(seq(2002, 2014, by = 2), download_CA_election_results, election_type = "general", output_directory = out_dir)

