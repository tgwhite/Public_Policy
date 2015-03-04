### Read excel files containing municipal finance data and export to .csv ###
# Taylor G. White
# 02/25/15

### County data source: 
# https://bythenumbers.sco.ca.gov/Raw-Data/Counties-Raw-Data-for-Fiscal-Years-2003-2013/esdm-5xr2


### City data source: 
# https://bythenumbers.sco.ca.gov/Raw-Data/Cities-Raw-Data-for-Fiscal-Years-2003-2013/qqwc-cejz

import pandas as pd
import os
import requests

file_name_list = ["ca_county_data_03_13.xlsx", "ca_city_data_03_13.xlsx"]
download_link_list = ["https://bythenumbers.sco.ca.gov/download/esdm-5xr2/application/zip", 
"https://bythenumbers.sco.ca.gov/download/qqwc-cejz/application/zip"]	


for it in range(len(file_name_list)):		

	filename = file_name_list[it]
	download_link = download_link_list[it]		
	out_parent = "C:/Users/taylor/Dropbox/WildPolicy/Data/Community Data/municipal_finance/" + filename.replace(".xlsx", "")
	
	print("--Importing data for ", filename, "--\n\n")
	
	if not os.path.exists(out_parent):
		os.makedirs(out_parent)
		
	os.chdir(out_parent)	
	
	print("--Downloading files--\n\n")
	file_download = requests.get(download_link)
	file_output = open(filename, "wb")
	file_output.write(file_download.content)
	file_output.close()	
    
	print("--Re-import Excel file into DataFrame--\n\n")	
	xl = pd.ExcelFile(filename)	

	for sheetname in xl.sheet_names:
		print("\nParsing sheet names:", sheetname, "\n")
		imported_df = xl.parse(sheetname)
		imported_df.to_csv((sheetname + ".csv"), sep = ",", index = False, na_rep = "NA")  
