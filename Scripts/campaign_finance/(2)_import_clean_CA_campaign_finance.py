## Import/export .tsv files on campaign finance from the California Secretary of State 
## Taylor G. White
## 02/21/15

print("--Importing modules--")
import pandas as pd
import numpy as np
import os
import time
import sys

current_date = time.strftime("%d%b%Y")
import_parent = sys.argv[1]
import_path = import_parent + current_date.upper() + "/extracted_files/CalAccess/DATA"

print("--Change working directory--\n")
os.chdir(path = import_path)
output_dir = "cleaned_csv_files/" 

print("--Here are the files in the current directory--\n")
print(os.listdir())

if not os.path.exists(output_dir):
	os.makedirs(output_dir.upper())

for filename in os.listdir():
	
	file_lower = filename.lower()
	
	if file_lower.find(".tsv") > -1:				
		print("importing and exporting", filename, "\n")
		
		try:		
			file_chunked = pd.read_table(filename, sep = "\t", error_bad_lines = False, low_memory = False, chunksize = 5e5)
			chunk_num = 0
					
			for chunk in file_chunked:
				chunk_num = chunk_num + 1
				out_file_name = output_dir + "/" + filename.replace(".TSV", "") + "_chunk_" + str(chunk_num) + ".csv"
				chunk.to_csv(out_file_name, sep = ",", index = False)					
		
		except (pd.parser.CParserError) as detail:		
			print ("import failed for ", filename, ", here are the details: ", detail,"\n")			
						
	else:
		print(filename, " is not a .tsv, skipping import\n")		

		
