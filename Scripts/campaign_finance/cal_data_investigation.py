print("--Importing modules--")
import pandas as pd
import numpy as np
import os

print("--Change working directory--\n")
os.chdir(path = "C:/Users/taylor/Dropbox/WildPolicy/Data/Campaign finance/CalAccess/DATA")
print("--Current working directory--\n")
print(os.getcwd())
print("--Here are the files in the current directory--\n")
print(os.listdir())

print("\n--Testing functionality on smaller file--\n")
sessions_full = pd.read_table("LEGISLATIVE_SESSIONS_CD.TSV", sep = "\t", error_bad_lines = False, low_memory = True)

print("--Full sessions file--\n")
print(sessions_full)

# chunk_num = 0
# for chunk in sessions_chunked:
	# chunk_num = chunk_num + 1
	# file_name = "sessions_chunked_" + str(chunk_num) + ".csv"
	# chunk.to_csv(file_name, sep = ",", index = False)
 
print("--Read in receipts data--\n")
receipts_chunked = pd.read_table("RCPT_CD.TSV", sep = "\t", error_bad_lines = False, low_memory = False, chunksize = 5e5)

print("--First rows of receipts data--")
print(receipts_chunked.get_chunk(10))

print("--Write receipts data to .csv--")
chunk_num = 0
for chunk in receipts_chunked:
	chunk_num = chunk_num + 1
	file_name = "receipts_chunked_" + str(chunk_num) + ".csv"
	print("Output file:", file_name)
	chunk.to_csv(file_name, sep = ",", index = False)


