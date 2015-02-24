# http://pandas.pydata.org/pandas-docs/stable/tutorials.html#excel-charts-with-pandas-vincent-and-xlsxwriter

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

# print("--Importing names file--\n")
# NAMES_CD = pd.read_table("NAMES_CD.TSV", sep = "\t", error_bad_lines = False)

# print("--NAMES_CD information --\n")
# print(NAMES_CD.head())
# print(NAMES_CD.info())

# print("--Most common last names --\n")
# last_name_counts = NAMES_CD.NAML.value_counts()
# top_ten_last_names = last_name_counts[:10]

print("--Filer and filing information--\n")
FILERNAME_CD = pd.read_table("FILERNAME_CD.TSV", sep =	"\t", error_bad_lines = False)

print("--FILERNAME_CD details--\n")
print(FILERNAME_CD.head())
print(FILERNAME_CD.tail())

print(FILERNAME_CD.FILER_TYPE.value_counts())

print("--read in filings--\n")
FILER_FILINGS_CD = pd.read_table("FILER_FILINGS_CD.TSV", sep = "\t", error_bad_lines = False, low_memory = False)

print(FILER_FILINGS_CD.head())
print(FILER_FILINGS_CD.tail())

print("--Read in receipts data --\n")
receipts_chunked_18 = pd.read_csv("receipts_chunked_18.csv", error_bad_lines = False)

print("--Merging filer information onto filing information--\n")

merged_filing_info = pd.merge(FILER_FILINGS_CD, FILERNAME_CD, how = "left", on = "FILER_ID")

