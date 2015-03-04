
# Libraries and options ---------------------------------------------------

library(ggplot2)
library(dplyr)
library(stringr)
library(data.table)
library(bit64)

options(stringsAsFactors = F)


# Helper functions --------------------------------------------------------

source("C:/Users/taylor/Dropbox/WildPolicy/Public/Scripts/public_data_downloads/get_FRED_county_data_CA.R")



# Download FRED data by county --------------------------------------------





# Read in general finance data for counties ---------------------------------

setwd("C:/Users/taylor/Dropbox/WildPolicy/Data/Community Data/municipal_finance/ca_county_data_03_13")

CI_EXP_GEN_GOV = fread("CO_EXP_GENERAL.csv")

args(get_FRED_county_data_CA)
