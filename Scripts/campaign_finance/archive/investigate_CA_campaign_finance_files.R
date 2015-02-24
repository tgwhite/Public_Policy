

# Libraries and options ---------------------------------------------------

library(plyr)
library(dplyr)
library(reshape2)
library(data.table)
library(ggplot2)
library(foreach)
library(parallel)
library(doSNOW)

# cl <- makeCluster(3, type = "SOCK")
# registerDoSNOW(cl)
# result <- foreach(counter = 1:100) %dopar% function(counter)
# stopCluster(cl)


# Import data -------------------------------------------------------------
setwd("C:/Users/taylor/Dropbox/WildPolicy/Data/Campaign finance/CalAccess/DATA")

# filer information #
head(FILERNAME_CD)
FILERNAME_CD = read.table("FILERNAME_CD.TSV", sep = "\t", fill = T, header = T)
FILERNAME_CD = data.table(FILERNAME_CD, key = "FILER_ID")
setkey(FILERNAME_CD, FILER_ID)


# Import maps for filers to filings
FILER_FILINGS_CD = fread("FILER_FILINGS_CD.TSV")
setkey(FILER_FILINGS_CD, FILING_ID)

# Import receipts data
receipts_chunked_18 = fread("receipts_chunked_18.csv")
setkey(receipts_chunked_18, FILING_ID)

receipts_chunked_18 %>% dim()

## merge receipt filings and filing info
receipts_filing_info_mapped <-
  receipts_chunked_18[FILER_FILINGS_CD, allow.cartesian = T]

## merge on filer namse
