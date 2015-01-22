
##### Libraries and options #####
library(ggplot2)
library(plyr)
library(dplyr)
library(stringr)
library(data.table)
options(stringsAsFactors = F)

##### Read in chunked receipts data ##### 
setwd("C:/Users/taylor/Dropbox/WildPolicy/Data/Public/Campaign finance")
chunked_receipts_list = list.files()[regexpr(text = list.files(), pattern = "receipts_chunked") > 0]

# # read in data and get info on date ranges #
# receipts_stats = rbind.fill(lapply(chunked_receipts_list, function(filename){  
#   receipts = mutate(fread(filename), 
#                     source_file = rep(filename),
#                     receipt_date = as.Date(RCPT_DATE, format = "%m/%d/%Y %r"),
#                     days_from_today = as.numeric(Sys.Date() - receipt_date))
#   
#   stats = summarize(receipts, 
#                     filename = filename[1],
#                     min_date = min(receipt_date, na.rm = T),
#                     max_date = max(receipt_date, na.rm = T),
#                     median_days_from_today = median(days_from_today, na.rm = T))
#   return(stats)
# }))
#   
# arrange(receipts_stats, median_days_from_today)

##### Extract just the last 4 chunks for analysis #####
selected_receipts_files = subset(chunked_receipts_list, chunked_receipts_list %in% paste0("receipts_chunked_", 15:18, ".csv"))

receipts_stats_sub = rbind.fill(lapply(selected_receipts_files, function(filename){  
  receipts = mutate(fread(filename), 
                    source_file = rep(filename),
                    receipt_date = as.Date(RCPT_DATE, format = "%m/%d/%Y %r"),                    
                    days_from_end_14 = as.numeric(as.Date("2014-12-31") - receipt_date))
  return(receipts)
}))

##### Update receipts file subset #####
receipts_stats_sub = mutate(receipts_stats_sub, 
                            Year = year(receipt_date),
                            Month = month(receipt_date),
                            month_year = paste(Month, Year, sep = "_"),
                            contrib_identifiers_comb = paste(str_trim(tolower(CTRIB_NAMF)), str_trim(tolower(CTRIB_NAML)), 
                                                             str_trim(tolower(CTRIB_CITY)), str_trim(tolower(CTRIB_ST)), 
                                                             str_trim(tolower(CTRIB_ZIP4)), sep = "|"))

##### Basic summary stats #####
summary_stats_list = list()
summary_stats_list[["line_item_counts_by_year"]] = as.data.frame(table(receipts_stats_sub$Year))
summary_stats_list[["filing_counts_by_year"]] = as.data.frame(table(unique(data.table(receipts_stats_sub, key = c("FILING_ID")))$Year))
summary_stats_list[["contributions_by_filing"]] = summarize(group_by(receipts_stats_sub, FILING_ID), 
                                                            n_line_items = length(unique(LINE_ITEM)),
                                                            total_contributions = sum(AMOUNT, na.rm = T),
                                                            unique_contributors = length(unique(contrib_identifiers_comb)),
                                                            earliest_receipt_date = min(receipt_date, na.rm = T), # results don't look right
                                                            latest_receipt_date = max(receipt_date, na.rm = T), # results don't look right
                                                            min_date_days = min(days_from_end_14, na.rm = T),
                                                            max_date_days = max(days_from_end_14, na.rm = T),
                                                            median_date_days = median(days_from_end_14, na.rm = T))

summary_stats_list[["contributions_by_month_year"]] = mutate(summarize(group_by(receipts_stats_sub, month_year), 
                                                                total_contributions = sum(AMOUNT, na.rm = T),
                                                                unique_contributors = length(unique(contrib_identifiers_comb)),
                                                                total_filings = length(unique(FILING_ID))),
                                                             Date = as.Date(paste(month_year, "01", sep = "_"), format = "%m_%Y_%d"))

ggplot(filter(summary_stats_list[["contributions_by_filing"]], max_date_days > 0), aes(-median_date_days, total_contributions)) +
  scale_x_continuous(limits = c(-1000,0)) +
  scale_y_continuous(limits = c(0, 1e6)) +  
  geom_point() +
  stat_smooth()

head(summary_stats_list[["contributions_by_month_year"]])
summary_stats_list[["contributions_by_month_year"]]

ggplot(filter(summary_stats_list[["contributions_by_month_year"]], Date >= "2008-01-01" & Date <= "2014-12-31"), 
       aes(Date, log(total_contributions))) +
  geom_line()

ggplot(filter(summary_stats_list[["contributions_by_month_year"]], Date >= "2008-01-01" & Date <= "2014-12-31"), 
       aes(Date, unique_contributors)) +
  geom_line()

