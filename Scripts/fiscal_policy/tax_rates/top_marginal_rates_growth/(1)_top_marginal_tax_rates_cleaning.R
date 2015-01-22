
# Libraries and options ---------------------------------------------------

library(plyr)
library(dplyr)
library(reshape2)
library(stringr)
library(data.table)
options(stringsAsFactors = F)


# Import and clean data ---------------------------------------------------

# Brookings data 
OECD_historical_brookings <- 
  read.csv("C:/Users/taylor/Dropbox/WildPolicy/Data/Public/fiscal_policy/tax_rates/Brookings/oecd_historical_toprate_raw.csv", 
           na.string = "--") %>%
  melt(id = "Country") %>%
  mutate(variable = as.character(variable), 
         Year = as.numeric(gsub(variable, pattern = "X", replacement = "", fixed = T)), 
         data_source = rep("2_Brookings"), 
         Top_marginal_tax_rate = value * 100, 
         Country = ifelse(Country == "Korea, Republic of", "Korea", Country))

# OECD official data
OECD_official <- 
  read.csv("C:/Users/taylor/Dropbox/WildPolicy/Data/Public/fiscal_policy/tax_rates/OECD/TABLE_I1_Data_9c5f68ff-6e66-44c4-9dd9-5c14abd938b2.csv") %>% 
  filter(Central.government.personal.income.tax.rates.and.thresholds %in% paste("Marginal rate", 1:20)) %>%  
  group_by(., Year, Country) %>%
  summarize(
    Top_tier_number = max(as.numeric(gsub(Central.government.personal.income.tax.rates.and.thresholds, 
                                          pattern = "Marginal rate", replacement = ""))), 
    Top_marginal_tax_rate = Value[Central.government.personal.income.tax.rates.and.thresholds == paste("Marginal rate", Top_tier_number)]) %>%
  mutate(data_source = rep("1_OECD")) 

## Stack data and consolidate cases where data is included from both datasets (use official) ## 
OECD_stacked <- 
  rbind.fill(OECD_historical_brookings, OECD_official) %>%
  group_by(Country, Year) %>%
  summarize(selected_data_source = ifelse("1_OECD" %in% data_source, "1_OECD", "2_Brookings"), 
            Top_marginal_tax_rate_fin = Top_marginal_tax_rate[data_source == selected_data_source]) %>%            
  select(Country, Year, selected_data_source, Top_marginal_tax_rate = Top_marginal_tax_rate_fin)

# Export ------------------------------------------------------------------

setwd("C:/Users/taylor/Dropbox/WildPolicy/Data/Public/fiscal_policy/tax_rates/cleaned_files")
write.csv(OECD_stacked, paste0("OECD_top_marginal_tax_rates_", 
                min(OECD_stacked$Year), "_",  max(OECD_stacked$Year), ".csv"))
