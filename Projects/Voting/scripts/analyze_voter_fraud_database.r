library(tidyverse)
library(data.table)
library(albersusa)
us_sf <- usa_sf("laea")

setwd("~/Public_Policy/Projects/Voting/data")

heritage_voterfraud_database = read_csv("heritage_voterfraud_database.csv")
stats_by_year = group_by(heritage_voterfraud_database, Year) %>%
  summarize(
    count = n()
  ) 


filter()



# https://www.weather.gov/safety/lightning-odds

yearly_odds_lightning = 1/1222000
not_struck = 1-yearly_odds_lightning
odds_struck_in_50_years = 1-(not_struck^80)
odds_struck_in_50_years


voter_fraud_counts_2019_2018 = filter(stats_by_year, Year >= 2009, Year <= 2018)
total_cases = sum(voter_fraud_counts_2019_2018$count)
annual_avg_cases = total_cases / length(2018:2009)
lighning_strikes_per_year = 270
