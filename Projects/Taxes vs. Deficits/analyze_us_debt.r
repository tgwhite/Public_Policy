library(quantmod)
library(tidyverse)
library(data.table)
library(roll)
library(scales)
library(ggforce)
library(gganimate)
library(readxl)
library(htmltab)
library(httr)
library(fuzzyjoin)

setwd('~\\Public_Policy\\Projects\\Taxes vs. Deficits\\data')

##### Get political data #####


the_url = 'https://www.presidency.ucsb.edu/statistics/data/house-and-senate-concurrence-with-presidents'
concurrence_table = html_table(GET(the_url) %>% content(), fill = TRUE)


# MIT data lab 
house_elections = read.csv('1976-2018-house.csv') %>% 
  mutate(candidate_votes = as.numeric(candidatevotes)) %>%
  filter(stage == 'gen') %>% data.table()

senate_elections = read.csv('1976-2018-senate.csv') %>% filter(stage == 'gen') %>% data.table()
presidential_elections = read.csv('1976-2016-president.csv') %>% data.table()

large_text_theme = theme(
  plot.title = element_text(size = 24),
  plot.subtitle = element_text(size = 18, face = 'italic'),
  plot.caption = element_text(size = 13, face = 'italic', hjust = 0),
  axis.text = element_text(size = 16),
  axis.title = element_text(size = 18)
) 

setwd("~/Public_Policy/Projects/Presidential Approval/data")
the_sheets = excel_sheets("American Presidency Project - Approval Ratings for POTUS.xlsx")

president_start_dates = tibble(
  President = c('Franklin D. Roosevelt','Harry S. Truman','Dwight D. Eisenhower','John F. Kennedy',
                'Lyndon B. Johnson','Richard Nixon','Gerald R. Ford','Jimmy Carter','Ronald Reagan','George Bush',
                'William J. Clinton','George W. Bush','Barack Obama','Donald Trump'),
  start_date = c('1933-03-04', '1945-04-12', '1953-01-20', '1961-01-20', 
                 '1963-11-22', '1969-01-20', '1974-08-09', '1977-01-20', 
                 '1981-01-20', '1989-01-20', '1993-01-20', '2001-01-20', 
                 '2009-01-20', '2017-01-20') %>% as.Date(),
  end_date = lead(start_date, 1) %>% as.Date()
) 


debt_gdp_ratio = getSymbols('GFDEGDQ188S', src = 'FRED', from = 1945, to = 2020, auto.assign = F)
annual_deficit = getSymbols('FYFSD', src = 'FRED', from = 1945, to = 2020, auto.assign = F)
annual_gdp = getSymbols('GDPA', src = 'FRED', from = 1945, to = 2020, auto.assign = F)

fuzzy_left_join(
  president_start_dates, by = c('month_date' = 'start_date', 'month_date' = 'end_date'), 
  match_fun = list(`>=`, `<=`)
