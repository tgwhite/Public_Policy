library(quantmod)
library(tidyverse)
library(data.table)
library(roll)
library(scales)
library(ggforce)
library(gganimate)

debt_gdp_ratio = getSymbols('GFDEGDQ188S', src = 'FRED', from = 1945, to = 2020, auto.assign = F)
annual_deficit = getSymbols('FYFSD', src = 'FRED', from = 1945, to = 2020, auto.assign = F)
annual_gdp = getSymbols('GDPA', src = 'FRED', from = 1945, to = 2020, auto.assign = F)

