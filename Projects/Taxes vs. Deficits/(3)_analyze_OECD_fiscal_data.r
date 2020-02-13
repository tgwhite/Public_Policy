library(rvest)
library(httr)
library(data.table)
library(tidyverse)
library(WDI)
library(countrycode)
library(lmtest)
library(tseries)
library(plm)
library(rvest)
library(httr)
library(quantmod)
library(fredr)
library(scales)
library(quantreg)

stacked_oecd_wdi_data_lags_diffs = read_csv('data/stacked_oecd_wdi_data_lags_diffs.csv')
wide_oecd_wdi_data = read_csv('data/wide_oecd_wdi_data.csv')

##### Simple panel data models ##### 
oecd_wdi_pdata = pdata.frame(wide_oecd_wdi_data, index = c('Country', 'Year'))

pooling_model = plm(diff_value_TAXINCOME ~ diff_value_NY.GDP.PCAP.KD.ZG, model = 'pooling', data = oecd_wdi_pdata)
fixed_model = plm(diff_value_TAXINCOME ~ diff_value_NY.GDP.PCAP.KD.ZG, model = 'within', data = oecd_wdi_pdata)
random_model = plm(diff_value_TAXINCOME ~ diff_value_NY.GDP.PCAP.KD.ZG, model = 'random', data = oecd_wdi_pdata)
fixed_time_model = plm(diff_value_TAXINCOME ~ diff_value_NY.GDP.PCAP.KD.ZG + factor(Year), model = 'within', data = oecd_wdi_pdata)

summary(fixed_model)
summary(random_model)

# check whether to use random effects. <0.05 then use fixed
phtest(fixed_model, random_model)

# check for time based effects -- there are significant time based effects
pFtest(fixed_time_model, fixed_model)
plmtest(fixed_model, c("time"), type=("bp"))

# check for panel effects  -- there are time based but not individual effects
plmtest(pooling_model, type=c("bp"))


# check for cross-sectional dependence
pcdtest(fixed_model, test = c("lm")) # there is cross sectional dependence
pcdtest(fixed_model, test = c("cd")) # there is cross sectional dependence

# test for serial correlation 
pbgtest(fixed_model) # there is serial correlation 

# test for unit roots
adf.test(filter(oecd_wdi_pdata, !is.na(diff_value_TAXINCOME))$diff_value_TAXINCOME, k=1) # none present

# check for heteroskedacity
bptest(diff_value_TAXINCOME ~ diff_value_NY.GDP.PCAP.KD.ZG, data = oecd_wdi_pdata, studentize=F)
coeftest(fixed_model, vcovHC(fixed_model, method = "arellano"))
