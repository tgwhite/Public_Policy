library(tidyverse)
library(R0)
library(scales)
library(earlyR)
library(incidence)
# https://cran.r-project.org/web/packages/earlyR/earlyR.pdf
## example: onsets on days 1, 5, 6 and 12; estimation on day 24



##### pull in data ####
johns_hopkins_cases = read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv') %>%
  pivot_longer(cols = matches('^([0-9])'), names_to = 'date', values_to = 'cases')

names(johns_hopkins_cases) = names(johns_hopkins_cases) %>% tolower() %>% str_replace('[\\/]', '_')

us_jh_cases = filter(johns_hopkins_cases, country_region == 'US') %>%
  mutate(date = as.Date(date, format = '%m/%d/%y')) %>%
  arrange(date) %>%
  mutate(
    lag_cases = lag(cases, 1), 
    new_cases = cases - lag_cases,
    new_cases = ifelse(is.na(new_cases), 1, new_cases),
    t = as.numeric(date - min(date))
  )

# fit a simple exponnetial model using non-linear least squares
simple_exponential_model = nls(cases ~  case_networks * r0^(t/6.4), data = us_jh_cases, 
                               start = list(case_networks = 1, r0 = 2.5))
summary(simple_exponential_model)

confint(simple_exponential_model)

# Formula: cases ~ case_networks * r0^(t/6.4)
# 
# Parameters:
#   Estimate Std. Error t value Pr(>|t|)    
# case_networks  9.14908    2.09934   4.358 4.17e-05 ***
#   r0             2.48856    0.05072  49.063  < 2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 8936 on 74 degrees of freedom
# 
# Number of iterations to convergence: 9 
# Achieved convergence tolerance: 7.431e-06

# Formula: cases ~ case_networks * r0^(t/5)
# 
# Parameters:
#   Estimate Std. Error t value Pr(>|t|)    
# case_networks 0.004309   0.001168   3.691 0.000474 ***
#   r0            3.729612   0.082046  45.457  < 2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 887.8 on 62 degrees of freedom
# 
# Number of iterations to convergence: 23 
# Achieved convergence tolerance: 4.429e-06

# fit using existing R package, R0

# this is the generation time or incubation period
# https://annals.org/aim/fullarticle/2762808/incubation-period-coronavirus-disease-2019-covid-19-from-publicly-reported

# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7014672/
# mGT <- generation.time("gamma", c(6.5, 2.6))
mGT <- generation.time("weibull", c(6.4, 2.3))
plot(mGT$time, mGT$GT)
lines(mGT$time, mGT$GT)


est.R0.EG(us_jh_cases$new_cases, mGT, begin=as.integer(1), end=as.integer(length(us_jh_cases$new_cases)))


# Reproduction number estimate using  Exponential Growth  method.
# R :  3.836266[ 3.799067 , 3.873994 ]


### plot confirmed vs. predicted ###
us_jh_cases$simple_exp_prediction = predict(simple_exponential_model)
us_jh_cases_long = pivot_longer(us_jh_cases, cols = c('simple_exp_prediction', 'cases'))


ggplot(us_jh_cases_long, aes(date, value, colour = name)) +
  geom_line() +
  scale_colour_hue(name = 'Estimate', labels = c('cases' = 'Confirmed Cases', 'simple_exp_prediction' = 'Modeled Cases')) +
  scale_y_continuous(labels = comma) +
  labs(x = 'Date', y = 'Cases', title = 'Exponential Growth Model vs. Confirmed Cases')
ggsave('exp_growth_vs_cases.png', height = 6, width = 6, units = 'in', dpi = 600)


