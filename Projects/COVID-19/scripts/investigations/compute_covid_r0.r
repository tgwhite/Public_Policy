library(tidyverse)
library(R0)
library(scales)
library(incidence)
library(EpiDynamics)

# https://cran.r-project.org/web/packages/earlyR/earlyR.pdf
## example: onsets on days 1, 5, 6 and 12; estimation on day 24


# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7014672/
weibull_mean = 6.4
weibull_sd = 2.3

e = exp(1)
r0=3.765201
incubation_period = 5
infection_rate = r0/incubation_period
t = 0:65
n0=1
e = exp(1)
n_t = n0*e^(infection_rate*t)
par(mfrow = c(1,1))
plot(t, n_t)

n_t2 = 0.007900 * r0^(t/incubation_period)
plot(n_t2, n_t)

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
  ) %>%
  filter(
    date <= as.Date('2020-03-26')
  )

# fit a simple exponnetial model using non-linear least squares
exponential_model_long_incubation = nls(cases ~  case_networks * r0^(t/6.4), data = us_jh_cases, 
                               start = list(case_networks = 1, r0 = 2.5))
summary(exponential_model_long_incubation)

exponential_model_short_incubation = nls(cases ~  case_networks * r0^(t/5), data = us_jh_cases, 
                                       start = list(case_networks = 1, r0 = 2.5))
summary(exponential_model_short_incubation)



mGT_weibull <- generation.time("weibull", c(6.4, 2.3))
mGT_gamma <- generation.time("gamma", c(5, 1.5))

par(mfrow = c(2, 1))
plot(mGT_weibull$time, mGT_weibull$GT)
lines(mGT_weibull$time, mGT_weibull$GT)
plot(mGT_gamma$time, mGT_gamma$GT)
lines(mGT_gamma$time, mGT_gamma$GT)

est.R0.EG(us_jh_cases$new_cases, mGT_weibull, begin=as.integer(1), end=as.integer(length(us_jh_cases$new_cases)))
est.R0.EG(us_jh_cases$new_cases, mGT_gamma, begin=as.integer(1), end=as.integer(length(us_jh_cases$new_cases)))






# https://web.stanford.edu/~jhj1/teachingdocs/Jones-Epidemics050308.pdf
# λ(t) = λ(0)e^(Λt)
# Λ = ν(R0 − 1)

# Formula: cases ~ case_networks * r0^(t/6.4)
# 
# Parameters:
#   Estimate Std. Error t value Pr(>|t|)    
# case_networks  9.14908    2.09934   4.358 4.17e-05 ***
#   r0             2.48856    0.05072  49.063  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
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


# get shape/scale for weibull distribution
fit_weibull = function(pars) {
  shape = pars[1]
  scale = pars[2]
  the_dist = rweibull(10000, shape, scale)
  the_sd = abs(sd(the_dist) - 2.3)
  the_mean =  abs(mean(the_dist) - 6.4)
  return(
    the_sd * the_mean
  )
}
the_solution = optim(par = c(3, 7), fn = fit_weibull)
# 
# > the_solution$par
# [1] 3.005484 7.201339
the_weibull = rweibull(1000, the_solution$par[1], the_solution$par[2])
# 
# r0_dist = map_dbl(the_weibull, function(gen_time){
#   return_val = NA
#   tryCatch({
#     simple_exponential_model = nls(cases ~  case_networks * r0^(t/gen_time), data = us_jh_cases, 
#                                    start = list(case_networks = 1, r0 = 2.5))
#     
#     return_val = coef(simple_exponential_model)[2]  
#   }, error = function(e){
#     return(return_val)
#   })
#   
#   return(return_val)
# })
summary(r0_dist)

cumulative_dist = tibble(
  time = mGT$time,
  proportion = mGT$GT,
  cum_prop = cumsum(proportion)
)

?SIR

# covid
parameters <- c(beta = 2.5/5, gamma = 1/5)
initials <- c(S = 1 - 1e-06, I = 1e-06, R = 1 - (1 - 1e-06 - 1e-06))

covid_sir <- SIR(pars = parameters, init = initials, time = 0:180)

# flu
parameters <- c(beta = 1.3/5, gamma = 1/5)
initials <- c(S = 1 - 1e-06, I = 1e-06, R = 1 - (1 - 1e-06 - 1e-06))

# Solve and plot.
flu_sir <- SIR(pars = parameters, init = initials, time = 0:180)
# PlotMods(sir)

distancing_effect = 0.6

comb_sirs = tibble(
  time = covid_sir$time, 
  covid_s = covid_sir$results$S * 15e6,
  new_cases = round(lag(covid_s, 1) - covid_s),
  covid_i = covid_sir$results$I,
  flu_i = flu_sir$results$I
) %>%
  mutate(
    new_cases = ifelse(is.na(new_cases), 0, new_cases),
    deaths = rbinom(length(comb_sirs$new_cases), comb_sirs$new_cases, prob = 0.004)
  )

comb_sirs$deaths
sum(comb_sirs$new_cases, na.rm = T) / 60e6
sum(comb_sirs$deaths, na.rm = T)

filter(comb_sirs, time <= 42) %>% pull(deaths) %>% sum(na.rm=T)

ggplot(comb_sirs, aes(time, deaths)) +
  geom_point()

sum(comb_sirs$covid_i * 19e6)

sum(comb_sirs$flu_deaths)
sum(comb_sirs$covid_deaths)

ggplot(comb_sirs, aes(time)) +
  geom_line(aes(y = covid_i)) +
  geom_line(aes(y = flu_i))
sum(comb_sirs$covid_i)

# http://rstudio-pubs-static.s3.amazonaws.com/6852_c59c5a2e8ea3456abbeb017185de603e.html

# simulate proportion of deaths
# x1 = 1:(30*6)
# 
# z = 0.15 + 0.015*x1 
# pr = 1/(1+exp(-z))         # pass through an inv-logit function
# y = rbinom(length(x1),1,pr)      # bernoulli response variable
# 
# #now feed it to glm:
# df = data.frame(y=y,x1=x1)
# mod = glm( y~x1,data=df,family="binomial")
# 
# plot(x1, predict(mod, type = 'response'))   
# summary(pr)
# 
# 

