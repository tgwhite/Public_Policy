library(tidyverse)
library(R0)
library(scales)
library(incidence)
library(EpiDynamics)

initial_r0 = 2.5
distancing_effect = 0.6
distancing_r0 = initial_r0 * (1- distancing_effect)

incubation_period = 5
gamma = 1/incubation_period

initial_beta = initial_r0 / incubation_period
secondary_beta = distancing_r0 / incubation_period

italy_pop = 60.36e6
italy_susceptible = italy_pop * 0.5

##### pull in data ####
setwd("~/Public_Policy/Projects/COVID-19")
italy_jh_joined = read_csv('data/jh_joined.csv') %>% filter(country_region == 'Italy') %>% 
  filter(cases > 0) %>%
  mutate(
    time = as.numeric(date_upd - min(date_upd)),
    new_deaths = c(deaths[1], diff(deaths, 1)),
    days_since_first_death = as.numeric(date_upd - min(date_upd[deaths > 0])),
    weeks_since_first_death = days_since_first_death %/% 7
  )

initial_shutdown = as.Date('2020-03-07')
first_cases = min(italy_jh_joined$date_upd)
initial_r0_period = as.numeric(initial_shutdown - first_cases) - 1

secondary_period = as.numeric(max(italy_jh_joined$date_upd) - initial_shutdown)
total_time = initial_r0_period + secondary_period - 1 + 60
time_since_initial_case = as.numeric(max(italy_jh_joined$date_upd) - min(italy_jh_joined$date_upd))

# covid
initial_parameters <- c(beta = initial_beta, gamma = gamma)
initials <- c(S = 1 - 1e-06, I = 1e-06, R = 1 - (1 - 1e-06 - 1e-06))

initial_covid_sir <- SIR(pars = initial_parameters, init = initials, time = 0:initial_r0_period)
initial_covid_sir$results$S

secondary_parameters <- c(beta = secondary_beta, gamma = gamma)
secondary_initials <- c(S = tail(initial_covid_sir$results$S, 1), I = tail(initial_covid_sir$results$I, 1), 
              R = tail(initial_covid_sir$results$R, 1))

distancing_covid_sir = SIR(pars = secondary_parameters, init = secondary_initials, time = (initial_r0_period+1):total_time)

# combine sirs


est_deaths = function(par, missing_death_rate = 0.4, return_dat = F) {
  # death_rate = 0.01
  death_rate = par[1]
  
  combined_sirs = 
    tibble(
      time = initial_covid_sir$time,
      S = initial_covid_sir$results$S,
      I = initial_covid_sir$results$I,
      R = initial_covid_sir$results$R,
      model = 'initial'
    ) %>%
    bind_rows(
      tibble(
        time = distancing_covid_sir$time - 1,
        S = distancing_covid_sir$results$S,
        I = distancing_covid_sir$results$I,
        R = distancing_covid_sir$results$R,
        model = 'secondary'
      ) %>%
        filter(time != min(time))
    ) %>%
    mutate(
      new_cases = round(lag(S * italy_susceptible, 1) - S*italy_susceptible),
      deaths = rbinom(length(S), new_cases, prob = death_rate),
      death_timing = rweibull(length(S), 3, 1),
      death_date =  round(time + death_timing)
    )
  
  # # [1] 3.005484 7.201339
  # the_weibull = rweibull(1000, the_solution$par[1], the_solution$par[2])
  # # 
  
  italy_reported_deaths_to_date = max(italy_jh_joined$deaths)
  italy_true_death_estimate = italy_reported_deaths_to_date / (1-missing_death_rate)
  sir_deaths_to_date = filter(combined_sirs, death_date <= (time_since_initial_case-1)) %>% pull(deaths) %>% sum(na.rm = T)
  
  difference_sir_true_est = sir_deaths_to_date - italy_true_death_estimate
  if (return_dat) {
    return(combined_sirs)
  } else {
    return(difference_sir_true_est^2)  
  }
}

the_solution = optim(par = 0.01, fn = est_deaths, method = 'Brent', lower = 0.001, upper = 0.05)
the_solution$value^(1/2)
the_solution$par
# > the_solution$par
# [1] 0.003152483

sir_dat = est_deaths(the_solution$par, return_dat = T) %>%
  mutate(
    death_week = death_date %/% 7
  )
sir_dat %>% head()

weekly_deaths = group_by(sir_dat, death_week) %>%
  summarize(
    total_deaths = sum(deaths, na.rm = T)
  ) 

italy_jh_joined_weekly_deaths = group_by(italy_jh_joined, weeks_since_first_death) %>%
  summarize(
    obs = n(),
    total_deaths = sum(new_deaths)
  ) %>%
  mutate(
    projected = total_deaths / (obs/7)
  )


ggplot(weekly_deaths, aes(death_week, total_deaths)) +
  geom_bar(stat = 'identity') +
  geom_line(data = italy_jh_joined_weekly_deaths %>% filter(weeks_since_first_death >= 0), aes(weeks_since_first_death, projected), colour = 'red')
sum(weekly_deaths$total_deaths*0.6)
sum(weekly_deaths$total_deaths)
# > sum(weekly_deaths$total_deaths*0.6)
# [1] 23556
# > sum(weekly_deaths$total_deaths)
# [1] 39260
# sum(italy_jh_joined_weekly_deaths$projected)


