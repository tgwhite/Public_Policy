
library(R0)
library(uuid)
library(igraph)
library(ggnetwork)
library(data.table)
library(EpiDynamics)
library(scales)
library(tidyverse)

# https://www.ijidonline.com/article/S1201-9712(20)30119-3/pdf
weibull_mean = 6.4
# weibull_mean = 4.8
weibull_sd = 2.3
# 
# > 10977 / (19e6 * .2)
# [1] 0.002888684

# get shape/scale for weibull distribution
fit_weibull = function(pars) {
  shape = pars[1]
  scale = pars[2]
  the_dist = rweibull(10000, shape, scale)
  the_sd = abs(sd(the_dist) - weibull_sd)
  the_mean =  abs(mean(the_dist) - weibull_mean)
  return(
    (the_sd * the_mean)^2 
  )
}
weibull_params = optim(par = c(3, 7), fn = fit_weibull)
the_weibull_dist = rweibull(10000, weibull_params$par[1], weibull_params$par[2])
summary(the_weibull_dist)

fit_weibull_r0 = function(pars) {
  shape = pars[1]
  scale = pars[2]
  the_dist = rweibull(10000, shape, scale)
  mean_dist = mean(the_dist) - 2.5
  min_dist = min(the_dist) - 0.75
  
  return(
    (mean_dist)^2 + min_dist^2
  )
}
r0_params = optim(par = c(3, 7), fn = fit_weibull_r0)


run_case_simulation = function(initial_susceptible = 20e6, the_r0, 
                               lockdown_date = NULL, lockdown_effect = NULL, r0_type = NULL) {
  this_r0 = the_r0
  current_susceptible = initial_susceptible
  
  case_info_list = list()
  
  generation = 0
  
  cases_gen_df = tibble(
    parent_id = NA,
    incubation_start = 0
  )
  n_cases_generated = 1
  
  continue_loop = T
  
  while (continue_loop) {
    loop_start = proc.time()
    
    # print(generation)
    
    if (n_cases_generated > current_susceptible) {
      # cat('too many cases generated!\n', n_cases_generated, '\n')
      n_cases_generated = current_susceptible
      suseptible_by_id = current_susceptible - 1:n_cases_generated
      # cat('n_cases_generated is now', n_cases_generated, '\n')
      cases_gen_df = head(cases_gen_df, n_cases_generated)
    } else {
      suseptible_by_id = current_susceptible - 1:n_cases_generated
    }
    # cat('n_cases_generated:', n_cases_generated, '\n')
    # cat('current_susceptible:', current_susceptible, '\n')
    # 
    
    # suseptible_by_id = suseptible_by_id[suseptible_by_id > 0]
    
    if (current_susceptible < 0) {
      # cat('no more susceptible')
      break
    }
    
    the_id = initial_susceptible - suseptible_by_id
    # https://wwwnc.cdc.gov/eid/article/26/6/20-0357_article
    # https://www.ijidonline.com/article/S1201-9712(20)30119-3/pdf
    
    # incubation_time = rnorm(n_cases_generated, 3.96, 4.75)
    # incubation_time = rgamma(n_cases_generated, 4.37,  scale = 1.25) 
    # incubation_time = rweibull(n_cases_generated, weibull_params$par[1], weibull_params$par[2])
    incubation_time = 5
    incubation_start_date = cases_gen_df$incubation_start
    incubation_end_date = incubation_start_date + incubation_time 
    prob_running_into_someone_else = suseptible_by_id / initial_susceptible
    
    if (r0_type == 'weibull') {
      the_r0 = rweibull(n_cases_generated, r0_params$par[1], r0_params$par[2])   
    } else if (r0_type == 'unif') {
      the_r0 = runif(n_cases_generated, this_r0*0.75, this_r0*1.25)   
    }
    
    if (!is.null(lockdown_date)) {
      lockdown_effects = ifelse(incubation_start_date >= lockdown_date, lockdown_effect, 1)
      r0_adj = the_r0 * prob_running_into_someone_else * lockdown_effects
    } else {
      r0_adj = the_r0 * prob_running_into_someone_else
    }
    
    cases_generated = floor(r0_adj) + rbinom(length(r0_adj), 1, r0_adj - floor(r0_adj))
    n_cases_generated = sum(cases_generated)
    
    these_cases_df = data.table(
      the_id = the_id %>% as.integer(),
      generation = generation,
      parent_id = cases_gen_df$parent_id,
      incubation_time,
      incubation_start_date,
      incubation_end_date,
      prob_running_into_someone_else,
      r0_adj,
      cases_generated
    ) %>% 
      setkey(the_id)
    
    case_info_list[[length(case_info_list) + 1]] = these_cases_df
    
    if (n_cases_generated > 0) {
      # important to sort by cases generated so the recyling works
      with_gen_cases = filter(these_cases_df, cases_generated > 0) %>% arrange(-cases_generated)
      
      cases_gen_df = data.table(
        parent_id = rep(with_gen_cases$the_id, length.out = n_cases_generated),
        incubation_start = runif(n_cases_generated, with_gen_cases$incubation_start_date, with_gen_cases$incubation_end_date)
      ) %>% 
        setorder(incubation_start)
      
      stopifnot(nrow(cases_gen_df) == n_cases_generated)
    }
    
    # print(proc.time() - loop_start)
    generation = generation + 1
    current_susceptible = min(suseptible_by_id)
    continue_loop = n_cases_generated > 0 & current_susceptible > 0
  }
  
  
  case_info_df = rbindlist(case_info_list) %>%
    mutate(
      start_day = floor(incubation_start_date),
      end_day = floor(incubation_end_date)
    )
  return(rbindlist(case_info_list))
}


covid_ifr = 0.003
flu_ifr = 0.001
n_susceptible = 10e6

covid_case_simulation = run_case_simulation(r0_type = 'covid', the_r0 = 2.5, 
                                            initial_susceptible = n_susceptible/2) %>% 
  mutate(
    start_day = floor(incubation_start_date),
    died = rbinom(length(the_id), 1, covid_ifr),
    death_date = ifelse(died, start_day + 14, NA)
  )

generation_stats = group_by(covid_case_simulation, generation) %>%
  summarize(
    obs = n(),
    mean_cases = mean(cases_generated),
    mean_r0 = mean(r0_adj),
    mean_prob = mean(prob_running_into_someone_else ),
    min_prob = min(prob_running_into_someone_else),
    max_prob = max(prob_running_into_someone_else),
    start = min(incubation_start_date),
    end = max(incubation_end_date),
    mean_start = mean(incubation_start_date),
    mean_end = mean(incubation_end_date)
  )

ggplot(generation_stats, aes(start, max_prob, colour = factor(generation))) +
  geom_point() +
  geom_point(aes(y = min_prob))

covid_case_simulation$incubation_time
covid_case_simulation %>% pull(cases_generated) %>% summary()

ggplot(covid_case_simulation, aes(incubation_start_date, cases_generated, size = r0_adj)) +
  geom_point()

incubation_period = 5
initial_r0 = 2.5
gamma = 1/incubation_period
initial_beta = initial_r0 / incubation_period
initial_parameters <- c(beta = initial_beta, gamma = gamma)
initials <- c(S = 1 - 1e-06, I = 1e-06, R = 1 - (1 - 1e-06 - 1e-06))
# initials <- c(S = 1 - 1e-06, I = 1e-06, R = 1 - (1 - 1e-06 - 1e-06))
initial_covid_sir <- SIR(pars = initial_parameters, init = initials, time = 0:240)

counts_by_day = group_by(covid_case_simulation, start_day) %>%
  summarize(
    obs = n_distinct(the_id)
  )

sir_results =  initial_covid_sir$results %>%
  mutate(
    n_susceptible = n_susceptible/2 * S,
    n_cases = lag(n_susceptible, 1) - n_susceptible 
  )
b = left_join(sir_results, counts_by_day, by = c('time' = 'start_day')) %>%
  mutate(
    obs= ifelse(is.na(obs), 0, obs),
    n_cases = ifelse(is.na(n_cases), 0, n_cases)
  )
with(b, cor(obs, n_cases))

ggplot(counts_by_day, aes(start_day, obs)) +
  geom_area(alpha = 0.3) +
  geom_area(data = sir_results, aes(time, n_cases), alpha = 0.3, fill = 'blue') +
  scale_y_continuous(labels = comma)



covid_case_simulation_lockdown = run_case_simulation(the_r0 = 2.5, r0_type = 'covid', 
                                                     lockdown_date = 15, lockdown_effect = 0.65, 
                                                     initial_susceptible = n_susceptible/2) %>% 
  mutate(
    start_day = floor(incubation_start_date),
    died = rbinom(length(the_id), 1, covid_ifr),
    death_date = ifelse(died, start_day + 14, NA)
  )


flu_case_simulation = run_case_simulation(the_r0 = 1.3, r0_type = 'flu', 
                                          initial_susceptible = 327e6/4) %>% 
  mutate(
    start_day = floor(incubation_start_date),
    died = rbinom(length(the_id), 1, flu_ifr),
    death_date = ifelse(died, start_day + 14, NA)
  )

covid_cases_by_day = dplyr::group_by(covid_case_simulation, start_day) %>%
  dplyr::summarize(
    obs = n_distinct(the_id),
    died = sum(died)
  ) %>%
  mutate(
    cum_obs = cumsum(obs),
    Virus = 'COVID-19',
    with_lockdown = 'No'
  )

covid_cases_by_day_lockdown = dplyr::group_by(covid_case_simulation_lockdown, start_day) %>%
  dplyr::summarize(
    obs = n_distinct(the_id),
    died = sum(died)
  ) %>%
  mutate(
    cum_obs = cumsum(obs),
    Virus = 'COVID-19 Early Lockdown',
    with_lockdown = 'Yes'
  )

flu_cases_by_day = dplyr::group_by(flu_case_simulation, start_day) %>%
  dplyr::summarize(
    obs = n_distinct(the_id),
    died = sum(died)
  ) %>%
  mutate(
    cum_obs = cumsum(obs),
    Virus = 'Influenza',
    with_lockdown = 'No'
  )
sum(flu_cases_by_day$died)

stacked_stats = bind_rows(
  covid_cases_by_day, flu_cases_by_day,
  covid_cases_by_day_lockdown) %>%
  mutate(
    week = start_day %/% 7
  )

group_by(stacked_stats, Virus, with_lockdown) %>%
  summarize(
    t_obs = sum(obs),
    pct_infected = t_obs / n_susceptible,
    t_died = sum(died),
    crude_mortality_rate = t_died / n_susceptible,
    infection_fatality_rate = t_died / t_obs,
    time_period = as.numeric(max(start_day)-min(start_day))
  )
weekly_stats = group_by(stacked_stats, week, Virus, with_lockdown) %>%
  summarize(
    weekly_deaths = sum(died),
    weekly_cases = sum(obs)
  )
max(weekly_stats$weekly_deaths * 32.7)

ggplot(stacked_stats, aes(start_day, obs, fill = Virus)) +
  geom_area(alpha = 0.3, position = 'identity')

ggplot(weekly_stats, aes(week, weekly_deaths*32.7, colour = Virus)) +
  # facet_wrap(~Virus, scales = 'free_y') +
  geom_line() +
  geom_point() 
  # geom_bar(stat = 'identity', position = 'identity')



# mGT_weibull <- generation.time("weibull", c(6.4, 2.3))
# mGT_weibull <- generation.time("gamma", c(5, 1.5))
# est.R0.EG(cases_by_day$obs, mGT_weibull, t = cases_by_day$start_day, begin = 0, end = 20)
# 
