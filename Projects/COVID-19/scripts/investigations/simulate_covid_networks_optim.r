library(tidyverse)
library(R0)
library(uuid)
library(igraph)
library(ggnetwork)
library(data.table)

weibull_mean = 6.4
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
    the_sd * the_mean
  )
}
weibull_params = optim(par = c(3, 7), fn = fit_weibull)

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


run_case_simulation = function(initial_susceptible = 20e6, the_r0) {
  
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
    
    print(generation)
    
    if (n_cases_generated > current_susceptible) {
      cat('too many cases generated!\n', n_cases_generated, '\n')
      n_cases_generated = current_susceptible
      suseptible_by_id = current_susceptible - 1:n_cases_generated
      cat('n_cases_generated is now', n_cases_generated, '\n')
      cases_gen_df = head(cases_gen_df, n_cases_generated)
    } else {
      suseptible_by_id = current_susceptible - 1:n_cases_generated
    }
    cat('n_cases_generated:', n_cases_generated, '\n')
    cat('current_susceptible:', current_susceptible, '\n')
    
    
    # suseptible_by_id = suseptible_by_id[suseptible_by_id > 0]
    
    if (current_susceptible < 0) {
      cat('no more susceptible')
      break
    }
    
    the_id = initial_susceptible - suseptible_by_id
    
    incubation_time = rweibull(n_cases_generated, weibull_params$par[1], weibull_params$par[2])
    incubation_start_date = cases_gen_df$incubation_start
    incubation_end_date = incubation_start_date + incubation_time 
    prob_running_into_someone_else = suseptible_by_id / initial_susceptible
    
    r0_adj = the_r0 * prob_running_into_someone_else
    
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
    
    print(proc.time() - loop_start)
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

covid_case_simulation = run_case_simulation(the_r0 = 2.5) %>% 
  mutate(start_day = floor(incubation_start_date))
flu_case_simulation = run_case_simulation(the_r0 = 1.3) %>% 
  mutate(start_day = floor(incubation_start_date))


covid_cases_by_day = dplyr::group_by(covid_case_simulation, start_day, generation) %>%
  dplyr::summarize(
    obs = n_distinct(the_id)
  ) %>%
  mutate(
    cum_obs = cumsum(obs),
    Virus = 'COVID-19'
  )

flu_cases_by_day = dplyr::group_by(flu_case_simulation, start_day, generation) %>%
  dplyr::summarize(
    obs = n_distinct(the_id)
  ) %>%
  mutate(
    cum_obs = cumsum(obs),
    Virus = 'Influenza'
  )

stacked_stats = bind_rows(covid_cases_by_day, flu_cases_by_day)
ggplot(stacked_stats, aes(start_day, obs, fill = Virus)) +
  geom_bar(stat = 'identity', alpha = 0.3)

ggplot(cases_by_day, aes(start_day, obs, fill = factor(generation))) +
  geom_bar(stat = 'identity', colour = 'black') 
# 
# mGT_weibull <- generation.time("weibull", c(6.4, 2.3))
# mGT_weibull <- generation.time("gamma", c(5, 1.5))
# est.R0.EG(cases_by_day$obs, mGT_weibull, t = cases_by_day$start_day, begin = 0, end = 20)
# 
# filter(case_info_df, start_day <= 15) %>% pull(r0_adj) %>% summary()
# 
# 
# 
# obs_by_parent = group_by(case_link_df, parent_id) %>%
#   summarize(
#     cases_generated = n()
#   )
# 
# sum(case_info_df$cases_generated) - nrow(case_link_df)
# 
# ggplot(case_info_df, aes(incubation_end, cases_generated)) +
#   geom_point()
# 
# head(case_info_df)
# 
# x = tibble(id = 1:1e7, val = rnorm(length(id))) %>% data.table()
# head(x)
# system.time({
#   x[, {
#     list(val_upd = val + 1)
#   }, by = id]
#   
# })
# 
# system.time({
#   for (it in 1:1e7) {
#     it+1
#   }
# })
# 
# # capture information about each case, store it 
# # capture information about case links, store it
