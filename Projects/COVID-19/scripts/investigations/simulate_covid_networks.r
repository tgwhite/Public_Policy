library(tidyverse)
library(R0)
library(uuid)
library(igraph)
library(ggnetwork)
library(data.table)

weibull_mean = 6.4
weibull_sd = 2.3

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


initial_susceptible = 10e6
current_susceptible = initial_susceptible
case_link_list = list()
case_info_list = list()

generation = 0
while (TRUE) {
  
  cat('iteration:', generation, '\n')
  gen_start = proc.time()
  
  if (generation == 0) {
    cases_gen_df = tibble(
      generation = generation,
      parent_id = NA, 
      infection_date = 0
    )
    the_id = 0
  }
  
  if (current_susceptible == 0) break
  
  # case_link_list[[length(case_link_list) + 1]] = cases_gen_df
  
  if(nrow(cases_gen_df) == 0) {
    cat('zero new case df!!!')
    break
  } else {
    cat('new cases:', nrow(cases_gen_df), '\n')
    cat('prop susceptible', (current_susceptible / initial_susceptible), '\n')
    cat('id:', the_id, '\n')
  }
  new_cases_list = list() 
  
  
  
  # capture all the new cases from this generation
  inner_case_info = list()
  
  for (case_it in 1:nrow(cases_gen_df)) {
    
    the_case = cases_gen_df[case_it,]
    
    the_id = the_id  
    current_susceptible = current_susceptible - 1
    incubation_time = rweibull(1, weibull_params$par[1], weibull_params$par[2])
    incubation_end_date = incubation_time + the_case$infection_date
    
    prob_running_into_someone_else = current_susceptible / initial_susceptible
    normal_r0 = rweibull(1, r0_params$par[1], r0_params$par[2]) 
    superspreader_r0 = rweibull(1, 1, 5)
    superspreader = rbinom(1, 1, prob = 0.01)
    orig_r0 = ifelse(superspreader, superspreader_r0, normal_r0)
    
    r0_adj = orig_r0 * prob_running_into_someone_else
    
    cases_generated = floor(r0_adj) + rbinom(1, 1, r0_adj - floor(r0_adj))
    
    case_info = list(
      id=the_id,
      parent_id = the_case$parent_id,
      generation = generation,
      incubation_start=the_case$infection_date, 
      incubation_end =incubation_end_date, 
      orig_r0 = orig_r0, 
      r0_adj = r0_adj,
      prob_running_into_someone_else = prob_running_into_someone_else,
      incubation_time=incubation_time, 
      cases_generated = cases_generated) 
    
    inner_case_info[[length(inner_case_info) + 1]] = case_info
    
    case_infection_times = runif(cases_generated, 0, incubation_time)
    case_infection_dates = the_case$infection_date + case_infection_times
    
    the_id = the_id +1
    if (cases_generated > 0) {
      these_new_cases = tibble(
        parent_id = the_id,
        generation = generation + 1,
        infection_date = case_infection_dates,
        case_infection_times = case_infection_times
      ) 
      new_cases_list[[length(new_cases_list) + 1]] = these_new_cases
    }
  }
  case_info_list[[length(case_info_list) + 1]] = bind_rows(inner_case_info)
  
  gen_end = proc.time()
  
  if (length(new_cases_list) > 0) {
    cases_gen_df = bind_rows(new_cases_list)  
  } else {
    cat('new_cases_list is empty')
    break
  }
  print(gen_end - gen_start)
  generation = generation + 1
}

# combine everything 
case_link_df = bind_rows(case_link_list)

case_info_df = bind_rows(case_info_list) %>% arrange(incubation_start, incubation_end) %>%
  mutate(
    start_day = floor(incubation_start),
    end_day = floor(incubation_end)
  )


cases_by_day = group_by(case_info_df, start_day, generation) %>%
  summarize(
    obs = n_distinct(id)
  ) %>%
  mutate(
    cum_obs = cumsum(obs)
  )

ggplot(cases_by_day, aes(start_day, obs, fill = factor(generation))) +
  geom_bar(stat = 'identity', colour = 'black') 

mGT_weibull <- generation.time("weibull", c(6.4, 2.3))
mGT_weibull <- generation.time("gamma", c(5, 1.5))
est.R0.EG(cases_by_day$obs, mGT_weibull, t = cases_by_day$start_day, begin = 0, end = 20)

filter(case_info_df, start_day <= 15) %>% pull(r0_adj) %>% summary()



obs_by_parent = group_by(case_link_df, parent_id) %>%
  summarize(
    cases_generated = n()
  )

sum(case_info_df$cases_generated) - nrow(case_link_df)

ggplot(case_info_df, aes(incubation_end, cases_generated)) +
  geom_point()

head(case_info_df)

x = tibble(id = 1:1e7, val = rnorm(length(id))) %>% data.table()
head(x)
system.time({
  x[, {
    list(val_upd = val + 1)
  }, by = id]
  
})

system.time({
  for (it in 1:1e7) {
    it+1
  }
})

# capture information about each case, store it 
# capture information about case links, store it
