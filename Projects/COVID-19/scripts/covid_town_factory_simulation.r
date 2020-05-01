
library(R0)
library(uuid)
library(igraph)
library(data.table)
library(EpiDynamics)
library(scales)
library(ggraph)
library(tidyverse)
library(cowplot)
setwd("~/Public_Policy/Projects/COVID-19")

# https://www.ijidonline.com/article/S1201-9712(20)30119-3/fulltext
# SD of 4.8 days (95% CrI: 3.8, 6.1) and 2.3 days
serial_interval_mean = 4.8
serial_interval_sd = 2.3

fit_weibull = function(par) {
  the_weibull = rweibull(10000, par[1], par[2])
  abs(mean(the_weibull) - serial_interval_mean) + abs(sd(the_weibull) - serial_interval_sd)
}

si_solution = optim(c(6, 3),fit_weibull)


set.seed(19)

run_case_simulation = function(
  initial_susceptible = 20e6, the_r0, 
  lockdown_date = NULL, 
  lockdown_effect = NULL,
  lockdown_prevalence = NULL
  ) {
  # initial_susceptible = 100
  # the_r0 = 2.5
  this_r0 = the_r0
  current_susceptible = initial_susceptible
  
  case_info_list = list()
  
  generation = 0
  
  cases_gen_df = tibble(
    parent_id = NA,
    si_start = 0
  )
  n_cases_generated = 1
  
  
  continue_loop = T
  
  while (continue_loop) {
    loop_start = proc.time()
    
    # decrement the number of susceptible 
    if (n_cases_generated > current_susceptible) {
      n_cases_generated = current_susceptible
      suseptible_by_id = current_susceptible - 1:n_cases_generated
      cases_gen_df = head(cases_gen_df, n_cases_generated)
    } else {
      suseptible_by_id = current_susceptible - 1:n_cases_generated
    }
    
    if (current_susceptible < 0) {
      break
    }
    
    # increment the ids
    the_id = initial_susceptible - suseptible_by_id
    
    # https://wwwnc.cdc.gov/eid/article/26/6/20-0357_article
    # https://www.ijidonline.com/article/S1201-9712(20)30119-3/pdf
    
    # incubation_time = rnorm(n_cases_generated, 3.96, 4.75)
    # incubation_time = rgamma(n_cases_generated, 4.37,  scale = 1.25) 
    # incubation_time = rweibull(n_cases_generated, weibull_params$par[1], weibull_params$par[2])
    
    # serial_interval = rweibull(n_cases_generated, si_solution$par[1], si_solution$par[2])
    serial_interval = 5
    si_start_date = cases_gen_df$si_start
    si_end_date = si_start_date + serial_interval
    
    # each person has a different probability of running into someone else and infecting them
    # if 99 people have already been infected, the 100th person in a group of 100 has no chance of infecting someone 
    # else
    
    prob_running_into_someone_else = suseptible_by_id / initial_susceptible
    
    if (!is.null(lockdown_date)) {
      lockdown_effects = ifelse(si_start_date >= lockdown_date, (1-lockdown_effect), 1)
      r0_adj = the_r0 * prob_running_into_someone_else * lockdown_effects
    } else if (!is.null(lockdown_prevalence)) {
      # lockdown_effects = ifelse((cumulative_cases/initial_susceptible) >= lockdown_prevalence, (1-lockdown_effect), 1)
      # r0_adj = the_r0 * prob_running_into_someone_else * lockdown_effects
    } else {
      r0_adj = the_r0 * prob_running_into_someone_else
    }
    
    # this simulates "fractions" of new cases and says they are either one or zero based on the fraction percentage
    cases_generated = floor(r0_adj) + rbinom(length(r0_adj), 1, r0_adj - floor(r0_adj))
    n_cases_generated = sum(cases_generated)
    
    # pack up data on the new cases
    these_cases_df = data.table(
      the_id = the_id %>% as.integer(),
      generation = generation,
      parent_id = cases_gen_df$parent_id,
      serial_interval,
      si_start_date,
      si_end_date,
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
        # this assumes uniform infection rate. In reality, people are more infectious later on, but there are 
        # cases of people spreading covid within a short time of infection (12 hours or so)
        si_start = rep(with_gen_cases$si_end_date, length.out = n_cases_generated)
        # incubation_start = runif(n_cases_generated, with_gen_cases$si_start_date, with_gen_cases$si_end_date)
      ) %>% 
        setorder(si_start)
      
      stopifnot(nrow(cases_gen_df) == n_cases_generated)
    }
    
    # print(proc.time() - loop_start)
    generation = generation + 1
    
    current_susceptible = min(suseptible_by_id)
    continue_loop = n_cases_generated > 0 & current_susceptible > 0
  }
  
  return(rbindlist(case_info_list))
}

covid_ifr = 0.005
n_susceptible = 10000000
covid_case_simulation = run_case_simulation(
  lockdown_date = 21,
  lockdown_effect = .5,
  lockdown_prevalence = NULL,
  the_r0 = 3, 
  initial_susceptible = n_susceptible
) %>% 
  mutate(
    start_day = floor(si_start_date),
    died = rbinom(length(the_id), 1, covid_ifr),
    death_date = ifelse(died, start_day + 14, NA)
  ) %>%
  arrange(
    start_day
  )
nrow(covid_case_simulation)

date_df = data.frame(start_day = 0:360)
cases_by_day = group_by(covid_case_simulation, start_day) %>%
  summarize(
    new_cases = n_distinct(the_id)
  ) %>%
  full_join(date_df) %>%
  arrange(start_day) %>%
  mutate(
    new_cases = ifelse(is.na(new_cases), 0, new_cases),
    cumulative_cases = cumsum(new_cases)
  ) 

ggplot(cases_by_day, aes(start_day, new_cases)) +
  geom_bar(stat = 'identity')

ggplot(cases_by_day, aes(start_day, cumulative_cases/n_susceptible)) +
  geom_line() +
  scale_x_continuous(limits = c(0, 360)) +
  scale_y_continuous(labels = percent)



simulations_stacked = map(1:nrow(simulation_params), function(sim_it){
  the_row = simulation_params[sim_it,]
  
  simulation_df = map(1:500, function(it){
    covid_case_simulation = run_case_simulation(
      lockdown_date = the_row$lockdown_date,
      lockdown_effect = the_row$lockdown_effect,
      the_r0 = 2.5, 
      initial_susceptible = 100
      ) %>% 
      
      mutate(
        start_day = floor(si_start_date),
        died = rbinom(length(the_id), 1, covid_ifr),
        death_date = ifelse(died, start_day + 14, NA)
      ) %>%
      arrange(
        start_day
      )
    
    date_df = data.frame(start_day = 0:120)
    cases_by_day = group_by(covid_case_simulation, start_day) %>%
      summarize(
        new_cases = n_distinct(the_id)
      ) %>%
      full_join(date_df) %>%
      arrange(start_day) %>%
      mutate(
        new_cases = ifelse(is.na(new_cases), 0, new_cases),
        cumulative_cases = cumsum(new_cases),
        it = it
      )
  }) %>%
    bind_rows() %>%
    mutate(
      lockdown_date = the_row$lockdown_date,
      lockdown_effect = the_row$lockdown_effect
    )
  
})

simulations_df = bind_rows(simulations_stacked)

stats_by_day = group_by(simulations_df, start_day, lockdown_date, lockdown_effect) %>%
  summarize(
    obs = n(),
    median_cases = median(cumulative_cases),
    sd_cumulative_cases = sd(cumulative_cases),
    q95 = quantile(cumulative_cases, probs = 0.95),
    q5 = quantile(cumulative_cases, probs = 0.05),
    q75 = quantile(cumulative_cases, probs = 0.75),
    q25 = quantile(cumulative_cases, probs = 0.25)
    ) %>%
  mutate(
    action = ifelse(lockdown_effect == 0, 'No Social Distancing', 'Social Distancing at'),
    effect_date = paste0(lockdown_date, ' days, ', percent(lockdown_effect), ' effect'),
    Action_Pretty = ifelse(lockdown_effect == 0, action, paste0(action, ' ', effect_date))
  ) %>% 
  arrange(Action_Pretty, start_day)


write.csv(stats_by_day, 'output/distancing_simulation_stats.csv', row.names = F)

ggplot(stats_by_day, aes(start_day, median_cases,  fill = Action_Pretty)) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    plot.subtitle = element_text(size = 10, face = 'italic')
  ) +
  facet_wrap(~Action_Pretty, ncol = 2) +
  geom_ribbon(aes(ymin = q25, ymax =  q75), alpha = 0.3, size = 0, show.legend = F) +
  geom_line(aes(colour = Action_Pretty), size = 1, show.legend = F) +
  scale_x_continuous(breaks = seq(0, 60, by = 10), limits = c(0, 60)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  labs(
    y = 'Median Cumulative Cases\n25th and 75th Percentiles',
    x = 'Day', 
    title = 'Network Simulations of Covid Infections',
    subtitle = 'Assumptions: closed group of 100, 2.5 initial R0, serial interval 4.8 days'
  )
ggsave('output/simulation_comparison_plot.png', height = 8, width = 8, units = 'in', dpi = 600)

# ggsave('output/cumulative_cases_simulated.png', height = 8, width = 8, units = 'in', dpi = 450)

sample_covid_case_simulation = run_case_simulation(the_r0 = 2.5, 
                                            initial_susceptible = 100) %>% 
  mutate(
    start_day = floor(si_start_date),
    died = rbinom(length(the_id), 1, covid_ifr),
    death_date = ifelse(died, start_day + 14, NA)
  ) %>%
  arrange(
    the_id
  )

the_edgelist = filter(sample_covid_case_simulation, !is.na(parent_id)) %>%
  mutate(
    parent = paste0('_', parent_id),
    child = paste0('_', the_id)
  )

the_graph = igraph::graph_from_edgelist(as.matrix(select(the_edgelist, parent, child)))
V(the_graph)$page_rank_centrality = page_rank(the_graph)$vector
V(the_graph)$v_id = str_extract(V(the_graph)$name, '[0-9]+')
V(the_graph)$generation = sample_covid_case_simulation$generation
V(the_graph)$betweenness_centrality = betweenness(the_graph)
 

network_plot = ggraph(the_graph, 'kk') + 
  geom_edge_link() +
  geom_node_point(aes(size = -generation), colour = 'tomato') +
  scale_size(name='Generation', range = c(3, 7), labels = function(x){-1*x}) +
  geom_node_text(aes(label = v_id, size = -generation), show.legend = F) +
  theme_graph() +
  theme(
    plot.title = element_text(size = 14, face = 'plain')
  ) +
  labs(
    title = 'A Simulated Network of Infections'
  )

ggsave('output/network_example.png', height = 8, width = 10, units = 'in', dpi = 400, plot = network_plot)


full_grid = plot_grid(cumulative_case_plot, network_plot, nrow = 2)
full_grid_sub = add_sub(full_grid, "Chart: Taylor G. White", 
                        x = 0.05, y = 0.35, hjust = 0, size = 10, fontface = 'italic')

save_plot('output/combined_network_analysis.png', base_width = 12, base_height = 14, dpi = 600, plot = full_grid_sub)



