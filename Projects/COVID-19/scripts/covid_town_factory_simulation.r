
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

set.seed(19)

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
    
    # right now, incubation time is really the infectious period and it's hard coded for five days
    # other options are available for this
    incubation_time = 5
    incubation_start_date = cases_gen_df$incubation_start
    incubation_end_date = incubation_start_date + incubation_time 
    
    # each person has a different probability of running into someone else and infecting them
    # if 99 people have already been infected, the 100th person in a group of 100 has no chance of infecting someone 
    # else
    
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
    
    # this simulates "fractions" of new cases and says they are either one or zero based on the fraction percentage
    cases_generated = floor(r0_adj) + rbinom(length(r0_adj), 1, r0_adj - floor(r0_adj))
    n_cases_generated = sum(cases_generated)
    
    # pack up data on the new cases
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
        # this assumes uniform infection rate. In reality, people are more infectious later on, but there are 
        # cases of people spreading covid within a short time of infection (12 hours or so)
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

covid_ifr = 0.005


simulation_list = map(1:500, function(it){
  covid_case_simulation = run_case_simulation(r0_type = 'covid', the_r0 = 2.5, 
                                              initial_susceptible = 100) %>% 
    mutate(
      start_day = floor(incubation_start_date),
      died = rbinom(length(the_id), 1, covid_ifr),
      death_date = ifelse(died, start_day + 14, NA)
    ) %>%
    arrange(
      start_day
    )
  
  cases_by_day = group_by(covid_case_simulation, start_day) %>%
    summarize(
      new_cases = n_distinct(the_id)
    ) %>%
    mutate(
      cumulative_cases = cumsum(new_cases),
      it = it
    )
})
simulations_stacked = bind_rows(simulation_list)
avg_by_day = group_by(simulations_stacked, start_day) %>%
  summarize(mean_cumulative_cases = mean(cumulative_cases))

cumulative_case_plot = ggplot(simulations_stacked, aes(start_day, cumulative_cases)) +
  geom_line(aes(group = it), alpha = 0.3) +
  geom_line(data = avg_by_day, aes(y = mean_cumulative_cases), size = 1.25, colour = 'tomato') +
  theme_bw() +
  theme(
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 10, face = 'italic'),
    plot.caption = element_text(size = 10, face = 'italic', hjust = 0)
  ) +
  labs(
    x = 'Infection Day',
    y = 'Cumulative Cases',
    title = 'Simulated Cumulative COVID-19 Cases for a Closed Group of 100',
    subtitle = 'Assumptions: 2.5 initial R0, 5-day infectious period, unmitigated spread, uniform infection rate'
  )

# ggsave('output/cumulative_cases_simulated.png', height = 8, width = 8, units = 'in', dpi = 450)

sample_covid_case_simulation = run_case_simulation(r0_type = 'covid', the_r0 = 2.5, 
                                            initial_susceptible = 100) %>% 
  mutate(
    start_day = floor(incubation_start_date),
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



