fredr_set_key('d0b9e64aba30b479343a06037a5a10c1')

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

##### Data import and cleanup #####
stacked_oecd_wdi_data_lags_diffs = read.csv('data/stacked_oecd_wdi_data_lags_diffs.csv')
wide_oecd_wdi_data = read.csv('data/wide_oecd_wdi_data.csv')
concurrence_with_president_clean = read_csv('data/concurrence_with_president_clean.csv')

brookings_house_senate_representation = read_csv('data/brookings congressional stats/1-20.csv',
                                                 na=c('', 'NA', '.')
                                                 ) %>%
  separate(Years, sep = '[ ]*[-]{1}[ ]*', into = c('start', 'end'), convert = T) %>%
  mutate(
    congress_start = (Congress - min(Congress)) * 2 + min(start),
    congress_end = congress_start + 1
  ) %>%
  data.table() 

brookings_house_senate_representation_stats_by_congress = brookings_house_senate_representation[, {
  dem_seats = Seats[PartyStatus == 'Democrat']
  rep_seats = Seats[PartyStatus == 'Republican']
  other_seats = Seats[PartyStatus == 'Other']
  vacant_seats = Seats[PartyStatus == 'Vacant']
  total_seats = Seats[PartyStatus == 'All']
  
  dem_rep_diff = dem_seats - rep_seats
  
  out_tab = tibble(
    Year = congress_start:congress_end, 
    congress_start = congress_start[1],
    dem_seats = dem_seats, rep_seats = rep_seats, 
    other_seats = other_seats, 
    vacant_seats = vacant_seats,
    total_seats = total_seats,
    dem_rep_diff = dem_rep_diff) %>%
    as.data.frame()
  out_tab$obs = nrow(out_tab)
  out_tab
}, by = list(Congress, Chamber)]

wide_brookings_house_senate_representation_stats_by_congress = pivot_wider(
  brookings_house_senate_representation_stats_by_congress,
  id_cols = 'Year', names_from = 'Chamber', values_from = 'dem_rep_diff'
)


ggplot(brookings_house_senate_representation_stats_by_congress, aes(congress_start, dem_rep_diff)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~Chamber, ncol = 1, scales = 'free_y')

# get additional data from FRED
us_real_gdp_per_capita = fredr('A939RX0Q048SBEA', aggregation_method = 'eop', frequency = 'a', units = 'pch') %>%
  rename(value_real_per_capita_gdp_growth = value) %>%
  mutate(
    Year = year(date),
    lag_value_real_per_capita_gdp_growth = dplyr::lag(value_real_per_capita_gdp_growth, 1)
  ) %>% 
  select(-date, -series_id)

recession_years = fredr('JHDUSRGDPBR', aggregation_method = 'sum', frequency = 'a') %>%
  rename(
    n_recession_quarters = value
  ) %>% 
  mutate(
    Year = year(date),
    pct_of_year_in_recession = n_recession_quarters / 4,
    recession_year = n_recession_quarters > 0
  ) %>%
  select(-date, -series_id)


US_wide = filter(wide_oecd_wdi_data, Country == 'United States') %>%
  left_join(concurrence_with_president_clean) %>%
  left_join(us_real_gdp_per_capita) %>%
  left_join(recession_years) %>%
  inner_join(wide_brookings_house_senate_representation_stats_by_congress) %>%
  mutate(
    real_gdp_per_capita_z = (value_real_per_capita_gdp_growth - mean(value_real_per_capita_gdp_growth)) / sd(value_real_per_capita_gdp_growth),
    lag_real_gdp_per_capita_z = dplyr::lag(real_gdp_per_capita_z, 1),
    dem_congress = House > 0 & Senate > 0,
    unified_congress = sign(House * Senate) == 1,
    unified_government = (dem_congress & unified_congress & president_party == 'DEM') | 
      (!dem_congress & unified_congress & president_party == 'REP')
  )

US_long = filter(stacked_oecd_wdi_data_lags_diffs, Country == 'United States')

##### Examine US data #####
# ggplot(US_wide, aes(Year)) +
#   geom_line(aes(y = diff_value_TAXINCOME), size = 1)  +
#   geom_line(aes(y = value_real_per_capita_gdp_growth), colour = 'red') +
#   geom_line(aes(y = diff_value_top_tax_rate), colour = 'blue') +
#   geom_line(aes(y = diff_value_GGDEBT), colour = 'orange') +
#   geom_line(aes(y = diff_value_GC.DOD.TOTL.GD.ZS), linetype = 'dashed', colour = 'purple4') +
#   geom_line(aes(y = -value_GGNLEND), linetype = 'dotted', size = 1.5, colour = 'steelblue') +
#   geom_line(aes(y = total_hs_concurrence), size = 2)


##### Model YOY differences in net lending (budget deficits) #####
options(na.action = na.exclude)  

reg_dat = select(US_wide, Year,  
                 president_party, 
                 dem_congress, 
                 unified_congress, 
                 unified_government, 
                 # house_majority, senate_majority,
                 pct_of_year_in_recession, 
                 recession_year, 
                 contains("GGNLEND"), 
                 contains('gdp_per_capita'),
                 contains('top_tax_rate'), 
                 contains('GGEXP'), contains('GGREV')
                 ) %>% na.omit()

ggplot(reg_dat, aes(diff_value_GGNLEND, fill = president_party)) +
  stat_density(position = 'identity', alpha = 0.4)

ggplot(reg_dat, aes(lag_real_gdp_per_capita_z, diff_value_GGNLEND)) +
  geom_point() +
  stat_smooth(method = 'lm')

all_model = lm(diff_value_GGNLEND ~ ., data = reg_dat)
step_model = step(all_model, direction = 'both')

bloated_model = lm(diff_value_GGNLEND ~ unified_government * president_party + 
                     real_gdp_per_capita_z + 
                     last_value_GGNLEND + 
                     lag_real_gdp_per_capita_z, data = reg_dat)

bloated_model = lm(diff_value_GGNLEND ~ 
                     president_party*real_gdp_per_capita_z*lag_real_gdp_per_capita_z +
                     real_gdp_per_capita_z + last_value_GGNLEND, data = reg_dat)
plot(bloated_model)
plot(residuals(bloated_model))
hist(residuals(bloated_model))
summary(bloated_model)
a = anova(bloated_model)
a$r_square = a$`Sum Sq` / sum(a$`Sum Sq`)
summary(bloated_model)
arrange(a, -r_square)

reg_dat$predicted = predict(bloated_model)

ggplot(reg_dat, aes(predicted, diff_value_GGNLEND, colour = president_party)) +
  geom_point() +
  stat_smooth(method = 'lm')

test_dat = expand.grid(
  president_party = c('DEM', 'REP'),
  last_value_GGNLEND = mean(reg_dat$last_value_GGNLEND),
  lag_real_gdp_per_capita_z = mean(reg_dat$lag_real_gdp_per_capita_z),
  real_gdp_per_capita_z = reg_dat$real_gdp_per_capita_z
)
test_dat$predicted_diff = predict(bloated_model, newdata = test_dat)

ggplot(test_dat, aes(real_gdp_per_capita_z, predicted_diff, colour = president_party)) +
  geom_point()

ggplot(test_dat, aes(lag_real_gdp_per_capita_z, predicted_diff, colour = president_party)) +
  geom_point()

plot(bloated_model)
a = anova(bloated_model)
a
a$`Sum Sq` / sum(a$`Sum Sq`)

bloated_model_noint = lm(diff_value_GGNLEND ~ dem_congress + unified_government +  president_party + 
                     real_gdp_per_capita_z + 
                     last_value_GGNLEND + 
                     lag_real_gdp_per_capita_z, data = reg_dat)
summary(bloated_model_noint)

filter(reg_dat, unified_government & president_party == 'REP')

summary(bloated_model)
pres_only = lm(diff_value_GGNLEND ~ president_party, data = reg_dat)

deficit_model = lm(diff_value_GGNLEND ~ real_gdp_per_capita_z + lag_real_gdp_per_capita_z, data = reg_dat)
deficit_model_full = lm(diff_value_GGNLEND ~ real_gdp_per_capita_z + 
                          lag_real_gdp_per_capita_z + last_value_GGNLEND, data = reg_dat)
deficit_model_pres = lm(diff_value_GGNLEND ~ real_gdp_per_capita_z + 
                          lag_real_gdp_per_capita_z + last_value_GGNLEND + president_party, data = reg_dat)


summary(deficit_model_pres)$coefficients
bootstrapped_model = lapply(1:1000, function(it){
  sample_dat = sample_n(reg_dat, nrow(reg_dat), replace = T)
  the_model = lm(diff_value_GGNLEND ~ real_gdp_per_capita_z + 
                   lag_real_gdp_per_capita_z + last_value_GGNLEND + president_party, data = sample_dat)
  the_summary= summary(the_model)$coefficients %>% as.data.frame() 
  the_summary = mutate(the_summary, 
                       it = it, variable = row.names(the_summary),
                       includes_outliers = any(outlier_years %in% sample_dat$Year)
  )
  
  return(the_summary)
}) %>%
  bind_rows()

group_by(bootstrapped_model, variable, includes_outliers) %>%
  summarize(
    mean_est = mean(Estimate),
    sd_est = sd(Estimate)
  )


deficit_model_pres_rq = rq(diff_value_GGNLEND ~ real_gdp_per_capita_z + 
                             lag_real_gdp_per_capita_z + last_value_GGNLEND + president_party, 
                           data = reg_dat, tau = 0.75)
deficit_model_pres_rq = rq(diff_value_GGNLEND ~ real_gdp_per_capita_z + 
                             lag_real_gdp_per_capita_z + last_value_GGNLEND + president_party, 
                           data = reg_dat, tau = 0.25)
deficit_model_pres_rq = rq(diff_value_GGNLEND ~ real_gdp_per_capita_z + 
                             lag_real_gdp_per_capita_z + last_value_GGNLEND + president_party, 
                           data = reg_dat, tau = 0.5)
summary(deficit_model_pres_rq, se = 'iid')

deficit_model_lag =  lm(diff_value_GGNLEND ~  
                          lag_real_gdp_per_capita_z + last_value_GGNLEND + president_party, data = reg_dat)
summary(deficit_model_lag)

growth_model = lm(real_gdp_per_capita_z ~ president_party, data = reg_dat)
summary(growth_model)
summary(deficit_model_pres)

summary(deficit_model_full)
reg_dat$full_model_residuals = residuals(deficit_model_full)
reg_dat$full_model_predicted = predict(deficit_model_full)

ggplot(US_wide, aes(last_value_GGNLEND, diff_value_GGNLEND)) +
  geom_point() +
  stat_smooth() +
  stat_smooth(method = 'lm')

anova(deficit_model_pres, deficit_model_full)
a = anova(deficit_model_pres)
a
a$`Sum Sq` / sum(a$`Sum Sq`)

ggplot(reg_dat, aes(Year, -diff_value_GGNLEND, fill = president_party)) +
  geom_hline(aes(yintercept = 0), size = 1, linetype = 'dashed') +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = c('DEM'='#00aef3', 'REP' = '#d8171e')) +
  labs(
    y = 'Changes to Budget Deficits\n',
    x = ''
  )

ggplot(reg_dat, aes(Year, -full_model_residuals, fill = president_party)) +
  geom_hline(aes(yintercept = 0), size = 1, linetype = 'dashed') +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = c('DEM'='#00aef3', 'REP' = '#d8171e')) +
  labs(
    y = 'Changes to Budget Deficits\nIndependent of Economic Conditions\n(% of GDP)',
    x = ''
  )


ggplot(reg_dat, aes(full_model_predicted, diff_value_GGNLEND)) +
  geom_point() +
  stat_smooth() +
  stat_smooth(method = 'lm')


ggplot(reg_dat, aes(full_model_residuals, fill = president_party)) +
  stat_density(position = 'identity', alpha = 0.4)

deficit_model$coefficients['president_partyREP']
summary(deficit_model)
plot(deficit_model)

hist(deficit_model$residuals)
plot(predict(deficit_model), -US_wide$diff_value_GGNLEND)

ggplot(US_wide, aes(real_gdp_per_capita_z, -diff_value_GGNLEND, colour = president_party)) +
  geom_hline(aes(yintercept = 0), linetype = 'dashed', size = 1) +
  geom_vline(aes(xintercept = 0), linetype = 'dashed', size = 1) +
  geom_point(aes(shape = recession_year, size = pct_of_year_in_recession)) +
  stat_smooth(method = 'lm', se = F) +
  scale_colour_manual(
    name = "President's Party",
    values = c('DEM' = 'blue', 'REP' = 'red')
  ) +
  labs(
    title = 'Economic Growth and Budget Deficits by Presidential Party\n1971-2018',
    subtitle = sprintf('Republicans add %s more debt each year than Democrats', 
                       percent(deficit_model$coefficients['president_partyREP']/100, accuracy = 0.01)),
    y = 'Annual Deficit (% of GDP)\n',
    x = '\nReal GDP Per Capita Growth\nStandard Deviations (Z Value)',
    caption = 'Chart: Taylor G. White\nData: OECD, St. Louis Federal Reserve'
  ) +
  scale_size(
    guide = F,
    range = c(3, 8)
  ) +
  scale_shape(name = 'Recession Year', na.translate = FALSE) +
  geom_text(aes(x = 1, y = -3, label = '(4) Strong Growth\nBudget Surplus'), colour = 'black', hjust=0) +
  geom_text(aes(x = -2, y = -3, label = '(3) Poor Growth\nBudget Surplus'), colour = 'black', hjust=0) +
  geom_text(aes(x = 1, y = 7, label = '(1) Strong Growth\nBudget Deficit'), colour = 'black', hjust=0) +
  geom_text(aes(x = -2, y = 7, label = '(2) Poor Growth\nBudget Deficit'), colour = 'black', hjust=0) 

ggsave('output/deficits_vs_economic_growth_by_party.png', height = 7, width = 7.5, units = 'in', dpi = 600)

