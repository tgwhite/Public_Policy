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
library(xtable)
library(stargazer)

caption_text = 'Chart: Taylor G. White\nData: OECD, FRED, WDI'

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

# check the difference in representation over time
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

# join everything together
US_wide = filter(wide_oecd_wdi_data, Country == 'United States') %>%
  left_join(concurrence_with_president_clean) %>%
  left_join(us_real_gdp_per_capita) %>%
  left_join(recession_years) %>%
  # inner join because there are way more years here
  inner_join(wide_brookings_house_senate_representation_stats_by_congress) %>% 
  mutate(
    real_gdp_per_capita_z = (value_real_per_capita_gdp_growth - mean(value_real_per_capita_gdp_growth)) / sd(value_real_per_capita_gdp_growth),
    lag_real_gdp_per_capita_z = dplyr::lag(real_gdp_per_capita_z, 1),
    z_value_NY.GDP.PCAP.KD.ZG = (value_NY.GDP.PCAP.KD.ZG - mean(value_NY.GDP.PCAP.KD.ZG, na.rm=T)) / sd(value_NY.GDP.PCAP.KD.ZG, na.rm =T),
    lag_z_value_NY.GDP.PCAP.KD.ZG = dplyr::lag(z_value_NY.GDP.PCAP.KD.ZG, 1),
    dem_congress = House > 0 & Senate > 0,
    unified_congress = sign(House * Senate) == 1,
    unified_government = (dem_congress & unified_congress & president_party == 'DEM') | 
      (!dem_congress & unified_congress & president_party == 'REP'),
    tax_cut = diff_value_top_tax_rate < 0,
    tax_increase = diff_value_top_tax_rate > 0
  )

filter(US_wide, diff_value_top_tax_rate != 0) %>% select(Year, President, diff_value_top_tax_rate, value_top_tax_rate) %>% View()

US_long = filter(stacked_oecd_wdi_data_lags_diffs, Country == 'United States')

save(US_wide, US_long, file = 'data/US_political_economic_data.rdata')

##### get clean dataset to model YOY differences in net lending (budget deficits) #####
options(na.action = na.exclude)  

reg_dat = select(US_wide, Year,  
                 president_party, 
                 dem_congress, 
                 tax_cut, tax_increase,
                 unified_congress, 
                 unified_government, 
                 # house_majority, senate_majority,
                 pct_of_year_in_recession, 
                 recession_year, 
                 contains("GGNLEND"), 
                 value_real_per_capita_gdp_growth, # real gdp per capita growth
                 lag_value_real_per_capita_gdp_growth,
                 contains('NY.GDP'),
                 # value_NY.GDP.PCAP.KD.ZG, # gdp per capita growth
                 # value_NY.GDP.MKTP.KD.ZG, # gdp growth
                 contains('gdp_per_capita'),
                 contains('top_tax_rate'), 
                 contains('GGEXP'), contains('GGREV')
                 ) %>% na.omit()



#### get correlation matrix #####
reg_dat_numeric = mutate_if(reg_dat, is.logical, as.numeric) %>%
  mutate(
    president_dem = (president_party == 'DEM') %>% as.numeric()
  ) %>%
  select(-president_party)


correlation_mat = cor(reg_dat_numeric) 
write.csv(correlation_mat, 'output/regression_cor_matrix.csv')

percents = correlation_mat[,'diff_value_GGNLEND'] %>% sort(decreasing = T) %>% percent(accuracy = 0.01)
names(percents) = names(correlation_mat[,'diff_value_GGNLEND'] %>% sort(decreasing = T))

correlation_mat[,'lag_value_real_per_capita_gdp_growth']
variable_mappings = c(
  'dem_congress' = 'Democratic Congress',
  'unified_congress' = 'Unified Congress',
  'unified_government' = 'United Government',
  'president_dem' = 'Democratic President',
  'recession_year' = 'Recession Year',
  'last_value_GGNLEND' = "Last Year's Deficit",
  'value_real_per_capita_gdp_growth' = 'Real Per Capita GDP Growth',
  'lag_value_real_per_capita_gdp_growth' = 'Real Per Capita GDP Growth, Last Year',
  'value_NY.GDP.PCAP.KD.ZG' = 'GDP Per Capita Growth',
  'last_value_NY.GDP.PCAP.KD.ZG' = 'GDP Per Capita Growth, Last Year',
  'diff_value_GGREV' = 'Change in Revenues/GDP',
  'diff_value_GGEXP' = 'Change in Expenditures/GDP',
  'lag_diff_value_GGEXP' = 'Change in Expenditures/GDP, Last Year',
  'lag_diff_value_GGREV' = 'Change in Revenues/GDP, Last Year',
  'diff_value_top_tax_rate' = 'Change in Top Marginal Tax Rate',
  'tax_cut' = 'Tax Cut',
  'tax_increase' = 'Tax Increase'
  )

correlation_df = 
  tibble(
    correlation_to_net_lending = correlation_mat[names(variable_mappings),'diff_value_GGNLEND'],
    positive_correlation = correlation_to_net_lending > 0,
    pretty_variable = variable_mappings
    ) %>%
  arrange(-correlation_to_net_lending) %>%
  mutate(
    pretty_variable = factor(str_wrap(pretty_variable, 24), levels = str_wrap(pretty_variable, 24))
  )

range(reg_dat$Year)
ggplot(correlation_df, aes(pretty_variable, correlation_to_net_lending*-1, fill = positive_correlation)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = percent(correlation_to_net_lending*-1)), hjust = 0) +
  scale_fill_manual(guide = F, values = c('TRUE' = 'steelblue', 'FALSE' = 'firebrick')) + 
  coord_flip() +
  labs(
    y = 'Correlation', x = '',
    title = 'Correlation to Changes to Annual Deficits',
    subtitle = 'U.S. 1977-2018',
    caption = caption_text
  ) +
  scale_y_continuous(labels = percent) +
  theme(axis.text = element_text(size = 14), title = element_text(size = 16), axis.title = element_text(size = 16))


ggsave('output/correlations_to_the_deficit.png', height = 8.5, width = 8.75, units = 'in', dpi = 600)

##### Fit models for difference in net lending #####

# confirm that net lending is really an accounting identity between general expenditures and revenues

deficit_change_model = lm(diff_value_GGNLEND ~ diff_value_GGEXP + diff_value_GGREV, data = reg_dat)


# fit several models

base_model = lm(diff_value_GGNLEND ~ last_value_GGNLEND + 
                  z_value_NY.GDP.PCAP.KD.ZG + lag_z_value_NY.GDP.PCAP.KD.ZG, data = reg_dat)

base_model_president = lm(diff_value_GGNLEND ~ last_value_GGNLEND + 
                            president_party +
                            z_value_NY.GDP.PCAP.KD.ZG + 
                            lag_z_value_NY.GDP.PCAP.KD.ZG, data = reg_dat)

interaction_model = lm(diff_value_GGNLEND ~ 
                         last_value_GGNLEND + 
                         z_value_NY.GDP.PCAP.KD.ZG*lag_z_value_NY.GDP.PCAP.KD.ZG, 
                       data = reg_dat)

interaction_model_president = lm(diff_value_GGNLEND ~ 
                                   last_value_GGNLEND + 
                                   president_party*z_value_NY.GDP.PCAP.KD.ZG*lag_z_value_NY.GDP.PCAP.KD.ZG, 
                                 data = reg_dat)

# there are outliers in the data -- a median regression is less sensitive to those
interaction_model_president_median = rq(diff_value_GGNLEND ~ 
                                          last_value_GGNLEND + 
                                          president_party*z_value_NY.GDP.PCAP.KD.ZG*lag_z_value_NY.GDP.PCAP.KD.ZG, 
                                        data = reg_dat, tau = 0.5)

summary(interaction_model_president)
summary(interaction_model_president_median, se = 'boot')


# the most important coefficients are very similar between the models
coef_comparison_table = data.frame(
  ols = interaction_model_president$coefficients,
  median = interaction_model_president_median$coefficients
)

##### check and compare the model results #####

# adding president to the base model is an improvement
# the interaction model w/o president is only a slight improvement over the base + president
# interaction with president is the superior model
full_anova = anova(base_model, base_model_president, 
                   interaction_model, interaction_model_president)

# get the variance explained by each term
interaction_model_president_anova = anova(interaction_model_president)
interaction_model_president_anova$`R Squared` =  interaction_model_president_anova$`Sum Sq` / sum(interaction_model_president_anova$`Sum Sq`)

# check for heteroskedacity - p value is greater than 0.05 so we're good
bptest(diff_value_GGNLEND ~ 
         last_value_GGNLEND + 
         president_party*real_gdp_per_capita_z*lag_real_gdp_per_capita_z, 
       data = reg_dat, studentize=F)


# regression analysis plots
par(mfrow=c(2,2))
plot(interaction_model_president, which = 2)
plot(interaction_model_president, which = 1)
plot(interaction_model_president, which = 5)
hist(residuals(interaction_model_president), main = 'Histogram of Residuals', xlab = 'Residuals')

# residuals aren't perfect but they're pretty good
# there are influential points but hard to argue getting rid of them




# president party and related terms explain significant variation

#### plot the components of deficits ####
long_budget_components = pivot_longer(reg_dat,  
                                      cols = c('diff_value_GGREV', 'diff_value_GGEXP')) %>%
  mutate(
    # adjust expenditures to have same directionality as net lending and revenues
    value = ifelse(name == 'diff_value_GGEXP', value * -1, value)
  )

president_starts_stops = group_by(US_wide, President, president_party) %>%
  summarize(
    start_year = min(Year), end_year = max(Year)
  ) %>%
  filter(start_year >= min(long_budget_components$Year)) %>%
  mutate(
    midpoint = (start_year + end_year)/2,
    pres_last_name = str_extract(President, '([ ]{1}[A-Za-z]+)$') %>% str_trim()
  )


ggplot(long_budget_components, aes(Year, value)) +
  geom_rect(data = president_starts_stops, aes(xmin = start_year, xmax = end_year, 
                                               x = NULL,  y = NULL, ymin = -6, ymax = 4, 
                                               colour = president_party, fill = president_party), 
            stat = 'identity', alpha = 0.3, guide = F) +
  scale_fill_manual(
    name = '',
    values = c('diff_value_GGREV' = 'steelblue', 'diff_value_GGEXP' = 'orange', 'DEM' = '#00aef3', 'REP' = '#d8171e'),
    labels = c('diff_value_GGREV' = 'Revenue/GDP', 'diff_value_GGEXP' = 'Expenditure/GDP')
  ) +
  geom_bar(aes(fill= name), stat = 'identity', colour = 'black') +
  geom_point(data = reg_dat, aes(Year, diff_value_GGNLEND), size = 2.5, shape = 18) +
  labs(
    y = 'Change from Prior Year (% of GDP)\n',
    x = '',
    title = 'Contributions to Changes in Budget Deficits\nU.S. 1977-2018',
    caption = caption_text,
    subtitle = 'Points Show the Deficit Change for the Year'
  ) +
  scale_colour_manual(guide = F, values = c('DEM'='#00aef3', 'REP' = '#d8171e')) +
  scale_x_continuous(breaks = seq(1977, 2018, by = 4)) +
  geom_text(data = president_starts_stops, 
            aes(y = 4.5, x = midpoint, label = pres_last_name, colour = president_party), hjust = 0.5, size = 4.5) +
  # geom_segment(data = president_starts_stops, aes(y = 4, yend = 4, x = start_year, xend = end_year)) +
  
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(angle = 0),
    title = element_text(size = 20),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18),
    legend.text = element_text(size = 14)
  ) +
  
  geom_segment(
    aes(x = 1976, xend = 1976, y = 1.5, yend = 3), 
    lineend = 'butt', linejoin = 'mitre',
    size = 1, arrow = arrow(length = unit(0.1, "inches"))
  ) +
  geom_segment(
    aes(x = 1976, xend = 1976, y = -1.5, yend = -3), 
    lineend = 'butt', linejoin = 'mitre',
    size = 1, arrow = arrow(length = unit(0.1, "inches"))
  ) +
  geom_text(
    aes(x = 1975, y = 2.5, label = 'Decreases Deficit'), angle = 90, hjust = 0.5, size = 4.5
  ) +
  geom_text(
    aes(x = 1975, y = -2.5, label = 'Increases Deficit'), angle = 90, hjust = 0.5, size = 4.5
  )
ggsave('output/contributions_to_deficits.png', height = 8, width = 10, units = 'in', dpi = 600)


##### find contributions to deficits after controlling for economic conditions #####

# imagine if all presidents were democratic
reg_dat_dems = mutate(reg_dat, president_party = 'DEM') 

# if all presidents were republican
reg_dat_reps = mutate(reg_dat, president_party = 'REP') 

# find the difference between the democratic and republican predictions
reg_dat = mutate(
  reg_dat,
  predicted_dem_diff_GGNLEND = predict(interaction_model_president, newdata = reg_dat_dems),
  predicted_rep_diff_GGNLEND = predict(interaction_model_president, newdata = reg_dat_reps),
  rep_dem_diff_GGNLEND = predicted_rep_diff_GGNLEND - predicted_dem_diff_GGNLEND,
  predicted_diff_GGNLEND = predict(interaction_model_president)
)

dem_rep_diff = pivot_longer(reg_dat, cols = c('predicted_dem_diff_GGNLEND', 'predicted_rep_diff_GGNLEND'))

ggplot(reg_dat, aes(Year, -rep_dem_diff_GGNLEND)) +
  geom_bar(aes(fill = recession_year), stat = 'identity', colour = 'black') +
  scale_fill_manual(name = 'Recession Year', values = c('TRUE' = 'firebrick', 'FALSE' = 'steelblue')) +
  geom_segment(
    aes(x = 1976, xend = 1976, y = 0.5, yend = 2), 
    lineend = 'butt', linejoin = 'mitre',
    size = 1, arrow = arrow(length = unit(0.1, "inches"))
  ) +
  geom_segment(
    aes(x = 1976, xend = 1976, y = -0.5, yend = -2), 
    lineend = 'butt', linejoin = 'mitre',
    size = 1, arrow = arrow(length = unit(0.1, "inches"))
  ) +
  geom_text(
    aes(x = 1975, y = 0.5, label = 'Rep Increase'), angle = 90, hjust = 0, size = 4.5
  ) +
  geom_text(
    aes(x = 1975, y = -0.5, label = 'Dem Increase'), angle = 90, hjust = 1, size = 4.5
  ) +
  theme(
    axis.text.x = element_text(angle = 45),
    legend.position = 'bottom',
      title = element_text(size = 16),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18),
    legend.text = element_text(size = 14)
  ) +
  
  labs(
    title = 'Predicted Difference, Democratic and Republican Deficit Changes',
    subtitle = 'U.S. 1977-2018',
    x = '',
    y = 'Predicted Difference in Deficit Changes (% of GDP)',
    caption = caption_text
  ) +
  scale_x_continuous(breaks = seq(1977, 2018, by = 2)) 

ggsave('output/dem_rep_difference_deficits.png', height = 8, width = 10, units = 'in', dpi = 600)  


#### plot model predictions ####
ggplot(reg_dat, aes(-predicted_diff_GGNLEND, -diff_value_GGNLEND)) +
  geom_point(aes(colour = president_party)) +
  stat_smooth(method = 'lm', colour = 'black') +
  labs(
    x = 'Predicted Deficit Change (% of GDP)', 
    y = 'Actual Deficit Change (% of GDP)',
    title = 'Predicted vs. Actual Deficit Changes',
    caption = caption_text
  ) +
  scale_colour_manual(name = 'President Party', values = c('DEM'='#00aef3', 'REP' = '#d8171e')) +
  geom_text(aes(x = -2, y = 5.5, 
                label = paste0('R Squared: ', summary(interaction_model_president)$r.squared %>% round(2))),
            hjust = 0) +
  geom_text(aes(x = -2, y = 4.75, 
                label = paste0('R Squared Adj.: ', summary(interaction_model_president)$adj.r.squared %>% round(2))),
            hjust = 0) +
  geom_text(aes(x = -2, y = 4, 
                label = paste0('DF: ', summary(interaction_model_president)$df[2] %>% round(2))),
            hjust = 0) +
  theme(legend.position = 'bottom')
ggsave('output/predicted_vs_actual_deficit_changes.png', height = 6, width = 6, units = 'in', dpi = 600)



# 
# ggplot(US_wide, aes(real_gdp_per_capita_z, -diff_value_GGNLEND, colour = president_party)) +
#   geom_hline(aes(yintercept = 0), linetype = 'dashed', size = 1) +
#   geom_vline(aes(xintercept = 0), linetype = 'dashed', size = 1) +
#   geom_point(aes(shape = recession_year, size = pct_of_year_in_recession)) +
#   stat_smooth(method = 'lm', se = F) +
#   scale_colour_manual(
#     name = "President's Party",
#     values = c('DEM' = 'blue', 'REP' = 'red')
#   ) +
#   labs(
#     title = 'Economic Growth and Budget Deficits by Presidential Party\n1971-2018',
#     subtitle = sprintf('Republicans add %s more debt each year than Democrats', 
#                        percent(deficit_model$coefficients['president_partyREP']/100, accuracy = 0.01)),
#     y = 'Annual Deficit (% of GDP)\n',
#     x = '\nReal GDP Per Capita Growth\nStandard Deviations (Z Value)',
#     caption = caption_text
#   ) +
#   scale_size(
#     guide = F,
#     range = c(3, 8)
#   ) +
#   scale_shape(name = 'Recession Year', na.translate = FALSE) +
#   geom_text(aes(x = 1, y = -3, label = '(4) Strong Growth\nBudget Surplus'), colour = 'black', hjust=0) +
#   geom_text(aes(x = -2, y = -3, label = '(3) Poor Growth\nBudget Surplus'), colour = 'black', hjust=0) +
#   geom_text(aes(x = 1, y = 7, label = '(1) Strong Growth\nBudget Deficit'), colour = 'black', hjust=0) +
#   geom_text(aes(x = -2, y = 7, label = '(2) Poor Growth\nBudget Deficit'), colour = 'black', hjust=0) 
# 
# ggsave('output/deficits_vs_economic_growth_by_party.png', height = 7, width = 7.5, units = 'in', dpi = 600)
# 

save(reg_dat,
  base_model, base_model_president, interaction_model, 
     interaction_model_president, interaction_model_president_median, 
     file = 'output/reg_dat_diff_GNLEND_models.rdata')

summary(interaction_model_president_median, se = 'boot')

reg_dat$rep_dem_diff_GGNLEND %>% mean()
reg_dat$rep_dem_diff_GGNLEND %>% summary()
