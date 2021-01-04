
library(tidyquant)
library(tidyverse)
# library(quantmod)
library(scales)
library(readxl)
library(fuzzyjoin)
library(data.table)
library(ggrepel)
library(WDI)


rev_gdp = WDI(country = 'all', indicator = 'GC.REV.XGRT.GD.ZS') %>% filter(country == 'United States')
tax_rev_gdp = WDI(indicator = 'GC.TAX.TOTL.GD.ZS') %>% filter(country == 'United States')
exp_gdp = WDI(country = 'all', indicator = 'GC.XPN.TOTL.GD.ZS') %>% filter(country == 'United States')

combined_stats = left_join(tax_rev_gdp, exp_gdp) %>% 
  mutate(
    deficit = GC.TAX.TOTL.GD.ZS - GC.XPN.TOTL.GD.ZS
  )
ggplot(combined_stats, aes(year, deficit)) +
  geom_bar(stat = 'identity')

setwd('~\\Public_Policy\\Projects\\Taxes vs. Deficits\\data')

##### Get political data #####


  the_url = 'https://www.presidency.ucsb.edu/statistics/data/house-and-senate-concurrence-with-presidents'
  concurrence_table = html_table(GET(the_url) %>% content(), fill = TRUE)
  

# MIT data lab 
house_elections = read.csv('1976-2018-house.csv') %>% 
  mutate(candidate_votes = as.numeric(candidatevotes)) %>%
  filter(stage == 'gen') %>% data.table()

senate_elections = read.csv('1976-2018-senate.csv') %>% filter(stage == 'gen') %>% data.table()
presidential_elections = read.csv('1976-2016-president.csv') %>% data.table()


large_text_theme = theme(
  plot.title = element_text(size = 24),
  plot.subtitle = element_text(size = 18, face = 'italic'),
  plot.caption = element_text(size = 13, face = 'italic', hjust = 0),
  axis.text = element_text(size = 16),
  axis.title = element_text(size = 18)
) 

setwd("~/Public_Policy/Projects/Presidential Approval/data")
the_sheets = excel_sheets("American Presidency Project - Approval Ratings for POTUS.xlsx")

president_start_dates = tibble(
  President = c('Franklin D. Roosevelt','Harry S. Truman','Dwight D. Eisenhower','John F. Kennedy',
  'Lyndon B. Johnson','Richard Nixon','Gerald R. Ford','Jimmy Carter','Ronald Reagan','George Bush',
  'William J. Clinton','George W. Bush','Barack Obama','Donald Trump'),
  Party = c('Democratic', 'Democratic', 'Republican', 'Democratic', 'Democratic', 'Republican', 'Republican', 
            'Democratic', 'Republican', 'Republican', 'Democratic', 'Republican', 'Democratic', 'Republican'),
  start_date = c('1933-03-04', '1945-04-12', '1953-01-20', '1961-01-20', 
           '1963-11-22', '1969-01-20', '1974-08-09', '1977-01-20', 
           '1981-01-20', '1989-01-20', '1993-01-20', '2001-01-20', 
           '2009-01-20', '2017-01-20') %>% as.Date(),
  end_date = lead(start_date, 1) %>% as.Date()
) 


date_df = tribble(
  ~date, ~description, 
  as.Date('1979-11-04'), 'Iran Hostage Crisis',
  as.Date('1986-11-01'), 'Iran Contra Affair',
  as.Date('1990-08-02'), 'First Gulf War',
  as.Date('2001-09-11'), 'September 11 Attacks',
  as.Date('2008-09-15'), 'Lehman Bankruptcy',
  as.Date('2010-03-23'), 'Obamacare Signed',
  as.Date('2013-01-15'), 'Debt Ceiling Crisis'
)


president_start_dates$end_date[is.na(president_start_dates$end_date)] = as.Date('2021-01-20')
president_start_dates = mutate(president_start_dates, 
    n_terms = ifelse(year(end_date) - year(start_date) >= 6, 2, 1)
  )

stacked_presidential_approval = map(the_sheets, function(the_president){
  read_excel("American Presidency Project - Approval Ratings for POTUS.xlsx", the_president) %>% 
    mutate(
      End_Date = as.Date(`End Date`),
      President = the_president,
      net_approve = Approving - Disapproving
      ) %>%
    left_join(president_start_dates) %>%
    rename(term_start = start_date, term_end = end_date) %>%
    arrange(End_Date) %>%
    mutate(
      first_term = End_Date <= term_start + years(4),
      delta_net_approve = c(NA, diff(net_approve, 1)),
      period_diff = as.numeric(End_Date - lag(End_Date, 1))
    ) 
    
}) %>%
  bind_rows() %>%
  arrange(End_Date) %>%
  mutate(
    year = year(End_Date),
    month = month(End_Date)
  ) %>%
  group_by(
    President, year, month, first_term
  ) %>%
  summarize(
    mean_monthly_approval = mean(Approving, na.rm = T)
  ) %>%
  ungroup() %>%
  mutate(
    month_date = paste(year, month, '01', sep = '-') %>% as.Date()
  ) %>%
  arrange(
    month_date 
  )

president_stats = group_by(stacked_presidential_approval, President) %>%
  summarize(
    obs = n(),
    start_date = min(month_date, na.rm = T),
    end_date = max(month_date, na.rm = T),
    min_approval = min(mean_monthly_approval), 
    mean_approve = mean(mean_monthly_approval),
    median_approve = median(mean_monthly_approval)
  ) %>%
  ungroup() %>%
  mutate(
    midpoint = as.numeric(end_date - start_date) / 2 + start_date,
    last_name = str_extract(President, '( [a-zA-Z]+)$') %>% str_trim()
  ) %>% 
  arrange(
    start_date
  ) 


president_stats_by_term = group_by(stacked_presidential_approval, President, first_term) %>% summarize(
  obs = n(),
  first_poll_date = min(month_date, na.rm = T),
  last_poll_date = max(month_date, na.rm = T),
  mean_approve = mean(mean_monthly_approval),
  median_approve = median(mean_monthly_approval)
) %>%
  arrange(first_poll_date )

first_term_stats = president_stats_by_term %>% filter(first_term) %>% ungroup() %>% arrange(mean_approve) %>% 
  mutate(pres_sorted = factor(President, levels = President)) 


setwd("~/Public_Policy/Projects/Presidential Approval/output")

ggplot(first_term_stats, aes(pres_sorted, mean_approve / 100)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  geom_text(aes(label = percent(mean_approve/100, accuracy = 0.1)), hjust = 1, fontface = 'bold') +
  coord_flip() +
  theme_bw() +
  scale_y_continuous(labels = percent) + 
  large_text_theme + 
  labs(
    x = '', y = 'Average Monthly Approval Rating',
    title = 'U.S. Presidential Approval Ratings',
    subtitle = 'First Term',
    caption = 'Chart: Taylor G. White\nData: UCSB Presidency Project'
  )
ggsave('first_term_approval.png', height = 10, width = 10, units = 'in', dpi = 600)


stacked_presidential_approval$President = factor(stacked_presidential_approval$President, levels = president_stats$President)





large_text_theme = theme(
  plot.title = element_text(size = 28),
  plot.subtitle = element_text(size = 18, face = 'italic'),
  plot.caption = element_text(size = 13, face = 'italic', hjust = 0),
  axis.text = element_text(size = 16),
  axis.title = element_text(size = 18),
  legend.text = element_text(size = 16),
  legend.title = element_text(size = 18)
) 

get_monthly_annual_index_fred = function(symbol) {
  
  the_dat = getSymbols(symbol, from = 1940, to = 2020, src = 'FRED', auto.assign = F)
  
  interest_rate_spread_df = tibble(
    date = index(the_dat),
    period_index = as.numeric(the_dat[,symbol])
  ) %>%
    mutate(
      year = year(date),
      month = month(date)
    ) %>%
    arrange(date) %>%
    group_by(year, month) %>%
    summarize(
      last_val = tail(period_index, 1)
    ) %>% 
    ungroup()
  
}

ten_year_interest_rates_df = get_monthly_annual_index_fred('DGS10') %>% rename(ten_year_yield = last_val) 
interest_rate_spread_df = get_monthly_annual_index_fred('T10Y2Y') %>% rename(ten_two_spread = last_val) 
unemployment_rate_df = get_monthly_annual_index_fred('UNRATE') %>% rename(unemployment_rate = last_val) 




# debt_gdp_ratio_df = get_monthly_annual_index_fred('GFDEGDQ188S') %>% rename(debt_to_gdp = last_val) 
# annual_deficit_df = get_monthly_annual_index_fred('FYFSD') %>% rename(annual_deficit = last_val) 
# annual_gdp_df = get_monthly_annual_index_fred('GDPA') %>% rename(annual_gdp = last_val) 


get_quarterly_changes = function(symbol, use_diff = F) {
  # symbol = 'GDPC1'
  the_symbol = getSymbols(symbol, from = 1940, to = 2020, src = 'FRED', auto.assign = F)
  
  index(the_symbol) = index(the_symbol) - 1 # change the reporting dates to end of period 
  
  
  
  the_df = tibble(
    date = index(the_symbol), 
    quarterly_index = as.numeric(the_symbol[,symbol])
  ) %>% 
    mutate(
      year = year(date),
      month = month(date),
      quarterly_diff = quarterly_index - lag(quarterly_index, 1),
      quarterly_difftime = as.numeric(date - lag(date, 1))
    )
  
  if (use_diff) {
    
    annual_diffs = group_by(the_df, year) %>%
      summarize(
        annual_index = quarterly_index[month == max(month, na.rm = T)],
        date = max(date)
      ) %>%
      mutate(
        annual_diff = annual_index - lag(annual_index, 1)
      )
    left_join(the_df, annual_diffs) %>% return()
    
  } else {
    changes = allReturns(the_symbol)
    bind_cols(the_df, as.data.frame(changes)) %>%
      rename(
        annual_val = yearly, 
        quarterly_val = quarterly
      ) %>%
      return()
  }
  
}

gdp_df = get_quarterly_changes('GDPC1') %>% rename(annual_gdp = annual_val, quarterly_gdp = quarterly_val)
debt_gdp_ratio_df = get_quarterly_changes('GFDEGDQ188S', T) %>% 
  rename(
    quarterly_debt_gdp = quarterly_index,
    annual_debt_gdp = annual_index
    )
deficit_gdp_df = get_quarterly_changes('FYFSGDA188S', T) %>% 
  rename(
    annual_deficit_gdp = quarterly_index
  ) %>%
  mutate(
    change_annual_deficit = annual_deficit_gdp - lag(annual_deficit_gdp, 1)
  ) %>%
  select(
    date, year, month, annual_deficit_gdp, change_annual_deficit
  )



# annual_deficit_df = get_monthly_annual_index_fred('FYFSD') %>% rename(annual_deficit = last_val) 



# gdp = getSymbols('GDPC1', from = 1940, to = 2020, src = 'FRED', auto.assign = F)

# index(gdp) = index(gdp) - 1 # change the reporting dates to end of period 
# gdp_changes = allReturns(gdp) %>% as.data.frame()
# gdp_df = tibble(
#   date = index(gdp), 
#   quarterly_value = as.numeric(gdp$GDPC1)
# ) %>% 
#   bind_cols(
#     gdp_changes
#   ) %>%
#   rename(
#     annual_gdp = yearly, 
#     quarterly_gdp = quarterly
#   ) %>%
#   mutate(
#     year = year(date),
#     month = month(date)
#   )

inflation = getSymbols('CPIAUCSL', from = 1940, to = 2020, src = 'FRED', auto.assign = F) 

inflation_changes = allReturns(inflation) %>% as.data.frame()
inflation_df = tibble(
  date = index(inflation), 
  monthly_index = as.numeric(inflation$CPIAUCSL)
) %>% 
  bind_cols(
    inflation_changes
  ) %>%
  rename(
    annual_inflation = yearly, 
    monthly_inflation = monthly
  ) %>%
  mutate(
    year = year(date),
    month = month(date)
  )


# annual_inflation = filter(inflation_df, !is.na(yearly)) %>% mutate(year = year(date))
# 
# ggplot(annual_inflation, aes(year, yearly)) +
#   geom_bar(stat = 'identity') 

get_monthly_symbol_returns = function(symbol, src) {
  
  the_dat = getSymbols(symbol, auto.assign = F, from = 2000)
  
  returns = allReturns(the_dat) %>% as.data.frame()
  adjusted_col = names(the_dat)[str_detect(names(the_dat), 'Adjusted')]
  
  gold_df = tibble(
    date = index(the_dat), 
    adjusted_close = as.numeric(the_dat[,adjusted_col])
  ) %>%
    bind_cols(
      returns
    ) %>%
    mutate(
      year = year(date),
      month = month(date)
    ) %>%
    arrange(year, month) %>%
    group_by(
      year, month
    ) %>%
    summarize(
      last_monthly_value = tail(monthly, 1),
      last_yearly_value = tail(yearly, 1),
      last_close_val = tail(adjusted_close, 1)
    )
  
}

sp500_df = get_monthly_symbol_returns('^GSPC') 
gold_df = get_monthly_symbol_returns('IAU') 


# get month end returns


financial_statistics_with_presidents = left_join(
  inflation_df %>% select(year, month, annual_inflation, monthly_inflation), 
  gold_df %>% select(year, month, annual_gold = last_yearly_value, monthly_gold = last_monthly_value, gold_adjusted_close = last_close_val)
) %>%
  left_join(
    sp500_df %>% select(year, month, annual_sp500 = last_yearly_value, monthly_sp500 = last_monthly_value, sp500_adjusted_close = last_close_val)
  ) %>%
  left_join(gdp_df %>% select(year, month, annual_gdp, quarterly_gdp, quarterly_gdp_value = quarterly_index)) %>%
  left_join(debt_gdp_ratio_df %>% select(year, month, quarterly_debt_gdp, annual_debt_gdp)) %>%
  left_join(deficit_gdp_df %>% select(year, month, annual_deficit_gdp, change_annual_deficit)) %>%
  left_join(interest_rate_spread_df) %>%
  left_join(ten_year_interest_rates_df) %>%
  left_join(unemployment_rate_df) %>%
  filter(
    !is.na(monthly_inflation)
  ) %>%
  mutate(
    month_date = as.Date(paste(year, month, '01', sep = '-')),
    ten_two_spread = ten_two_spread  / 100,
    ten_year_yield = ten_year_yield  / 100
  )  %>%
  fuzzy_left_join(
    president_start_dates, by = c('month_date' = 'start_date', 'month_date' = 'end_date'), 
    match_fun = list(`>=`, `<=`)
  ) %>%
  mutate(
    last_name = str_extract(President, '( [a-zA-Z]+)$') %>% str_trim(),
    years_into_presidency = as.numeric(month_date - start_date) / 365
  ) %>%
  arrange(month_date)


financial_statistics_with_presidents_dt = data.table(financial_statistics_with_presidents)
president_indexes = financial_statistics_with_presidents_dt[, {
  # obama_df = financial_statistics_with_presidents_dt[President == 'Barack Obama',]
  # attach(obama_df)
  filled_gdp = na.fill(quarterly_gdp_value, 'extend')
  filled_debt_gdp = na.fill(quarterly_debt_gdp, 'extend')
  
  list(
    month_date = month_date, 
    last_val = month_date == max(month_date),
    month_index = 1:length(month_date) - 1,
    last_month_index = length(month_date) -1,
    gdp_index = filled_gdp / filled_gdp[1],
    debt_to_gdp_added = filled_debt_gdp - filled_debt_gdp[1],
    unemployment_index = unemployment_rate / unemployment_rate[1],
    sp500_index = sp500_adjusted_close / sp500_adjusted_close[1]
  )
  # detach(obama_df)
}, by = list(President, last_name)] 


ggplot(financial_statistics_with_presidents_dt, aes(month_date, sp500_adjusted_close, colour = President)) + geom_point()


ggplot(president_indexes %>% filter(year(month_date) >= 1981), aes(month_index, gdp_index - 1, colour = President)) +
  geom_line(size = 1) + 
  scale_y_continuous(labels = percent)


##### stock market by president #####
ggplot(president_indexes %>% filter(month_index <= 48, month_date >= as.Date('1981-01-20')), aes(month_index, sp500_index-1, colour = President)) +
  theme_bw() +
  geom_line(size = 1) + 
  scale_y_continuous(labels = percent) + 
  scale_x_continuous(breaks = seq(0, 50, by = 10)) +
  large_text_theme + 
  scale_colour_hue(guide = F) +
  geom_label_repel(
    data = filter(president_indexes, ifelse(last_month_index < 48, last_month_index == month_index, month_index == 48),  
                  month_date >= as.Date('1981-01-20')), aes(month_index + 6, label = paste(President, percent(sp500_index-1, accuracy = 1))), show.legend = F) +
  labs(
    x = 'Month of Presidency', y = 'S&P 500 Index Cumulative Return',
    title = 'Stock Market Returns by President',
    subtitle = 'First four years in office',
    caption = 'Chart: Taylor G. White\nData: Yahoo Finance'
  )
ggsave('stock_market_by_president.png', height = 9, width = 12, units = 'in', dpi = 600)



##### debt by president #####


annual_comparison = filter(financial_statistics_with_presidents, !is.na(annual_inflation), month == 12) %>%
  mutate(
    scaled_growth = scale(annual_gdp) %>% as.numeric(),
    scaled_sp500 = scale(annual_sp500) %>% as.numeric()
  ) 
head(annual_comparison)

library(xgboost)

names(annual_comparison)
the_dat = select(
  annual_comparison, 
  change_annual_deficit,
  annual_gdp,
  annual_deficit_gdp,
  year, 
  Party, 
  years_into_presidency,
  ten_year_yield,
  # ten_two_spread,
  # annual_inflation,
  # annual_sp500
) %>%
  mutate(
    lag_1_annual_gdp = lag(annual_gdp, 1),
    lag_1_annual_deficit_gdp = lag(annual_deficit_gdp, 1)
  ) %>% na.omit() 

dim(the_dat)

test_data_predictions = lapply(1:100, function(it){
  train_dat = sample_n(the_dat, 40)
  test_dat = filter(the_dat, !year %in% train_dat$year)
  
  model_mat = model.matrix(change_annual_deficit ~ annual_gdp + lag_1_annual_gdp + 
                             lag_1_annual_deficit_gdp + ten_year_yield + Party + year + years_into_presidency, data = train_dat)
  
  model_mat_test = model.matrix(change_annual_deficit ~ annual_gdp + lag_1_annual_gdp + 
                                  lag_1_annual_deficit_gdp + ten_year_yield  + Party + year + years_into_presidency, data = test_dat)
  
  
  boost_mod = xgboost(model_mat, label = train_dat$change_annual_deficit, nrounds = 20)
  test_dat$predictions = predict(boost_mod, newdata = model_mat_test)
  test_dat$it = it
  test_dat
}) %>%
  bind_rows()

combined_preds = group_by(test_data_predictions, year) %>%
  summarize(
    obs = n(),
    mean_pred = mean(predictions),
    actual_val = change_annual_deficit[1]
  )

ggplot(combined_preds, aes(mean_pred, actual_val)) +
  geom_point()

cor(combined_preds$actual_val, combined_preds$mean_pred)

ggplot(combined_preds %>% filter(between(year, 2006, 2020)), aes(year)) +
  geom_bar(aes(y = actual_val), stat = 'identity') +
  geom_point(aes(y = mean_pred), colour = 'red') 
  



group_by(importance_dats, Feature) %>%
  summarize(
    mean_freq = mean(Frequency)
  ) %>%
  arrange(
    -mean_freq
  )


test_dat$predicted_boost = predict(boost_mod, newdata = model_mat_test)

ggplot(test_dat, aes(predicted_boost, change_annual_deficit)) + geom_point()


mod = lm(change_annual_deficit ~ annual_gdp + lag(annual_gdp, 1) + lag(annual_deficit_gdp, 1) + ten_year_yield + Party * years_into_presidency, data = annual_comparison)
summary(mod)

annual_comparison$ten_year_yield 
ggplot(annual_comparison, aes(annual_gdp, change_annual_deficit)) +
  geom_point() +
  stat_smooth(method = 'lm')

filter(annual_comparison, !is.na(annual_deficit_gdp)) %>% select(year) %>% tail()

ggplot(annual_comparison, aes(year, annual_deficit_gdp, fill = Party)) +
  geom_bar(stat = 'identity')

ggplot(annual_comparison, aes(year, change_annual_deficit  )) +
  geom_bar(stat = 'identity')
View(annual_comparison %>% select(year, annual_debt_gdp, quarterly_debt_gdp, annual_deficit, quarterly_deficit))





monthly_comparison = filter(joined_inflation_gold_growth) %>%
  mutate(
    scaled_sp500 = scale(monthly_sp500) %>% as.numeric(),
    scaled_gold = scale(monthly_gold) %>% as.numeric(),
    scaled_gold_close = scale(gold_adjusted_close) %>% as.numeric(),
    scaled_ten_two_spread = scale(ten_two_spread) %>% as.numeric(),
    scaled_inflation = scale(monthly_inflation) %>% as.numeric()
  )


blank_df = data.frame(
  description = c('Presidential Approval', 'Real GDP Growth'), 
  x = rep(as.Date('1947-01-01'), 2), 
  y = 0
)

##### approval vs growth #####
ggplot(stacked_presidential_approval %>% filter(month_date >= as.Date('1947-01-01'))) +
  labs(
    y = 'Scaled Value', x = '', title = 'U.S. Presidential Approval Ratings vs. Economic Growth', 
    caption = 'Chart: Taylor G. White\nData: UCSB Presidency Project, St. Louis Federal Reserve\nScaled values represent standard deviations from the mean for both presidential approval and economic growth.'
  ) +
  geom_bar(data = blank_df, aes(x, y, fill = description), stat = 'identity', alpha = 0) +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  scale_fill_manual(values = c('Presidential Approval' = 'steelblue', 'Real GDP Growth' = 'orange'), name = '') +
  theme_minimal() +
  theme(
    legend.position = 'bottom',
    panel.grid.minor = element_blank()
    ) +
  geom_rect(data = president_start_dates %>% filter(end_date   >= as.Date('1947-01-01')), 
            aes(xmin = start_date, xmax = end_date, ymin = -3.5, ymax = 3.5), size = 0.5, alpha = 0.10, show.legend = F, colour = 'black') + 
  geom_text(data = president_stats %>% filter(end_date >= as.Date('1947-01-01')), aes(x = midpoint, y = -2.75, 
                                                                                      label = paste0(last_name, '\n', sprintf('(%s)', percent(mean_approve /100, accuracy = 1)))), fontface = 'bold') + 
  scale_colour_hue(guide = F) +
  scale_y_continuous(breaks = seq(-3, 3, by = 1)) +
  scale_x_date(date_breaks = '4 years', date_labels = '%Y') +
  geom_hline(aes(yintercept = 0)) +
  # geom_bar(data = annual_comparison %>% filter(month_date >= as.Date('1947-01-01'), year < 2020), aes(month_date, scaled_growth), stat = 'identity', fill = 'orange', alpha = 0.5, colour = 'black') +
  
  geom_point(aes( month_date, scale(mean_monthly_approval )), colour = 'steelblue') +
  geom_line(data = annual_comparison %>% filter(month_date >= as.Date('1947-01-01'), year < 2020), aes(month_date, scaled_growth), size = 0.75, colour = 'orange') +
  geom_point(data = annual_comparison %>% filter(month_date >= as.Date('1947-01-01'), year < 2020), aes(month_date, scaled_growth), size = 1, colour = 'orange') +
  # stat_smooth(aes(colour = NA), span = .1) +
  
  large_text_theme 
  # geom_vline(data = date_df, aes(xintercept = date)) +
  # geom_text(data = date_df, aes(x = date, y = 2, label = description, angle = 90), vjust = 0, hjust = 0, fontface = 'bold')
  
ggsave('presidential_approval_timeline.png', height = 10, width = 20, units = 'in', dpi = 600)
getwd()

head(stacked_presidential_approval)
filter(stacked_presidential_approval, President == 'Jimmy Carter')
  


ggplot(monthly_comparison, aes(month_date, scaled_gold_close)) +
  theme_bw() +
  geom_line(colour = 'gold', size = 1) +
  geom_line(aes(y = scaled_ten_two_spread), colour = 'steelblue', size = 1)


ggplot(annual_comparison, aes(annual_inflation, annual_sp500)) +
  geom_point()


ggplot(annual_comparison, aes(annual_gdp, annual_sp500)) +
  geom_point() +
  stat_smooth(method = 'lm') +
  theme_bw() +
  scale_x_continuous(labels = percent, limits = c(-0.04, 0.065)) +
  scale_y_continuous(labels = percent, breaks = seq(-0.4, 0.3, by = 0.1)) +
  large_text_theme +
  labs(
    x = '\nReal Annual GDP Growth', y = 'Annual SP500 Market Return\n'
  )

ggsave('economic_growth_vs_stock_market.png', height = 10, width = 10, units = 'in', dpi = 400)

ggplot(monthly_comparison, aes(scaled_ten_two_spread, scaled_gold)) +
  geom_point()


ggplot(annual_comparison, aes(ten_year_yield, annual_gold)) +
  geom_point() 

ggplot(annual_comparison, aes(annual_inflation, annual_gold)) +
  geom_point() 



ggplot(annual_comparison, aes(annual_inflation, ten_year_yield)) +
  geom_point() +
  # stat_smooth() +
  stat_smooth(method = 'lm') +
  scale_y_continuous(labels = percent, breaks = seq(0, 0.15, by = 0.025)) +
  scale_x_continuous(labels = percent, breaks = seq(0, 0.13, by = 0.025)) +
  labs(
    x = '\nAnnual Inflation\nConsumer Price Index for All Urban Consumers', 
    y = '10 Year Treasury Bond Yield\n',
    caption = 'Chart: Taylor G. White\nData: St. Louis Federal Reserve'
  ) +
  theme_bw() +
  large_text_theme +
  coord_cartesian(xlim = c(0, 0.13), ylim = c(0, 0.15))

ggsave('inflation_vs_10yr_yield.png', height = 10, width = 10, units = 'in', dpi = 450)

