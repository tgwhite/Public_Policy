
library(tidyquant)
library(tidyverse)
# library(quantmod)
library(data.table)
library(scales)


setwd("~/Public_Policy/Projects/Finance/output")


large_text_theme = theme(
  plot.title = element_text(size = 24),
  plot.subtitle = element_text(size = 18, face = 'italic'),
  plot.caption = element_text(size = 13, face = 'italic', hjust = 0),
  axis.text = element_text(size = 16),
  axis.title = element_text(size = 18)
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

ten_year_interest_rates = get_monthly_annual_index_fred('DGS10') %>% rename(ten_year_yield = last_val) 
interest_rate_spread = get_monthly_annual_index_fred('T10Y2Y') %>% rename(ten_two_spread = last_val) 

gdp = getSymbols('GDPC1', from = 1940, to = 2020, src = 'FRED', auto.assign = F)
index(gdp) = index(gdp) - 1 # change the reporting dates to end of period 
gdp_changes = allReturns(gdp) %>% as.data.frame()
gdp_df = tibble(
  date = index(gdp), 
  quarterly_index = as.numeric(gdp$GDPC1)
) %>% 
  bind_cols(
    gdp_changes
  ) %>%
  rename(
    annual_gdp = yearly, 
    quarterly_gdp = quarterly
  ) %>%
  mutate(
    year = year(date),
    month = month(date)
  )

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

get_monthly_symbol_returns = function(symbol) {
  
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


joined_inflation_gold_growth = left_join(
  inflation_df %>% select(year, month, annual_inflation, monthly_inflation), 
  gold_df %>% select(year, month, annual_gold = last_yearly_value, monthly_gold = last_monthly_value, gold_adjusted_close = last_close_val)
) %>%
  left_join(
    sp500_df %>% select(year, month, annual_sp500 = last_yearly_value, monthly_sp500 = last_monthly_value, sp500_adjusted_close = last_close_val)
  ) %>%
  left_join(gdp_df %>% select(year, month, annual_gdp, quarterly_gdp)) %>%
  left_join(interest_rate_spread) %>%
  left_join(ten_year_interest_rates) %>%
  filter(
    !is.na(monthly_inflation)
  ) %>%
  mutate(
    month_date = as.Date(paste(year, month, '01', sep = '-')),
    ten_two_spread = ten_two_spread  / 100,
    ten_year_yield = ten_year_yield  / 100
  )

annual_comparison = filter(joined_inflation_gold_growth, !is.na(annual_inflation), month == 12) %>%
  mutate(
    scaled_growth = scale(annual_gdp) %>% as.numeric(),
    scaled_sp500 = scale(annual_sp500) %>% as.numeric()
  )

monthly_comparison = filter(joined_inflation_gold_growth, !is.na(monthly_gold)) %>%
  mutate(
    scaled_gold = scale(monthly_gold) %>% as.numeric(),
    scaled_gold_close = scale(gold_adjusted_close) %>% as.numeric(),
    scaled_ten_two_spread = scale(ten_two_spread) %>% as.numeric(),
    scaled_inflation = scale(monthly_inflation) %>% as.numeric()
  )

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

ggplot(monthly_comparison, aes(monthly_inflation, ten_year_yield )) +
  geom_point()

ggplot(annual_comparison, aes(annual_gdp, annual_inflation)) +
  geom_point()


ggplot(annual_comparison, aes(annual_gdp, annual_inflation)) +
  geom_point()

ggplot(annual_comparison, aes(year, annual_inflation)) +
  geom_bar(stat = 'identity', aes(fill = annual_gdp)) +
  scale_fill_viridis_c(option = 'A')

ggplot(annual_comparison, aes(year, annual_inflation)) +
  geom_point(stat = 'identity', aes(colour = annual_gdp, size = annual_gdp)) +
  scale_colour_viridis_c(option = 'A')




tail(joined_inflation_gold_growth, 12)


ggplot(monthly_comparison, aes(scaled_gold , scaled_inflation)) +
  geom_point() +
  stat_smooth(method = 'lm')



ggplot(monthly_comparison, aes(annual_inflation  , annual_gold )) +
  geom_point() +
  stat_smooth(method = 'lm')

summary(monthly_comparison$monthly_gold)
summary(monthly_comparison$monthly_inflation)
hist(monthly_comparison$monthly_gold)
hist(monthly_comparison$monthly_inflation)
scale(monthly_comparison$monthly_gold)
