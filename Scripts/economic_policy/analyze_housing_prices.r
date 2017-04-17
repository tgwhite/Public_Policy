library(RColorBrewer)
library(plyr)
library(dplyr)
library(reshape2)
library(dtplyr)
library(data.table)
library(ggplot2)
library(ggmap)
library(stringr)
library(zipcode)
library(scales)
library(animation)
library(magick)

# import zipcode location data
data(zipcode)

# subset to california, and then norcal
cali_zip = filter(zipcode, state == 'CA')
norcal_zip = filter(cali_zip, latitude >= 36 & latitude <= 39 & longitude >= -123 & longitude <= -119)  


## import and clean single family home data for california
setwd('~/tgwhite_github/Public_Policy/Data')
ca_zip_zhvi_single_family = fread('Zip_Zhvi_SingleFamilyResidence.csv') %>%
  melt(
    id = c('RegionID', 'RegionName', 'City', 'State', 'Metro', 'CountyName', 'SizeRank')
  ) %>%
  mutate(
    date = as.Date(paste0(variable, '-01')), 
    year = year(date),
    month = month(date),
    variable = NULL
  ) %>%
  rename(
    zip = RegionName
  ) %>%
  filter(
    State == 'CA'
  ) %>%
  arrange(zip, date) %>%
  data.table()


## compute year on year changes (from the same month in the prior year)

ca_year_on_year_changes = ca_zip_zhvi_single_family[, {
  last_year_val = lag(value, 1)
  pct_change = c(NA, diff(value, 1)) / last_year_val
  
  list(
    year = year, 
    date = date, 
    yoy_change = pct_change,
    last_year_val = last_year_val,
    this_year_val = value
  )
}, by = list(zip, month)] %>%
  arrange(zip, date) %>%
  as.data.frame() 

# get quantiles for binning on maps
overall_quantiles = quantile(ca_year_on_year_changes$yoy_change, probs = seq(0, 1, by = 1/7), na.rm = T)
ca_year_on_year_changes$yoy_change_bin = with(ca_year_on_year_changes, cut(yoy_change * 100, breaks = overall_quantiles * 100, include.lowest = T))

### plot time series across all zips
time_series_plot = ggplot(ca_year_on_year_changes %>% filter(!is.na(yoy_change)), aes(date, yoy_change, group = zip)) +
  scale_y_continuous(labels = percent) +
  labs(
    x = '\nTaylor G. White\ngithub: tgwhite, twitter: t_g_white', y = 'Year on Year Change (%)\n', 
    title = 'Tracking Prices of Single Family Residences in CA',
    subtitle = 'Monthly Zip-Level Zillow Home Value Index\nApril 1997-Feburary 2017'
  ) +
  theme(
    text = element_text(size = 12),
    title = element_text(size = 16),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  ) +  
  geom_line(alpha = 0.25) +
  geom_hline(aes(yintercept = 0), linetype = 'dashed', colour = 'blue', size = 1) +
  stat_smooth(aes(group = NA), colour = 'orange', size = 1.5) 
  
ggsave('ca_single_family_prices_plot.png', height = 6, width = 8, units = 'in', dpi = 400, plot = time_series_plot)

### create gifs of changes over time
# inner join with norcal zipcodes to get lat/lon and subset to desired vals
norcal_year_on_year_changes = 
  inner_join(ca_year_on_year_changes, norcal_zip)

# get vector of dates to loop over
unique_dates = unique(ca_year_on_year_changes$date[!is.na(ca_year_on_year_changes$yoy_change)])

# get color scales
the_colors = RColorBrewer::brewer.pal(7, 'PiYG')
names(the_colors) = levels(ca_year_on_year_changes$yoy_change_bin)

# get map of Modesto via google maps
mod_map = get_map('modesto', zoom = 8, maptype = 'hybrid', color='bw')

# create a blank map for the start of the gifs
blank_map <- ggmap(mod_map) +
  geom_point(data = norcal_year_on_year_changes %>% filter(!is.na(yoy_change_bin)) %>% head(1), aes(longitude, latitude, colour = yoy_change_bin), size = 0) +
  scale_colour_manual(name='YOY Percent Change', values = the_colors, drop = F) +
  scale_alpha(guide = F, name = '') +
  labs(
    x = '\nTaylor G. White\ngithub: tgwhite, twitter: t_g_white', y = '',
    title = 'Tracking Prices of Single Family Residences in CA\nMonthly Zip-Level Zillow Home Value Index',
    subtitle = "Can't see this!" # use for spacing purposes
  ) +
  theme(
    plot.subtitle = element_text(colour = 'white', size = 12),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  guides(colour = guide_legend(override.aes = list(size = 2)))

# save month level plots into a list
date_plots = lapply(unique_dates, function(this_date){

  the_sub = filter(norcal_year_on_year_changes, date == this_date & !is.na(yoy_change_bin))
  date_pretty = format(this_date, '%b-%Y')
  
  the_plot <- ggmap(mod_map) +
    geom_point(data = the_sub, aes(longitude, latitude, colour = yoy_change_bin), size = 2) +
    scale_colour_manual(name='YOY Percent Change', values = the_colors, drop = F) +
    labs(
      x = '\nTaylor G. White\ngithub: tgwhite, twitter: t_g_white', y = '',
      title = 'Tracking Prices of Single Family Residences in CA\nMonthly Zip-Level Zillow Home Value Index',
      subtitle = date_pretty
    ) +
    theme(
      plot.subtitle = element_text(size = 12),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    ) 
  
  return(the_plot)
})

# gif is too large in one file (and takes to long to view). Get indices to create  gifs in chunks
early_epoch = which(unique_dates <= as.Date('2001-12-31'))
up_to_housing_crisis = which(unique_dates >= as.Date('2002-01-01') & unique_dates <= as.Date('2005-12-31'))
housing_crisis = which(unique_dates >= as.Date('2006-01-01') & unique_dates <= as.Date('2011-12-31'))
post_crisis = which(unique_dates >= as.Date('2012-01-01'))

overall_plot_list = list(
  early_epoch = date_plots[early_epoch],
  up_to_housing_crisis = date_plots[up_to_housing_crisis],
  housing_crisis = date_plots[housing_crisis],
  post_crisis = date_plots[post_crisis]
)

# loop over overall plot list and then within each lists to print plots and convert to gif
dir.create('the_plots')
setwd('the_plots')

for (list_it in 1:length(overall_plot_list)) {
  
  plot_list = overall_plot_list[[list_it]]
  
  # save down blank map for first image  
  ggsave(paste0('plot_', 0, '.png'), height = 6, width = 6, units = 'in', dpi = 250, plot = blank_map)
  
  for (it in 1:length(plot_list)) {	
    the_plot = plot_list[[it]]
    ggsave(paste0('plot_', it, '.png'), height = 6, width = 6, units = 'in', dpi = 250, plot = the_plot)
  }
  
  gif_command = paste0("convert -size 800x800 -loop 0 -delay 50 ", paste(paste0('plot_', 0:length(plots_to_save), '.png'), collapse = ' '), sprintf(' house_prices_change_%s.gif', list_it))
  shell(gif_command)
  plot_files = list.files(pattern = '^(plot_)')
  lapply(plot_files, file.remove)
}



