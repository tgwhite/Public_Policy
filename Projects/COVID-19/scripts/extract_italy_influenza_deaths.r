library(tidyverse)
library(jpeg)
library(plotly)
library(readxl)
library(imager)
library(randomForest)

setwd("~/Public_Policy/Projects/COVID-19/literature")

# excess_mortality <- readJPEG("italy_excess_mortality_flu.jpg")
# round(excess_mortality[,,1] * 256)

excess_mortality <- load.image("italy_excess_mortality_flu.jpg")

excess_mortality_g = grayscale(excess_mortality)
plot(excess_mortality_g)
a = imrow(R(excess_mortality),70) 
imcol(R(excess_mortality),10) %>% plot(main="Red channel along 10th col",type="l")
plot(excess_mortality)
dim(excess_mortality)

total_plot = imsub(excess_mortality,y > 620)  
plot(total_plot)
# 
# imrow(R(total_plot),2) %>% plot(main="Red channel along 10th row",type="l")
# imcol(R(total_plot),35) %>% plot(type = 'l')
# imsub(total_plot,x > 30 & x < 100) %>% plot()
# imsub(total_plot,y > 50) %>% plot()
# dim(total_plot)
# total_plot[,,,1]
# R(total_plot) %>% plot()
# B(total_plot) %>% plot()
# G(total_plot) %>% plot()
# grayscale(total_plot) %>% plot()
# 
# imcol(B(total_plot),100) %>% plot(main="Blue channel along 10th line",type="l")
# for (it in 1:dim(total_plot)[1]) {
#   print(total_plot[it,,,2])
# }

red = R(total_plot)[,,,1] %>% as.data.frame() %>% mutate(row = 1:length(V1)) %>% 
  pivot_longer(cols = starts_with('V')) %>% rename(red= value)
blue = B(total_plot)[,,,1] %>% as.data.frame() %>% mutate(row = 1:length(V1)) %>%
  pivot_longer(cols = starts_with('V')) %>% rename(blue= value)
green = G(total_plot)[,,,1] %>% as.data.frame() %>% mutate(row = 1:length(V1)) %>%
  pivot_longer(cols = starts_with('V')) %>% rename(green= value)

total_plot

rgb_join = inner_join(red, blue) %>% inner_join(green) %>%
  mutate(
    red = round(red),
    green = round(green),
    blue = round(blue),
    col = str_extract(name, '[0-9]+') %>% as.integer(),
    rgb_hex = rgb(red, green, blue)
  )


ggplot(rgb_join, aes(row, col)) +
  geom_tile(aes(fill = red)) +
  scale_y_reverse() 
ggplot(rgb_join, aes(row, col)) +
  geom_tile(aes(fill = green)) +
  scale_y_reverse() 

colour_maps = unique(rgb_join$rgb_hex)
names(colour_maps) = colour_maps

# get rid of white values
rgb_join_not_white = filter(rgb_join, rgb_hex != '#FFFFFF')

all_tiles = ggplot(rgb_join_not_white, aes(row, col)) +
  geom_tile(aes(fill = rgb_hex)) +
  scale_y_reverse() +
  scale_fill_manual(values = colour_maps) 
all_tiles

interactive_plot = ggplot(rgb_join, aes(row, col)) +
  geom_tile(aes(fill = blue)) +
  scale_y_reverse() 
ggplotly(interactive_plot)

# column for value = 30 is roughly 20
# column for vlaue = 15 is roughly 140
# row for date = 2013-17 is roughly 72
# row for date = 2017-17 is roughly 364
# daily_pixels = (364-72) / (4 * 365.25)
# (217-171) / daily_pixels / 365 
as.Date('2019-10-07') %>% format('%U')

# row for date = 2017-09 is roughly 343
# scale start row = 60
# scale end row = 374

# base - col 114 to 88

rgb_join %>% filter(between(col, 87, 115)) %>%
ggplot(aes(row, col)) +
  geom_tile(aes(fill = blue)) +
  scale_y_reverse() 

rgb_join_not_white %>% filter(between(col, 87, 115) & between(row, 60, 374) & rgb_hex == '#000000') %>%
ggplot(aes(row, col)) +
  geom_tile(aes(fill = rgb_hex)) +
  scale_y_reverse() +
  scale_fill_manual(values = colour_maps) 


rgb_join_not_white %>% 
  filter( between(row, 60, 374), (rgb_hex == '#FF0000' & between(col, 30, 115)) | (rgb_hex == '#000000' & 
                                    between(col, 87, 115))) %>%
  ggplot(aes(row, col)) +
  geom_tile(aes(fill = rgb_hex)) +
  scale_y_reverse() +
  scale_fill_manual(values = colour_maps) 



rgb_join_not_white %>%
filter( between(row, 60, 374), (rgb_hex == '#FF0000' & between(col, 30, 115)) | (rgb_hex == '#000000' &
between(col, 87, 115))) %>%
ggplot(aes(row, col)) +
geom_tile(aes(fill = rgb_hex)) +
scale_y_reverse() +
scale_fill_manual(values = colour_maps)
