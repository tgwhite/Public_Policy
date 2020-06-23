

library(pdftools)
library(tidyverse)
library(data.table)
library(readxl)
setwd("C:/Users/Owner/Dropbox/Politics")

imported_calls_for_service = read_excel('police calls for service may 31.xlsx', 'may 31 clean'
                                        # col_types = 'text'
                                        ) %>%
  rename(
    incident_num = `Incident #`,
    type = `Incident Type`
  ) %>%
  mutate(
    received_time_minute = as.numeric(Received) * 24 * 60,
    cleared_minute = as.numeric(Cleared) * 24 * 60,
    received_hour = received_time_minute %/% 60,
    received_minute = floor(received_time_minute %% 60),
    received_datetime = ISOdatetime(2020, 5, 31, received_hour, received_minute, 0)
  ) %>%
  filter(
    received_hour >= 12
  )

calls_for_service_table = table(imported_calls_for_service$type) %>% sort(decreasing = T)

imported_calls_for_service$type_factor = factor(imported_calls_for_service$type, levels = names(calls_for_service_table))

ggplot(imported_calls_for_service %>% filter(type %in% names(calls_for_service_table)[1:12]), aes(received_datetime, fill = type_factor)) +
  geom_histogram(show.legend = F, binwidth = 60*10) +
  facet_wrap(~type_factor)

library(viridisLite)

ggplot(imported_calls_for_service %>% filter(type %in% names(calls_for_service_table)[1:5]), aes(received_datetime, fill = type_factor)) +
  geom_histogram(show.legend = T, binwidth = 60*10) +
  # scale_fill_hue(name = '') + 
  scale_fill_viridis_d(name = '') +
  labs(
    x = 'Time Received', y = 'Count',
    title = 'Top 5 Types of Police Calls for Service',
    subtitle = 'Santa Monica: May 31, 2020. 12pm onward in 10 minute intervals.'
  ) +
  scale_x_datetime(date_breaks = '1 hour', date_labels = '%H:%M')
ggsave('sm_police_calls_for_service.png', height = 8, width = 10, units = 'in', dpi = 800)

ggplot(imported_calls_for_service, aes(received_datetime)) +
  geom_histogram(show.legend = T, binwidth = 60*10) 




ggplot(imported_calls_for_service %>% filter(type %in% names(calls_for_service_table)[1:10]), aes(type_factor, received_datetime)) +
  geom_point(aes(colour = Disposition)) +
  coord_flip()



ggplot(imported_calls_for_service %>% filter(type %in% names(calls_for_service_table)[1:5]), aes(received_datetime, fill = type_factor)) +
  stat_density(position = 'identity', alpha = 0.3, ncol = 1) +
  facet_wrap(~type_factor)


ggplot(imported_calls_for_service %>% filter(type %in% names(calls_for_service_table)[1:5]), aes(type_factor, received_datetime)) +
  geom_violin() +
  coord_flip()

