library(tidyverse)
library(scales)
library(readxl)
setwd("~/Public_Policy/Projects/2020 Election")
list.files()
vote_share_data = read_excel("vote_share_data.xlsx") %>%
  arrange(Type, Place) %>%
  mutate(
    Place_Factor = factor(Place, levels = unique(Place))
  )

ggplot(vote_share_data, aes(Place_Factor, Share, fill = factor(Year))) +
  theme_bw() +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.75, colour = 'black') + 

  geom_text(aes(label = paste0(Candidate, sprintf(", %s", percent(Share, accuracy = 0.1)))), 
            position = position_dodge(width = 0.75), hjust = 0, size = 5) +
  scale_y_continuous(labels = percent, breaks = seq(0, 1, by = .2), limits = c(0, 1)) + 
  scale_fill_brewer(palette = 'Set1', name = 'Election') + 
  theme(legend.position = 'bottom') +
  coord_flip() + 
  labs(
    x = '', 
    caption = 'Chart: Taylor G. White\nData: New York Times',
    y = 'Vote Share',
    title = 'Presidential Vote Share Comparison in Selected Counties/States',
    subtitle = str_wrap('The winning candidate is labeled for each place. Comparisons show that even with new or expanded alternative voting programs (vote by mail, drive-up), vote shares for the same party see little change between elections. This shows that the method of voting had little to no impact on results.', 100)
  ) +
  theme(
    plot.title = element_text(size = 25),
    plot.subtitle = element_text(size = 16, face = 'italic'),
    plot.caption = element_text(size = 13, face = 'italic', hjust = 0),
    axis.text = element_text(size = 16), 
    axis.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 18)
  )
ggsave('vote_share_comparison.png', height = 10, width = 13, units = 'in', dpi = 600)  
x  