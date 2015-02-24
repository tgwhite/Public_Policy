# Taylor G. White
# 12/21/14

##### Libraries and options #####
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(data.table)
library(XML)
library(stringr)
library(Hmisc)
library(extrafont)
options(stringsAsFactors = F)

##### Helper parameters #####
out_directory = "C:/Users/taylor/Dropbox/WildPolicy/Public/Output/social_policy"

##### Data import and cleaning #####
marriage_tab_url = "http://www.cdc.gov/nchs/nvss/marriage_divorce_tables.htm"
extracted_tables = readHTMLTable(marriage_tab_url)
names(extracted_tables) = c("national_marriage_rate", "national_divorce_rate")

## clean tables ## 
cleaned_tables_stacked = rbindlist(lapply(names(extracted_tables), function(list_name){
  df = extracted_tables[[list_name]]
  names(df) = c("year", "value", "population", "rate_per_1000")
  
  return(mutate(df, 
                year = as.numeric(substr(year, 1, 4)),
                value = as.numeric(gsub(x = value, pattern = ",", replacement = "")),
                population = as.numeric(gsub(x = population, pattern = ",", replacement = "")),
                rate_per_1000 = as.numeric(rate_per_1000), 
                table = rep(list_name), 
                description = capitalize(gsub(table, pattern = "_", replacement = " "))))  
}))

## push wide for scatterplot ##
cleaned_tables_stacked_wide = dcast(cleaned_tables_stacked, year ~ table, value.var = "rate_per_1000")

##### Plots #####

# lineplot # 
lineplot = ggplot(cleaned_tables_stacked, aes(year, rate_per_1000)) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 11, face = "bold", family = "Garamond"),
    axis.title = element_text(size = 12, face = "bold", family = "Garamond"),
    title = element_text(size = 14, face = "bold", family = "Garamond"),
    strip.text = element_text(size = 12, face = "bold", family = "Garamond"),
    strip.background = element_rect(fill = "lightblue", colour = "black", size = .5),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(size = .50, colour = "steelblue", linetype = "dashed"),
    panel.grid.major.x = element_blank()
    ) +
  labs(
    x = "\nYear\n\nTaylor G. White\nSource: Centers for Disease Control and Prevention\n",
    y = "\nRate Per 1,000 People\n",
    title = "\nNational Marriage and Divorce Rates\nU.S. 2000-2011\n"
    ) +
  facet_wrap(~description, ncol = 1, scales = "free_y") +
  geom_line(size = 1)

# scatterplot # 
scatterplot = ggplot(cleaned_tables_stacked_wide, aes(national_marriage_rate, national_divorce_rate)) +
  theme_bw() +
  theme( 
    axis.text = element_text(size = 11, face = "bold", family = "Garamond"),
    axis.title = element_text(size = 12, face = "bold", family = "Garamond"),
    title = element_text(size = 14, face = "bold", family = "Garamond"),
    strip.text = element_text(size = 12, face = "bold", family = "Garamond"),
    strip.background = element_rect(fill = "lightblue", colour = "black", size = .5)
  ) +  
  labs(
    x = "\nNational Marriage Rate Per 1,000 People\n\nTaylor G. White\nSource: Centers for Disease Control and Prevention\n",
    y = "\nNational Divorce Rate Per 1,000 People\n",
    title = "\nNational Marriage vs. Divorce Rates\nU.S. 2000-2011\n"
  ) +
  stat_smooth(method = "lm", se = F, size = 1, colour = "forestgreen") +
  geom_point()

# export plots #
dir.create(out_directory, recursive = T)

ggsave(paste0(out_directory, "/marriage_vs_divorce_scatter.png"),        
       plot = scatterplot, 
       height = 8, width = 10, units = "in")

ggsave(paste0(out_directory, "/marriage_vs_divorce_lines.png"), 
       plot = lineplot, 
       height = 8, width = 10, units = "in")
