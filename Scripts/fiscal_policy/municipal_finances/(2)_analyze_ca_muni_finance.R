
# Libraries and options ---------------------------------------------------

library(ggplot2)
library(dplyr)
library(stringr)
library(reshape2)
library(data.table)
library(bit64)
library(extrafont)
library(scales)
library(GGally)

options(stringsAsFactors = F)


# Helper functions --------------------------------------------------------

source("C:/Users/taylor/Dropbox/WildPolicy/Public/Scripts/public_data_downloads/get_FRED_county_data_CA.R")

# Helper parameters -------------------------------------------------------

run_data_import_cleaning = T

if (run_data_import_cleaning) {
  
  # Download FRED data by county --------------------------------------------
  
  population_by_county <- get_FRED_county_data_CA("population")
  
  # Read in general finance data for counties ---------------------------------
  
  setwd("C:/Users/taylor/Dropbox/WildPolicy/Data/Community Data/municipal_finance/ca_county_data_03_13")
  
  CI_EXP_GEN_GOV = fread("CO_EXP_GENERAL.csv")
  
  setnames(CI_EXP_GEN_GOV, 
           names(CI_EXP_GEN_GOV), 
           str_replace_all(names(CI_EXP_GEN_GOV), pattern = fixed(" "), replacement = "_"))
  
  # Update FRED downloads and merge with general exp ------------------------
  
  # lag population to sync with fiscal year idea (population reported at year start, not year end as fiscal years typically are)
  
  population_by_county_upd <- 
    population_by_county %>%  
    mutate(
      Entity_Name = apply(matrix(Description), 1, function(row){    
        str_split(row, pattern = "in") %>%      
          unlist() %>%
          tail(1) %>%
          str_trim() %>%
          str_replace_all(pattern = fixed(" County, CA"), replacement = "")}), 
      Entity_Name = ifelse(str_detect(Entity_name, "Francisco"), "San Francisco", Entity_Name), # this county dataset doesn't actually have SF
      Fiscal_Year = year(Date), 
      current_population_actual = Value * 1e3      
    ) 
  
  ## apply lead operation by county
  
  population_lead_one <-
    population_by_county_upd[, {
      list(
        Fiscal_Year = Fiscal_Year, 
        current_population_actual = current_population_actual, 
        lead_pop_actual = dplyr::lead(current_population_actual, 1)        
        )
    }, by = Entity_Name]
  
  # merge by county/year  
  CI_EXP_GEN_GOV_POP <- 
    left_join(CI_EXP_GEN_GOV, population_lead_one) %>%    
    melt(
      id = c("Entity_Name", "Fiscal_Year", "current_population_actual", "lead_pop_actual", "Entity_ID")
    ) %>%
    mutate(
      val_per_capita = value/lead_pop_actual, 
      variable = as.character(variable)
    ) %>%
    filter(!(is.na(value)))
  
  # export in order to avoid having to re-download FRED data
  write.csv(CI_EXP_GEN_GOV_POP, "cleaned_CI_EXP_GEN_GOV_POP.csv", row.names = F) 
  rm(list = subset(ls(), !(ls() %in% "CI_EXP_GEN_GOV_POP")))
}


# Take a look -------------------------------------------------------------

# re import if necessary 

if (!("CI_EXP_GEN_GOV_POP" %in% ls())) {
  setwd("C:/Users/taylor/Dropbox/WildPolicy/Data/Community Data/municipal_finance/ca_county_data_03_13")
  CI_EXP_GEN_GOV_POP <- fread("cleaned_CI_EXP_GEN_GOV_POP.csv")
}


## update variable names
CI_EXP_GEN_GOV_POP <- 
  mutate(CI_EXP_GEN_GOV_POP, 
         variable_pretty = str_replace_all(variable, pattern = "_", " ")
         )

setkey(CI_EXP_GEN_GOV_POP, variable)
  
# get overall summary stats # 

# stats for all variables 
stats_by_var_all<-
  CI_EXP_GEN_GOV_POP[, {
    list(
      mean_val_per_capita = mean(val_per_capita, na.rm = T),     
      sd_val_per_capita = sd(val_per_capita, na.rm = T), 
      Q10_val_per_capita = quantile(val_per_capita, 0.10, na.rm = T),
      Q25_val_per_capita = quantile(val_per_capita, 0.25, na.rm = T), 
      median_val_per_capita = median(val_per_capita, na.rm = T), 
      Q75_val_per_capita = quantile(val_per_capita, 0.75, na.rm = T),
      Q90_val_per_capita = quantile(val_per_capita, 0.90, na.rm = T)        
    )
  }, by = variable] %>%
  arrange(desc(median_val_per_capita))

# take a look 
head(stats_by_var_all, 10) # top ten categories
tail(stats_by_var_all, 10) # bottom ten categories

## Which variables are available (and interesting)?

# It looks like the variables are split by "total" , "capital outlay", and "operating expenditures"

vars_of_interest <- 
  c("Personnel_Total", "Total_Plant_Acquisition_Total", 
    "Total_Finance_Total", "Property_Management_Total", 
    "Jails_Operating_Expenditures", "Total_Legislative_and_Administrative_Total"    
  )

# subset to selected variables #
CI_EXP_GEN_GOV_POP_SUB <- 
  CI_EXP_GEN_GOV_POP[J(vars_of_interest)] 

# overall stats for selected vars
stats_by_var_sub <-
  CI_EXP_GEN_GOV_POP_SUB[, {
    list(
      mean_val_per_capita = mean(val_per_capita, na.rm = T),     
      sd_val_per_capita = sd(val_per_capita, na.rm = T), 
      Q10_val_per_capita = quantile(val_per_capita, 0.10, na.rm = T),
      Q25_val_per_capita = quantile(val_per_capita, 0.25, na.rm = T), 
      median_val_per_capita = median(val_per_capita, na.rm = T), 
      Q75_val_per_capita = quantile(val_per_capita, 0.75, na.rm = T),
      Q90_val_per_capita = quantile(val_per_capita, 0.90, na.rm = T)        
    )
  }, by = variable_pretty]
  
# stats over time
stats_by_var_year <-
  CI_EXP_GEN_GOV_POP_SUB[, {
    list(
      mean_val_per_capita = mean(val_per_capita, na.rm = T),     
      sd_val_per_capita = sd(val_per_capita, na.rm = T), 
      Q10_val_per_capita = quantile(val_per_capita, 0.10, na.rm = T),
      Q25_val_per_capita = quantile(val_per_capita, 0.25, na.rm = T), 
      median_val_per_capita = median(val_per_capita, na.rm = T), 
      Q75_val_per_capita = quantile(val_per_capita, 0.75, na.rm = T),
      Q90_val_per_capita = quantile(val_per_capita, 0.90, na.rm = T)        
    )
  }, by = list(variable_pretty, Fiscal_Year)]


# plot overall distributions of selected variables , 
density_for_selected_vars <- 
  ggplot(CI_EXP_GEN_GOV_POP_SUB, aes(val_per_capita)) +
  facet_wrap(~variable_pretty, ncol = 2, scales = "free_y") +
  theme(
    axis.text = element_text(size = 11, face = "bold", family = "Garamond"),
    strip.text = element_text(size = 12, face = "bold", family = "Garamond"),
    axis.title = element_text(size = 12, face = "bold", family = "Garamond"),
    title = element_text(size = 14, face = "bold", family = "Garamond")
    ) +
  labs(
    x = "\nExpenditures Per Capita\n", 
    y = "\nDensity Estimate\n", 
    title = paste0("\nDistributions of County-Level Expenditures\nCalifornia: ", 
                   min(CI_EXP_GEN_GOV_POP_SUB$Fiscal_Year, na.rm = T), "-", 
                   max(CI_EXP_GEN_GOV_POP_SUB$Fiscal_Year, na.rm = T), "\n")
    ) +  
  scale_x_continuous(labels = dollar, limits = c(0, 150)) +
  stat_density(position = "identity") 

# plot trends in expenditures over time
trends_for_selected_vars <- 
  ggplot(stats_by_var_year, aes(Fiscal_Year, median_val_per_capita, ymin = Q25_val_per_capita, ymax = Q75_val_per_capita)) +
  facet_wrap(~variable_pretty, ncol = 2, scales = "free_y") +  
  theme(
    axis.text = element_text(size = 11, face = "bold", family = "Garamond"),
    strip.text = element_text(size = 12, face = "bold", family = "Garamond"),
    axis.title = element_text(size = 12, face = "bold", family = "Garamond"),
    title = element_text(size = 14, face = "bold", family = "Garamond")
  ) +
  labs(
    y = "\nMedian Expenditures Per Capita\n", 
    x = "\nFiscal Year\n\nRibbons Represent the 25th and 75th Percentiles", 
    title = paste0("\nTrends in County-Level Expenditures\nCalifornia: ", 
                   min(CI_EXP_GEN_GOV_POP_SUB$Fiscal_Year, na.rm = T), "-", 
                   max(CI_EXP_GEN_GOV_POP_SUB$Fiscal_Year, na.rm = T), "\n")
  ) +  
  scale_y_continuous(labels = dollar) +
  scale_x_continuous(breaks = seq(min(CI_EXP_GEN_GOV_POP_SUB$Fiscal_Year, na.rm = T), 
                                  max(CI_EXP_GEN_GOV_POP_SUB$Fiscal_Year, na.rm = T), by = 2)) +
  geom_ribbon(fill = "steelblue", alpha = .6) +
  geom_line(colour = "navyblue", size = 1) 


# Export data and plots ---------------------------------------------------

# data 
write.csv(CI_EXP_GEN_GOV_POP, 
          "C:/Users/taylor/Dropbox/WildPolicy/Public/Data/fiscal_policy/municipal_finance/CI_EXP_GEN_GOV_POP.csv", row.names = F)

# plots
county_finance_plot_list = list(density_for_selected_vars = density_for_selected_vars, 
                                trends_for_selected_vars = trends_for_selected_vars)

setwd("C:/Users/taylor/Dropbox/WildPolicy/Public/Output/fiscal_policy/municipal_finance")

save(county_finance_plot_list, file = "county_finance_plot_list.rdata")
