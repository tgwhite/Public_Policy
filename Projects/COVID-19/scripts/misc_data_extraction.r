
library(jsonlite)

setwd("~/Public_Policy/Projects/COVID-19")
list.files('data', pattern = 'json')
test_sites_in = fromJSON("data/EVIVE_scarped_COVID19TestingSites.json")

stacked_testing_info = map(test_sites_in, function(inner_list){
  if ('data.frame' %in% class(inner_list)) {
    if ('location' %in% names(inner_list)) {
      return(inner_list)
    } else {
      return(NULL)
    }
  } else {
    return(NULL)
  }
}) %>% 
  bind_rows()
head(stacked_testing_info)
stacked_testing_info$location
write.csv(stacked_testing_info, 'data/cleaned_testing_sites.csv', row.names = F)