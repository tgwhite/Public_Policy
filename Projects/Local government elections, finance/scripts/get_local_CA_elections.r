library(tidyverse)
library(rvest)
library(readxl)

setwd("~/Public_Policy/Projects/Local government elections, finance")
download_election_files = F

base_url = 'https://csus-dspace.calstate.edu/'

local_election_archive = read_html('https://csus-dspace.calstate.edu/handle/10211.3/210187') %>%
  html_nodes('.file-wrapper') %>%
  html_nodes('li') %>%
  html_nodes('a') %>%
  html_attr('href')

excel_files = local_election_archive[str_detect(local_election_archive, '.xls')]

election_files_read_in = map(excel_files, function(the_filename){
  out_filename = basename(the_filename) %>% str_remove('[//?]sequence.*') %>% str_remove_all('%20')
  full_out_filename = paste0('data/ca_local_election_data/', out_filename)
  
  if (download_election_files) {
    download.file(paste0(base_url, the_filename %>% str_remove('[//?]sequence.*')), 
                  destfile = full_out_filename,
                  mode = 'wb')  
  }
  
  candidates_sheet = excel_sheets(full_out_filename)
  candidates_sheet = candidates_sheet[str_detect(candidates_sheet %>% tolower(), 'cand')]
  file_imported = read_excel(full_out_filename, candidates_sheet)
  names(file_imported) = str_replace(names(file_imported), '#', '_num') %>% tolower()
  file_imported
})
names(election_files_read_in) = excel_files %>% basename() %>% str_remove('[//?]sequence.*') %>% str_remove_all('%20')
names(election_files_read_in)




# 
# # Numeric categories for office – 1 = County Supervisor; 2 = City Council; 3 = School Board
# Member; 4 = CSD/CSA Director; 5 = Other County Office; 6 = Other City Office; 7 = Other
# School District Office.

# newelected: Multi-county outcome for candidate – 1=Elected to office; 2=Not elected to office; 3=Runoff 

all_files_stacked = election_files_read_in[1:7] %>% bind_rows() %>%
  mutate(
    race_unique = paste(raceid, year),
    judge = office %>% tolower() %>% str_detect('judge'),
    attorney = office %>% tolower() %>% str_detect('attorney|prosecutor'),
    mayor = office %>% tolower() %>% str_detect('mayor'),
    sheriff = office %>% tolower() %>% str_detect('sheriff'),
    office_type = ifelse(judge, 'Judge', ifelse(mayor, 'Mayor', ifelse(sheriff, 'Sheriff', ifelse(attorney, 'Attorney', 
                                                                                                  recode_office)))),
    office_type_fin = recode(office_type, `1` = 'Supervisor', `2` = 'City Council'),
    elected_to_office = newelected == 1,
    incumbent_fin = ifelse(is.na(incumb), 'Unknown', incumb),
    n_cand_per_office = cand_num / vote_num,
    uncontested = cand_num == vote_num
  ) %>% 
  filter(
    !office_type_fin %in% paste0(3:6) & !is.na(office_type_fin),
    newelected != 3 # remove runoffs, about 3% of sample
  )

stats_by_race = group_by(all_files_stacked, race_unique, office_type_fin, year) %>%
  summarize(
    candidates_per_office = cand_num[1] / vote_num[1],
    n_incumb = length(incumbent_fin[incumbent_fin == 'Y']),
    n_incumb_per_office = n_incumb / vote_num[1],
    n_open_seats = vote_num[1] - n_incumb
  ) %>%
  group_by(
    office_type_fin
  ) %>%
  summarize(
    mean_cand_per_office = mean(candidates_per_office, na.rm = T),
    median_cand_per_office = median(candidates_per_office, na.rm = T),
    mean_open_seats = mean(n_open_seats, na.rm = T),
    mean_n_incumb_per_office = mean(n_incumb_per_office, na.rm = T)
  ) %>%
  ungroup() %>%
  arrange(-mean_cand_per_office) %>%
  mutate(office_type_fact = factor(office_type_fin, levels= office_type_fin))

ggplot(stats_by_race, aes(office_type_fact, mean_cand_per_office, fill = office_type_fin)) +
  geom_bar(stat = 'identity',  position = 'dodge', show.legend = F) +
  geom_text(aes(label = mean_cand_per_office %>% round(1)))

  
# get stats by date_r

head(all_files_stacked)
all_files_stacked$incumb %>% table()
