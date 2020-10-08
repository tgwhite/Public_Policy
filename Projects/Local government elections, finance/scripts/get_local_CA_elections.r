library(tidyverse)
library(rvest)
library(readxl)
library(data.table)
library(scales)
library(viridisLite)

# get the competitiveness of various types of races
# figure out the winning vote margin / population for various types of races (over time)
# take a look at los angeles and santa monica data: how many votes does it take to win....
# how many people decide an election in??? This works for congress too
# how many multiple runners are there? can we see people moving up the food chain

# https://csus-dspace.calstate.edu/bitstream/handle/10211.3/210187/Codebook.pdf?sequence=119

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
  
  char_cols = c('totalwritein_votes', 'writein', 'totvotes')
  
  for (col in char_cols) {
    if ('character' %in% class(file_imported[[col]])) {
      file_imported[[col]][file_imported[[col]] == '#NULL!'] = NA
      file_imported[[col]] = as.numeric(file_imported[[col]])
    }
  }
  
  
  file_imported
})
names(election_files_read_in) = excel_files %>% basename() %>% str_remove('[//?]sequence.*') %>% str_remove_all('%20')
names(election_files_read_in)




# 
# # Numeric categories for office – 1 = County Supervisor; 2 = City Council; 3 = School Board
# Member; 4 = CSD/CSA Director; 5 = Other County Office; 6 = Other City Office; 7 = Other
# School District Office.

# newelected: Multi-county outcome for candidate – 1=Elected to office; 2=Not elected to office; 3=Runoff 

last_valid_file = 'CEDA1996Data.xls'
last_file_index = which(names(election_files_read_in) == last_valid_file)

all_files_stacked = election_files_read_in[1:last_file_index] %>% bind_rows() %>%
  mutate(
    date = as.Date(date),
    race_unique = paste(date, raceid),
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
  ) %>% 
  arrange(
    race_unique, -indivtotal_votes
  )

## for each race, get the margin of victory between the lowest winner and highest loser


all_files_stacked_dt = data.table(all_files_stacked)
all_files_stacked_dt$area

## get the number of candidates for a race and keep the top N winners
## find the next lowest

stats_by_race = all_files_stacked_dt[, {
  n_to_vote_for = vote_num[1]
  total_votes = multitotal_votes[1]
  
  n_incumb = length(incumbent_fin[incumbent_fin == 'Y'])
  n_incumb_per_office = n_incumb / vote_num[1]
  n_open_seats = vote_num[1] - n_incumb
  
  top_n_winners = indivtotal_votes[1:n_to_vote_for]
  top_loser = indivtotal_votes[(n_to_vote_for+1)]
  
  lowest_winner = min(top_n_winners)
  loser_margin = lowest_winner - top_loser
  loser_margin_pct = loser_margin / total_votes
  
  list(
    office_type_fin = office_type_fin[1],
    place = place[1],
    area = area[1],
    term = term[1],
    cntyname = cntyname[1],
    date = date[1],
    office = office[1],
    recode_offname = recode_offname[1],
    total_votes = total_votes, 
    n_to_vote_for = n_to_vote_for,
    n_candidates = cand_num[1],
    n_incumb = n_incumb,
    n_incumb_per_office = n_incumb_per_office,
    n_candidates_per_office = cand_num[1] / n_to_vote_for,
    n_open_seats = n_open_seats,
    avg_winner = mean(top_n_winners),
    sd_winner = sd(top_n_winners),
    median_winner = median(top_n_winners), 
    lowest_winner = lowest_winner,
    top_loser = top_loser,
    loser_margin = loser_margin,
    loser_margin_pct = loser_margin_pct,
    loser_difference_median_winner = loser_margin / median(top_n_winners),
    avg_votes_per_office = total_votes / n_to_vote_for
  )
  
}, by = list(race_unique)] %>%
  mutate(
    entity_name = str_to_title(place),
    year = year(date),
    office_pretty = str_to_title(office_type_fin)
  )

write.csv(stats_by_race, 'data/ca_election_results_by_race.csv', row.names = F)
write.csv(all_files_stacked, 'data/ca_all_election_results.csv', row.names = F)

head(stats_by_race)


stats_by_race_year = group_by(stats_by_race, year, office_type_fin) %>%
  summarize(
    mean_n_candidates_per_office = mean(n_candidates_per_office),
    mean_loser_margin_pct = mean(loser_margin_pct, na.rm = T),
    n_races = n()
  ) %>%
  ungroup() %>%
  mutate(
    odd_year = year %% 2 == 1
  )
ggplot(stats_by_race_year, aes(year, mean_loser_margin_pct)) +
  facet_wrap(~office_type_fin) +
  geom_line(aes(colour = office_type_fin)) +
  geom_point(aes(shape = odd_year, size = mean_n_candidates_per_office))



santa_monica_elections = filter(stats_by_race, str_detect(place %>% tolower(), 'monica'))
write.csv(santa_monica_elections, 'data/santa_monica_elections.csv')

ggplot(santa_monica_elections, aes(race_unique, loser_margin_pct, fill = n_candidates_per_office)) +
  geom_bar(stat = 'identity') +
  # geom_point() +
  # geom_pointrange(aes(ymin = 0, ymax = loser_margin_pct)) +
  geom_text(aes(label = comma(loser_margin)), vjust = -0.5) +
  scale_y_continuous(labels = percent) +
  labs(
    y = 'Minimum Winning Margin (%)\nHigher values are less competitive', x = '',
    title = 'Competitiveness of Santa Monica City Council Elections, 1996-2018',
    subtitle = 'Minimum winning margin is the difference in votes between the winning candidate with the least votes and losing candidate with the most votes as a percentage of all votes cast.' %>%
      str_wrap(120),
    caption = 'Chart: Taylor G. White\nData: California Elections Data Archive (CEDA)'
  ) +
  scale_x_discrete(labels = with(santa_monica_elections, paste(year(date), term, 'Term'))) +
  scale_fill_viridis_c(name = 'Candidates Per\nOpen Seat', option = 'B') +
  # scale_x_date(breaks = unique(santa_monica_elections$date), date_labels = '%b-%y') +
  theme(
    legend.position = 'right',
    axis.text.x = element_text(angle = 45),
    plot.subtitle = element_text(face = 'italic'),
    plot.caption = element_text(face = 'italic', hjust = 0)
  )
ggsave('output/santa_monica_council_competitiveness.png', height = 8, width = 10, units = 'in', dpi = 800)
   
