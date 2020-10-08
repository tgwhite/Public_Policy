
library(tidyverse)
library(data.table)
library(fastLink)
library(readxl)
# contribution records downloaded from here: 
# https://public.netfile.com/pub2/Default.aspx?aid=CSM&AspxAutoDetectCookieSupport=1
setwd("~/Public_Policy/Projects/Local government elections, finance/data")
ca_all_election_results = fread('ca_all_election_results.csv') %>%
  mutate(
    basic_committee_name= paste(str_to_title(last), office_type_fin),
    first_name_clean = str_extract(first, '[a-zA-Z]+[ ]*') %>% str_trim(),
    full_name = paste(first_name_clean, last) %>% str_trim(),
    date = as.Date(date)
  )

sm_results = filter(ca_all_election_results, str_to_title(place) %>% str_trim() == 'Santa Monica')
sm_elections_since_2010 = filter(sm_results, year(date) >= 2010)
sm_elections_since_2010 %>% select(contains('first'), full_name)

current_sm_council = filter(sm_results, year(date) >= 2016, elected_to_office)


setwd('~/Public_Policy/Projects/Local government elections, finance/data/santa monica political contributions')

zipped_filelist = list.files(pattern = 'zip')
# lapply(zipped_filelist, unzip, exdir = getwd())
contribution_excel_files = list.files(pattern = 'efile.+xlsx')

stacked_contributions = map(contribution_excel_files, function(the_file){
  read_excel(the_file, "A-Contributions") %>% 
    mutate(
      filename = the_file,
      Tran_Type = as.character(Tran_Type)
    )
}) %>%
  bind_rows()

stacked_expenditures = map(contribution_excel_files, function(the_file){
  read_excel(the_file, "E-Expenditure") %>% 
    mutate(
      filename = the_file
    )
}) %>%
  bind_rows()

stacked_summaries = map(contribution_excel_files, function(the_file){
  read_excel(the_file, "Summary") %>% 
    mutate(
      filename = the_file
    )
}) %>%
  bind_rows()


police_exp = filter(stacked_expenditures, str_detect(Filer_NamL %>% tolower(), 'police'))
write.csv(police_exp, 'sm_police_exp.csv', row.names = F)

candidate_committees = filter(stacked_contributions, Committee_Type == 'CTL') %>% select(Filer_NamL, Filer_ID) %>% unique() %>%
  mutate(
    basic_committee_name = Filer_NamL %>% str_to_title()
  )
  
current_sm_council_matches = lapply(1:nrow(current_sm_council), function(it){
  the_row = current_sm_council[it,]
  matches = filter(candidate_committees, str_detect(Filer_NamL %>% tolower(), 'council'), 
                   str_detect(Filer_NamL %>% tolower(), tolower(the_row$last))
                   )
  if (nrow(matches) == 0) {
    matches = filter(candidate_committees, str_detect(Filer_NamL %>% tolower(), 'council'), 
                     str_detect(Filer_NamL %>% tolower(), tolower('Vazquez'))
    )
    
  }
  # bind_cols(matches, the_row) 
  matches$last = the_row$last
  matches
}) %>% bind_rows() %>%
  filter(Filer_NamL != 'Shari Davis for City Council 2012')


current_sm_council$basic_committee_name = current_sm_council$last
linked_committees = fastLink(
  dfA = current_sm_council,
  dfB = candidate_committees,
  varnames = c('basic_committee_name'),
  stringdist.match = c("basic_committee_name"),
  threshold.match = 0.1
)

deduped_committees <- getMatches(dfA = current_sm_council, dfB = candidate_committees, fl.out = linked_committees)


# 
# distinct_contributors = select(stacked_contributions, starts_with('Tran'), -Tran_ID, 
#                              -contains('date'), -contains('amt'), -Tran_Self, -Tran_Type, -Tran_Dscr) %>%
#   unique() %>%
#   filter(!is.na(Tran_NamL)) %>%
#   mutate(
#     Tran_Zip4 = str_sub(Tran_Zip4, 1, 5),
#     Tran_Zip4_Num = as.numeric(Tran_Zip4)
#   )

# 
# linked_contributors = fastLink(
#   dfA = distinct_contributors, 
#   dfB = distinct_contributors, 
#   varnames = c('Tran_NamL', 'Tran_NamF', 'Tran_City', 'Tran_Occ'),
#   stringdist.match = c("Tran_NamL", "Tran_NamF",  "Tran_Occ"),
#   partial.match = c('Tran_Occ')
# )

# deduped_contributors <- getMatches(dfA = distinct_contributors, dfB = distinct_contributors, fl.out = linked_contributors)

# deduped_contributors_dedupe[deduped_contributors_dedupe$dedupe.ids == 3359,]

##### get police contributions #####

#### get all council contributions ##### 

# cycle through candidates and determine whether police have offered support

setwd('~/Public_Policy/Projects/Local government elections, finance/data/santa monica political contributions')

matching_police_expenditures = map(1:nrow(sm_elections_since_2010), function(it){
  # it = 1
  the_row = sm_elections_since_2010[it,]
  
  exp_for_election = filter(police_exp, year(From_Date) == the_row$year | year(Thru_Date) == the_row$year) %>%
    filter(str_detect(Payee_NamL %>% tolower(), the_row$full_name %>% tolower()) | str_detect(Expn_Dscr %>% tolower(), the_row$full_name %>% tolower()))
  
  # exp_for_election$Payee_NamL
  # exp_for_election$Expn_Dscr
  # select(exp_for_election, )
  exp_for_election$election_record = the_row$recordid
  return(exp_for_election)
  }) %>% 
  bind_rows() %>%
  data.table() %>%
  unique(by = c('election_record', 'Tran_ID'))



sm_elections_police_exp = left_join(sm_elections_since_2010, matching_police_expenditures, by = c('recordid' = 'election_record')) %>%
  select(recordid, date,full_name, last, first, Payee_NamL, Expn_Dscr, baldesig, elected_to_office, incumb, office_type_fin, 
         Filer_ID, Filer_NamL, From_Date, Thru_Date, Rec_Type, Tran_ID, Entity_Cd, Payee_NamF, Expn_Date, Expn_Code, Amount)
write.csv(sm_elections_police_exp, 'sm_elections_police_exp.csv', row.names = F)

police_support_summary_stats = group_by(sm_elections_police_exp, recordid, full_name, last, date, elected_to_office) %>%
  summarize(
    any_police_support = length(Amount[!is.na(Amount)]) > 0,
    value_of_support = sum(Amount, na.rm = T), 
  ) 

current_council_police_support = inner_join(current_sm_council, police_support_summary_stats)

total_police_support_since_2010 = group_by(police_support_summary_stats, full_name) %>%
  summarize(
    total_support = sum(value_of_support),
    n_elections_with_support = n_distinct(recordid[any_police_support])
  ) %>%
  arrange(-total_support)

current_council_support_stats = inner_join(current_sm_council, total_police_support_since_2010)

ggplot(current_council_support_stats, aes(full_name, total_support)) +
  geom_bar(stat = 'identity')

current_council_support = filter(police_support_summary_stats, full_name %in% current_sm_council$full_name)
ggplot(current_council_support, aes(date, full_name)) +
  geom_point(aes(shape = elected_to_office, colour = any_police_support), size = 8) +
  # scale_size(range = c(2, 8)) +
  scale_x_date(date_labels = '%b-%y', breaks = unique(police_support_summary_stats$date)) +
  theme(
    panel.grid.minor = element_blank()
  ) +
  scale_shape(name = 'Elected to Office') +
  scale_colour_manual(name = 'Supported by Police', values = c('TRUE' = 'steelblue', 'FALSE' = 'orange')) +
  labs(
    x = '', y = '',
    caption = 'Tony Vasquez was elected in 2016 but left office in 2018, whose seat has since been filled by appointment (Ana Maria Jara).'
  ) +
  guides(
    shape = guide_legend(override.aes = list(size = 6)),
    colour = guide_legend(override.aes = list(size = 6))
  )
setwd('~/Public_Policy/Projects/Local government elections, finance')
ggsave('output/current_council_police_support.png', height = 8, width = 10, units = 'in', dpi = 800)


ggplot(current_council_support, aes(date, value_of_support, fill = full_name)) +
  geom_bar(stat = 'identity', position = 'dodge', size = 0.5) +
  scale_x_date(date_labels = '%b-%y', breaks = unique(police_support_summary_stats$date)) 
  

ggplot(police_support_summary_stats %>% filter(any_police_support), aes(date, full_name)) +
  geom_point(aes(size = value_of_support, colour = elected_to_office)) 
  
write.csv(police_support_summary_stats, 'police_support_summary_stats.csv', row.names = F)
group_by(police_support_summary_stats, any_police_support) %>%
  summarize(
    pct_elected = mean(elected_to_office),
    obs = n()
  )


shell('explorer . ')


##### top expenditure committees #####
top_exp = 
  mutate(stacked_expenditures, year = year(From_Date)) %>%
  filter(year >= 2010) %>%
group_by(Filer_ID) %>%
  summarize(
    Filer_NamL = Filer_NamL[1],
    sum_exp = sum(Amount, na.rm = T)
  ) %>%
  arrange(-sum_exp) %>%
  ungroup() %>%
  head(20) %>%
  mutate(filer_factor = factor(Filer_NamL %>% str_wrap(40) %>% str_to_title(), 
                               levels = rev(Filer_NamL %>% str_wrap(40) %>% str_to_title())))

top_exp$is_police = top_exp$Filer_NamL == 'Santa Monica Police Officers Association for a Better Community'
top_exp$filer_factor
non_police_vals = filter(top_exp, !is_police)$filer_factor
non_police_cols = rep("gray", length(non_police_vals))
names(non_police_cols) = non_police_vals
ggplot(top_exp, aes(filer_factor, sum_exp, fill = filer_factor)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = dollar(sum_exp)), hjust = 1, size = 2.5) +
  scale_fill_manual(guide = F, values = c('Santa Monica Police Officers Association\nFor A Better Community' = 'orange', non_police_cols)) +
  coord_flip() +
  theme_bw() +
  scale_y_continuous(labels = dollar) +
  labs(
    y = 'Total Expenditures Since 2010',
    x = '',
    title = 'Top-20 Political Action Commmittees by Expenditures Since 2010',
    subtitle = 'PACs Filing with Santa Monica'
  )

setwd('~/Public_Policy/Projects/Local government elections, finance')
ggsave('output/santa_monica_expenditures_since_2010.png', height = 8, width = 10, units = 'in', dpi = 800)
