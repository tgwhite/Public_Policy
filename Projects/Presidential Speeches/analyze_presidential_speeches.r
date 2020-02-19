
library(tidyverse)
library(quanteda)
library(topicmodels)
library(tm)
library(tidytext)
library(textmineR)
library(data.table)
library(gridExtra)
# SnowballC

setwd("~/Public_Policy/Projects/Presidential Speeches")

all_campaign_docs_stacked = readRDS('data/all_campaign_docs_stacked.rds') %>%
  filter(
    str_trim(content) != ''
  ) %>%
  mutate(
    date_clean = as.Date(date, format = '%B %d, %Y')
  ) %>%
  data.table() %>% 
  unique(by = 'url')

top_2016_candidates = filter(all_campaign_docs_stacked_sub, year(date_clean) == 2016) %>% pull(person_name) %>% table() %>% sort() %>%
  tail(3)

all_campaign_docs_stacked_sub = filter(all_campaign_docs_stacked, person_name %in% names(top_2016_candidates))

the_stop_words = c(stopwords::stopwords("en"),
  stopwords::stopwords(source = "smart"), 'applause')

dtm <- CreateDtm(doc_vec = all_campaign_docs_stacked_sub$content,
                 doc_names = all_campaign_docs_stacked_sub$url, 
                 ngram_window = c(1, 1),
                 stopword_vec = the_stop_words,
                 stem_lemma_function = function(x) SnowballC::wordStem(x, "porter"))

the_lda = LDA(dtm, k = 10)
the_lda_tidy = tidy(the_lda, matrix = 'beta') %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(
    term = reorder_within(term, beta, topic)
  )

the_lda_tidy_doc = tidy(the_lda, matrix = 'gamma') %>%
  inner_join(all_campaign_docs_stacked_sub %>% select(url, person_name), by = c('document' = 'url'))

counts_by_topic = group_by(the_lda_tidy_doc, topic, person_name) %>%
  summarize(
    avg_gamma = mean(gamma)
  )

main_plot = ggplot(counts_by_topic, aes(factor(topic), avg_gamma, fill = person_name)) +
  geom_bar(stat = 'identity', colour = 'black') 



sub_plot = ggplot(the_lda_tidy, aes(term, beta)) +
  facet_wrap(~topic, ncol = 3, scales = 'free') +
  geom_bar(stat = 'identity') +
  scale_x_reordered() +
  coord_flip() 


grid.arrange(main_plot, sub_plot, heights = unit(c(3, 6), 'in'))
# 
# reut21578 <- system.file("texts", "crude", package = "tm")
# reuters <- VCorpus(DirSource(reut21578, mode = "binary"),
#                      readerControl = list(reader = readReut21578XMLasPlain))
# 
# as.VCorpus(all_campaign_docs_stacked$content)
# ?VCorpus
# a = VCorpus(all_campaign_docs_stacked$content)
# reuters <- tm_map(reuters, stripWhitespace)
# reuters <- tm_map(reuters, content_transformer(tolower))
# reuters <- tm_map(reuters, removeWords, stopwords("english"))
# tm_map(reuters, stemDocument)
# dtm <- DocumentTermMatrix(reuters)
# inspect(dtm)


names(the_lda)
the_lda
ap_topics <- tidy(the_lda, matrix = "beta") %>% arrange(-beta)
ap_topics




coleman.liau.grade = 
  textstat_readability(all_campaign_docs_stacked$content, 
    measure = 'Coleman.Liau.grade')

ELF = 
  textstat_readability(all_campaign_docs_stacked$content, 
                       measure = 'ELF')

Flesch = 
  textstat_readability(all_campaign_docs_stacked$content, 
                       measure = 'Flesch')


coleman.liau.grade %>% head()
all_campaign_docs_stacked$coleman.liau.grade = coleman.liau.grade[[2]]
all_campaign_docs_stacked$ELF = ELF[[2]]
all_campaign_docs_stacked$Flesch = Flesch[[2]]

all_campaign_docs_stacked$content = NULL

ggplot(all_campaign_docs_stacked, aes(date_clean, coleman.liau.grade)) +
  geom_point() +
  stat_smooth() +
  coord_cartesian(x = c('2000-01-01', '2020-01-01') %>% as.Date())
head(all_campaign_docs_stacked)

group_by(all_campaign_docs_stacked, person_name) %>%
  summarize(
    mean_grade_level = mean(coleman.liau.grade)
  ) %>% View()
