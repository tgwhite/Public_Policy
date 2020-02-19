
library(tidyverse)
library(rvest)
library(httr)

setwd("~/Public_Policy/Projects/Presidential Speeches")

document_base_url = 'https://www.presidency.ucsb.edu'

get_campaign_document_links = function(){
  base_url = 'https://www.presidency.ucsb.edu/documents/app-categories/elections-and-transitions/campaign-documents?items_per_page=60'
  
  link_list = list()
  for (page_it in 0:100) {
    
    if (page_it == 0) {
      the_url = base_url
    } else {
      the_url = paste0(base_url, '&page=', page_it)
    }
    campaign_speeches_html = read_html(the_url)
    links = html_nodes(campaign_speeches_html, '.field-title')
    
    if (length(links) == 0) {
      cat('no more links at page', page_it, '\n')
      break
    }
    
    link_titles = html_text(links) %>% str_trim()
    inner_links = html_nodes(links, 'a') %>% html_attr('href')
    link_df = tibble(title = link_titles, link = inner_links, url = the_url)
    
    Sys.sleep(0.25) # sleep to avoid overwhelming the server with requests
    link_list[[length(link_list) + 1]] = link_df
  }
  
  stacked_links = bind_rows(link_list)
  return(stacked_links)  
}

campaign_document_links = get_campaign_document_links()


all_docs_read_in = map(campaign_document_links$link, function(the_link){
  print(which(the_link == campaign_document_links$link)/length(campaign_document_links$link))
  the_url = paste0(document_base_url, the_link)
  doc_html = read_html(the_url)
  
  person_name = html_node(doc_html, '.field-docs-person') %>% html_node('.field-title') %>% html_text() %>% str_trim()
  person_title = html_node(doc_html, '.field-docs-person') %>% html_node('.field-ds-byline') %>% html_text() %>% str_trim()
  doc_title = html_node(doc_html, '.field-docs-person') %>% html_node('.field-ds-doc-title') %>% html_text() %>% str_trim()
  doc_time = html_node(doc_html, '.field-docs-start-date-time') %>% html_text() %>% str_trim()
  citation = html_node(doc_html, '.field-prez-document-citation') %>% html_text() %>% str_trim()
  doc_content = html_node(doc_html, '.field-docs-content') %>% html_text()
  
  doc_df = tibble(
    url = the_url,
    person_name, 
    person_title, 
    title = doc_title,
    date = doc_time,
    citation,
    content = doc_content
  )
  Sys.sleep(0.25) 
  return(doc_df)
})

all_docs_stacked = bind_rows(all_docs_read_in)
# write.csv(all_docs_stacked, 'data/ucsb_campaign_speeches_documents.csv', row.names = F)
saveRDS(all_docs_stacked, 'data/all_campaign_docs_stacked.rds')


head(all_docs_stacked)
all_docs_stacked$content[1]
