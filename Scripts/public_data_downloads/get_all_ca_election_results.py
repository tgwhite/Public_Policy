
import pandas as pd
import os
import requests
from bs4 import BeautifulSoup

statewide_base_url = 'http://statewidedatabase.org/'

## MAIN PAGE SOUP
state_soup = BeautifulSoup(requests.get(statewide_base_url + 'data.html').content)

## FIND ELECTION PRECINCT DATA
list_elements = state_soup.findAll('ul')

election_precinct_items = [x for x in list_elements if 'Election Precinct' in x.get_text()][0]
precinct_links = [x for x in election_precinct_items.findAll('li') if 'Election Precinct' in x.get_text()]

## LOOP OVER PRECINCT DATA TO GET OUTER PAGE WITH ELECTION YEAR SELECTIONS

for election_link in precinct_links: 

	link = election_link.find('a').get('href')

	page_soup = BeautifulSoup(requests.get(statewide_base_url + link).content)
	data_page_links = [x for x in page_soup.findAll('a') if 'Election Data' in x.get_text('')]

	## LOOP OVER DATA PAGE LINKS TO GET COUNTY LEVEL ELECTION RESULTS 
	for this_link in data_page_links: 
		
		election_desc = this_link.get_text()

		os.makedirs('CA' + election_desc)
		os.chdir('CA' + election_desc)

		full_data_page_link = statewide_base_url + link.replace('index.html', this_link.get('href'))

		data_page_soup = BeautifulSoup(requests.get(full_data_page_link).content)
		data_page_rows = [x for x in data_page_soup.findAll('tr') if 'County' in x.get_text()]


		## LOOP OVER RESULTS FOR EACH COUNTY
		for county_row in data_page_rows: 
			
			county_name = county_row.find('th').get_text()

			## get codebook data then SOV by srprec, contained in first table data element
			SOV_dat = county_row.find('td')

			if 'no data' in SOV_dat.get_text(): 
				continue

			codebook_path = statewide_base_url + [x.get('href') for x in SOV_dat.findAll('a') if 'codes' in x.get('href')][0]
			srprec_path = statewide_base_url + [x.get('href') for x in SOV_dat.findAll('a') if 'sov_data' in x.get('href') and 'srprec' in x.get('href')][0]

			## READ IN CODEBOOK AND SRPREC RESULTS
			codebook_df = pd.read_csv(codebook_path, sep = '\t', header = -1)
			codebook_df.columns = ['variable', 'description', 'value']

			srprec_df = pd.read_csv(srprec_path)

			## EXPORT EVERYTHING			
			codebook_df.to_csv(county_name + 'codebook.csv')
			srprec_df.to_csv(county_name + 'srprec_SOV.csv')
		
		os.chdir('..')

