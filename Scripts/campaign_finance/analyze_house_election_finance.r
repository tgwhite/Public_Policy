
library(plyr)
library(dplyr)
library(data.table)
library(stringr)
library(httr)
library(zipcode)
library(maps)
library(ggplot2)
library(scales)

setwd('~')
setwd('../Downloads')

## get zcta to congressional district mappings ## 
zcta_to_cd = fread('http://www2.census.gov/geo/relfiles/cdsld13/natl/natl_zccd_delim.txt', colClasses = 'character')
setnames(zcta_to_cd, 'Congressional District', 'District')
ca_10_mappings = filter(zcta_to_cd, State == '06' & District == '10')

## get zipcode coordinates 
data(zipcode)

## get committee and candidate master files 
cm_header = GET('http://www.fec.gov/finance/disclosure/metadata/cm_header_file.csv') %>% 
	content() %>% 
	names()

committee_master = fread('cm16/cm.txt', sep = '|')
names(committee_master) = cm_header


cn_header = GET('http://www.fec.gov/finance/disclosure/metadata/cn_header_file.csv') %>% 
	content() %>% 
	names()

candidate_master = fread('cn16/cn.txt', sep = '|')
names(candidate_master) = cn_header

# get denham candidate id and committee ids
cd10_cand = filter(candidate_master, CAND_OFFICE_DISTRICT == '10' & CAND_OFFICE == 'H' & CAND_OFFICE_ST == 'CA' &  CAND_ELECTION_YR == '2016')
denham_cand_id = filter(cd10_cand, str_detect(tolower(CAND_NAME), 'denham'))$CAND_ID
denham_cmte_ids = filter(committee_master, CAND_ID == denham_cand_id)$CMTE_ID

# read in individual contributions in chunks and save everything associated with denham's committees
indiv_header = GET('http://www.fec.gov/finance/disclosure/metadata/indiv_header_file.csv') %>%
	content() %>% 
	names()


denham_contributions_list = list()

chunk_size = 5e5
chunk_it = 0
continue_loop = TRUE

while (continue_loop) {
	
	print(chunk_it)

	the_cont = fread('indiv16/itcont.txt', sep = '|', nrows = chunk_size, skip = chunk_it * chunk_size)
	names(the_cont) = indiv_header

	chunk_it = chunk_it + 1

	continue_loop = nrow(the_cont) > 0
	
	if (continue_loop) {
		denham_contributions = the_cont[the_cont$CMTE_ID %in% denham_cmte_ids, ]

		if (nrow(denham_contributions) > 0) {
			cat('found', nrow(denham_contributions), 'denham_contributions\n\n')
			list_index = length(denham_contributions_list) + 1
			denham_contributions_list[[list_index]] = denham_contributions
		}
		rm(the_cont)
		gc()	
	}	
}

## stack all contributions
denham_contributions_stacked = rbindlist(denham_contributions_list)

# take a look at the transaction types 
# > table(denham_contributions_stacked$TRANSACTION_TP)
# 11   15  22Y 
# 51 1174    3 

#http://www.fec.gov/finance/disclosure/metadata/DataDictionaryTransactionTypeCodes.shtml
# 11 Native American Tribe contribution
# 15 Contribution to political committees (other than Super PACs and Hybrid PACs) from an individual, partnership or limited liability company
# 22Y Contribution refund to an individual, partnership or limited liability company

refunded_contributions = filter(denham_contributions_stacked, TRANSACTION_TP == '22Y')

contributors_that_were_refunded = filter(denham_contributions_stacked, NAME %in% refunded_contributions$NAME) %>% 
select(NAME, EMPLOYER, OCCUPATION) %>%
unique()
# contributors_that_were_refunded
#               NAME                      EMPLOYER  OCCUPATION
# 1       FUND, JOHN                      FOX NEWS COMMENTATOR
# 2  VALLARINE, ALAN                          SELF     DENTIST
# 3  VALLARINE, ALAN                 VALLEY DENTAL     DENTIST
# 6       FUND, JOHN                                          
# 7      RIKLIS, IRA                                          
# 8  VALLARINE, ALAN         SELF - ALAN VALLARINE     DENTIST
# 9  VALLARINE, ALAN                                          
# 11     RIKLIS, IRA       SUTHERLAND CAPITAL MGT.   EXECUTIVE
# 12     RIKLIS, IRA SUTHERLAND CAPITAL MGMT. INC.   EXECUTIVE

contributions_net_of_refunds = 
	denham_contributions_stacked %>%
	group_by(NAME, CITY, STATE, ZIP_CODE) %>%
	summarize(		
		net_contributions = sum(ifelse(TRANSACTION_TP == '22Y', -1 * TRANSACTION_AMT, TRANSACTION_AMT))
		)

# filter(contributions_net_of_refunds, NAME %in% refunded_contributions$NAME)

#              NAME         CITY STATE ZIP_CODE net_contributions cont_from_district
#             <chr>        <chr> <chr>    <chr>             <dbl>              <lgl>
# 1      FUND, JOHN EDWARDSVILLE    IL    62025              3700              FALSE
# 2     RIKLIS, IRA     NEW YORK    NY    10028              5000              FALSE
# 3 VALLARINE, ALAN      TURLOCK    CA    95382              7700               TRUE



contributions_by_zipcode = 
	contributions_net_of_refunds %>% 
	group_by(ZIP_CODE) %>%
	summarize(
		n_donors = n_distinct(paste0(NAME, CITY, STATE, ZIP_CODE)),
		n_donations = n(),
		sum_donations = sum(net_contributions, na.rm = T)
		) 

contributions_by_zipcode$cont_from_district = contributions_by_zipcode$ZIP_CODE %in% ca_10_mappings$ZCTA

contributions_from_outsize_district =
contributions_by_zipcode %>%
filter(! cont_from_district) %>%	
	left_join(
		zipcode, by = c('ZIP_CODE' = 'zip')
		) %>%
	filter(
		# only continental US
		! state %in% c('HI', 'AK')
		)

contributions_outside_CA = filter(contributions_from_outsize_district, state != 'CA')

modesto_coord = filter(zipcode, zip == '95350')

# get percent of donations outside of district 
sum_donations_outside_dist = sum(contributions_by_zipcode$sum_donations[!contributions_by_zipcode$cont_from_district])
total_donations = sum(contributions_by_zipcode$sum_donations)

percent_outside_dist = percent(sum_donations_outside_dist/total_donations)
# percent_outside_dist
# [1] "79.6%"

## map the contributions 

us_map = map_data('usa')
us_state_map = map_data('state')

outside_ca_map = ggplot(us_map, aes(long, lat)) +
geom_polygon(aes(group = group)) +
geom_path(aes(group = group)) +
geom_path(data = us_state_map, aes(group = group), colour = 'white') +
coord_quickmap() +
geom_point(data = contributions_outside_CA, aes(longitude, latitude, size = sum_donations, alpha = sum_donations), colour = 'steelblue') +
geom_curve(data = contributions_outside_CA, aes(x = longitude, y = latitude, xend = modesto_coord$longitude, yend = modesto_coord$latitude, alpha = sum_donations), curvature = 0.2, colour = 'orange') +
labs(
	title = "Mapping Contributions from Outside Donors To Rep. Jeff Denham (CA-10)",
	subtitle = sprintf("%s of contributions were from outside the district", percent_outside_dist),
	x = '',
	caption = 'Source: FEC Data, 2016 Election Cycle',
	y = ''	
	) +
theme(
	legend.position = 'bottom',
	axis.text = element_blank(),
	axis.ticks = element_blank()
	) +
scale_size_continuous(name = 'Sum Donations ($)', range = c(.5, 4)) +
scale_alpha(name = 'Sum Donations ($)')

setwd('~')
ggsave('Outside_Donations_to_Denham.png', height = 6, width = 8, units = 'in', dpi = 500, plot = outside_ca_map)


