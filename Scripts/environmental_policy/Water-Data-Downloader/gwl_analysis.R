library("ggplot2", lib.loc = "/Users/user/Documents/R-packages")
library("reshape2", lib.loc = "/Users/user/Documents/R-packages")
library("plyr", lib.loc = "/Users/user/Documents/R-packages")
library("dplyr", lib.loc = "/Users/user/Documents/R-packages")
library("stringr", lib.loc = "/Users/user/Documents/R-packages")
library("data.table", lib.loc = "/Users/user/Documents/R-packages")
library("gridExtra", lib.loc = "/Users/user/Documents/R-packages")
library("scales", lib.loc = "/Users/user/Documents/R-packages")
library("lazyeval", lib.loc = "/Users/user/Documents/R-packages")
library("labeling", lib.loc = "/Users/user/Documents/R-packages")

options(stringsAsFactors = TRUE)

input_path = "/Users/user/Documents/California Water Data/Groundwater Level Data"
data_output_path = "/Users/user/Documents/California Water Data/Groundwater Level Data/R-output"
sumstats_output_path = "/Users/user/Documents/California Water Data/Groundwater Level Data/R-output/Sumstats"
graphs_output_path = "/Users/user/Documents/California Water Data/Groundwater Level Data/R-output/Graphs"

setwd(input_path)
regions = c('Central_Coast', 'Colorado_River', 'North_Coast', 'North_Lahontan', 'Sacramento_River', 'San_Francisco_Bay', 'San_Joaquin_River', 'South_Coast', 'South_Lahontan', 'Tulare_Lake')
file_list = paste0(regions,'_gwl_well_data.csv')

# Create single well data file from all of the separate regional well data files
all_well_data = rbindlist(lapply(file_list,fread, na.strings = "NA"))
all_well_data = mutate(all_well_data,
                       Measurement_Date = as.Date(Measurement_Date,"%m-%d-%Y"),                       
                       Region = as.factor(Region),
                       Basin = as.factor(Basin),
                       Use = as.factor(Use),
                       measurement_year = year(Measurement_Date))

get_na_grid = function(start_year, end_year, mode = "All"){

   # Get wells and years for which there are water level data 
   wells_nonmiss_uyr = mutate(
      distinct(
         filter(all_well_data, !is.na(RPWS) & !is.na(GSWS)), 
         State_Well_Number, measurement_year
         ),
      data_status = 'non_missing'
   )

   # Use this to create an index of wells and years
   wells_nonmiss_all_yrs = rbindlist(
      lapply(
         c(start_year:end_year), 
         function(year){
            mutate(distinct(select(wells_nonmiss_uyr, Region, Basin, Township, State_Well_Number)), measurement_year = year)
         }
      )
   ) 

   # Merge this index with unique-by-year well data to get data for yearly missingness   
   well_dat_na_grid = merge(filter(wells_nonmiss_uyr, measurement_year >= start_year & measurement_year <= end_year), 
                            wells_nonmiss_all_yrs, 
                            by = c('Region', 'Basin', 'Township', 'State_Well_Number', 'measurement_year'),
                            all = TRUE)   
   well_dat_na_grid = mutate(well_dat_na_grid, 
                             data_status = ifelse(is.na(data_status),'missing', data_status), 
                             data_status = as.factor(data_status)
   )      
   
   # If specified, get data for yearly missingness that counts a well as missing only after it has been observed
   if(tolower(mode) == 'first_nonmiss'){
      wells_first_obs = summarize(group_by(wells_nonmiss_uyr, State_Well_Number),
                                  first_obs_year = min(measurement_year))
      well_dat_na_grid = filter(merge(well_dat_na_grid, wells_first_obs, by = 'State_Well_Number', all = TRUE),
                                measurement_year >= first_obs_year)      
   }
   
   # Get a similar index for townships, basins, and wells for which we know no data are available
   # combine it with the 'well_dat_na_grid' dataset
   missing_data = distinct(filter(all_well_data,is.na(Use)), Region, Basin, Township)  
   missing_data_all_yrs = rbindlist(
      lapply(
         c(start_year:end_year),
         function(year){
            mutate(missing_data, data_status = 'missing', measurement_year = year)
         }
      )
   )
   well_dat_na_grid = rbindlist(list(well_dat_na_grid, missing_data_all_yrs), fill = TRUE)   
   
   return(well_dat_na_grid)
}

well_dat_na_grid = get_na_grid(1950, 2010, 'first_nonmiss')

# Get summary statistics
get_sumstats = function(geo_units){

   geo_units = 'Region'
   if(length(geo_units) == 1){
      well_dat_na_grid = mutate(well_dat_na_grid, geo_unit = Region)
      all_well_data = mutate(all_well_data, geo_unit = Region)
   }
   if(length(geo_units) == 2){
      well_dat_na_grid = mutate(well_dat_na_grid, geo_unit = paste(Region, Basin, sep = '_'))
      all_well_data = mutate(all_well_data, geo_unit = paste(Region, Basin, sep = '_'))
   }
   if(length(geo_units) == 3){
      well_dat_na_grid = mutate(well_dat_na_grid, geo_unit = paste(Region, Basin, Township, sep = '_'))
      all_well_data = mutate(all_well_data, geo_unit = paste(Region, Basin, Township, sep = '_'))
   }
   
   nonmiss_counts = summarize(group_by(well_dat_na_grid, geo_unit, measurement_year, data_status), n_data_status = n()) 
   nonmiss_counts_wide = mutate(
      dcast(as.data.frame(nonmiss_counts),
            geo_unit + measurement_year ~ data_status,
            value.var = "n_data_status"
      ),
      missing = ifelse(is.na(missing), 0, missing),
      non_missing = ifelse(is.na(non_missing), 0, non_missing),
      n_wells = non_missing + missing,
      n_observed = non_missing,
      mean_nonmissing = n_observed/n_wells
   )   
   geo_sep = colsplit(nonmiss_counts_wide$geo_unit, '_', names = geo_units)
   na_sumstats = cbind(geo_sep, select(nonmiss_counts_wide, measurement_year, n_wells, n_observed, mean_nonmissing))
   
   # Get summary stats for the proxy for the water level (RPWS) in each region
   yrly_rpws = as.data.frame(
      summarize(
         group_by(all_well_data, geo_unit, measurement_year),
         n_observed = n(), 
         median_level = median(RPWS, na.rm = TRUE),
         mean_level = mean(RPWS, na.rm = TRUE)
      )
   )
   geo_sep = colsplit(yrly_rpws$geo_unit, '_', names = geo_units)
   rpws_sumstats = cbind(geo_sep, select(yrly_rpws, measurement_year, n_observed, median_level, mean_level))

   sumstats_list = list(na_sumstats, rpws_sumstats)
   names(sumstats_list) = c('na_sumstats', 'rpws_sumstats')
   return(sumstats_list)
}

sumstats = get_sumstats(c('Region', 'Basin', 'Township'))

# Plots!
yearly_freqs_plot = ggplot(regions_sumstats, aes(measurement_year, mean_nonmissing, colour = Region))+
   geom_point(aes(size = n_wells))+
   geom_line(aes(colour = Region))+
   scale_x_continuous(limits = c(1950,2010), breaks = c(1900, 1910, 1920, 1930, 1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010))
yearly_freqs_plot


