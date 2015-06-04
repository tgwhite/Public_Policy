'''
Created on Mar 31, 2015
Tries to download all of the water quality data 
from the www.water.ca.gov website
@author: jbolorinos
'''

from urllib import request, parse
from bs4 import BeautifulSoup
from selenium.selenium import selenium
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import Select
import sqlite3
import os
from queue import Queue
import csv
import time
from math import ceil


def get_form_field_names(url): 
    import urllib.request, urllib.parse
    page_content = urllib.request.urlopen(url).read()
    forms = BeautifulSoup(page_content).findAll('form')
    form_list = []
    for form in forms:
        if form.has_attr('id'):
            form_list.append(form['id'])
    return form_list

def download_all_water_level_data(data_format, browser, wql_start_url, region_list, download_dir, wait_time_interval):
    start_time = time.time()
    next_start_time = start_time
    default_download_dir = '/Users/user/Downloads'
    default_download_filename = 'GWLData.csv'
    
    # Prepare SQL statements       
    SQL_create_data_table = '''CREATE TABLE well_data
                               (id INTEGER PRIMARY KEY, Region TEXT, Basin TEXT, Township TEXT, Use TEXT, State_Well_Number TEXT, Measurement_Date TEXT, 
                                RP_Elevation TEXT, GS_Elevation TEXT, RPWS TEXT, WSE TEXT, GSWS TEXT, QM_Code TEXT, NM_Code TEXT, Agency TEXT, Comment TEXT
                                )
                            '''
     
    SQL_create_coord_table = ''' CREATE TABLE well_coords(
                                 id INTEGER PRIMARY KEY, Region TEXT, Basin TEXT, Township TEXT, Use TEXT, State_Well_Number TEXT,           
                                 Projection TEXT, Datum TEXT, Easting TEXT, Northing TEXT, Units TEXT, Zone TEXT     
                                 )
                             '''
    
    SQL_import_data = """insert into well_data
                         (Region, Basin, Township, Use, State_Well_Number, Measurement_Date, RP_Elevation, GS_Elevation, RPWS, WSE, GSWS, QM_Code, NM_Code, Agency, Comment)
                         values('%s', '%s', '%s', '%s', :State_Well_Number, :Measurement_Date, :RP_Elevation, :GS_Elevation, :RPWS, :WSE, :GSWS, :QM_Code, :NM_Code, :Agency, :Comment)
                      """
    SQL_import_coords = """insert into well_coords
                           (Region, Basin, Township, Use, State_Well_Number, Projection, Datum, Easting, Northing, Units, Zone)
                           values('%s', '%s', '%s', '%s', '%s', :Projection, :Datum, :Easting, :Northing, :Units, :Zone)
                        """         
    empty_imp_data = """insert into well_data
                        (Region, Basin, Township, Use, State_Well_Number, Measurement_Date, RP_Elevation, GS_Elevation, RPWS, WSE, GSWS, QM_Code, NM_Code, Agency, Comment)
                        values('%s', '%s', '%s', '%s', '%s', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA')
                     """
    
    empty_imp_coords = """insert into well_coords
                          (Region, Basin, Township, Use, State_Well_Number, Projection, Datum, Easting, Northing, Units, Zone)
                          values('%s', '%s', '%s', '%s', '%s', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA')
                       """                       
    
    # Initialize a variable name for the Queue() object
    queue = Queue()
    
    # Set browser type according to user specification
    if browser == 'Chrome':
        driver = webdriver.Chrome()
    elif browser == 'Firefox':
        driver = webdriver.Firefox()
    elif browser == 'Ie':
        driver = webdriver.Ie()
    else:
        driver = webdriver.Remote()
    
    # Open the water quality download index page    
    driver.get(wql_start_url)

    def download_csv_from_website(well):
        select_well = Select(driver.find_element_by_name('wellNumber'))
        select_well.select_by_visible_text(well)                                            
        select_format = Select(driver.find_element_by_name('OPformat'))
        select_format.select_by_visible_text(data_format)
        driver.find_element_by_name('cmdSubmit').click() 
        try:
            alert = driver.switch_to_alert()
            alert.accept()
            print("No data available for well: %s" %(well))
            return 0
        except:         
            return 1

    def get_tables_from_csv(input_path): 
        input = csv.reader(open(input_path, 'rt'))
        # Break input data into multiple csv_objects, returning a list of lists (each being a row in the table) for each table
        tables = []
        table = []
        for row in input:
            if not row:
                tables.append(table)       
                table = []     
            else:
                table.append(row)
        tables.append(table)
        return tables
    
    def load_csv_to_sql(tables):    
        use = 'NA'
        # If the csv file contains two tables then load each (if non-empty) into the sql database
        if tables and len(tables) == 2:
            if tables[1]:
                use = tables[1][len(tables[1])-1][0]   
                cursor.executemany(SQL_import_coords %(region, basin, township, use, well), tables[1][2:len(tables[1])-2] )             
            if tables[0]:         
                cursor.executemany(SQL_import_data %(region, basin, township, use), tables[0][1:])
        # If the csv file only contains one table then load the table into the correct database by checking its header first
        # Insert missing values into the other table database
        elif tables and tables[0]:
            if  'State Well Number' in tables[0][0]:
                cursor.executemany(SQL_import_data %(region, basin, township, use), tables[0][1:])
                cursor.execute(empty_imp_coords %(region, basin, township, use, well))   
                print("No coordinate data available for well: %s" %(well))                       
            elif 'Well Coordinate Information' in tables[0][0]:
                cursor.executemany(SQL_import_coords %(region, basin, township, use, well), tables[1][2:len(tables[1])-2] )
                cursor.execute(empty_imp_data %(region, basin, township, use, well))
                print("No data available for well: %s" %(well))
        # Insert missing values for wells with no data
        else: 
            cursor.execute(empty_imp_data %(region, basin, township, use, well))
            cursor.execute(empty_imp_coords %(region, basin, township, use, well)) 
            print("No data available for well: %s" %(well))
        conn.commit()         
        return
    
    def download_and_store_well_data(well):        
        data_available = download_csv_from_website(well)
        tables = [[]]
        if data_available:
            # Wait until the file has finished downloading before trying to load it into the sql database
            wait_time = 0
            while os.path.isfile(default_download_dir + '/' + default_download_filename) == False:
                time.sleep(wait_time_interval)
                wait_time += wait_time_interval
                if wait_time > 60:
                    print('Error: Script Aborted because the file for well ' + well + ' never finished downloading')
                    quit()         
            tables = get_tables_from_csv(default_download_dir + '/' + default_download_filename)
        try:
            load_csv_to_sql(tables)
        except IndexError:
            print('Skipped data for well %s in %s Region, %s Basin , Township %s because of data irregularity' %(well, region, basin, township))
        except:
            print('Skipped data for well %s in %s Region, %s Basin , Township %s due to unknown error' %(well, region, basin, township))
        if data_available:
            os.remove(default_download_dir + '/' + default_download_filename)        
        print('Downloaded and stored data for %s %s %s %s' %(region, basin, township, well))
        return

    # Start a loop through all of the regions
    for region in region_list:
        
        # Open the SQL connection and create the table for the given region
        conn = sqlite3.connect(':memory:')
        cursor = conn.cursor()
        cursor.execute(SQL_create_data_table)    
        cursor.execute(SQL_create_coord_table)         
        
        # Reload the URL for each iteration so objects aren't lost
        driver.get(wql_start_url)
        region_selection = Select(driver.find_element_by_name('select1'))
        print("Checking Region: " + region)
        region_selection.select_by_visible_text(region)
        driver.find_element_by_name('Search1').click()
        all_basins = driver.find_element_by_name('select2')
        all_basins = all_basins.find_elements_by_tag_name('option')
        all_basin_names = []
        
        for basin in all_basins:
            all_basin_names.append(basin.get_attribute('text'))
        if all_basin_names != ['']:
            for basin in all_basin_names:
                # Reload the URL and data for each iteration so objects aren't lost
                driver.get(wql_start_url)
                if basin != "":
                    print('Checking Basin: ' + basin)
                    # Reload the URL and data for each iteration so objects aren't lost
                    region_selection = Select(driver.find_element_by_name('select1'))
                    region_selection.select_by_visible_text(region)
                    driver.find_element_by_name('Search1').click() 
                    
                    # Proceed to selection of basin in loop
                    basin_selection = Select(driver.find_element_by_name('select2'))                   
                    basin_selection.select_by_visible_text(basin)
                    driver.find_element_by_name('Search1').click()
                    
                    # Get data for townships (if any)
                    all_townships = driver.find_element_by_xpath("//select[@name = 'select3']")
                    all_townships = all_townships.find_elements_by_tag_name('option')
                    all_township_names = []
                    for township in all_townships:
                        all_township_names.append(township.get_attribute('text'))
                    if all_township_names != ['']:
                        for township in all_township_names:
                            # Reload the URL and data for each iteration so objects aren't lost
                            driver.get(wql_start_url) 
                            if township != "":
                                
                                print('Checking Township: ' + township)
                                
                                # Reload the URL and data for each iteration so objects aren't lost  
                                region_selection = Select(driver.find_element_by_name('select1'))
                                region_selection.select_by_visible_text(region)
                                driver.find_element_by_name('Search1').click()                                     
                                basin_selection = Select(driver.find_element_by_name('select2'))                   
                                basin_selection.select_by_visible_text(basin)
                                driver.find_element_by_name('Search1').click()
                                
                                # Proceed to selection of township in loop                                   
                                township_selection = Select(driver.find_element_by_name('select3'))
                                township_selection.select_by_visible_text(township)
                                driver.find_element_by_name("cmdNext").click()
                                
                                # Get data for wells (if any) and download it!
                                all_wells = driver.find_element_by_xpath("//select[@name = 'wellNumber']")
                                all_wells = all_wells.find_elements_by_tag_name('option')
                                all_well_names = []
                                for well in all_wells:
                                    all_well_names.append(well.get_attribute('text'))
                                if all_well_names != []:                              
                                    for well in all_well_names:
                                        if well != None:
                                            queue.put(well)
                                    while not queue.empty():
                                        if os.path.isfile(default_download_dir + '/' + default_download_filename) == False:
                                            well = queue.get()
                                            download_and_store_well_data(well)
                                        else:
                                            os.remove(default_download_dir + '/' + default_download_filename)
                                else:
                                    print("No data available for township: %s" %(township))
                                    cursor.execute(empty_imp_data %(region, basin, township, 'NA', 'NA'))
                    else:
                        print("No data available for %s basin" %(basin))
                        cursor.execute(empty_imp_data %(region, basin, 'NA', 'NA', 'NA'))                
        else:
            print("No data available for %s region" %(region))
            cursor.execute(empty_imp_data %(region, 'NA', 'NA', 'NA', 'NA'))
            
        #Write the region's well data in memory to a csv file in the desired directory
        all_rows = cursor.execute("""SELECT * FROM well_data WHERE Region = '%s' """ %(region))        
        with open(download_dir + '/%s_gwl_well_data.csv' %(region.replace(' ','-')), 'w', newline = '' ) as csv_file:
            writer = csv.writer(csv_file, delimiter=',')
            # Write the header to the output file
            writer.writerow([i[0] for i in all_rows.description])          
            for row in all_rows:
                writer.writerow(row)
                
        #Write the region's well coordinate information in memory to a csv file in the desired directory
        all_rows = cursor.execute("""SELECT * FROM well_coords WHERE Region = '%s' """ %(region))        
        with open(download_dir + '/%s_well_coordinate_data.csv' %(region.replace(' ','-')), 'w', newline = '' ) as csv_file:
            writer = csv.writer(csv_file, delimiter=',')
            # Write the header to the output file
            writer.writerow([i[0] for i in all_rows.description])          
            for row in all_rows:
                writer.writerow(row)
        
        minutes_elapsed = (time.time() - next_start_time)/60
        next_start_time = time.time()
        print('Finished downloading well data for %s. The process took %i minutes' %(region, minutes_elapsed))
        
        #Close SQL connection
        conn.close()       
                
    # Close browser when Finished
    driver.close()
    
    #Print time elapsed to the log
    minutes_elapsed = (time.time() - start_time)/60
    print('Data finished downloading. The process took: %i minutes' %(minutes_elapsed))                      

download_all_water_level_data('Text', 
                              'Chrome',
                              'http://www.water.ca.gov/waterdatalibrary/groundwater/hydrographs/index.cfm',
                              ['South Lahontan', 'Tulare Lake'],
                              '/Users/user/Documents/California Water Data/Groundwater Level Data',
                              0.1
                              )

def download_all_water_quality_data(browser, wql_start_url, download_dir):
    
    # Parameters for the run
    default_download_dir = '/Users/user/Downloads'
    default_download_filename = 'WQData.xls'
    download_limit = 15 # This is the number of files to download at the same time
    wait_time_interval = 0.1 # This is the increment by which the program is put to sleep while waiting for a file to download/delete
    download_timeout = 300
    table_load_timeout = 10
    
    start_time = time.time()
    next_start_time = time.time()
    
    # Prepare sql statements    
    SQL_create_table = """ create table wql_data(
                            id INTEGER PRIMARY KEY, County TEXT, Long_Station_Name TEXT, Short_Station_Name TEXT, Station_Number TEXT, Sample_Code TEXT, Collection_Date TEXT,  Analyte TEXT,    
                            CAS_Reg_Number TEXT, Result TEXT, Rpt_Limit TEXT, Units TEXT, Method TEXT, Depth TEXT, Matrix TEXT, Purpose TEXT, Parent_Sample TEXT, Description TEXT, Notes TEXT
                           )
                       """
    
    SQL_import_data = """insert into wql_data(
                         County, Long_Station_Name, Short_Station_Name, Station_Number, Sample_Code, Collection_Date, Analyte, 
                         CAS_Reg_Number, Result, Rpt_Limit, Units, Method, Depth, Matrix, Purpose, Parent_Sample, Description, Notes
                         )
                         values(
                         '%s', :Long_Station_Name, :Short_Station_Name, :Station_Number, :Sample_Code, :Collection_Date, :Analyte, 
                         :CAS_Reg_Number, :Result, :Rpt_Limit, :Units, :Method, :Depth, :Matrix, :Purpose, :Parent_Sample, :Description, :Notes
                         )
                      """
    
    # Initialize a variable name for the Queue() object
    queue = Queue()                       
    
    # Set browser type according to user specification
    if browser == 'Chrome':
        driver = webdriver.Chrome()
    elif browser == 'Firefox':
        driver = webdriver.Firefox()
    elif browser == 'Ie':
        driver = webdriver.Ie()
    else:
        driver = webdriver.Remote()
    
    # Open the water quality download index page    
    driver.get(wql_start_url) 
    
    # Get a lit of all counties
    all_counties = driver.find_element_by_name('ddmCounty')   
    all_counties = all_counties.find_elements_by_tag_name('option')
    all_county_names = []       
    for county in all_counties:
        all_county_names.append(county.get_attribute('text'))
    
    all_county_names = ['Tulare', 'Tuolumne', 'Ventura', 'Yolo', 'Yuba']
    
    # Loop through each county to get data
    for county in all_county_names:
        if county:
            
            # Open the SQL connection and create the table
            conn = sqlite3.connect(':memory:')
            cursor = conn.cursor()
            cursor.execute(SQL_create_table)
            
            # Reload the page for each county
            driver.get(wql_start_url)             
            county_selection = Select(driver.find_element_by_name('ddmCounty'))
            print("Checking County: " + county)
            
            #Select County
            county_selection.select_by_visible_text(county)
            driver.find_element_by_name('chkWDIS').click()
            button_xpath = '//input[@type="submit" and @value="Get Stations"]'
            driver.find_element_by_xpath(button_xpath).click()
            
            # Here, download data for wells in groups of 15 (the max number of files that can be selected at the same time)
            # and then download the wells that are leftover
            all_stations = driver.find_elements_by_name('boxStationID') 
            all_station_ids = [] 
            for station in all_stations:
                all_station_ids.append(station.get_attribute('value'))
                       
            nwells = len(all_stations)
            nselections = ceil(nwells/download_limit)
            start_index = 0   
            
            for sel in range(nselections):
                queue.put(sel) 
            
            while not queue.empty():
                if os.path.isfile(default_download_dir + '/' + default_download_filename) == False:
                    sel = queue.get()
                    nchecks = download_limit
                    if sel == nselections - 1:
                        nchecks = nwells%download_limit
                     
                    # Check the appropriate number of boxes in the form    
                    for checkno in range(nchecks):
                        checkbox_xpath = '//input[@name="boxStationID" and @value=%s]' %(all_station_ids[start_index + checkno])
                        driver.find_element_by_xpath(checkbox_xpath).click()
                        
                    # Include field data in selection
                    field_data_xpath = '//input[@name="rdoFielddata" and @value="Yes"]'
                    driver.find_element_by_xpath(field_data_xpath).click()                
                    
                    # Get data for all dates available
                    all_dates_xpath = '//input[@name="rdoDate" and @value="AllDates"]'
                    driver.find_element_by_xpath(all_dates_xpath).click()
                    
                    # Select download format (Excel) and download!
                    format_selection = Select(driver.find_element_by_name("ddmOutputFormat"))
                    format_selection.select_by_visible_text("MS Excel")
                    button_xpath = '//input[@type="submit" and @value="Get Data"]'
                    driver.find_element_by_xpath(button_xpath).click()
                    
                    #Wait until the file has finished downloading before trying to load it into the sql database
                    wait_time = 0
                    while os.path.isfile(default_download_dir + '/' + default_download_filename) == False:
                        time.sleep(wait_time_interval)
                        wait_time += wait_time_interval
                        if wait_time > download_timeout:
                            print('Error: Script Aborted because the file for selection %i-%i for %s never finished downloading' %(start_index, start_index + range(nchecks), county))
                            quit() 
                                                
                    # Check that the excel file isn't empty (use size to check, maybe a better method...)                     
                    file_stats = os.stat(default_download_dir + '/' + default_download_filename)                             
                    if file_stats.st_size > 0:    
                        table = []
                        wait_time = 0
                        table_load_start_time = time.time() 
                        
                        # Keep trying to load the table until it loads or timeout                   
                        while table == []:
                            # Use cp1252 encoding to handle µ and other special characters when reading in the excel file
                            with open(default_download_dir + '/'+ default_download_filename, encoding = 'cp1252', errors = 'replace') as excel_file:
                                input = csv.reader(excel_file, delimiter = '\t')
                                table = []
                                for i, row in enumerate(input):
                                    if i == 0:
                                        nvariables = len(row)
                                    if len(row) <= nvariables:
                                        table.append(row)
                                    else:
                                        print("Encountered an irregular row in one of the tables for county %s and skipped it:" %(county))
                                        print(row)      
                            wait_time = time.time() - table_load_start_time
                            if wait_time > table_load_timeout:
                                print('Error: Script Aborted because python could not read in the file for selection %i-%i for %s' %(start_index, start_index + range(nchecks), county))  
                                quit()                      
                        if table:
                            # Add empty observation to last variable of each row if table doesn't identify it (because they are all missing)
                            for row in table:
                                while len(row) < nvariables:
                                    row.append('')
                                        
                            #Add the county's data to the SQL database
                            cursor.executemany(SQL_import_data %(county),table[1:])
                            conn.commit()
                      
                    #Delete the downloaded file so that the next one can be downloaded                       
                    os.remove(default_download_dir + '/' + default_download_filename) 
                   
                    # Uncheck the boxes    
                    for checkno in range(nchecks):
                        checkbox_xpath = '//input[@name="boxStationID" and @value=%s]' %(all_station_ids[start_index + checkno])
                        driver.find_element_by_xpath(checkbox_xpath).click()
                    
                    start_index += nchecks   
                else:
                    #Delete the downloaded file so that the next one can be downloaded                       
                    os.remove(default_download_dir + '/' + default_download_filename) 
                      
            minutes_elapsed = (time.time() - next_start_time)/60
            next_start_time = time.time()
            
            #Write water quality data for all counties in the SQL database to a file in the given location
            all_rows = cursor.execute("""SELECT * FROM wql_data""")        
            # Use cp1252 encoding to handle µ and other special characters when writing to the csv file
            with open(download_dir + '/%s_wql_data.csv' %(county), 'w', newline = '' , encoding = 'cp1252', errors = 'replace') as csv_file:
                writer = csv.writer(csv_file, delimiter=',')
                # Write the header to the output file  
                writer.writerow([i[0] for i in all_rows.description])     
                for row in all_rows:
                    writer.writerow(row)
            
            print('Finished downloading water quality data for %s. The process took %i minutes' %(county, minutes_elapsed))
            
            #Close SQL connection
            conn.close()   

    # Close browser when finished
    driver.close()    
    
    #Print time elapsed to the log
    minutes_elapsed = (time.time() - start_time)/60
    print('Data finished downloading. The process took: %i minutes' %(minutes_elapsed))  
    

download_all_water_quality_data('Chrome', 
                                'http://water.ca.gov/waterdatalibrary/waterquality/station_county/index.cfm', 
                                'C:/Users/taylor/Dropbox/WildPolicy/Data/environmental_data')
