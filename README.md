# README--Local marine meta-analysis

This is the repository containing code and data for the local marine meta-analysis that is trying to attribute local global drivers of biodiversity change.

### Getting started

Clone the repository to your computer by doing:

~~~
git clone https://github.com/jdunic/local-marine-meta
~~~

The raw data is contained in the directory `master_data`. 

### Important note for editting data
Data will be added, using git to track changes in data. Please try and make your commit messages meaningful, describing the purposes of your changes (e.g., correcting study ID value; quality controlled - checked; etc.,). You will most likely be editting the data in Excel. This will require an extra step (because computers are kind of stupid).

You can find and edit your .git/config file by doing (in a terminal):

~~~
cd path_to_local_meta_folder
~~~

Once there you can use the use the text editor nano to edit the .git/config file:

~~~
nano .git/config
~~~

In your .git/config file add the following:

~~~
[filter "cr"]
    clean = tr '\\r' '\\n'
    smudge = tr '\\n' '\\r'
~~~

Save and quit. You can check that your diffs are working. If you are finding that it looks like an entire csv is changing (deletions and additions that you didn't make -- i.e., massive amounts of red and green) then please post an issue to let me know. Tracking our specific changes is not useful if the entire file changes!

~~~
git diff master_data_sheet_of_choice.csv
~~~


###Data cleaning

Raw inputs:  

*Study data*
* master_data/Data.csv
* master_data/robin_bts_reformat.csv

*Event types and classifications*
* master_data/Event_types.csv

*Spatial data for studies*
* master_data/SiteSpatialData.csv


# Cleaning pipeline:

1. Clean raw meta-analysis data (data from studies)
  1. Inputs:
    * master_data/Data.csv
    * master_data/robin_bts_reformat.csv
    * master_data/Event_types.csv
  2. Script: 
    * masterDataCleanSort.R (sources cb_data_processing.R)
  3. Outputs:  
    * firstLastData_v0.9-20160223.csv  

2. Extract data from all spatial layers:   
  1. Inputs  
    * CI_2013_OneTimePeriod/global_cumul_impact_2013_all_layers.tif
    * invasives_raw/invasives.tif
    * plumes_fertilizer_raw/plumes_fert.tif
    * plumes_pesticide_raw/plumes_pest.tif
    * HadISST_sst.nc
  2. Script:  
    * Create_master_fl_combined_df.R
  3. Outputs
    * Data_outputs/spatial_data_with_cumulative_impacts.csv
    * Data_outputs/spatial_data_with_invasives.csv
    * Data_outputs/spatial_data_with_nutrients.csv
    * Data_outputs/spatial_data_with_pesticides.csv
    * Data_outputs/spatial_data_with_single_temp_changes.csv
    * Data_outputs/spatial_data_with_temp_data.csv

3. Average the spatial data for each site and combine this data with the first 
   last data. End result is a dataset that contains a single spatial data value 
   for each unique Study.ID - Site - Taxonomic Group  
  1. Inputs 
    * Data_outputs/firstLastData_v0.9-20160223.csv  
    * Data_outputs/spatial_data_with_cumulative_impacts.csv
    * Data_outputs/spatial_data_with_invasives.csv
    * Data_outputs/spatial_data_with_nutrients.csv
    * Data_outputs/spatial_data_with_pesticides.csv
    * Data_outputs/spatial_data_with_single_temp_changes.csv
    * Data_outputs/spatial_data_with_temp_data.csv
  2. Script:  
    * combine_spatial_master_data.R
  3. Outputs
    * Data_outputs/fl_combined.csv