if (getwd() != '/Users/jillian/R_projects/Meta_analysis_ms') setwd('Meta_analysis_ms')

source('masterDataCleanSort.R')
source('cb_data_processing.R')

# This script is slow, only need to run when the spatial data has been updated
#source('Create_master_fl_combined_df.R')

# Combines data output by Create_master_fl_cobined_df.R
source('combine_spatial_master_data.R')