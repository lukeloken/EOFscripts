

# Work flow for data compilation and cross-site analysis

#load master R files
source('scripts/data_compliation/1_load_master_data.R')

#compile site approved data
#These data contain estimated loads
source('scripts/data_compliation/1_load_all_siteapproved_data.R')

#load all surface water data
#output is a data.frame named "data_df"
source('scripts/data_compliation/1_load_all_data.R')

#load soil data
# as of January 2020, this only includes spring 2016 and fall 2017 data
source('scripts/data_compliation/1_load_soil_data.R')

#load farm management data
source('scripts/data_compliation/1_load_field_data.R')


#load precip data
#Takes multiple hours and needs network
# source('scripts/data_compliation/1_load_precip_data.R')

#summarize water quality data
source('scripts/data_compliation/2_summarize_data.R')

#Merge soil and water quality data. 
#Currently takes the mean of individual events for water, but should be revisited
#Script also makes a lot of figures that should be moved elsewhere
source('scripts/data_compliation/3_combine_soil_water_data.R')

#plot loads and concentration data by water year and runoff volume
source('scripts/data_compliation/4_plot_stormrunoff_data.R')

#plot soil quality data. Lots of figures
source('scripts/data_compliation/4_plot_soilquality.R')