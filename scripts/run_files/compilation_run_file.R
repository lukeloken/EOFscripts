

# Work flow for data compilation and cross-site analysis

#load functions
source('scripts/functions/fxns_data_compilation.R')
source('scripts/functions/g_legend.R')
source('scripts/functions/not_all_na.R')

#compile site approved data
#These data contain estimated loads
source('scripts/data_compliation/1_load_all_siteapproved_data.R')


#load all surface water data
#output is a data.frame named "data_df"
source('scripts/data_compliation/1_load_all_data.R')


#summarize water quality data
#Currently makes lots of figures
source('scripts/data_compliation/2_summarize_data.R')

#load soil data
# as of January 2020, this only includes spring 2016 and fall 2017 data
source('scripts/data_compliation/3_load_soil_data.R')

#plot loads and concentration data by water year and runoff volume
source('scripts/data_compliation/4_plot_stormrunoff_data.R')
