

# Work flow for data compilation and cross-site analysis

#load functions
source('scripts/functions/fxns_data_compilation.R')
source('scripts/functions/g_legend.R')

#load all surface water data
#output is a data.frame named "data_df"
source('scripts/data_compliation/1_load_all_data.R')


source('scripts/data_compliation/2_summarize_data.R')
