# this script will source all scripts required for analyzing percent change for all Edge of Field Sites

#################################
## Get the R packages you need ##
# Not sure which of these are actually needed. 

# load libraries
library(dplyr)
library(Rainmaker)
library(dataRetrieval)
library(USGSHydroTools)
library(lubridate)
library(rnoaa)
library(randomForest)
library(ggplot2)
library(pdp)
library(jtools)
library(caret)
library(bestglm)


#load repo functions
source('scripts/functions/not_all_na.R')


#Indicate where data are stored
#This currently points at the P:Drive
path_to_data <- "P:/0301"

#Currently outputs are saved here. 
# This can change later when/if results should be saved in a shared location
path_to_results <- "C:/Users/lloken/OneDrive - DOI/EOF_SoilHealth"

# Code largely uses the 'analysis_run_file.R' file that Sam Oliver generated to analyze each site individually
# This code differs in that it processes all the sites serially. 


# load predictors and responses for one site. two R objects are loaded. 
# Will need to see how variable these are among sites
load(file = 'P:/0301/analysis/WI/WI-SW3/results/2020-01-29-0851/cache/modvars.Rdata')

#Currently the 'clean names' for WI-3 
clean_names <- c('SS load (pounds)', 'Chloride load (pounds)',
                 'NO2 + NO3 load (pounds)', 'Ammonium load (pounds)', 
                 'TKN load (pounds)', 'Orthophosphate load (pounds)', 
                 'TP load (pounds)', 'TN load (pounds)', 
                 'Org N load (pounds)', 'Peak discharge (cfs)', 'Volume (cf)')


#Load compiled data from P drive. 
#This data is the output from the compilation script
#Rather could use the 'mod' file in each site sub-folder
data_df <- readRDS(file=(file_out(file.path(path_to_data, "compiled_data", "storm_event_loads", "storm_event_loads_allsites.rds" ))))

all_sites <- unique(data_df$site)

# Loop through each site and perform the same analysis
#Eventually each of these 'steps' will be there own script. 

#Empty list to populate with percent change tables
per.change.list.allsites<-list()

site_nu <- "WI-SW4"
# for (site_nu in all_sites){
  
  
  #before step 0


  #Subset to only one site and drop all columns with NAs
  dat <- filter(data_df, site == site_nu) %>%
    select_if(not_all_na)
  
  
  #Step 0
  source('scripts/percent_change_analysis/0_before_after_perchange_prep.R', echo = F)
  
  #Step 1
  #Random forest analysis of entire dataset
  source('scripts/percent_change_analysis/1_before_after_perchange_mdc.R', echo = F)
  
  # Step 2
  #Use random forest to determine set of predictors to use in multi linear model
  #Use 'before' data to build model
  #Predict 'after' data and compare to observations
  #Use model coefficient standard error to include confidence intervals in prediction
  #output a summary table, model list, and set of figures
  source('scripts/percent_change_analysis/2_before_after_perchange_mlm.R', echo = F)
  
  per.change.tableout
  temp_filename <- file.path('data_cached', paste0(site_nu, '_percent_reduction_before_after_mlm.csv'))
  # write.csv(per.change.tableout, temp_filename, row.names = F)
  
  per.change.list.allsites[[which(site_nu==all_sites)]] <- per.change.tableout
  names(per.change.list.allsites)[[which(site_nu==all_sites)]] <-site_nu
  #end
# }

per.change.df.allsites <-ldply(per.change.list.allsites, data.frame, .id='site')
  