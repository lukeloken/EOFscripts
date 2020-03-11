# this script will source all scripts required for analyzing percent change for all Edge of Field Sites

#################################
## Get the R packages you need ##
# Not sure which of these are actually needed. 

# load libraries
library(plyr)
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
library(drake)
library(quantregForest)

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

#Currently the 'clean names' for WI-3 
clean_names <- c('SS load (pounds)', 'Chloride load (pounds)',
                 'NO2 + NO3 load (pounds)', 'Ammonium load (pounds)',
                 'TKN load (pounds)', 'Orthophosphate load (pounds)',
                 'TP load (pounds)', 'TN load (pounds)',
                 'Org N load (pounds)', 'Peak discharge (cfs)', 'Volume (cf)')

#These are the response names in the merged data file. 
responses_clean <- c("suspended_sediment_load_pounds", "chloride_load_pounds",
                     "no2_no3n_load_pounds", "ammonium_n_load_pounds",
                     "tkn_unfiltered_load_pounds", "orthophosphate_load_pounds",
                     "tp_unfiltered_load_pounds", "total_nitrogen_load_pounds", 
                     "organic_nitrogen_load_pounds", "peak_discharge", "runoff_volume") 

predictors_all <- c("weq" , "duration", "Ievent", "I5", "I10", "I15", "I30", "I60",          
                    "energy_m1", "erosivity_m1", "energy_m2", "erosivity_m2", 
                    "ARFdays1", "ARFdays2" , "ARFdays7"  , "ARFdays14", "sin_sdate", "cos_sdate",
                    "tmax","tmin", "days_since_planting", "days_since_fertilizer",
                    "days_since_cultivation", "days_since_disturbance", "ant_dis_1day_sum",
                    "ant_dis_2day_sum", "ant_dis_3day_sum", "ant_dis_7day_sum","ant_dis_14day_sum",
                    "frozen" )


#Load compiled data from P drive. 
#This data is the output from the compilation script
#Rather could use the 'mod' file in each site sub-folder
data_df <- readRDS(file=(file_in(file.path(path_to_data, "compiled_data", "storm_event_loads", "storm_event_loads_allsites_model_data.rds" ))))

all_sites <- as.character(unique(data_df$site))

# Loop through each site and perform the same analysis
#Eventually each of these 'steps' will be there own script. 

#Empty list to populate with percent change tables
per.change.list.allsites<-list()

# site_nu <- "WI-SW4"

# load predictors and responses for one site. two R objects are loaded. 
# Will need to see how variable these are among sites
# load(file = file.path(path_to_data, 'analysis/WI/WI-SW3/results/2020-01-29-0851/cache/modvars.Rdata'))
# load(file = file.path(path_to_data, 'analysis/WI/WI-SW3/results/2020-01-29-0851/cache/modvars.Rdata'))


#Identify how many states have data
# states <- list.files(file.path(path_to_data, "analysis"))
states <- c("IN", "MI", "WI", "OH", "NY")

predictors_list <- list()

site_nu <- 15
for (site_nu in 1:length(all_sites)){
  site_name <- all_sites[site_nu]
  print(site_name)
  
  state <- substr(site_name, 1,2)
  
  folders<-list.files(file.path(path_to_data, "field_analysis", "results", site_name))
  
      #Subset to only one site and drop all columns with NAs
    dat <- filter(data_df, site == site_name) %>%
      select_if(not_all_na)
  
  # dat <- filter(data_df, site == site_name) 
    
    if (nrow(dat)==0){
      warning(paste(site_nu, " is not included in compiled data. Skipping"))
      next
    }
    
    #Identify rundates files
    rundates <- as.Date(paste0(folders, "-01"), format="%Y-%m-%d")
    recent_folder <- folders[which.max(rundates)]

    cache <- list.files(file.path(path_to_data, "field_analysis", "results", site_name, recent_folder, "cache" ), full.names=T)

    modvars_files <- cache[grepl("modvars.Rdata" , cache)]


    if (length(modvars_files) == 0){
      if (state == 'NY') {
        modvars_files <- "P:/0301/field_analysis/results/NY-SW4/2020-02-20-1027/cache/modvars.Rdata"
        warning(paste0("no modvars.Rdata file. Used NY-SW4 file for", toString(site_name)))
      } else if (state == 'WI'){
        modvars_files <- "C:/Users/lloken/OneDrive - DOI/EOF_SoilHealth/Data/modvars.Rdata"
        warning(paste0("no modvars.Rdata file. Used WI-SW1 file for", toString(site_name)))
      } else if (state %in% c('IN', 'MI', 'OH')){
        predictors <- intersect(predictors_all, names(dat))
        } else {
      warning(paste("folder contains no `modvars.Rdata` file.", toString(file.path(path_to_data, "field_analysis", "results", site_name, recent_folder, "cache" ))))
    }
      } else {
      if (length(modvars_files)>1){

        modvars_files <- modvars_files[which.min(nchar(modvars_files))]
        modvars_name <- tail(unlist(strsplit(modvars_files[1], split='/')),1)

        warning(paste("More than 1 file listed with 'modvars.Rdata'. Used file with shortest name:", modvars_name, "    Check folder:",  toString(file.path(path_to_data, "analysis", state, folder, "results", recent_folder, "cache" ))))
      }
    }

if (length(modvars_files)>1){
    load(file = modvars_files)
}
    

    if(nrow(dat)==0) {
      warning(paste0("Skipping folder ", toString(site_nu), ". Folder name does not match compiled data.frame site name. Check folder and site names"))
      next
      rm(responses, predictors)
    }

    responses <- responses_clean
    
    # if(site_name =='NY-SW4'){
    #   warning("skipping site NY-SW4. predictor vars not correct")
    #   next
    # }
    
    # predictors_list[[(length(predictors_list)+1)]] <- predictors
    # names(predictors_list)[[(length(predictors_list))]] <- site_name
    
    #create direcotry for figures
    dir.create(file.path(path_to_results, 'Figures', 'PercentChange', site_name), showWarnings = F)
    
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
    # temp_filename <- file.path('data_cached', paste0(site_nu, '_percent_reduction_before_after_mlm.csv'))
    # write.csv(per.change.tableout, temp_filename, row.names = F)
    
    # per.change.list.allsites[[which(site_nu==all_sites)]] <- per.change.tableout
    # names(per.change.list.allsites)[[which(site_nu==all_sites)]] <-site_nu
    #end
}


per.change.df.allsites <-ldply(per.change.list.allsites, data.frame, .id='site')
