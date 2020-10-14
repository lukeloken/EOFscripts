
#Summarize storm event load data for all sites

print(path_to_data)

source('scripts/functions/fxns_data_processing.R')


#***# SETTINGS/PREFERENCES  #***#***#***#
library(devtools)
library(dataRetrieval)
dataRetrieval::setAccess('internal')
library(ggplot2)
library(dplyr)
library(stringr)
# devtools::install_github("USGS-R/Rainmaker")

# Turn on Rainmaker package
library(Rainmaker)
options(scipen = 999)

#***# RAINMAKER OPTIONS #***#***#***#***#    
# Time between events in hours (interevent)
ieHr <- 2
# Amount it must rain to count as an event in tenths of inches (rain threshold) -- note RMevents_sample does not use rainthresh
rainthresh <- 0.008
# Antecedent Rainfall in days (ARF.days)
antecedentDays = c(0.5, 1, 7, 14)

#Load the rds file from the P drive
#This file was created using the "1_load_all_siteapproved_data.R" script
data_df_approved <- readRDS(file_in(file.path(path_to_data, "compiled_data", "storm_event_loads", "storm_event_loads_allsites_approved_data.rds" )))

#This file was created using the "1_load_all_data.R" script
data_df_model <- readRDS(file=(file_out(file.path(path_to_data, "compiled_data", "storm_event_loads", "storm_event_loads_allsites_model_data.rds" ))))

#Rain data
StormSummary_df <- readRDS(file.path(path_to_data, 'compiled_data', 'rain', 'Compiled_Rain_Data.rds'))

master_beforeafter_df <- readRDS(file.path(path_to_data, 'compiled_data', 'rain', 'Compiled_Masters.rds'))

common_vars <- c("site", "FieldName", "storm_start", "storm_end",
                 "peak_discharge", "runoff_volume")

approved_vars <- c("discrete", "estimated", "exclude", 
                   "frozen", "unique_storm_number", "storm")


rain_vars <- c("rain", "duration", "Ievent", 
               "I5", "I10", "I15", 
               "I30", "I60", "energy_m1", 
               "erosivity_m1", "energy_m2", "erosivity_m2",
               "weq")

#Preferred load names
loadvars <- c('suspended_sediment_load_pounds', 
              'chloride_load_pounds',
              'no2_no3n_load_pounds', 
              'ammonium_n_load_pounds',
              'tkn_unfiltered_load_pounds', 
              'orthophosphate_load_pounds',
              'tp_unfiltered_load_pounds',
              'total_nitrogen_load_pounds',
              'organic_nitrogen_load_pounds',
              'doc_load_pounds', 
              'toc_load_pounds')

#Preferred concentration names
concvars <- c('suspended_sediment_conc_mgL', 
              'chloride_conc_mgL',
              'no2_no3_n_conc_mgL', 
              'ammonium_n_conc_mgL',
              'tkn_unfiltered_conc_mgL', 
              'orthophosphate_conc_mgL',
              'total_phosphorus_conc_mgL',
              'total_nitrogen_conc_mgL',
              'organic_nitrogen_conc_mgL',
              'doc_conc_mgL',
              'toc_conc_mgL')

#Preferred yield names
yieldvars <- c('suspended_sediment_yield_pounds_per_acre', 
               'chloride_yield_pounds_per_acre',
               'no2_no3n_yield_pounds_per_acre', 
               'ammonium_n_yield_pounds_per_acre',
               'tkn_unfiltered_yield_pounds_per_acre', 
               'orthophosphate_yield_pounds_per_acre',
               'tp_unfiltered_yield_pounds_per_acre',
               'total_nitrogen_yield_pounds_per_acre',
               'organic_nitrogen_yield_pounds_per_acre',
               'doc_yield_pounds_per_acre', 
               'toc_yield_pounds_per_acre')

flagvars <- c("flag_suspended_sediment", "flag_chloride", "flag_no2_no3n", 
              "flag_ammonium_n", "flag_tkn_unfiltered", "flag_orthophosphate",
              "flag_tp_unfiltered", "flag_tn", "flag_orgN", "flag_doc", "flag_toc" )

#Clean up approved df
data_df_approved2 <- data_df_approved %>%
  select(intersect(names(data_df_approved),
                   all_of(c(common_vars, approved_vars, loadvars, 
                            concvars, flagvars, yieldvars)))) %>%
  filter(exclude == 0) %>%
  left_join(select(master_beforeafter_df, site, discharge_site_no)) %>%
  select(site, discrete, estimated, frozen, storm, 
         unique_storm_number, storm_start, storm_end, runoff_volume, peak_discharge, 
         everything()) 


data_df_approved2$storm[grepl("SW", data_df_approved2$site) & 
                         is.na(data_df_approved2$storm)] <- 1

data_df_approved3 <- data_df_approved2 %>%
  rename(FieldName = site,
         site = discharge_site_no) %>%
  mutate(project = "GLRI") %>%
  select(site, FieldName, project, discrete, estimated, frozen, storm, exclude,
         unique_storm_number, storm_start, storm_end, runoff_volume, peak_discharge, 
         everything()) 

head(data_df_approved3)
str(data_df_approved3)

saveRDS(data_df_approved3, file.path(path_to_data, "Data Publication", 
                                     "CleanedFinalVersions", 
                                     "GLRI_StormEventLoadsFormatted.rds"))


#Clean up model df
data_df_model2 <- data_df_model %>%
  select(intersect(names(data_df_model),
                   all_of(c(common_vars, rain_vars)))) %>%
  select(site, storm_start, storm_end, runoff_volume, peak_discharge, 
         rain, weq, everything())

data_df_model2$duration[data_df_model2$duration <= 0.01666667 & 
                          data_df_model2$rain == 0] <- 0


str(data_df_model2)

data_df_publish <- left_join(data_df_approved2, data_df_model2, 
                              by = all_of(common_vars)) %>%
  select(-exclude, -unique_storm_number) %>%
  arrange(site, storm_start) %>%
  mutate(across(contains("load"), round, 3),
         across(contains("yield"), round, 5),
         across(contains("energy"), round, 1),
         across(contains("erosivity"), round, 3),
         Ievent = round(Ievent, 5),
         weq = round(weq, 2))

head(data_df_publish)

str(data_df_publish)

# #####################################
# Get rain data for 'other' flow events
# #####################################

Rain.uv.combined <- readRDS(file.path(path_to_data, 'compiled_data', 'rain', 'Compiled_Rain_UV_Data.rds')) 

attributes(Rain.uv.combined$pdate)$tzone <- attributes(data_df_publish$storm_start)$tzone

flow_sites <- unique(data_df_publish$site)

flow_site_i <- 1
new_rain_list <- list()
for (flow_site_i in 1:length(flow_sites)){
  
  rain_site_i <- master_beforeafter_df$`rain station ID`[match(flow_sites[flow_site_i], master_beforeafter_df$site)]
  
  Rain.uv.i <- filter(Rain.uv.combined, rain_site == rain_site_i) %>%
    arrange(pdate)

  wq.dat <- filter(data_df_publish, site == flow_sites[flow_site_i])
  
  
precip.dat <- run.rainmaker(precip_raw = Rain.uv.i, ieHr = 2, rainthresh = 0.008, wq.dat = wq.dat,
                            xmin = c(5,10,15,30,60), antecedentDays = c(1,2,7,14))

precip.dat <- rename(precip.dat, 
                     'rain_startdate' = 'StartDate', 
                     'rain_enddate' = 'EndDate')

precip.dat <- wq.dat %>%
  select(site:runoff_volume) %>%
  bind_cols(precip.dat) %>%
  select(-stormnum)

new_rain_list[[flow_site_i]] <- precip.dat

print(flow_sites[flow_site_i])

}

#Compile rain and flow data
#What to do for: events with rain == 0? events with rain from both places that differ?



#save rds file to the P drive
saveRDS(data_df_publish , file=(file_out(file.path(path_to_data, "compiled_data", "storm_event_loads", "storm_event_publish_data.rds"))))

write.csv(data_df_publish, file=(file_out(file.path(path_to_data, "compiled_data", "storm_event_loads", "storm_event_publish_data.csv" ))), row.names=F)



