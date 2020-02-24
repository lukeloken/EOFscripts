
#Summarize storm event load data for all sites

print(path_to_data)

#Load the rds file from the P drive
#This file was created using the "1_load_all_data.R" script
data_df <- readRDS(file_in(file.path(path_to_data, "compiled_data", "storm_event_loads", "storm_event_loads_allsites_model_data.rds" )))

#watershed areas
EOF_areas <-read.csv(file=file_in(file.path(path_to_data, 'SiteCharacteristics','EOF_WatershedAreas.csv'))) %>%
  rename(site = Site)



#Test to make sure data are loaded
if (exists("data_df")){
  print("Object data_df exists. Great job!!! You may proceed")
} else {
  stop ("no data loaded into R environment. Check 1_load_all_data.R script and path_to_data")
}

#Identify load variables
loadvars <- c('suspended_sediment_load_pounds', 
              'chloride_load_pounds',
              'no2_no3n_load_pounds', 
              'ammonium_n_load_pounds',
              'tkn_unfiltered_load_pounds', 
              'orthophosphate_load_pounds',
              'tp_unfiltered_load_pounds',
              'total_nitrogen_load_pounds',
              'organic_nitrogen_load_pounds',
              'toc_load_pounds',
              'doc_load_pounds')

#Identify concentration variables
concvars <- c('suspended_sediment_conc_mgL', 
              'chloride_conc_mgL',
              'no2_no3n_conc_mgL', 
              'ammonium_n_conc_mgL',
              'tkn_unfiltered_conc_mgL', 
              'orthophosphate_conc_mgL',
              'tp_unfiltered_conc_mgL',
              'total_nitrogen_conc_mgL',
              'organic_nitrogen_conc_mgL',
              'toc_conc_mgL',
              'doc_conc_mgL')

#Identify yield variables
yieldvars <- c('suspended_sediment_yield_poundperAcre', 
              'chloride_yield_poundperAcre',
              'no2_no3n_yield_poundperAcre', 
              'ammonium_n_yield_poundperAcre',
              'tkn_unfiltered_yield_poundperAcre', 
              'orthophosphate_yield_poundperAcre',
              'tp_unfiltered_yield_poundperAcre',
              'total_nitrogen_yield_poundperAcre',
              'organic_nitrogen_yield_poundperAcre',
              'toc_yield_poundperAcre',
              'doc_yield_poundperAcre')

#Identify rain variables
rainvars <-c("rain", "duration", "Ievent", "I5", "I10", "I15", "I30", "I60",
             "energy_m1", "erosivity_m1", "energy_m2", "erosivity_m2")

# Calculate storm mid date. 
# Remove other date time objects
data_df2 <- data_df %>%
  mutate(site = as.character(site),
         storm_middate = storm_start + difftime(storm_end, storm_start, units='secs')/2) %>%
  mutate(wateryear = as.factor(getWY (storm_middate))) %>%
  dplyr::select(-file_id, -unique_storm_number, -sub_storms, -rain_startdate, -rain_enddate, -storm_start, -storm_end, -ant_discharge_date) %>%
  left_join(EOF_areas)

# constants for converstions from load (pounds) and volume (cf) to concentration (mg/L)
# 453592 mg per pound
# 28.3168 Liters per cubic foot

#Calculate concentration (mg per L) from load (pounds) and runoff volume (cf)
conc_df <- data.frame(sapply(data_df2[,loadvars], function (x) x/data_df2$runoff_volume*453592/28.3168))
colnames(conc_df) <- concvars

# calculate yield (pounds per acre) 
yield_df <- data.frame(sapply(data_df[,loadvars], function (x) x/data_df2$Area_acres))
colnames(yield_df) <- yieldvars

yieldperweq_df <- data.frame(sapply(data_df[,loadvars], function (x) x/data_df2$Area_acres/data_df2$weq))
colnames(yieldperweq_df) <- paste0(yieldvars, "perInchWEQ")



#Bind concentration data frame to bigger data frame
data_df3 <- bind_cols(data_df2, conc_df) %>%
  bind_cols(yield_df) %>%
  bind_cols(yieldperweq_df)




#save rds file to the P drive
#This file was created using the "1_load_all_data.R" script
saveRDS(data_df3 , file=(file_out(file.path(path_to_data, "compiled_data", "storm_event_loads", "storm_event_conc_allsites_model.rds"))))

write.csv(data_df3, file=(file_out(file.path(path_to_data, "compiled_data", "storm_event_loads", "storm_event_conc_allsites_model.csv" ))), row.names=F)



