
#Loop through all site folders and load edge of field runoff data

#This is where the data live
print(path_to_data)

#Create empty list for all site data
allsites_list <-list()

#Identify how many states have data
folders <- list.files(file.path(path_to_data, "field_analysis", "results"))
folders <- folders[which(grepl("pair", folders)==FALSE)]
# states <- unique(substr(states, 1,2))
# states <- list.files(file.path(path_to_data, "analysis"))[1:2]

folder_nu <- 1
for (folder_nu in 1:length(folders)){
  folder <- folders[folder_nu]
  
  state <- substr(folder, 1,2)
  
  file_site_name <- folder

  if (state %in% c('WI')){
    state_tz <- 'America/Chicago'
  } else if (state %in% c('OH', 'NY', 'IN', 'MI')){
    state_tz <- 'America/New_York'
  }
  

    #Identify model files
    rundate_folders<-list.files(file.path(path_to_data, "field_analysis", "results", folder))
    rundates <- as.Date(paste0(rundate_folders, "-01"), format="%Y-%m-%d")
    recent_folder <- rundate_folders[which.max(rundates)]
    
    tables <- list.files(file.path(path_to_data, "field_analysis", "results", folder, recent_folder, "tables" ), full.names=T)
    
    mod_files <- tables[grepl("mod_dat.csv" , tables)]
    
    if (length(mod_files) == 0){
      warning(paste("folder contains no `mod_dat.csv` files.", toString(file.path(path_to_data, "analysis", state, folder, "results", recent_folder, "tables" ))))
    } else { 
      if (length(mod_files)>1){
        
        mod_files <- mod_files[which.min(nchar(mod_files))]
        mod_name <- tail(unlist(strsplit(mod_files[1], split='/')),1)
        
        warning(paste("More than 1 file listed with 'mod_dat.csv'. Used file with shortest name:", mod_name, "    Check folder:",  toString(file.path(path_to_data, "field analysis", "results", folder, recent_folder, "tables" ))))
        
      } 
      

      data_i <- lapply(mod_files, read.csv, stringsAsFactors = F, header=T) %>%
        bind_rows(.id = "file_id") %>%
        arrange(unique_storm_number) %>%
        # select(-file_id) %>%
        distinct() %>%
        mutate(storm_start = as.POSIXct(storm_start, tz=state_tz),
               storm_end = as.POSIXct(storm_end, tz=state_tz),
               rain_startdate = as.POSIXct(rain_startdate, tz=state_tz),
               rain_enddate = as.POSIXct(rain_enddate, tz=state_tz),
               ant_discharge_date = as.POSIXct(ant_discharge_date, tz=state_tz))
      
      #place data into list
      allsites_list[[folder_nu]]<-data_i

    }
}

    
      data_df <- ldply(allsites_list, data.frame, .id = "site")
  

loadvars <- c('suspended_sediment_load_pounds', 
              'chloride_load_pounds',
              'no2_no3n_load_pounds', 
              'ammonium_n_load_pounds',
              'tkn_unfiltered_load_pounds', 
              'orthophosphate_load_pounds',
              'tp_unfiltered_load_pounds',
              'total_nitrogen_load_pounds',
              'organic_nitrogen_load_pounds',
              'tp_unfiltered_load_pounds',
              'no2_no3n_load_pounds',
              'total_nitrogen_load_pounds',
              'organic_nitrogen_load_pounds')

badvars <- c('Suspended.Sediment.Load..pounds', 
             'Chloride.Load..pounds',
             'NO2.NO3.N..Load..pounds', 
             'Ammonium..N..Load..pounds',
             'TKN.Unfiltered.Load..pounds', 
             'Dissolved.Reactive.Phosphorus.Load..pounds',
             'TP.Unfiltered.Load..pounds',
             'Total.Nitrogen.Load..in.pounds',
             'Organic.Nitrogen.Load..pounds',
             'total_phosphorus_unfiltered_load_pounds',
             'no2_no3_n_load_pounds', 
             'total_nitrogen_computed_load_pounds',
             'organic_nitrogen_computed_load_pounds')

# combine columns with the same data but differnet names
# For some reason tidy:unite was not working for me

var=1
for (var in 1:length(badvars)){
  NAs <- which(is.na(data_df[,loadvars[var]]))
  data_df[,loadvars[var]][NAs] <- data_df[,badvars[var]][NAs]
  # print(paste0("Replaced ", toString(length(NAs)), " NAs in col ", toString(loadvars[var]), " with col ", toString(badvars[var])))
  print(paste0("Column ", toString(loadvars[var]), " contained ", toString(length(NAs)), " NAs. Replaced with ", toString(badvars[var])))
    }

data_df <- data_df %>%
  dplyr::select(-badvars)


rm(allsites_list, data_i)
rm(file_site_name, folder, folder_nu, folders, mod_files, recent_folder, rundate_folders, rundates, state, state_tz, tables, badvars, NAs, loadvars)

saveRDS(data_df, file=(file_out(file.path(path_to_data, "compiled_data", "storm_event_loads", "storm_event_loads_allsites_model_data.rds" ))))

write.csv(data_df, file=(file_out(file.path(path_to_data, "compiled_data", "storm_event_loads", "storm_event_loads_allsites_model_data.csv" ))), row.names=F)
