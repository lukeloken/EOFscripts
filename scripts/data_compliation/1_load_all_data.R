
#Loop through all site folders and load edge of field runoff data

#This is where the data live
print(path_to_data)

#Create empty list for all site data
allsites_list <-list()

#Identify how many states have data
states <- list.files(file.path(path_to_data, "analysis"))

state_nu <- 1
for (state_nu in 1:length(states)){
  state <- states[state_nu]
  

  if (state %in% c('WI')){
    state_tz <- 'America/Chicago'
  } else if (state %in% c('OH', 'NY', 'IN', 'MI')){
    state_tz <- 'America/New_York'
  }
  
  folders<-list.files(file.path(path_to_data, "analysis", state))
  
  #Create empty list for all sites within this state
  statesites_list <- vector(mode="list", length=length(folders))
  names(statesites_list) <- folders
  
  #Remove paired and tile folders
  # folders <- folders[which(grepl('pair', folders)==FALSE)]
  # folders <- folders[which(grepl('TL', folders)==FALSE)]
  
  
  folder_nu<-1
  for (folder_nu in 1:length(folders)){
    #Folder name
    folder<-folders[folder_nu]
    
    #Identify model files
    rundate_folders<-list.files(file.path(path_to_data, "analysis", state, folder, "results"))
    rundates <- as.Date(paste0(rundate_folders, "-01"), format="%Y-%m-%d")
    recent_folder <- rundate_folders[which.max(rundates)]
    
    tables <- list.files(file.path(path_to_data, "analysis", state, folder, "results", recent_folder, "tables" ), full.names=T)
    
    mod_files <- tables[grepl("mod_dat.csv" , tables)]
    
    
    if (length(mod_files) == 0){
      warning(paste("folder contains no `mod_dat.csv` files.", toString(file.path(path_to_data, "analysis", state, folder, "results", recent_folder, "tables" ))))
    } else { 
      if (length(mod_files)>1){
        warning(paste("More than 1 file listed with 'mod_dat.csv'. Combined multiple files. Check folder:",  toString(file.path(path_to_data, "analysis", state, folder, "results", recent_folder, "tables" ))))
      } 
      
      #Load site name, timezone, and other metadata for the site
      # source(file.path(path_to_data, folder, 'scripts/0_master_file.R'), echo = F)
      # print(site_tz)
      # print(site)
      
      file_site_list <- strsplit(mod_files, "_")
      name_length <- unlist(lapply(file_site_list, length))
      file_site_name <- tail(unlist(strsplit(unlist(file_site_list[which.min(name_length)])[1], "/")),1)
      
      # if (identical(site, file_site_name)==FALSE){
      #   warning(paste("Site name in `0_master_file.R` does not match site name in data_cached folder:", toString(folder)))
      #   }
      
      #load and combine all model (mod) files
      
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
      
      #place data into state list
      statesites_list[[folder_nu]]<-data_i
      # names(statesites_list)[[folder_nu]] <- file_site_name
      
    }
  }
  
  allsites_list[[state_nu]] <- ldply(statesites_list, data.frame, .id = "site")
  
}

#Combine all sites into a single data.frame
data_df <- ldply(allsites_list, data.frame, .id = "state") 

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
              'no2_no3n_load_pounds')

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
             'no2_no3_n_load_pounds')

# combine columns with the same data but differnet names
# For some reason tidy:unite was not working for me

var=1
for (var in 1:length(badvars)){
  NAs <- which(is.na(data_df[,loadvars[var]]))
  data_df[,loadvars[var]][NAs] <- data_df[,badvars[var]][NAs]
  print(paste ("Replaced ", toString(length(NAs)), "NAs in variable ", toString(loadvars[var])))
    }

data_df <- data_df %>%
  dplyr::select(-badvars)


rm(allsites_list, data_i, file_site_list, statesites_list)
rm(file_site_name, folder, folder_nu, folders, mod_files, name_length, recent_folder, rundate_folders, rundates, state, state_nu, state_tz, states, tables, badvars, NAs, loadvars)

saveRDS(data_df, file=(file_out(file.path(path_to_data, "compiled_data", "storm_event_loads", "storm_event_loads_allsites.rds" ))))

write.csv(data_df, file=(file_out(file.path(path_to_data, "compiled_data", "storm_event_loads", "storm_event_loads_allsites.csv" ))), row.names=F)
