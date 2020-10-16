
#Loop through all site folders and load edge of field runoff data

#This is where the data live
print(path_to_data)

#Master table
master_beforeafter_df <- readRDS(file.path(path_to_data, 'compiled_data', 'rain', 'Compiled_Masters.rds'))

#Create empty list for all site data
allsites_list <-list()

#Identify how many states have data
folders <- list.files(file.path(path_to_data, "field_analysis", "results"))
folders <- folders[which(grepl("pair", folders)==FALSE)]
folders <- folders[which(grepl(".txt", folders)==FALSE)]

# states <- unique(substr(states, 1,2))
# states <- list.files(file.path(path_to_data, "analysis"))[1:2]

folder_nu <- 1
for (folder_nu in 1:length(folders)){
  state_tz <- full_site <- state <- site <- datetime_format <- NA
  
  
  folder <- folders[folder_nu]
  
  state <- substr(folder, 1,2)
  
  file_site_name <- folder

  state_tz <- master_beforeafter_df$site_tz[match(file_site_name, master_beforeafter_df$site)]
  # datetime_format <- master_beforeafter_df$datetime_format[match(file_site_name, master_beforeafter_df$site)]
  
  # if (state %in% c('WI')){
  #   state_tz <- 'America/Chicago'
  # } else if (state %in% c('OH', 'NY', 'IN', 'MI')){
  #   state_tz <- 'America/New_York'
  # }
  

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
      names(allsites_list)[[folder_nu]] <- file_site_name

    }
}

    
      data_df <- ldply(allsites_list, data.frame, .id = "site")
  
      
      
      #New code to merge load variables
      
      
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
      
      
      #Identify multiple columns for each variable and data type
      sediment_names <- names(data_df)[grepl('sediment', names(data_df), ignore.case=T)]
      chloride_names <- names(data_df)[grepl('chloride', names(data_df), ignore.case=T)]
      nitrate_names <- names(data_df)[grepl('no3', names(data_df), ignore.case=T)]
      ammonium_names <- names(data_df)[grepl('Ammonium', names(data_df), ignore.case=T)]
      TKN_names <- names(data_df)[grepl('tkn', names(data_df), ignore.case=T)]
      SRP_names <- unique(c(names(data_df)[grepl('ortho', names(data_df), ignore.case=T)], 
                            names(data_df)[grepl('Reactive.Phosphorus', names(data_df), ignore.case=T)]))
      TP_names <- unique(c(names(data_df)[grepl('tp', names(data_df), ignore.case=T)], 
                           names(data_df)[grepl('total_phosphorus', names(data_df), ignore.case=T)]))
      TN_names <- unique(c(names(data_df)[grepl('total_nitrogen', names(data_df), ignore.case=T)], 
                           names(data_df)[grepl('Total.Nitrogen', names(data_df), ignore.case=T)]))
      organicN_names <-unique(c(names(data_df)[grepl('organic_nitrogen', names(data_df), ignore.case=T)], 
                                names(data_df)[grepl('organic.nitrogen', names(data_df), ignore.case=T)]))
      DOC_names <- names(data_df)[grepl('doc', names(data_df), ignore.case=T)]
      TOC_names <- names(data_df)[grepl('toc', names(data_df), ignore.case=T)]
      
      #Combine names into a list. The order of this list must match the good column names listed above
      names_list <- list(sediment_names, chloride_names, nitrate_names, ammonium_names, 
                         TKN_names,  SRP_names,
                         TP_names, TN_names, organicN_names, DOC_names, TOC_names)
      
      #Load and concentration
      load_names <- unique(c(names(data_df)[grepl('Load', names(data_df), ignore.case=T)],
                             names(data_df)[grepl('lound', names(data_df), ignore.case=T)]))
      # conc_names <- names(data_df)[grepl('mg', names(data_df), ignore.case=T)]
      # flag_names <- names(data_df)[grepl('flag', names(data_df), ignore.case=T)]
      
      
      #Loop through each variable and combine concentration and load data
      var_i = 1
      data_df_new <- data_df
      for (var_i in 1:length(names_list)){

        #merge load
        load_names_i <- intersect(load_names, names_list[[var_i]])
        good_load_name <- loadvars[var_i]
        
        data_load_i <- data_df %>%
          select(all_of(load_names_i)) %>%
          mutate_all(as.character) 
        
        data_load_i_unite <- unite(data_load_i, col=merge_var, 1:(length(load_names_i)), na.rm=T)
        
        names(data_load_i_unite)[1] <- as.character(good_load_name)
        
        data_load_i_unite <- mutate_all(data_load_i_unite, as.numeric) %>%
          mutate(site_load = data_df$site)
        
        
        data_df_new <- data_df_new %>%
          select(-all_of(load_names_i)) %>%
          bind_cols(data_load_i_unite[,1:2]) %>%
          select(-site_load)
        
      }
      
      

rm(allsites_list, data_i)
rm(file_site_name, folder, folder_nu, folders, mod_files, recent_folder, rundate_folders, rundates, state, state_tz, tables, badvars, NAs, loadvars)

saveRDS(data_df_new, file=(file_out(file.path(path_to_data, "compiled_data", "storm_event_loads", "storm_event_loads_allsites_model_data.rds" ))))

write.csv(data_df_new, file=(file_out(file.path(path_to_data, "compiled_data", "storm_event_loads", "storm_event_loads_allsites_model_data.csv" ))), row.names=F)

