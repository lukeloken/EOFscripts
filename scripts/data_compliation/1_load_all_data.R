
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
      
      # #Preferred concentration names
      # concvars <- c('suspended_sediment_conc_mgL', 
      #               'chloride_conc_mgL',
      #               'no2_no3_n_conc_mgL', 
      #               'ammonium_n_conc_mgL',
      #               'tkn_unfiltered_conc_mgL', 
      #               'orthophosphate_conc_mgL',
      #               'total_phosphorus_conc_mgL',
      #               'total_nitrogen_conc_mgL',
      #               'organic_nitrogen_conc_mgL',
      #               'doc_conc_mgL',
      #               'toc_conc_mgL')
      # 
      # flagvars <- c("flag_suspended_sediment", "flag_chloride", "flag_no2_no3n", 
      #               "flag_ammonium_n", "flag_tkn_unfiltered", "flag_orthophosphate",
      #               "flag_tp_unfiltered", "flag_tn", "flag_orgN", "flag_doc", "flag_toc" )
      
      
      
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
        
        # data_flag_i_unite <- data_flag_i <- '.dplyr'
        # 
        # #find flags
        # flag_names_i <- intersect(flag_names, names_list[[var_i]])
        # 
        # if (length(flag_names_i)>0) {
        #   
        #   data_flag_i <- data_df %>%
        #     select(all_of(flag_names_i)) %>%
        #     mutate_all(as.character)
        #   
        #   data_flag_i_unite <- unite(data_flag_i, col=merge_var, 1:(length(flag_names_i)), na.rm=T, sep='| ')
        #   
        #   names(data_flag_i_unite)[1] <- as.character(flagvars[var_i])
        # } else { 
        #   data_flag_i_unite <- data.frame(rep("", nrow(data_df)))
        #   names(data_flag_i_unite)[1] <- as.character(flagvars[var_i])
        # }
        # 
        # 
        # 
        # #Identify concentration names to merge/compare
        # conc_names_i <- intersect(conc_names, names_list[[var_i]])
        # good_conc_name <- concvars[var_i]
        # 
        # data_conc_i <- data_df %>%
        #   select(all_of(conc_names_i)) %>%
        #   mutate_all(as.character)
        # 
        # data_conc_i_unite <- unite(data_conc_i, col=merge_var, 1:(length(conc_names_i)), na.rm=T)
        # 
        # names(data_conc_i_unite)[1] <- as.character(good_conc_name)
        # 
        # data_conc_i_unite <- data_conc_i_unite %>%
        #   bind_cols(data.frame('site_conc' = data_df[,c('site')])) %>%
        #   bind_cols(data_flag_i_unite)
        # 
        # data_conc_i_unite$used_conc_numeric <- as.numeric(gsub("<", "", data_conc_i_unite[,1]))
        # 
        # #For values with less than symbol, assign a value of half of MDL
        # Below_mdl_conc <- grepl("<", data_conc_i_unite[,3])
        # data_conc_i_unite$used_conc_numeric[Below_mdl_conc] <- data_conc_i_unite$used_conc_numeric[Below_mdl_conc]/2
        # 
        # 
        # names(data_conc_i_unite)[which(names(data_conc_i_unite)=='used_conc_numeric')] <- 
        #   paste0(good_conc_name, '_used')
        # 
        # # data_conc_i_unite <- data_conc_i_unite 
        # # 
        # # data_conc_i_unite$reported_conc_numeric_half <-   data_conc_i_unite$reported_conc_numeric / 2
        # 
        # #For values with less than symbol, assign a value of half of MDL
        # # Below_mdl_conc <- grepl("<", data_conc_i_unite[,1])
        # # data_conc_i_unite[Below_mdl_conc,1] <- as.numeric(gsub("<", "", data_conc_i_unite[Below_mdl_conc,1]))/2
        # 
        
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
        
        
        # Ratio <- data_i_unite[,1] / data_i_unite[,2]
        # Diff <- data_i_unite[,1] - data_i_unite[,2]
        # 
        # plot(Diff, main=good_name)
        # plot(Ratio, main=good_name)
        
        #Compare calculated (from loads) with united column
        # plot_list[[var_i]] <- ggplot(data_i_unite, aes_string(x=names(data_i_unite)[1], y=names(data_i_unite)[2])) +
        #   geom_point(aes(col=site), alpha=.5, size=2) +
        #   geom_abline() +
        #   theme_bw() +
        #   scale_x_log10nice(name='merged concentrations') +
        #   scale_y_log10nice(name='calculated from load') +
        #   ggtitle(good_name)
        # 
        data_df_new <- data_df_new %>%
          select(-all_of(load_names_i)) %>%
          bind_cols(data_load_i_unite[,1:2]) %>%
          select(-site_load)
        
      }
      
      
      #Old code to merge variables with different names
      
      
# 
# loadvars <- c('suspended_sediment_load_pounds', 
#               'chloride_load_pounds',
#               'no2_no3n_load_pounds', 
#               'ammonium_n_load_pounds',
#               'tkn_unfiltered_load_pounds', 
#               'orthophosphate_load_pounds',
#               'tp_unfiltered_load_pounds',
#               'total_nitrogen_load_pounds',
#               'organic_nitrogen_load_pounds',
#               'tp_unfiltered_load_pounds',
#               'no2_no3n_load_pounds',
#               'total_nitrogen_load_pounds',
#               'organic_nitrogen_load_pounds')
# 
# badvars <- c('Suspended.Sediment.Load..pounds', 
#              'Chloride.Load..pounds',
#              'NO2.NO3.N..Load..pounds', 
#              'Ammonium..N..Load..pounds',
#              'TKN.Unfiltered.Load..pounds', 
#              'Dissolved.Reactive.Phosphorus.Load..pounds',
#              'TP.Unfiltered.Load..pounds',
#              'Total.Nitrogen.Load..in.pounds',
#              'Organic.Nitrogen.Load..pounds',
#              'total_phosphorus_unfiltered_load_pounds',
#              'no2_no3_n_load_pounds', 
#              'total_nitrogen_computed_load_pounds',
#              'organic_nitrogen_computed_load_pounds')
# 
# # combine columns with the same data but differnet names
# # For some reason tidy:unite was not working for me
# 
# var=1
# for (var in 1:length(badvars)){
#   NAs <- which(is.na(data_df[,loadvars[var]]))
#   data_df[,loadvars[var]][NAs] <- data_df[,badvars[var]][NAs]
#   # print(paste0("Replaced ", toString(length(NAs)), " NAs in col ", toString(loadvars[var]), " with col ", toString(badvars[var])))
#   print(paste0("Column ", toString(loadvars[var]), " contained ", toString(length(NAs)), " NAs. Replaced with ", toString(badvars[var])))
#     }
# 
# data_df <- data_df %>%
#   dplyr::select(-badvars)


rm(allsites_list, data_i)
rm(file_site_name, folder, folder_nu, folders, mod_files, recent_folder, rundate_folders, rundates, state, state_tz, tables, badvars, NAs, loadvars)

saveRDS(data_df_new, file=(file_out(file.path(path_to_data, "compiled_data", "storm_event_loads", "storm_event_loads_allsites_model_data.rds" ))))

write.csv(data_df_new, file=(file_out(file.path(path_to_data, "compiled_data", "storm_event_loads", "storm_event_loads_allsites_model_data.csv" ))), row.names=F)

