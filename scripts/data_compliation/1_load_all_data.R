

#Loop through all site folders and load edge of field runoff data
library(drake)


print(path_to_data)

#Create empty list for all site data
allsites_list <-list()

#Identify how many states have data
states <- list.files(file.path(path_to_data, "analysis"))

state_nu <- 1
for (state_nu in 1:length(states)){
  state <- states[state_nu]
  
  #Create empty list for all sites within this state
  statesites_list <- list()
  
  if (state %in% c('WI')){
    state_tz <- 'America/Chicago'
  } else if (state %in% c('OH', 'NY', 'IN', 'MI')){
    state_tz <- 'America/New_York'
  }
  
  folders<-list.files(file.path(path_to_data, "analysis", state))
  
  #Remove paired and tile folders
  folders <- folders[which(grepl('pair', folders)==FALSE)]
  folders <- folders[which(grepl('TL', folders)==FALSE)]
  
  
  folder_nu<-1
  for (folder_nu in 1:length(folders)){
    #Folder name
    folder<-folders[folder_nu]
    
    #Identify model files
    rundate_folders<-list.files(file.path(path_to_data, "analysis", state, folder, "results"))
    rundates <- as.Date(paste0(rundate_folders, "-01"), format="%Y-%m-%d")
    recent_folder <- rundate_folders[which.max(rundates)]
    
    tables <- list.files(file.path(path_to_data, "analysis", state, folder, "results", recent_folder, "tables" ))
    
    mod_file <- tables[grepl("mod_dat.csv" , tables)]
    
    if (length(mod_file)>1){
      warning(paste("More than 1 file listed with 'mod_dat.csv'. Using first file. Check folder:",  toString(file.path(path_to_data, "analysis", state, folder, "results", recent_folder, "tables" ))))
      mod_file <- mod_file[1]
    }
  
    #Load site name, timezone, and other metadata for the site
    # source(file.path(path_to_data, folder, 'scripts/0_master_file.R'), echo = F)
    # print(site_tz)
    # print(site)
    
    file_site_name <- paste(unlist(strsplit(mod_file, "_"))[1], unlist(strsplit(mod_file, "_"))[2], sep="_")
    
    # if (identical(site, file_site_name)==FALSE){
    #   warning(paste("Site name in `0_master_file.R` does not match site name in data_cached folder:", toString(folder)))
    #   }
    
    #Identify model (mod) file and load
    mod_path <- file.path(path_to_data, "analysis", state, folder, "results", recent_folder, "tables", mod_file)
    data_i <- read.csv(file_in(mod_path), stringsAsFactors = F, header=T)
    
    #Change timezone based on `0_master_file` script
    data_i <- data_i %>%
      mutate(storm_start = as.POSIXct(storm_start, tz=state_tz),
             storm_end = as.POSIXct(storm_end, tz=state_tz),
             rain_startdate = as.POSIXct(rain_startdate, tz=state_tz),
             rain_enddate = as.POSIXct(rain_enddate, tz=state_tz),
             ant_discharge_date = as.POSIXct(ant_discharge_date, tz=state_tz))
    
    #place data frame into a list
    statesites_list[[folder_nu]]<-data_i
    names(statesites_list)[[folder_nu]] <- file_site_name
    
  }
  allsites_list[[state_nu]] <- ldply(statesites_list, data.frame, .id = "site")
  
}

#Combine all sites into a single data.frame
data_df <- ldply(allsites_list, data.frame, .id = "state")


head(data_df)
str(data_df)

