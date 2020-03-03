
#Loop through all site folders and load edge of field runoff data

library(anytime)

#This is where the data live
print(path_to_data)

#Create empty list for all site data
allsites_list <-list()

#Identify how many files are in the approved_site_data folder
approved_files <- list.files(file.path(path_to_data, "field_analysis", "approved_site_data"))
approved_files <- approved_files[grepl(".csv", approved_files)]
approved_files <- approved_files[which(grepl("Management", approved_files)==FALSE)]
approved_files <- approved_files[which(grepl("_orig", approved_files)==FALSE)]
approved_files <- approved_files[which(grepl("NYTL", approved_files)==FALSE)]
approved_files <- approved_files[which(grepl("paired", approved_files)==FALSE)]



file_nu =1
for (file_nu in 1:length(approved_files)){
  file <- approved_files[file_nu]
  path_to_sitefile <- file.path(path_to_data, "field_analysis", "approved_site_data", file)
  
      #Find site name and timezone  for the site
      
      file_site_list <- unlist(strsplit(file, "-"))
      state <- substr(file_site_list[1], nchar(file_site_list[1])-1, nchar(file_site_list[1]))
      site <- substr(file_site_list[2], 1,3)
      full_site <- paste(state, site, sep='-')

      if (state %in% c('WI')){
        state_tz <- 'America/Chicago'
      } else if (state %in% c('OH', 'NY', 'IN', 'MI')){
        state_tz <- 'America/New_York'
      }
      
      #load and combine all model (mod) files
        data_i <- read.csv(path_to_sitefile, stringsAsFactors = F, header=T) %>%
          mutate(sample_start = anytime(sample_start, tz=state_tz),
               sample_end = anytime(sample_end, tz=state_tz),
               storm_start =anytime(storm_start, tz=state_tz),
               storm_end = anytime(storm_end, tz=state_tz)) %>%
          arrange(storm_start) %>%
          # select(-file_id) %>%
          distinct()
        
        #remove rows that are entirely NA
        data_i[as.logical((rowSums(is.na(data_i))-ncol(data_i))),]
        
        # data_i <- read.csv(path_to_sitefile, stringsAsFactors = F, header=T) %>%
        #   mutate(sample_start = as.POSIXct(sample_start, tz=state_tz, format='%m/%d/%Y %H:%M'),
        #          sample_end = as.POSIXct(sample_end, tz=state_tz, format='%m/%d/%Y %H:%M'),
        #          storm_start = as.POSIXct(storm_start, tz=state_tz, format='%m/%d/%Y %H:%M'),
        #          storm_end = as.POSIXct(storm_end, tz=state_tz, format='%m/%d/%Y %H:%M')) %>%
        #   arrange(storm_start) %>%
        #   # select(-file_id) %>%
        #   distinct()
      
      #place data into list
        allsites_list[[file_nu]]<-data_i
        names(allsites_list)[[file_nu]] <- full_site
    }

#Combine all sites into a single data.frame
data_df <- ldply(allsites_list, data.frame, .id = "site") 

goodvars <- c('suspended_sediment_load_pounds', 
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

goodcolumns <- which(names(data_df) %in% goodvars)

loadcolumns <- which(grepl("load", names(data_df), ignore.case=T))
badcolumns <- setdiff(loadcolumns, goodcolumns)
badnames<- names(data_df)[badcolumns]

#Combine columns that are runoff
runoffcolumns <- which(grepl("runoff", names(data_df), ignore.case=T))
names(data_df)[runoffcolumns]

data_df$runoff_volume[which(is.na(data_df$runoff_volume))] <- data_df$storm_runoff_cubic_feet[which(is.na(data_df$runoff_volume))]

head(data_df)
str(data_df)



if (identical(badnames, c("no2_no3_n_load_pounds", "total_phosphorus_unfiltered_load_pounds",
                          "total_nitrogen_computed_load_pounds", "organic_nitrogen_computed_load_pounds",
                          "Suspended.Sediment.Load..pounds", "Chloride.Load..pounds",
                          "NO2.NO3.N..Load..pounds", "Ammonium..N..Load..pounds",
                          "TKN.Unfiltered.Load..pounds", "Dissolved.Reactive.Phosphorus.Load..pounds",
                          "TP.Unfiltered.Load..pounds", "Total.Nitrogen.Load..in.pounds",
                          "Organic.Nitrogen.Load..pounds")
              )==FALSE) {
  
  stop("check column names in '1_load_all_siteapproved_data.R'. Trying to merge columns, and more columns need to be identified")
  
} else {


replacenames <- c("no2_no3n_load_pounds", "tp_unfiltered_load_pounds", 
                  "total_nitrogen_load_pounds", "organic_nitrogen_load_pounds", 
                  "suspended_sediment_load_pounds", "chloride_load_pounds",
                  "no2_no3n_load_pounds", "ammonium_n_load_pounds",
                  "tkn_unfiltered_load_pounds", "orthophosphate_load_pounds",
                  "tp_unfiltered_load_pounds", "total_nitrogen_load_pounds",
                  "organic_nitrogen_load_pounds", "total_nitrogen_load_pounds")


var=1
for (var in 1:length(badnames)){
  NAs <- which(is.na(data_df[,replacenames[var]]))
  data_df[,replacenames[var]][NAs] <- data_df[,badnames[var]][NAs]
  # print(paste0("Replaced ", toString(length(NAs)), " NAs in col ", toString(loadvars[var]), " with col ", toString(badvars[var])))
  print(paste0("Column ", toString(replacenames[var]), " contained NAs. Replaced with ", toString(length(NAs)), " ",  toString(badnames[var])))
    }

data_df <- data_df %>%
  dplyr::select(-badnames, -storm_runoff_cubic_feet)




saveRDS(data_df, file=(file_out(file.path(path_to_data, "compiled_data", "storm_event_loads", "storm_event_loads_allsites_approved_data.rds" ))))

write.csv(data_df, file=(file_out(file.path(path_to_data, "compiled_data", "storm_event_loads", "storm_event_loads_allsites_approved_data.csv" ))), row.names=F)

merged_sites <- data.frame(sites = as.character(unique(data_df$site)), stringsAsFactors = F)
merged_sites <- arrange(merged_sites, sites)

}

if (nrow(merged_sites) != 20){
  warning(paste0(toString(nrow(merged_sites)), " sites in dataset. Should be 20"))
  print(merged_sites)
} else {
  print("20 sites in dataset. Great job!!!")
}


