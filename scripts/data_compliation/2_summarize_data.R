
#Loop through all site folders and load edge of field runoff data

#This is where the data live
print(path_to_data)

if (exists("data_df")){
  print("Object data_df exists. Great job!!! You may proceed")
} else {
  stop ("no data loaded into R environment. Check 1_load_all_data.R script")
}


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
              
data_df2 <- data_df %>%
  mutate(site = as.character(site),
         storm_middate = storm_start + difftime(storm_end, storm_start, units='secs')/2) %>%
  mutate(wateryear = getWY (storm_middate)) %>%
  select(-file_id, -unique_storm_number, -sub_storms, -rain_startdate, -rain_enddate, -storm_start, -storm_end, -sample_end, -sample_start, -ant_discharge_date) 


  # unite("suspended_sediment_load_pounds2", c("suspended_sediment_load_pounds", "Suspended.Sediment.Load..pounds"), na.rm=T, remove=F)  


data_concentration

data_wateryear_summary <- data_df2 %>%
  group_by (site, wateryear, period) %>%
  select(-storm_middate) %>%
  filter(frozen == FALSE) %>% 
  summarize_all(mean, na.rm=T)

data_wateryear_summary
