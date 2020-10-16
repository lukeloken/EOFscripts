

#Script to merge all EOF projects

final_files <- list.files(file.path(path_to_data, "Data Publication", "CleanedFinalVersions"),
                          full.names = TRUE)

final_files <- final_files[!grepl('All_EOF', final_files)]

final_df <- lapply(final_files, readRDS) %>%
  bind_rows(.id = NULL) 

unique(site_table$USGSSiteNumber[which(!site_table$USGSSiteNumber %in% final_df$site)])
unique(final_df$site[which(!final_df$site %in% site_table$USGSSiteNumber)])
unique(final_df$FieldName[which(!final_df$site %in% site_table$USGSSiteNumber)])

unique(final_df$site)
unique(final_df$FieldName)
unique(final_df$project)


length(which(!unique(final_df$site) %in% site_table$USGSSiteNumber))

#Test are all site numbers and field names in the site_table?
if(
  all(unique(final_df$site) %in% site_table$USGSSiteNumber) & 
  all(unique(final_df$FieldName) %in% site_table$FieldName)) {
  print('Great Job!!! All site numbers and field names are in the site table')
} else {
  unique(final_df$site[!final_df$site %in% site_table$USGSSiteNumber])
  unique(final_df$FieldName[!final_df$FieldName %in% site_table$FieldName]) 
  warning("Above site numbers and/or field names are not in site_table. 
          Check original site spreadsheets")
}


combined_df <- final_df %>%
  group_by(site, FieldName, project, unique_storm_number) %>%
  mutate(across(contains("conc_mgL"), ~.*runoff_volume)) %>%
  summarize(storm_start = min(storm_start, na.rm = TRUE),
            storm_end  = max(storm_end, na.rm = TRUE),
            across(discrete:storm, ~max(., na.rm = TRUE)),
            exclude = max(exclude, na.rm = TRUE),
            runoff_volume_total = sum(runoff_volume, na.rm = TRUE),
            peak_discharge = max(peak_discharge, na.rm = TRUE),
            across(contains("yield"), ~sum(., na.rm = TRUE)),
            across(contains("load"), ~sum(., na.rm = TRUE)),
            across(contains("flag"), ~paste(unique(.)[-which(is.na(unique(.)) |
                                                               unique(.) == "")], 
                                            collapse = " | ")),
            across(contains("conc_mgL"), ~sum(., na.rm = FALSE)),
            n_substorms = n(),
            .groups = "drop") %>%
  mutate(across(contains("conc_mgL"), ~./runoff_volume_total)) %>%
  rename(runoff_volume = runoff_volume_total)

head(data.frame(combined_df))

 if (sum(combined_df$n_substorms-1) == nrow(final_df) - nrow(combined_df)){
   print("Number of sub-storms match! Great job!")
 }

site_summary_report <- combined_df %>%
  filter(is.finite(storm_start), is.finite(storm_end)) %>%
  group_by(site, FieldName, project, estimated) %>%
  dplyr::summarise(first_storm = min(as.Date(storm_start), na.rm = TRUE),
                   last_storm = max(as.Date(storm_start), na.rm = TRUE),
                   number_of_storms = n(),
                   number_of_substorms = sum(n_substorms),
                   runoff_volume = round(sum(runoff_volume, na.rm=T),0),
                   .groups = "drop") %>%
  spread(key=estimated, value=runoff_volume) %>%
  rename(Volume_Estimated = '1',
         Volume_Measured = '0') 

site_summary_report$Number_Estimated <- ifelse(is.na(site_summary_report$Volume_Estimated), NA, 
                                               site_summary_report$number_of_storms)

site_summary_report$Number_Measured <- ifelse(is.na(site_summary_report$Volume_Measured), NA, 
                                              site_summary_report$number_of_storms)

site_summary_report2 <- site_summary_report %>%
  group_by(site, FieldName, project) %>%
  summarize_at(vars(number_of_storms:Number_Measured), sum, na.rm=T) %>%
  mutate(fraction_volume = round(Volume_Measured / (Volume_Measured + Volume_Estimated),2),
         fraction_number = round(Number_Measured / (Number_Measured + Number_Estimated),2)) %>%
  left_join(select(site_summary_report, site, first_storm, last_storm) %>%
              group_by(site) %>%
              summarize(first_storm = min(first_storm, na.rm=T),
                        last_storm = max(last_storm, na.rm=T))) %>%
  select(site, FieldName, project, first_storm, last_storm, number_of_storms, number_of_substorms, Number_Measured, Number_Estimated, fraction_number, Volume_Measured, Volume_Estimated, fraction_volume, everything()) %>%
  arrange(project, FieldName)

print(data.frame(site_summary_report2[,1:5]))
print(data.frame(site_summary_report2))


if (nrow(site_summary_report2) != nrow(site_table[which(site_table$FieldName != "NY-SW3"),])
){
  warning(paste0(toString(nrow(site_summary_report2)), " sites in dataset. Should be ", toString(nrow(site_table[which(site_table$FieldName != "NY-SW3"),]))))
  # print(merged_sites)
} else {
  print("Correct number of sites in dataset. Great job!!!")
}


write.csv(site_summary_report2, 
          file=file.path(path_to_data,
                         "Data Publication",
                         "All.EOF.site.summary.report.csv"),
          row.names=F)


saveRDS(final_df, file.path(path_to_data, "Data Publication", 
                            "CleanedFinalVersions", 
                            "All_EOF_StormEventLoadsFormatted.rds"))


saveRDS(combined_df, file.path(path_to_data, "Data Publication", 
                            "CleanedFinalVersions", 
                            "All_EOF_StormEventLoadsCombined.rds"))


