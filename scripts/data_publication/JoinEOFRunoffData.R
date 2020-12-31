
#Script to merge all EOF projects

final_files <- list.files(file.path(path_to_data, "Data Publication", "CleanedFinalVersions"),
                          full.names = TRUE)

final_files <- final_files[!grepl('All_EOF', final_files)]
final_files <- final_files[grepl('.rds', final_files)]

final_df <- lapply(final_files, readRDS) %>%
  bind_rows(.id = NULL) %>%
  filter(exclude == 0) %>%
  select(-exclude) %>%
  filter(abs(runoff_volume) > 0) 

attributes(final_df$storm_end)$tzone <- "UTC"
attributes(final_df$storm_start)$tzone <- "UTC"

#Three flow events have an NA in the discrete column
final_df$discrete[is.na(final_df$discrete)] <- 0

# x = flagvars[1]
final_df[,flagvars] <- sapply(flagvars, function(x) ifelse(grepl("<", final_df[,x]), "<", ""))

setdiff(names(final_df), c(concvars, loadvars, yieldvars, flagvars))
setdiff(c(concvars, loadvars, yieldvars, flagvars), names(final_df))

rounding_table <- data.frame(concvars, loadvars, yieldvars, 
                             sigdigits = c(1,1,2,3,2,3,3,1,1,1,1,2,3,1,1,1,1))

final_df[which(is.na(final_df$peak_discharge)),]
final_df[which(final_df$peak_discharge == 0),]

# final_df$runoff_volume[which(final_df$runoff_volume < 0.1 & final_df$runoff_volume >= 0.05)] <- 0.1

final_df <- final_df %>%
  left_join(select(site_table, site = USGS_Station_Number, Area)) %>%
  mutate(runoff_volume = round(runoff_volume+0.001, 1), 
         peak_discharge = round(peak_discharge+0.0000001, 5),
         runoff_inches = round(runoff_volume/Area/43560*12, digits = 6)) %>%
  select(-Area) %>%
  select(all_of(common_vars), runoff_inches, everything())



# min(final_df[,concvars], na.rm = TRUE)

if (length(intersect(names(final_df), c(concvars, loadvars, yieldvars, flagvars)) == 66)){
  print("Great job. Column names are consistent among projects")
} else {
  warning("Check variable names in project files. Likely need to change GLRI sites to match")
}


unique(site_table$USGS_Station_Number[which(!site_table$USGS_Station_Number %in% final_df$site)])
unique(site_table$Field_Name[which(!site_table$USGS_Station_Number %in% final_df$site)])
unique(final_df$site[which(!final_df$site %in% site_table$USGS_Station_Number)])
unique(final_df$Field_Name[which(!final_df$site %in% site_table$USGS_Station_Number)])

unique(final_df$site)
unique(final_df$FieldName)
unique(final_df$project)

length(which(!unique(final_df$site) %in% site_table$USGS_Station_Number))


#Test are all site numbers and field names in the site_table?
if(
  all(unique(final_df$site) %in% site_table$USGS_Station_Number) & 
  all(unique(final_df$Field_Name) %in% site_table$Field_Name)) {
  print('Great Job!!! All site numbers and field names are in the site table')
} else {
  print(unique(final_df$site[!final_df$site %in% site_table$USGS_Station_Number]))
  print(unique(final_df$Field_Name[!final_df$Field_Name %in% site_table$Field_Name])) 
  final_df <- final_df %>%
    filter(site %in% site_table$USGS_Station_Number)
  warning("Above site numbers and/or field names are not in site_table and are being dropped. 
          Check original site spreadsheets")
}


combined_df <- final_df %>%
  group_by(site, Field_Name, project, unique_storm_number) %>%
  mutate(across(contains("conc_mgL"), ~.*runoff_volume)) %>%
  summarize(n_sub_flow_events = n(),
            storm_start = min(storm_start, na.rm = TRUE),
            storm_end  = max(storm_end, na.rm = TRUE),
            across(discrete:storm, ~max(., na.rm = TRUE)),
            # exclude = max(exclude, na.rm = TRUE),
            runoff_volume_total = sum(runoff_volume, na.rm = FALSE),
            runoff_inches = sum(runoff_inches, na.rm = FALSE),
            peak_discharge = max(peak_discharge, na.rm = TRUE),
            across(contains("load"), ~sum(., na.rm = FALSE)),
            across(contains("conc_mgL"), ~sum(., na.rm = FALSE)),
            across(contains("remark"), ~paste(unique(.)[-which(is.na(unique(.)) |
                                                               unique(.) == "")], 
                                            collapse = " | ")),
            across(contains("yield"), ~sum(., na.rm = FALSE)),
            .groups = "drop") %>%
  mutate(across(contains("conc_mgL"), ~./runoff_volume_total)) %>%
  rename(runoff_volume = runoff_volume_total)

row = 1
for (row in 1:length(concvars)){
  final_df[concvars[row]] <- 
    round(final_df[concvars[row]]+0.0001, rounding_table$sigdigits[row])
  
  final_df[loadvars[row]] <- 
    round(final_df[loadvars[row]]+0.000000001, rounding_table$sigdigits[row] + 5)
  
  final_df[yieldvars[row]] <- 
    round(final_df[yieldvars[row]]+0.000000001, rounding_table$sigdigits[row] + 6)
  
  combined_df[concvars[row]] <- 
    round(combined_df[concvars[row]]+0.0001, rounding_table$sigdigits[row])
  
  combined_df[loadvars[row]] <- 
    round(combined_df[loadvars[row]]+0.000000001, rounding_table$sigdigits[row] + 5)
  
  combined_df[yieldvars[row]] <- 
    round(combined_df[yieldvars[row]]+0.000000001, rounding_table$sigdigits[row] + 6)
}

# head(data.frame(combined_df))
# 
# head(combined_df$storm_start)
# head(combined_df$storm_end)

sapply(final_df[concvars], min, na.rm = T)
sapply(final_df[loadvars], min, na.rm = T)
sapply(final_df[yieldvars], min, na.rm = T)

sapply(combined_df[concvars], min, na.rm = T)
sapply(combined_df[loadvars], min, na.rm = T)
sapply(combined_df[yieldvars], min, na.rm = T)


 if (sum(combined_df$n_sub_flow_events-1) == nrow(final_df) - nrow(combined_df)){
   print("Number of sub-storms match! Great job!")
 }

siteyears_df <- combined_df %>%
  mutate(wateryear_start = getWY(storm_start),
         wateryear_end = getWY(storm_end),
         wateryear_diff = wateryear_end - wateryear_start) %>%
  filter(wateryear_diff == 0) %>%
  group_by(site, Field_Name, project) %>%
  summarize(n_years = length(unique(wateryear_start)), 
            n_years2 = length(unique(wateryear_end)),
            number_of_storms = length(unique(unique_storm_number)))

# colSums(siteyears_df[,4:5])

site_summary_report <- final_df %>%
  filter(is.finite(storm_start), is.finite(storm_end)) %>%
  group_by(site, Field_Name, project, estimated) %>%
  dplyr::summarise(first_storm = min(as.Date(storm_start, tz = "UTC"), na.rm = TRUE),
                   last_storm = max(as.Date(storm_start, tz = "UTC"), na.rm = TRUE),
                   number_of_substorms = n(),
                   # number_of_substorms = sum(n_sub_flow_events),
                   runoff_volume = round(sum(runoff_volume, na.rm=T),0),
                   .groups = "drop") %>%
  spread(key=estimated, value=runoff_volume) %>%
  rename(Volume_Estimated = '1',
         Volume_Measured = '0') 

site_summary_report$Number_Estimated <- ifelse(is.na(site_summary_report$Volume_Estimated), NA, 
                                               site_summary_report$number_of_substorms)

site_summary_report$Number_Measured <- ifelse(is.na(site_summary_report$Volume_Measured), NA, 
                                              site_summary_report$number_of_substorms)

site_summary_report2 <- site_summary_report %>%
  group_by(site, Field_Name, project) %>%
  summarize_at(vars(number_of_substorms:Number_Measured), sum, na.rm=T) %>%
  mutate(fraction_volume = round(Volume_Measured / (Volume_Measured + Volume_Estimated),2),
         fraction_number = round(Number_Measured / (Number_Measured + Number_Estimated),2)) %>%
  left_join(select(site_summary_report, site, first_storm, last_storm) %>%
              group_by(site) %>%
              summarize(first_storm = min(first_storm, na.rm=T),
                        last_storm = max(last_storm, na.rm=T))) %>%
  left_join(select(siteyears_df, site, Field_Name, n_years, number_of_storms)) %>%
  select(site, Field_Name, project, n_years, first_storm, last_storm, number_of_storms, number_of_substorms, Number_Measured, Number_Estimated, fraction_number, Volume_Measured, Volume_Estimated, fraction_volume, everything()) %>%
  arrange(project, Field_Name)

print(data.frame(site_summary_report2[,1:6]))
print(data.frame(site_summary_report2))



if (nrow(site_summary_report2) != nrow(site_table)){
  warning(paste0(toString(nrow(site_summary_report2)), 
                 " sites in dataset. Should be ", 
                 toString(nrow(site_table))))
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

write.csv(final_df, file.path(path_to_data, "Data Publication", 
                            "CleanedFinalVersions", 
                            "All_EOF_StormEventLoadsFormatted.csv"),
          row.names=F)


saveRDS(combined_df, file.path(path_to_data, "Data Publication", 
                            "CleanedFinalVersions", 
                            "All_EOF_StormEventLoadsCombined.rds"))


