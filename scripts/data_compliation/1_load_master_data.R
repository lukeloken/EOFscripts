

#Script to open all master R file and extract important information


library(readxl)
library(openxlsx)

#path_to_data

sites_table <- read_excel(file.path(path_to_data, 'field_analysis', 'meta-analysis', 'EOF sites for analysis.xlsx'))

sites_table_prep <- sites_table %>%
  dplyr::rename(site = `site abbreviation`,
         site_number = `site number`,
         bmp_date2 = `BMP date`) %>%
  mutate(bmp_date2 = convertToDate(bmp_date2))

master_files <- list.files((file.path(path_to_data, 'field_analysis', 'approved_site_data', '0_masters')), 
                           full.names = TRUE, pattern = 'master', ignore.case = TRUE)

master_df <- data.frame(matrix(nrow=length(master_files), ncol=21))
names(master_df) <- c('site', 'study_type', 'site_tz',
                      'start_date', 'end_date', 'bmp_date', 
                      'datetime_format', 'date_format',
                      'rain_site', 'discharge_site_no', 'noaa_site', 
                      'wq_file', 'rain_file', 'discharge_file', 'weather_file',
                      'predictors_drop', 'filter_exclude', 'filter_estimated',
                      'filter_discrete', 'filter_frozen', 'filter_storm')

master_df$bmp_date <- as.Date(NA)
master_df$start_date <- as.Date(NA)
master_df$end_date <- as.Date(NA)


file_nu <- 1
for (file_nu in 1:length(master_files)){
  
  #Override objects so they are not populated into table for next file
  site <- study_type <- site_tz <- start_date <- end_date <- NA
  datetime_format <- date_format <- NA 
  bmp_date <- rain_site <- discharge_site_no <- noaa_site <- NA 
  wq_file <- rain_file <- discharge_file <- weather_file <- NA
  predictors_drop <- filter_exclude <- filter_estimated <- NA
  filter_discrete <- filter_frozen <- filter_storm <- NA
  
  #Load variables from master
  source(master_files[file_nu])
  
  #Extract R objects and save in table
  master_df$site[file_nu] <- site
  master_df$study_type[file_nu] <- study_type
  master_df$site_tz[file_nu] <- site_tz
  master_df$start_date[file_nu] <- as.Date(start_date)
  master_df$end_date[file_nu] <- as.Date(end_date)
  master_df$bmp_date[file_nu] <- as.Date(bmp_date)
  master_df$date_format[file_nu] <- date_format
  master_df$datetime_format[file_nu] <- datetime_format
  master_df$rain_site[file_nu] <- rain_site
  master_df$discharge_site_no[file_nu] <- discharge_site_no
  master_df$noaa_site[file_nu] <- noaa_site
  master_df$wq_file[file_nu] <- wq_file
  master_df$rain_file[file_nu] <- rain_file
  master_df$discharge_file[file_nu] <- discharge_file
  master_df$weather_file[file_nu] <- weather_file
  master_df$predictors_drop[file_nu] <- paste(predictors_drop, sep='', collapse = '|')
  master_df$filter_exclude[file_nu] <- filter_exclude
  master_df$filter_estimated[file_nu] <- filter_estimated
  master_df$filter_discrete[file_nu] <- filter_discrete
  master_df$filter_frozen[file_nu] <- filter_frozen
  master_df$filter_storm[file_nu] <- filter_storm

  #Remove all objects
  rm(list = c('site', 'study_type', 'site_tz',
    'start_date', 'end_date', 'bmp_date', 
    'datetime_format', 'date_format',
    'rain_site', 'discharge_site_no', 'noaa_site', 
    'wq_file', 'rain_file', 'discharge_file', 'weather_file',
    'predictors_drop', 'filter_exclude', 'filter_estimated',
    'filter_discrete', 'filter_frozen', 'filter_storm'))
  
  }

  
master_df$site[grepl('ETL1', master_df$site)] <- 'WI-TL1'
# master_df$site[grepl('SW2', master_df$site) & grepl('Chicago', master_df$site_tz)] <- 'WI-SW2'
# master_df$site[grepl('SW3', master_df$site) & grepl('Chicago', master_df$site_tz)] <- 'WI-SW3'
master_df$site[grepl('NYSW1', master_df$site)] <- 'NY-SW1'
master_df$site[grepl('NYSW4', master_df$site)] <- 'NY-SW4'
master_df$site[grepl('MI_FM1', master_df$site)] <- 'MI-SW1'
master_df$site[grepl('MI_FM2', master_df$site)] <- 'MI-SW2'
master_df$site[grepl('MI_TL1_bf', master_df$site)] <- 'MI-TL1'
# master_df$site[grepl('oh_rain_test', master_df$site)] <- 'OH-SW1'



master_beforeafter_df <- master_df %>%
  filter(study_type == 'before_after') %>%
  left_join(sites_table_prep, by='site')

write.csv(master_beforeafter_df, file.path(path_to_data, 'SiteCharacteristics', 'Compiled_Master_Files.csv'), row.names=FALSE)

saveRDS(master_beforeafter_df, file.path(path_to_data, 'compiled_data', 'rain', 'Compiled_Masters.rds'))

