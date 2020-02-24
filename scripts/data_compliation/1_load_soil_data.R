
#load soil quality/health data.
#Input is one or more csv files from UW-Green Bay and Purdue


soil_df <- read.csv(file_in(file.path(path_to_data, 'soil', 'raw_data', "EoF_SoilData_2016-2017.csv")), header=T, stringsAsFactors = F)

#When more than one file is included, add code here

#First file is missing dates
#These are for the Spring 2016 data only
soil_dates<-data.frame(Site = c("WI-01", "WI-02", "WI-03", "WI-04", "WI-05", "OH-01", "MI-01", "MI-02", "IN-01", "IN-02", "NY-01", "NY-02", "NY-03", "NY-04" ), 
                       Date = as.Date(c("2016-06-21", "2016-06-21", "2016-06-21", "2016-06-22", "2016-06-22", "2016-06-14", "2016-06-14", "2016-06-14", "2016-06-13", "2016-06-13", "2016-06-16", "2016-06-16", "2016-06-16", "2016-06-16" ))) 
soil_dates$Type <- rep('Spring', nrow(soil_dates))


#Add dates to spring
# soil_dates$site_wq <-  c("WI-SW1", "WI-SW2", "WI-SW3", "WI-SW4", "WI-SW5", "OH-SW1", "MI-SW1", "MI-SW2", "IN-SW1", "IN-SW2", "NY-SW1", "NY-SW2", "NY-SW3", "NY-SW4" )

soil_df <- full_join(soil_dates, soil_df)

soil_df$Site <- gsub("-0", "-SW", soil_df$Site)


#subset fall data
soil_fall <- filter(soil_df, Type=='Fall') %>%
  select(-Type, -Depth, -Project_Year, -SampleID, -Lat, -Long)

soil_fall <- soil_fall[,colSums(is.na(soil_fall))<nrow(soil_fall)]

soil_fall_summary <- soil_fall %>%
  dplyr::group_by(Site, Manure) %>%
  summarize_at(vars(Bulk_Den:Penotrometer_6_18), mean, na.rm=T) %>%
  select_if(~sum(!is.na(.)) > 0) %>%
  mutate(Depth = "0-15") %>%
  rename(OM_fall = OM,
         Bulk_Den_fall = Bulk_Den)


#Calculate percent silt and sand for 0-15 combined depth
soil_spring <- filter(soil_df, Type=='Spring') %>%
  select(Site, SamplePt, Depth, P_Clay, P_Silt, P_Sand, Bulk_Den) %>%
  gather(variable, value, -(Site:Depth)) %>%
  unite(temp, variable, Depth) %>%
  spread(temp, value) %>%
  mutate(`P_Sand_0-15` = CombineDepthPercent(P_Sand_05, Bulk_Den_05, 5, P_Sand_15, Bulk_Den_15, 10),
         `P_Silt_0-15` = CombineDepthPercent(P_Silt_05, Bulk_Den_05, 5, P_Silt_15, Bulk_Den_15, 10),
         `Bulk_Den_0-15` = Bulk_Den_05/3 + Bulk_Den_15*2/3) %>%
  tidyr::gather(variable, value, 3:18) %>%
  separate(variable, into=c("variable1", "variable2", "Depth"), sep='_') %>%
  unite(variable, variable1, variable2) %>%
  spread(variable, value) %>%
  mutate(Type = 'Spring') 

soil_df2 <- left_join(soil_df[,-which(names(soil_df) %in% c("P_Clay", "P_Silt", "P_Sand", "Bulk_Den", "Project_Year", "SampleID"))], soil_spring)

#summarize Spring data by site/date
#Calculate means of reps
soil_df3 <- soil_df2 %>%
  filter(Type == 'Spring') %>%
  mutate(Depth = factor(Depth, c( '05', '0-15', '15', '30'))) %>%
  dplyr::group_by(Site, Date, Depth, Manure) %>%
  summarize_at(vars(OM:P_Silt), mean, na.rm=T) %>%
  select_if(~sum(!is.na(.)) > 0)
  


intersect(names(soil_df3), names(soil_fall_summary))

soil_joined <- full_join(soil_df3, soil_fall_summary) %>%
  mutate(wateryear = getWY(Date)) %>%
  rename(OM_spring = OM, Bulk_Den_spring = Bulk_Den)

soil_0_15 <- filter(soil_joined, Depth == "0-15") %>%
  select_if(not_all_na)

#save 0 to 15 cm depth for converging with water quality data
write.csv(soil_df3, file=file_out(file.path(path_to_data, 'soil', 'cleaned_data', 'Soil2016_Spring.csv')), row.names = F)

saveRDS(soil_df, file=file_out(file.path(path_to_data, 'soil', 'cleaned_data', 'Soil2016_Spring.rds')))


#save 0 to 15 cm depth for converging with water quality data
write.csv(soil_0_15, file=file_out(file.path(path_to_data, 'soil', 'cleaned_data', 'Soil2016_0_to_15cm.csv')), row.names = F)

saveRDS(soil_0_15, file=file_out(file.path(path_to_data, 'soil', 'cleaned_data', 'Soil2016_0_to_15cm.rds')))


