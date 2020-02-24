
#load storm data
data_df3 <- readRDS(file=(file_in(file.path(path_to_data, "compiled_data", "storm_event_loads", "storm_event_conc_allsites_model.rds" ))))

#Load soil data
soil_0_15 <- readRDS(file=file_out(file.path(path_to_data, 'soil', 'cleaned_data', 'Soil2016_0_to_15cm.rds')))




#collapse data to only water year with soil data
#Compute annual metrics



data_merge <- data_df3 %>%
  filter(wateryear == unique(soil_0_15$wateryear)) %>%
  mutate(wateryear = as.numeric(as.character(wateryear))) %>%
  rename(Site = site) %>%
  # mutate(LoadPerAreaPerRain = )
  group_by(wateryear, Site) %>%
  summarize_at(concvars, .funs=mean, na.rm=T) %>%
  full_join(soil_0_15)
  

ggplot(data=data_merge, aes(x=Bray_P, y=orthophosphate_conc_mgL)) +
  geom_point()

ggplot(data=data_merge, aes(x=WE_TN, y=total_nitrogen_conc_mgL)) +
  geom_point()
