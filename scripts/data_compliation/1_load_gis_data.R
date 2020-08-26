

#Script to combine GIS summary data by site

library(readxl)
library(openxlsx)

#Master site data table
master_beforeafter_df <- readRDS(file.path(path_to_data, 'compiled_data', 'rain', 'Compiled_Masters.rds'))

#load farm data
farm_area <- read.csv(file_in(file.path(path_to_data, "SiteCharacteristics", "EOF_WatershedAreas.csv")), header=T, stringsAsFactors = F) %>%
  select(Site, Area_acres) %>%
  rename(site = Site)

#path_to_data

gis_file <- file.path(path_to_data, 'SiteCharacteristics', 'GLRI_Basins_Stats.xlsx')

Elevations <- read_excel(gis_file, sheet = 'Elevation Stats') %>%
  mutate(site = gsub('_', "-", SiteID)) %>%
  select(-SiteID) 
Slopes <- read_excel(gis_file, sheet = 'Slope stats') %>%
  mutate(site = gsub('_', "-", SiteID)) %>%
  select(-SiteID) %>%
  mutate_if(is.numeric, round, digits=2)
Roughness <- read_excel(gis_file, sheet = 'Roughness Stats') %>%
  mutate(site = gsub('_', "-", SiteID)) %>%
  select(-SiteID)

names(Elevations)[1:5] <- paste("elevation", names(Elevations)[1:5], sep="_" )
names(Slopes)[1:5]     <- paste("slope",     names(Slopes)[1:5],     sep="_" )
names(Roughness)[1:5]  <- paste("roughness", names(Roughness)[1:5],  sep="_" )

gis_combined <- full_join(Elevations, Slopes, by = 'site') %>%
  full_join(Roughness, by = 'site') %>% 
  mutate_if(is.numeric, round, digits=2) %>%
  filter(!site %in% c("MI-SW13A", "MI-TL13A")) %>%
  mutate(site = gsub("15A", "2", site)) %>%
  full_join(farm_area, by = "site") %>%
  select(site, Area_acres, everything()) %>%
  arrange(site)



write.csv(gis_combined, file.path(path_to_data, 'SiteCharacteristics', 'GIS_Compiled.csv'), row.names=FALSE)

saveRDS(gis_combined, file.path(path_to_data, 'compiled_data', 'GIS_Compiled.rds'))

