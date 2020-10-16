
library(ggplot2)
library(dplyr)
library(stringr)
library(Rainmaker)
library(readxl)
options(scipen = 999)


# source functions that are needed
source('scripts/functions/fxns_data_processing.R')


#***# RAINMAKER OPTIONS #***#***#***#***#    
# Time between events in hours (interevent)
ieHr <- 2
# Amount it must rain to count as an event in tenths of inches (rain threshold) -- note RMevents_sample does not use rainthresh
rainthresh <- 0.008



#unit rain data for all sites
Rain.uv.combined.other <- readRDS(file.path(path_to_data, 'compiled_data', 'rain', 'Compiled_Rain_UV_DataPublication.rds'))

Rain.uv.combined.GLRI <- readRDS(file.path(path_to_data, 'compiled_data', 'rain', 'Compiled_Rain_UV_Data.rds'))

Rain.uv.combined <- bind_rows(Rain.uv.combined.other, Rain.uv.combined.GLRI)

combined_df <- readRDS(file.path(path_to_data, "Data Publication", 
                            "CleanedFinalVersions", 
                            "All_EOF_StormEventLoadsCombined.rds")) 

combined_df$storm_end[which(combined_df$FieldName == "JF1" & 
                              combined_df$unique_storm_number == "2015-1")] <- 
  as.POSIXct("2015-03-08  03:00:00", tz = "America/Chicago")

combined_df <- arrange(combined_df, project, FieldName, site, storm_start)

site_i <- 1
wq.precip.list <- list()
for (site_i in 1:nrow(site_table)){
  USGSSiteNumber_i <-  site_table$USGSSiteNumber[site_i]
  RainNumber_i <-  site_table$USGSSiteNumberforPrecipitation[site_i]
  
  wq.dat <- as.data.frame(filter(combined_df, site == USGSSiteNumber_i))

  precip_raw <- filter(Rain.uv.combined, rain_site == RainNumber_i)
  attributes(precip_raw$pdate)$tzone <- attributes(wq.dat$storm_end)$tzone 
  
  if (nrow(wq.dat)==0 | nrow(precip_raw) == 0){
    wq.precip.list[[site_i]] <- wq.dat
    warning(paste0("Water or rain data are all NAs. skipping ", toString(USGSSiteNumber_i)))
    next
  }

  if(length(which(!is.finite(c(wq.dat$storm_start, wq.dat$storm_end))))>0){
    warning(paste0("Some observations have NA storm_start or storm_end times. Check ", toString(USGSSiteNumber_i)))
    wq.dat <- filter(wq.dat, is.finite(storm_start) & is.finite(storm_end))
  }
  
  precip.dat <- run.rainmaker(precip_raw = precip_raw, ieHr = ieHr, 
                              rainthresh = rainthresh, wq.dat = wq.dat,
                              xmin = c(5,10,15,30,60), antecedentDays = c(1,2,7,14))
  
  precip.dat <- rename(precip.dat, 'rain_startdate' = 'StartDate', 'rain_enddate' = 'EndDate') %>%
    select(-unique_storm_number)
  
  precip.dat[is.na(precip.dat$rain), which(names(precip.dat) == "duration"):
               which(names(precip.dat) == "ARFdays14")] <- NA
  
  precip.dat[which(precip.dat$rain == 0), which(names(precip.dat) == "duration"):
               which(names(precip.dat) == "erosivity_m2")] <- NA
    
  
  wq.precip.list[[site_i]] <- wq.dat %>%
    mutate(USGSSiteNumberforPrecipitation = RainNumber_i) %>%
    # select(-unique_storm_number) %>%
    bind_cols(precip.dat)
  
  print(site_table[site_i,1:3])  
}


wq.precip.df <- wq.precip.list %>% 
  bind_rows(.id = NULL) %>%
  distinct()

saveRDS(wq.precip.df, file.path(path_to_data, "Data Publication", 
                               "CleanedFinalVersions", 
                               "All_EOF_StormEventLoadsRainCalculated.rds"))


# View(filter(wq.precip.df, FieldName == 'WI-SW1') %>% select(unique_storm_number, storm_start, storm_end, rain_startdate, rain))


rain_summary <- wq.precip.df %>%
  # filter(is.na(rain) | rain == 0) %>%
  group_by(project, FieldName, site) %>%
  summarize(n_rain_na = length(which(is.na(rain))),
            # n_rain_0  = length(which(rain == 0)),
            n_rain_0_unfrozen = length(which(rain == 0 & frozen == 0)),
            n_rain_0_frozen = length(which(rain == 0 & frozen == 1)),
            n_rain_lessthan0.2 = length(which(rain > 0 & rain<0.2)),
            n_rain_greaterthan0.2 = length(which(rain >= 0.2)),
            n_total = n())

data.frame(rain_summary)
colSums(rain_summary[,4:9])


runoff_summary <- combined_df %>%
  mutate(wateryear = getWY(combined_df$storm_start)) %>%
  group_by(site, FieldName, project, wateryear) %>%
  summarize(n_total = n())


# data.frame(runoff_summary)

wq.precip.norain <- wq.precip.df %>%
  filter(is.na(rain) | rain < 0.2) %>%
  # rename(USGSSiteNumber = site) %>%
  left_join(select(site_table, USGSSiteNumber, Area), by = c("site" = "USGSSiteNumber")) %>%
  mutate(USGSSiteNumberforPrecipitation = paste0("'", USGSSiteNumberforPrecipitation),
         site = paste0("'", site)) %>%
  mutate(runoff_index = runoff_volume / (rain * Area * 43560 / 12)) %>%
  arrange(project, FieldName, site, storm_start)

write.csv(wq.precip.norain, file.path(path_to_data, "Data Publication", "All.EOF.missingrain.csv"), row.names = FALSE)

table(wq.precip.norain$FieldName)
