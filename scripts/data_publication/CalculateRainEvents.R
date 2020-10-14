
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
# Antecedent Rainfall in days (ARF.days)
antecedentDays = c(0.5, 1, 7, 14)


#unit rain data for all non-GLRI sites
Rain.uv.combined <- readRDS(file.path(path_to_data, 'compiled_data', 'rain', 'Compiled_Rain_UV_DataPublication.rds'))


final_df <- readRDS(file.path(path_to_data, "Data Publication", 
                            "CleanedFinalVersions", 
                            "All_EOF_StormEventLoadsFormatted.rds"))

final_df$storm_end[which(final_df$FieldName == "JF1" & 
                           final_df$unique_storm_number == "2015-1")] <- 
  as.POSIXct("2015-03-08  03:00:00", tz = "America/Chicago")

site_i <- 1
wq.precip.list <- list()
for (site_i in 1:nrow(site_table)){
  USGSSiteNumber_i <-  site_table$USGSSiteNumber[site_i]
  RainNumber_i <-  site_table$USGSSiteNumberforPrecipitation[site_i]
  
  wq.dat <- filter(final_df, site == USGSSiteNumber_i)

  precip_raw <- filter(Rain.uv.combined, rain_site == RainNumber_i)
  attributes(precip_raw$pdate)$tzone <- attributes(wq.dat$storm_end)$tzone 
  
  if (nrow(wq.dat)==0 | nrow(precip_raw) == 0){
    wq.precip.list[[site_i]] <- wq.dat
    warning(paste0("Water or rain data are all NAs. skipping ", toString(USGSSiteNumber_i)))
    next
  }

  precip.dat <- run.rainmaker(precip_raw = precip_raw, ieHr = 2, rainthresh = 0.008, wq.dat = wq.dat,
                              xmin = c(5,10,15,30,60), antecedentDays = c(1,2,7,14))
  
  precip.dat <- rename(precip.dat, 'rain_startdate' = 'StartDate', 'rain_enddate' = 'EndDate')
  
  precip.dat[is.na(precip.dat$rain), which(names(precip.dat) == "duration"):
               which(names(precip.dat) == "ARFdays14")] <- NA
  
  precip.dat[which(precip.dat$rain == 0), which(names(precip.dat) == "duration"):
               which(names(precip.dat) == "erosivity_m2")] <- NA
    
  
  wq.precip.list[[site_i]] <- wq.dat %>%
    mutate(USGSSiteNumberforPrecipitation = RainNumber_i) %>%
    select(-unique_storm_number) %>%
    bind_cols(precip.dat)
  
  print(site_table[site_i,1:3])  
}


wq.precip.df <- wq.precip.list %>% 
  bind_rows(.id = NULL) %>%
  distinct()

rain_summary <- wq.precip.df %>%
  # filter(is.na(rain) | rain == 0) %>%
  group_by(site, FieldName, project) %>%
  summarize(n_rain_na = length(which(is.na(rain))),
            n_rain_0  = length(which(rain == 0)),
            n_rain_positive = length(which(rain>0)),
            n_total = n())

data.frame(rain_summary)


runoff_summary <- final_df %>%
  # filter(is.na(rain) | rain == 0) %>%
  group_by(site, FieldName, project, frozen) %>%
  summarize(n_total = n())


data.frame(runoff_summary)
