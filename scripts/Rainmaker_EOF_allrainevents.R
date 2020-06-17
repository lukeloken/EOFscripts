# General workflow for Rainmaker
# Last update: 5/15/2020 RBC
# Last update June/15/2020 LCL

# FYI - The Rainmaker workflow follows this path:
# - 1 Aquire and prepare precipitation data for use in `Rainmaker` from either dataRetrieval/NWIS or Aquarius exported rainfall data
# - 2 Optional - Aquire storm start and end times for `RMevents_sample`
# - 3 Determine precipitation event start and end times using `RMevents` or `RMevents_sample`
# - 4 Compute intensities using `RMintensity`
# - 5 Compute erosivity index using `RMerosivity`
# - 6 Compute antecedent rainfall using `RMarf`
# - 7 Output the results to a file

# This script is written for the user to change a few settings at the top, 
# then run the remainder of the script without needing to make any further changes.

#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***
#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***
#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***

#***# SETTINGS/PREFERENCES  #***#***#***#

#***# RAINFALL DATA #***#***#***#***#
# Choose if you will use dataRetieval to get data from NWIS, or load a .csv exported from Aquarius (AQ)
# Opt 1) DataRetrieval


# Check for Rainmaker updates
library(devtools)
library(dataRetrieval)
dataRetrieval::setAccess('internal')
library(ggplot2)
library(dplyr)
library(anytime)
# devtools::install_github("USGS-R/Rainmaker")

# Turn on Rainmaker package
library(Rainmaker)
options(scipen = 999)

path_to_data <- "P:/0301"
path_to_results <- "C:/Users/lloken/OneDrive - DOI/EOF_SoilHealth"

#***# RAINMAKER OPTIONS #***#***#***#***#    
# Time between events in hours (interevent)
ieHr <- 2
# Amount it must rain to count as an event in tenths of inches (rain threshold) -- note RMevents_sample does not use rainthresh
rainthresh <- 0.008
# Antecedent Rainfall in days (ARF.days)
antecedentDays = c(0.5, 1, 7, 14)


#***# Raw precip files from P:Drive. #***#***#***#***#    

raw_files <- list.files(file.path(path_to_data, 'compiled_data', 'rain', 'raw_data'), full.names=TRUE)

file=1
data_list <- list()
for (file in 1:length(raw_files)){
  data_list[[file]] <- read.csv(raw_files[file], header=T, stringsAsFactors = F)
  data_list[[file]] <- data_list[[file]] %>%
    mutate(pdate = as.POSIXct(pdate, tz='Etc/GMT+5', 
                              tryFormats = c("%Y-%m-%d %H:%M:%OS",  "%m/%d/%Y %H:%M",
                                             "%Y-%m-%d %H:%M")))
}

names(data_list) <- list.files(file.path(path_to_data, 'compiled_data', 'rain', 'raw_data'))

names(data_list)[grepl('424421077495301', names(data_list))] <- '424421077495301'
names(data_list)[grepl('424133077495701', names(data_list))] <- '424133077495701'
names(data_list)[grepl('411228084541701', names(data_list))] <- '411228084541701'
names(data_list)[grepl('Precip_MI-FM2', names(data_list))] <- '425520083495901'


str(data_list)

raw_rain_data <- bind_rows(data_list, .id = "rain_site")%>%
  distinct() %>%
  select(pdate, rain, Approval.Level, Grade, Qualifiers, rain_site) 

str(raw_rain_data)

StormSummary_list.raw <- Storm_hist_list.raw <- list()
i=1
for (i in 1:length(data_list)){
  
  rain_site <- names(data_list)[i] # <-- insert station ID
  
  Rain.uv.raw <- data_list[[i]] 
  
  # - 4 Compute intensities
  Rain.events.raw <- RMevents(df=Rain.uv.raw, ieHr=ieHr, rainthresh=rainthresh, rain="rain", time="pdate")
  # extract data from events output
  Rain.event.list.raw <- Rain.events.raw$storms2
  tipsbystorm.raw <- Rain.events.raw$tipsbystorm
  
  StormSummary.raw <- RMintensity(df=tipsbystorm.raw, date="pdate", df.events=Rain.event.list.raw, depth="rain", xmin=c(5,10,15,30,60))
  
  # - 5 Compute erosivity index
  # method 1
  StormSummary.raw <- RMerosivity(df=tipsbystorm.raw, ieHr=ieHr, rain="rain", StormSummary=StormSummary.raw, method=1)
  StormSummary.raw <- dplyr::rename(StormSummary.raw, 'erosivity_m1' = "erosivity", 'energy_m1' = 'energy')
  # method 2
  StormSummary.raw <- RMerosivity(df= tipsbystorm.raw, ieHr=ieHr, rain="rain", StormSummary=StormSummary.raw, method=2)
  StormSummary.raw <- dplyr::rename(StormSummary.raw, 'erosivity_m2' = "erosivity", 'energy_m2' = 'energy')
  # - 6 Compute antecedent rainfall using `RMarf`
  StormSummary.raw <- RMarf(df = Rain.uv.raw, date = 'pdate', df.events = StormSummary.raw,
                            days = antecedentDays, varnameout = "ARFdays")
  
  StormSummary_list.raw[[i]] <- StormSummary.raw
  names(StormSummary_list.raw)[i] <- rain_site
  
  # Storm_hist_list.raw[[i]] <- ggplot(StormSummary_list.raw[[i]], aes(x=rain)) +
  #   geom_histogram() +
  #   ggtitle(paste(rain_site))
  
  print(i)
  
}

str(StormSummary_list.raw)


master_beforeafter_df <- readRDS(file.path(path_to_data, 'compiled_data', 'rain', 'Compiled_Masters.rds'))

rain_sites <- master_beforeafter_df$`rain station ID`
start_dates <- master_beforeafter_df$start_date
end_dates <- master_beforeafter_df$end_date
site_names <- master_beforeafter_df$site
site_tzs <- master_beforeafter_df$site_tz

StormSummary_list <-  list()
i <- 9
for (i in 1:length(rain_sites)){
  # for (i in 1:2){
  
  rain_site <- rain_sites[i] # <-- insert station ID
  # Set dates that bound what data to retrieve:
  start_date <- start_dates[i] # YYYY-MM-DD date/time of study start
  end_date <- end_dates[i] # YYYY-MM-DD date/time of study end
  site_tz <- site_tzs[i]
  
  if (rain_site %in% rain_sites[0:(i-1)]) {
    message(paste0('skipping row ', toString(i), '. Already ran'))
    
    StormSummary_list[[i]] <- StormSummary_list[[head(match(rain_site,  rain_sites), 1)]]
    names(StormSummary_list)[i] <- rain_site
    
    # Storm_hist_list[[i]] <- ggplot(StormSummary_list[[i]], aes(x=rain)) +
    #   geom_histogram() +
    #   ggtitle(paste(site_names[i], rain_site))
    print(i)
    next
  } 
  
  
  
  
  #***# SELECT ALL BELOW THIS LINE AND RUN ***#***#***#          
  
  
  # - 1 Aquire data
  # get data from NWIS using dataRetrieval
  
  parameterCd <- "00045"  # Precipitation
  startDate <- as.Date(start_dates[i]) - 15 # put a week buffer on the study start date in case storm started prior to first sample date
  endDate <- as.Date(end_dates[i]) - 15
  
  # get NWIS data
  message('Pulling precip data from NWIS.')
  start_time <- Sys.time()
  Rain.uv <- readNWISuv(rain_site, parameterCd)
  end_time <- Sys.time()
  
  if (nrow(Rain.uv) > 0){
    message(paste0(nrow(Rain.uv)), ' rows of data pulled from NWIS in ', round(difftime(end_time , start_time, units = 'secs'), 0), ' seconds.')
  } else {
    stop('No precip data pulled from NWIS. Please check inputs to verify correct site number and start and end dates')
  }
  
  # rename columns
  Rain.uv <- renameNWISColumns(Rain.uv)
  
  # rename columns
  names(Rain.uv)[grep('precip_inst$', names(Rain.uv), ignore.case = TRUE)] <- 'rain'
  names(Rain.uv)[grep('dateTime', names(Rain.uv), ignore.case = TRUE)] <- 'pdate'
  
  # print warning if dates of rain do not span dates of study
  if (min(as.Date(Rain.uv$pdate)) > startDate | 
      max(as.Date(Rain.uv$pdate)) < endDate) {
    warning(paste0('Data pulled from NWIS does not span the entire study period. Row ',  toString(i)))
  }
  
  # - 4 Compute intensities
  Rain.events <- RMevents(df=Rain.uv, ieHr=ieHr, rainthresh=rainthresh, rain="rain", time="pdate")
  # extract data from events output
  Rain.event.list <- Rain.events$storms2
  tipsbystorm <- Rain.events$tipsbystorm
  
  StormSummary <- RMintensity(df=tipsbystorm, date="pdate", df.events=Rain.event.list, depth="rain", xmin=c(5,10,15,30,60))
  
  # - 5 Compute erosivity index
  # method 1
  StormSummary <- RMerosivity(df=tipsbystorm, ieHr=ieHr, rain="rain", StormSummary=StormSummary, method=1)
  StormSummary <- rename(StormSummary, 'erosivity_m1' = "erosivity", 'energy_m1' = 'energy')
  # method 2
  StormSummary <- RMerosivity(df= tipsbystorm, ieHr=ieHr, rain="rain", StormSummary=StormSummary, method=2)
  StormSummary <- rename(StormSummary, 'erosivity_m2' = "erosivity", 'energy_m2' = 'energy')
  # - 6 Compute antecedent rainfall using `RMarf`
  StormSummary <- RMarf(df = Rain.uv, date = 'pdate', df.events = StormSummary,
                        days = antecedentDays, varnameout = "ARFdays")
  
  StormSummary_list[[i]] <- StormSummary
  names(StormSummary_list)[i] <- rain_site
  
  # 
  # Storm_hist_list[[i]] <- ggplot(StormSummary_list[[i]], aes(x=rain)) +
  #   geom_histogram() +
  #   ggtitle(paste(site_names[i], rain_site))
  # 
  print(i)
}

# StormSummary_list

saveRDS(Storm_hist_list, file.path(path_to_data, 'compiled_data', 'rain', 'Compiled_Rain_Histograms.rds'))
saveRDS(StormSummary_list, file.path(path_to_data, 'compiled_data', 'rain', 'Compiled_Rain_Data.rds'))

# ggplot(StormSummary_list[[2]], aes(x=rain)) +
#   geom_histogram()


# ggplot(StormSummary, aes(x=StartDate,xend=StartDate,y=0,yend=rain)) + 
#   geom_segment()
# 
# 
i=1
for (i in 1:length(StormSummary_list)) {
  print(ggplot(StormSummary_list[[i]], aes(x=StartDate,xend=StartDate,yend=0,y=rain)) +
          geom_segment() +
          geom_point(col='red') +
          ggtitle(paste(site_names[i], names(StormSummary_list)[i])))
}
# 

head(StormSummary_list[[1]])

StormSummary_df.raw <- bind_rows(StormSummary_list.raw, .id = "rain_site")%>%
  distinct() %>%
  mutate(site = site_names[match(rain_site, rain_sites)],
         source = 'raw')


rain_allsites.raw <- ggplot(StormSummary_df.raw, aes(x=StartDate,xend=StartDate,yend=0,y=rain)) +
  geom_segment(size=0.2) +
  geom_point(aes(col=rain_site, shape=source), size=.75) +
  scale_shape_manual(values = c(1,16)) +
  facet_wrap(~site, ncol=2) +
  theme_bw() +
  theme(legend.position='bottom')


StormSummary_df <- bind_rows(StormSummary_list, .id = "rain_site")%>%
  distinct() %>%
  mutate(site = site_names[match(rain_site, rain_sites)],
         source = 'NWIS')


rain_allsites <- ggplot(StormSummary_df, aes(x=StartDate,xend=StartDate,yend=0,y=rain)) +
  geom_segment(size=0.2) +
  geom_point(aes(col=rain_site), size=0.75) +
  scale_shape_manual(values = c(1,16)) +
  facet_wrap(~site, ncol=2) +
  theme_bw() +
  theme(legend.position='bottom') +
  ggtitle('Storm Size: NWIS')

ggsave(file.path(path_to_results, 'Figures', 'Rain', 'Rain_Timeseries_stormsizes.png'), rain_allsites, height=12, width = 8, units='in')

df.nwis.times <- StormSummary_df %>% 
  group_by(rain_site, site) %>%
  summarize_at(vars(StartDate), list(max=max, min=min), na.rm=T) %>%
  filter(rain_site %in% unique(StormSummary_df.raw$rain_site))

df.raw.prep.1 <- filter(StormSummary_df.raw, rain_site==df.nwis.times$rain_site[1]) %>%
  filter(StartDate>df.nwis.times$max[1] | StartDate<df.nwis.times$min[1])
df.raw.prep.2 <- filter(StormSummary_df.raw, rain_site==df.nwis.times$rain_site[2]) %>%
  filter(StartDate>df.nwis.times$max[2] | StartDate<df.nwis.times$min[2])
df.raw.prep.3 <- filter(StormSummary_df.raw, rain_site==df.nwis.times$rain_site[3]) %>%
  filter(StartDate>df.nwis.times$max[3] | StartDate<df.nwis.times$min[3])
df.raw.prep.4 <- filter(StormSummary_df.raw, rain_site==df.nwis.times$rain_site[4]) %>%
  filter(StartDate>df.nwis.times$max[4] | StartDate<df.nwis.times$min[4])

df.raw.prep.all <- bind_rows(df.raw.prep.1, df.raw.prep.2, df.raw.prep.3, df.raw.prep.4)

StormSummary_df.combined <- df.raw.prep.all %>%
  full_join(StormSummary_df) %>%
  distinct()


rain_allsites.combined <- ggplot(StormSummary_df.combined,
                                 aes(x=StartDate,xend=StartDate,yend=0,y=rain)) +
  geom_segment(size=0.2) +
  geom_point(aes(col=rain_site), size=.75) +
  scale_shape_manual(values = c(16,1)) +
  facet_wrap(~site, ncol=2) +
  theme_bw() +
  theme(legend.position='bottom') +
  ggtitle('Storm Sizes: NWIS + site files')

ggsave(file.path(path_to_results, 'Figures', 'Rain', 'Rain_Timeseries_stormsizes_bothsources.png'), rain_allsites.combined, height=12, width = 8, units='in')


rain_allsites.hist <- ggplot(StormSummary_df.combined, aes(x=rain)) +
  geom_histogram(aes(fill=rain_site), stat='density') +
  # geom_point(aes(col=rain_site), size=.75) +
  # scale_shape_manual(values = c(16,1)) +
  facet_wrap(~site, ncol=2) +
  theme_bw() +
  theme(legend.position='bottom') +
  guides(fill = guide_legend(nrow=4)) +
  ggtitle('Storm sizes (in)')

ggsave(file.path(path_to_results, 'Figures', 'Rain', 'Rain_Histograms_stormsizes_bothsources.png'), rain_allsites.hist, height=12, width = 6, units='in')




