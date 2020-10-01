# General workflow for Rainmaker
# Last update: 5/15/2020 RBC
# Last update June/15/2020 LCL

# FYI - The Rainmaker workflow follows this path:
# - 1 Load all csv files not in NWIS
# - 2 Download NWIS data for all rain gages available 
# - 3 Merge raw and NWIS data, preferring NWIS
# - 4 Compute intensities using `RMintensity`
# - 5 Compute erosivity index using `RMerosivity`
# - 6 Compute antecedent rainfall using `RMarf`
# - 7 Output the results to a file and plot figures


#***# SETTINGS/PREFERENCES  #***#***#***#
library(devtools)
library(dataRetrieval)
dataRetrieval::setAccess('internal')
library(ggplot2)
library(dplyr)
library(stringr)
library(readxl)
# devtools::install_github("USGS-R/Rainmaker")

# Turn on Rainmaker package
library(Rainmaker)
options(scipen = 999)

#***# RAINMAKER OPTIONS #***#***#***#***#    
# Time between events in hours (interevent)
ieHr <- 2
# Amount it must rain to count as an event in tenths of inches (rain threshold) -- note RMevents_sample does not use rainthresh
rainthresh <- 0.008
# Antecedent Rainfall in days (ARF.days)
antecedentDays = c(0.5, 1, 7, 14)

#For edge of field data, these can be commented out
path_to_data <- "P:/0301"
path_to_results <- "C:/Users/lloken/OneDrive - DOI/EOF_SoilHealth"
path_to_site <- "C:/Users/lloken/DOI/GS-UMid GLRI EOF - Data Publication"

#Site data with gage IDs
site_table <- read_excel(file.path(path_to_site, "EOF_Site_Table.xlsx")) %>%
  filter(!is.na(`USGS Site Number`))

names(site_table) <- gsub(" ", "", names(site_table))

site_table <- site_table %>%
  mutate(across(c(ApproxStartDate, ApproxEndDate), as.Date)) %>%
  filter(Project != "GLRI")
  

head(data.frame(site_table))


rain_table <- site_table %>%
  select(USGSSiteNumberforPrecipitation, FieldName, Project) %>%
  group_by(USGSSiteNumberforPrecipitation) %>%
  summarize(AllFieldNames = paste(unique(FieldName), collapse = "|"),
            AllProjects = paste(unique(Project), collapse = "|")) %>%
  arrange(AllProjects, AllFieldNames) 

rain_table <- rain_table %>%
  mutate(AllFieldNames = factor(AllFieldNames, AllFieldNames))
  
site_names <- site_table$USGSSiteNumber
field_names  <- site_table$FieldName 
rain_sites <- site_table$USGSSiteNumberforPrecipitation


#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#
# 1: Raw precip files from P:Drive. #***#***#***#***#    
# # as of June, 2020, four files in path. All in Eastern Standard time
# raw_files <- list.files(file.path(path_to_data, 'compiled_data', 'rain', 'raw_data'), full.names=TRUE)
# 
# file=1
# data_list <- list()
# for (file in 1:length(raw_files)){
#   data_list[[file]] <- read.csv(raw_files[file], header=T, stringsAsFactors = F)
#   data_list[[file]] <- data_list[[file]] %>%
#     mutate(pdate = as.POSIXct(pdate, tz='Etc/GMT+5', 
#                               tryFormats = c("%Y-%m-%d %H:%M:%OS",  "%m/%d/%Y %H:%M",
#                                              "%Y-%m-%d %H:%M")))
# }
# 
# names(data_list) <- list.files(file.path(path_to_data, 'compiled_data', 'rain', 'raw_data'))
# 
# #Rename with rain gage IDS
# names(data_list)[grepl('424421077495301', names(data_list))] <- '424421077495301'
# names(data_list)[grepl('424133077495701', names(data_list))] <- '424133077495701'
# names(data_list)[grepl('411228084541701', names(data_list))] <- '411228084541701'
# names(data_list)[grepl('Precip_MI-FM2', names(data_list))] <- '425520083495901'
# 
# #Combine data.frames and prepare for merge
# raw_rain_data <- bind_rows(data_list, .id = "rain_site")%>%
#   distinct() %>%
#   select(pdate, rain, rain_site) 
# 
# attributes(raw_rain_data$pdate)$tzone <- 'UTC'


#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#
# 2: NWIS data

rain_sites_unique <- unique(rain_sites)
# rain_sites_unique <- c("423740088592701")

start_date <- as.Date(min(site_table$ApproxStartDate, na.rm=T)) - 30
end_date <- Sys.Date()
parameterCd <- "00045"  # Precipitation

# get NWIS data (This takes a long time, >1 hour)
message('Pulling precip data from NWIS. Process can take multiple hours.')
start_time <- Sys.time()
Rain.uv <- readNWISuv(siteNumbers = rain_sites_unique, parameterCd = parameterCd, 
                      startDate = start_date, endDate = end_date, tz='UTC')
end_time <- Sys.time()

if (nrow(Rain.uv) > 0){
  message(paste0(nrow(Rain.uv)), ' rows of data pulled from NWIS in ', round(difftime(end_time , start_time, units = 'secs'), 0), ' seconds.')
} else {
  stop('No precip data pulled from NWIS. Please check inputs to verify correct site number and start and end dates')
}

# rename columns
Rain.uv <- renameNWISColumns(Rain.uv)
names(Rain.uv)[grep('precip_inst$', names(Rain.uv), ignore.case = TRUE)] <- 'rain'
names(Rain.uv)[grep('dateTime', names(Rain.uv), ignore.case = TRUE)] <- 'pdate'
names(Rain.uv)[grep('site_no', names(Rain.uv), ignore.case = TRUE)] <- 'rain_site'


#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#
# 3. Combine raw and NWIS data

#Identify overlapping times
# df.nwis.times <- Rain.uv %>% 
#   group_by(rain_site) %>%
#   summarize_at(vars(pdate), list(max=max, min=min), na.rm=T) %>%
#   filter(rain_site %in% unique(raw_rain_data$rain_site))

# df.raw.prep.1 <- filter(raw_rain_data, rain_site==df.nwis.times$rain_site[1]) %>%
#   filter(pdate>df.nwis.times$max[1] | pdate<df.nwis.times$min[1])
# df.raw.prep.2 <- filter(raw_rain_data, rain_site==df.nwis.times$rain_site[2]) %>%
#   filter(pdate>df.nwis.times$max[2] | pdate<df.nwis.times$min[2])
# df.raw.prep.3 <- filter(raw_rain_data, rain_site==df.nwis.times$rain_site[3]) %>%
#   filter(pdate>df.nwis.times$max[3] | pdate<df.nwis.times$min[3])
# df.raw.prep.4 <- filter(raw_rain_data, rain_site==df.nwis.times$rain_site[4]) %>%
#   filter(pdate>df.nwis.times$max[4] | pdate<df.nwis.times$min[4])
# 
# df.raw.prep.all <- bind_rows(df.raw.prep.1, df.raw.prep.2, 
#                              df.raw.prep.3, df.raw.prep.4)

#Combine with NWIS data
# Rain.uv.combined <- Rain.uv %>% 
#   select(rain_site, pdate, rain) %>%
#   bind_rows(df.raw.prep.all) %>%
#   distinct() %>%
#   arrange(rain_site, pdate) %>%
#   filter(!is.na(rain))

Rain.uv.combined <- Rain.uv %>% 
  select(rain_site, pdate, rain) %>%
  # bind_rows(df.raw.prep.all) %>%
  distinct() %>%
  arrange(rain_site, pdate) %>%
  filter(!is.na(rain))

# saveRDS(Rain.uv.combined, file.path(path_to_data, 'compiled_data', 'rain', 'Compiled_Rain_UV_Data.rds'))



#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#
# 4-6. Compute intensities, erosivity index, and antecedent rainfall
# This takes a long time

StormSummary_list <- list()
site_numbers <- unique(Rain.uv.combined$rain_site)
site_nu <- 1
for (site_nu in 1:length(site_numbers)){
  
  rain.uv.i <- Rain.events.i <- tipsbystorm.i <- StormSummary.i <- ".dplyr"
  
  rain.uv.i <- dplyr::filter(Rain.uv.combined, rain_site == site_numbers[site_nu]) %>%
    arrange(pdate)
  
  Rain.events.i <- RMevents(df=rain.uv.i, ieHr=ieHr, rainthresh=rainthresh, 
                            rain="rain", time="pdate")
  # extract data from events output
  Rain.event.list.i <- Rain.events.i$storms2
  tipsbystorm.i <- Rain.events.i$tipsbystorm
  
  # - 4 Compute intensities
  StormSummary.i <- RMintensity(df=tipsbystorm.i, date="pdate", rain = "rain",
                                df.events=Rain.event.list.i, depth="rain",
                                xmin = c(5,10,15,30,60))

  # - 5 Compute erosivity index
  # method 1
  StormSummary.i <- RMerosivity(df=tipsbystorm.i, ieHr=ieHr, rain="rain",
                              StormSummary=StormSummary.i, method=1)
  StormSummary.i <- rename(StormSummary.i, 'erosivity_m1' = "erosivity", 'energy_m1' = 'energy')
  # method 2
  StormSummary.i <- RMerosivity(df= tipsbystorm.i, ieHr=ieHr, rain="rain",
                              StormSummary=StormSummary.i, method=2)
  StormSummary.i <- rename(StormSummary.i, 'erosivity_m2' = "erosivity", 'energy_m2' = 'energy')

  # - 6 Compute antecedent rainfall using `RMarf`
  StormSummary.i <- RMarf(df = rain.uv.i, date = 'pdate', df.events = StormSummary.i,
                          days = antecedentDays, varnameout = "ARFdays")

  #Save output to list
  StormSummary_list[[site_nu]] <- StormSummary.i
  names(StormSummary_list)[site_nu] <- site_numbers[site_nu]

  print(site_nu)
}

#Bind list together
StormSummary_df <- bind_rows(StormSummary_list, .id = "rain_site")%>%
  distinct() %>%
  left_join(rain_table, by = c("rain_site" = "USGSSiteNumberforPrecipitation"))


# StormSummary_df$state <- str_split(StormSummary_df$site, "-", simplify = TRUE)[,1]


#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#***#
# 7. Save output and figures 

#Save object
# saveRDS(StormSummary_df, file.path(path_to_data, 'compiled_data', 'rain', 'Compiled_Rain_Data.rds'))



#Timeseries figures
rain_allsites.TS <- ggplot(StormSummary_df, aes(x=StartDate, xend=StartDate, 
                                                yend=0, y=rain,
                                                color = AllProjects)) +
  geom_segment(size=0.2, col='grey') +
  geom_point(size=1.25, alpha=0.5) +
  facet_wrap(~AllFieldNames, ncol=4) +
  theme_bw() +
  theme(legend.position='bottom', legend.title = element_blank()) +
  ggtitle('Storm Size: NWIS and raw files') +
  scale_y_sqrt(limits=c(0, NA), expand=c(0, 0)) +
  scale_x_datetime(date_breaks = 'years', date_labels = "%Y", minor_breaks = NULL) + 
  theme(strip.background = element_rect(fill=NA, color=NA)) +
  labs(x = "Calendar year", y = "Rain (in)") +
  geom_text(aes(x=min(StartDate, na.rm=T), y=max(rain, na.rm=T), label=rain_site),
            hjust = 0, vjust = 1, show.legend=FALSE)

print(rain_allsites.TS)
  
ggsave(file.path(path_to_results, 'Figures', 'DataPublication', 
                 'Rain_Timeseries_stormsizes_NWIS.png'),
       rain_allsites.TS, height=12, width = 14, units='in')

#Histogram
rain_allsites.hist <- ggplot(StormSummary_df, aes(x=rain, fill = AllProjects)) +
  geom_histogram(bins=20) +
  facet_wrap(~AllFieldNames, ncol=4, scales='free_y') +
  theme_bw() +
  theme(legend.position='bottom') +
  guides(fill = guide_legend(nrow=1)) +
  ggtitle('Storm sizes (in)') +
  scale_x_sqrt(limits=c(0, NA), expand=c(0, 0), name='rain (in)') +
  theme(strip.background = element_rect(fill=NA, color=NA),
        legend.title = element_blank())

print(rain_allsites.hist)

ggsave(file.path(path_to_results, 'Figures', 'DataPublication',
                 'Rain_Histograms_stormsizes_NWIS.png'), 
       rain_allsites.hist, height=12, width = 9, units='in')

#End

