
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
antecedentDays = c(1, 2, 7, 14)


#Storm summaries for all rain events
StormSummary_df <- readRDS(file.path(path_to_data, 'compiled_data', 'rain', 'DataPublication_Compiled_Rain_Data.rds'))

StormSummary_df_GLRI <- readRDS(file.path(path_to_data, 'compiled_data', 'rain', 'Compiled_Rain_Data.rds')) %>%
  mutate(All_Projects = "GLRI")

rain_site_table <- site_table %>% 
  select(USGS_Station_Number_for_Precipitation, Field_Name) %>%
  group_by(USGS_Station_Number_for_Precipitation) %>%
  summarize(All_Field_Names = paste0(unique(Field_Name), collapse = "|"))

StormSummary_df_combined <- bind_rows(StormSummary_df, StormSummary_df_GLRI) %>%
  rename(USGS_Station_Number_for_Precipitation = rain_site,
         Project = All_Projects) %>%
  select(-All_Field_Names) %>%
  left_join(rain_site_table) %>%
  filter(USGS_Station_Number_for_Precipitation %in% site_table$USGS_Station_Number_for_Precipitation)

attributes(StormSummary_df_combined$StartDate)$tzone <- "UTC"
attributes(StormSummary_df_combined$EndDate)$tzone <- "UTC"


StormSummary_df_out <- StormSummary_df_combined %>%
  mutate(rain_interval = difftime(EndDate, StartDate, units = "mins")) 

StormSummary_df_out[which(StormSummary_df_out$rain_interval <= 1.5),
                     which(names(StormSummary_df_out) %in% c("duration",
                                                              "Ievent",
                                                              "I5",
                                                              "I10",
                                                              "I15",
                                                              "I30",
                                                              "I60",
                                                              "energy_m1",
                                                              "erosivity_m1",
                                                              "energy_m2",
                                                              "erosivity_m2",
                                                              "erosivity_m2"))] <- NA


StormSummary_df_out <- StormSummary_df_out %>%
  select(-source, -state, -site, -stormnum, -rain_interval) %>%
  select(USGS_Station_Number_for_Precipitation, 
         Project,
         All_Field_Names, StartDate, EndDate,
         rain, duration, Ievent,
         everything()) 


attributes(StormSummary_df_out$StartDate)$tzone <- "UTC"
attributes(StormSummary_df_out$EndDate)$tzone <- "UTC"


write.csv(StormSummary_df_out, file = file.path(path_to_data, "Data Publication", "CleanedFinalVersions", "All_EOF_RainEvents.csv"), row.names = FALSE)

saveRDS(StormSummary_df_out, file = file.path(path_to_data, "Data Publication", "CleanedFinalVersions", "All_EOF_RainEvents.rds"))


#unit rain data for all sites
Rain.uv.combined.other <- readRDS(file.path(path_to_data, 'compiled_data', 'rain', 'Compiled_Rain_UV_DataPublication.rds'))

Rain.uv.combined.GLRI <- readRDS(file.path(path_to_data, 'compiled_data', 'rain', 'Compiled_Rain_UV_Data.rds'))

Rain.uv.combined <- bind_rows(Rain.uv.combined.other, Rain.uv.combined.GLRI)

combined_df <- readRDS(file.path(path_to_data, "Data Publication", 
                                 "CleanedFinalVersions", 
                                 "All_EOF_StormEventLoadsCombined.rds")) 

# combined_df$storm_end[which(combined_df$Field_Name == "JF1" & 
#                               combined_df$unique_storm_number == "2015-1")] <- 
#   as.POSIXct("2015-03-08  03:00:00", tz = "America/Chicago")

names(combined_df)[which(names(combined_df) == "project")] <- "Project"

combined_df <- arrange(combined_df, Project, Field_Name, site, storm_start)

site_i <- 1
wq.precip.list <- list()
for (site_i in 1:nrow(site_table)){
  USGSSiteNumber_i <-  site_table$USGS_Station_Number[site_i]
  RainNumber_i <-  site_table$USGS_Station_Number_for_Precipitation[site_i]
  
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
                              xmin = c(5,10,15,30,60), antecedentDays = antecedentDays)
  
  precip.dat <- rename(precip.dat, 'rain_startdate' = 'StartDate', 'rain_enddate' = 'EndDate') %>%
    select(-unique_storm_number)
  
  precip.dat[is.na(precip.dat$rain), which(names(precip.dat) == "duration"):
               which(names(precip.dat) == "ARFdays14")] <- NA
  
  precip.dat[which(precip.dat$rain == 0), which(names(precip.dat) == "duration"):
               which(names(precip.dat) == "erosivity_m2")] <- NA
  
  
  wq.precip.list[[site_i]] <- wq.dat %>%
    mutate(USGS_Station_Number_for_Precipitation = RainNumber_i) %>%
    # select(-unique_storm_number) %>%
    bind_cols(precip.dat)
  
  print(site_table[site_i,1:3])  
}


wq.precip.df <- wq.precip.list %>% 
  bind_rows(.id = NULL) %>%
  distinct() %>%
  mutate(rain_interval = difftime(rain_enddate, rain_startdate, units = "mins")) 

wq.precip.df[which(wq.precip.df$rain_interval <= 1.5),
                    which(names(wq.precip.df) %in% c("duration",
                                                            "Ievent",
                                                            "I5",
                                                            "I10",
                                                            "I15",
                                                            "I30",
                                                            "I60",
                                                            "energy_m1",
                                                            "erosivity_m1",
                                                            "energy_m2",
                                                            "erosivity_m2",
                                                            "erosivity_m2"))] <- NA

attributes(wq.precip.df$rain_startdate)$tzone <- "UTC"
attributes(wq.precip.df$rain_enddate)$tzone <- "UTC"



#Get rain flags
wq.precip.df <- wq.precip.df %>%
  left_join(select(site_table, USGS_Station_Number, Area, Site_Type), by = c("site" = "USGS_Station_Number")) %>%
  mutate(runoff_index = runoff_volume / (rain * Area * 43560 / 12)) %>%
  mutate(remark_1 = ifelse(is.na(rain), "missing",
                         ifelse(rain == 0 , "zero", 
                                ifelse(runoff_index > .8, "runoff_index", 
                                       NA))),
         remark_3 = ifelse(frozen == TRUE, "frozen", NA),
         remark_2 = ifelse(Site_Type == "Tile" , "tile", NA)) %>%
  unite(col = remark_rain, c(remark_1, remark_2, remark_3), sep = " ", na.rm = TRUE, remove = TRUE) %>%
  select(-Area, -Site_Type, -runoff_index, -rain_startdate, -rain_enddate, 
         -stormnum, -USGS_Station_Number_for_Precipitation, -rain_interval) %>%
  select("site", "Field_Name", "Project", "discrete", "estimated",
         "frozen", "storm", "unique_storm_number", "n_sub_flow_events",
         "storm_start", "storm_end", "runoff_volume", "peak_discharge", 
         everything())

summary(wq.precip.df)

saveRDS(wq.precip.df, file.path(path_to_data, "Data Publication", 
                                "CleanedFinalVersions", 
                                "All_EOF_StormEventLoadsRainCalculated.rds"))

write.csv(wq.precip.df, file.path(path_to_data, "Data Publication", 
                                  "CleanedFinalVersions", 
                                  "All_EOF_StormEventLoadsRainCalculated.csv"), row.names = FALSE)


#Load data from here to save time processing

wq.precip.df <- readRDS(file.path(path_to_data, "Data Publication", 
                                  "CleanedFinalVersions",
                                  "All_EOF_StormEventLoadsRainCalculated.rds"))


rain_summary <- wq.precip.df %>%
  # filter(is.na(rain) | rain == 0) %>%
  group_by(Project, Field_Name, site) %>%
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
  group_by(site, Field_Name, Project, wateryear) %>%
  summarize(n_total = n())


# data.frame(runoff_summary)

wq.precip.norain <- wq.precip.df %>%
  filter(is.na(rain) | rain < 0.2) %>%
  # rename(USGSSiteNumber = site) %>%
  left_join(select(site_table, USGS_Station_Number, Area, USGS_Station_Number_for_Precipitation),
            by = c("site" = "USGS_Station_Number")) %>%
  mutate(USGS_Station_Number_for_Precipitation = paste0("'", USGS_Station_Number_for_Precipitation),
         site = paste0("'", site)) %>%
  mutate(runoff_index = runoff_volume / (rain * Area * 43560 / 12)) %>%
  mutate(exclude_rain = ifelse(is.na(rain) | rain == 0 | !is.finite(runoff_index) |runoff_index > 0.5, "1", "0")) %>%
  arrange(Project, Field_Name, site, storm_start)

write.csv(wq.precip.norain, file.path(path_to_data, "Data Publication", "All.EOF.missingrain.csv"), row.names = FALSE)

table(wq.precip.norain$Field_Name)



#Investigate rainless events

wq.precip.df.norain <- wq.precip.df %>%
  mutate(remark_rain = case_when(is.na(rain) ~ "NA",
                               rain < 0.2 & rain > 0 ~ "<0.2", 
                               rain == 0 ~ "0",
                               rain >= 0.2 ~ ">0.2")) %>%
  filter(remark_rain != "NA", frozen == 0) %>%
  left_join(select(site_table, USGS_Station_Number, Area, Site_Type), by = c("site" = "USGS_Station_Number")) %>%
  mutate(remark_rain = factor(remark_rain, c(">0.2", "<0.2", "0")),
         date2020 = storm_start, 
         runoff_index = runoff_volume / (rain * Area * 43560 / 12))

year(wq.precip.df.norain$date2020) <- 2020

runoff_byrain_ts <- ggplot(wq.precip.df.norain) +
  geom_point(aes(x=storm_start, y = runoff_volume, color = remark_rain, alpha = remark_rain), 
             size = 1) +
  facet_wrap(~Field_Name, scales = "free_x") + 
  scale_color_manual(values = c("black", "blue", "red")) + 
  scale_alpha_manual(values = c(0.2, 1, 1)) + 
  scale_y_log10() +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA, color = NA))

print(runoff_byrain_ts)

ggsave(file.path(path_to_results, "Figures", "Rain", "StormRunoff_byrain_ts.png"), runoff_byrain_ts, height = 12, width = 18)

runoff_index_hist <- ggplot(filter(wq.precip.df.norain, remark_rain == ">0.2" & 
                                     runoff_index < 2 & Site_Type == "Surface")) +
  geom_histogram(aes(x=runoff_index), 
                 size = 1) +
  facet_wrap(~Field_Name, scales = "free_y") + 
  scale_color_manual(values = c("black", "blue", "red")) + 
  # scale_alpha_manual(values = c(0.2, 1, 1)) + 
  # scale_y_log10() +
  geom_vline(xintercept = 0.8, col = "red") + 
  theme_bw() +
  theme(strip.background = element_rect(fill = NA, color = NA))

print(runoff_index_hist)

ggsave(file.path(path_to_results, "Figures", "Rain", "StormRunoffIndex_hist.png"), runoff_index_hist, height = 12, width = 18)


runoff_byrain_ts_2020 <- ggplot(wq.precip.df.norain) +
  geom_point(aes(x=date2020, y = runoff_volume, color = remark_rain, alpha = remark_rain), 
             size = 1) +
  facet_wrap(~Field_Name) + 
  scale_color_manual(values = c("black", "blue", "red")) + 
  scale_alpha_manual(values = c(0.2, 1, 1)) + 
  scale_x_datetime(date_labels = "%m", minor_breaks = "months") + 
  scale_y_log10() +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA, color = NA)) +
  labs(x = "Month")

print(runoff_byrain_ts_2020)

ggsave(file.path(path_to_results, "Figures", "Rain", "StormRunoff_byrain_2020_ts.png"), runoff_byrain_ts_2020, height = 12, width = 18)


#Compare storm and rain totals

head(StormSummary_df_combined)
head(Rain.uv.combined)


storm_sub <- StormSummary_df_combined %>%
  select(USGS_Station_Number_for_Precipitation , StartDate, EndDate, rain, All_Field_Names, Project) %>%
  mutate(wateryear = getWY(StartDate)) %>%
  group_by(USGS_Station_Number_for_Precipitation , All_Field_Names, Project, wateryear) %>%
  summarize(storm_total = sum(rain, na.rm = TRUE))


rain_sub <- Rain.uv.combined %>%
  mutate(wateryear = getWY(pdate)) %>%
  rename(USGS_Station_Number_for_Precipitation = rain_site) %>%
  group_by(USGS_Station_Number_for_Precipitation, wateryear) %>%
  summarize(rain_total = sum(rain, na.rm = TRUE))


combined_sub <- full_join(storm_sub, rain_sub) %>%
  mutate(rain_diff = storm_total - rain_total)

rain_v_storm_scatter <- ggplot(combined_sub, aes(x=storm_total, y = rain_total)) +
  geom_abline() + 
  geom_point(aes(color = as.factor(wateryear)), size = 3) +
  facet_wrap(~USGS_Station_Number_for_Precipitation, nrow = 5) +
  labs(color = "Wateryear") + 
  theme_bw()


ggsave(file.path(path_to_results, "Figures", "Rain", "StormTotalsVersusRainTotals.png"), rain_v_storm_scatter, height = 10, width = 18)

combined_long <- combined_sub %>%
  pivot_longer(cols = contains("total"), names_to = "method", values_to = "rain_total" )

rain_v_storm_ts <- ggplot(combined_long) +
  # geom_hline(y_intercept = 0) + 
  geom_col(aes(x = as.factor(wateryear), y = rain_total, fill = method, group = method), 
           position = "dodge") +
  facet_wrap(~USGS_Station_Number_for_Precipitation, nrow = 5) +
  labs(x = "Wateryear") + 
  theme_bw() 

print(rain_v_storm_ts)

ggsave(file.path(path_to_results, "Figures", "Rain", "StormTotalsVersusRainTotals_bar.png"), rain_v_storm_ts, height = 10, width = 18)

rain_v_storm_diff_ts <- ggplot(combined_long) +
  geom_hline(yintercept = 0) +
  geom_col(aes(x = as.factor(wateryear), y = rain_diff)) +
  facet_wrap(~USGS_Station_Number_for_Precipitation, nrow = 5) +
  labs(x = "Wateryear") + 
  theme_bw()

print(rain_v_storm_diff_ts)

ggsave(file.path(path_to_results, "Figures", "Rain", "StormTotalsVersusDiff_bar.png"), rain_v_storm_diff_ts, height = 10, width = 18)




StormSummary_df_working <- StormSummary_df_out %>%
  mutate(wateryear = getWY(StartDate)) %>%
  filter(wateryear <= 2019, Project == "GLRI")

StormSummary_df_working$All_Field_Names[which(StormSummary_df_working$USGS_Station_Number_for_Precipitation == "441520088045001")] <- "WI-SW3"

erosivity_box <- ggplot(StormSummary_df_working, aes(x = wateryear, group = wateryear, y = erosivity_m2)) +
  geom_jitter(height = NULL, width = .1, size = .5, color = "grey") + 
  geom_boxplot(alpha = .5, fill = "grey") +
  scale_y_log10() +
  facet_wrap(~All_Field_Names, nrow = 2) +
  theme_bw()

erosivity_box

ggsave(file.path(path_to_results, "Figures", "Rain", "ErosivityGLRISitesYears.png"), erosivity_box, width = 10, height = 5)

erosivity_bar <- ggplot(StormSummary_df_working, aes(x = wateryear, group = wateryear, y = erosivity_m2)) +
  geom_col(aes(fill = as.factor(wateryear))) +
  # scale_y_log10() +
  facet_wrap(~All_Field_Names, nrow = 2) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(y = "sum or erosivity")

erosivity_bar

ggsave(file.path(path_to_results, "Figures", "Rain", "ErosivityGLRISitesYears_sum.png"), erosivity_bar, width = 10, height = 5)

wq_working <- wq.precip.df %>%
  mutate(wateryear = getWY(storm_start)) %>%
  filter(wateryear <= 2019, Project == "GLRI", grepl("SW", Field_Name))


erosivity_box_flow <- ggplot(wq_working, aes(x = wateryear, group = wateryear, y = erosivity_m2)) +
  geom_jitter(height = NULL, width = .1, size = .5, color = "grey") + 
  geom_boxplot(alpha = .5,  outlier.shape = NA, aes(fill = as.factor(wateryear))) +
  scale_y_log10() +
  facet_wrap(~Field_Name, nrow = 4) +
  theme_bw() +
  ggtitle("Erosivity for flow events") +
  theme(legend.position = "none")

print(erosivity_box_flow)

ggsave(file.path(path_to_results, "Figures", "Rain", "ErosivityGLRISitesYears_Flow.png"), erosivity_box_flow, width = 12, height = 8)
