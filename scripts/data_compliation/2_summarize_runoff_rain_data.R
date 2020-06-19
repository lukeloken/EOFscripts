
library(stringr)
library(cowplot)

#Site data
master_beforeafter_df <- readRDS(file.path(path_to_data, 'compiled_data', 'rain', 'Compiled_Masters.rds'))


#Rain data
StormSummary_df <- readRDS(file.path(path_to_data, 'compiled_data', 'rain', 'Compiled_Rain_Data.rds'))

#Water data
data_df_new <- readRDS(file=(file_out(file.path(path_to_data, "compiled_data", "storm_event_loads", "storm_event_loads_allsites_approved_data.rds" ))))

data_df_new$state <- str_split(data_df_new$site, "-", simplify = TRUE)[,1]

head(data_df_new)



#Plot runoff figures

runoff_allsites <- ggplot(data_df_new,
                          aes(x=storm_start,xend=storm_start,yend=0,y=runoff_volume)) +
  geom_segment(size=0.2, color = 'grey') +
  geom_point(aes(col=state), size=1.75, shape = 16, alpha=.5) +
  facet_wrap(~site, ncol=4, scales = 'free') +
  theme_bw() +
  theme(legend.position='none') + 
  scale_y_sqrt(limits=c(0, NA), expand=c(0,0)) +
  theme(strip.background = element_rect(fill=NA, color=NA)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), )

print(runoff_allsites)

ggsave(file.path(path_to_results, 'Figures', 'Runoff_Timeseries_AllSites.png'), runoff_allsites, height=8, width = 12, units='in')




#Plot rain figures

#Timeseries figures
rain_allsites.TS <- ggplot(StormSummary_df, aes(x=StartDate, xend=StartDate, yend=0, y=rain)) +
  geom_segment(size=0.2, col='grey') +
  geom_point(aes(col=state), size=1.25, alpha=0.5) +
  facet_wrap(~paste(site, rain_site, sep=': '), ncol=2, scales='free_x') +
  theme_bw() +
  theme(legend.position='none') +
  ggtitle('Storm Size: NWIS and raw files') +
  scale_y_sqrt(limits=c(0, NA), expand=c(0, 0)) +
  theme(strip.background = element_rect(fill=NA, color=NA))

print(rain_allsites.TS)

ggsave(file.path(path_to_results, 'Figures', 'Rain', 'Rain_Timeseries_stormsizes_bothsources_v2.png'),
       rain_allsites.TS, height=12, width = 10, units='in')

#Histogram
rain_allsites.hist <- ggplot(StormSummary_df, aes(x=rain)) +
  geom_histogram(aes(fill=state), bins=20) +
  facet_wrap(~site, ncol=2, scales='free_y') +
  theme_bw() +
  theme(legend.position='bottom') +
  guides(fill = guide_legend(nrow=1)) +
  ggtitle('Storm sizes (in)') +
  scale_x_sqrt(limits=c(0, NA), expand=c(0, 0), name='rain (in)') +
  theme(strip.background = element_rect(fill=NA, color=NA))

print(rain_allsites.hist)

ggsave(file.path(path_to_results, 'Figures', 'Rain', 'Rain_Histograms_stormsizes_bothsources.png'), rain_allsites.hist, height=12, width = 6, units='in')





#Link rain with storms



all_sites <- master_beforeafter_df$site
rain_new_list <- flow_new_list <- list()
site_nu <- 4
for (site_nu in 1:length(all_sites)){
  
  site_name <- rain_name <- start_date <- end_date <- bmp_date <- '.dplyr'
  rain_df_i <- flow_df_i <- storm_start <- storm_end <- rain_storm_i <- '.dplyr'
  aligned <- plot_out <- flow_ts_i <- rain_ts_i <-'dplyr'
  
  site_name <- all_sites[site_nu]
  rain_name <- master_beforeafter_df$`rain station ID`[site_nu]
  
  start_date <- master_beforeafter_df$start_date[site_nu]
  end_date   <- master_beforeafter_df$end_date[site_nu]
  bmp_date   <- master_beforeafter_df$bmp_date[site_nu]
  
  rain_df_i <- filter(StormSummary_df, rain_site == rain_name) %>%
    mutate(DidItFlow = NA)
  flow_df_i <- filter(data_df_new, site == site_name) %>%
    mutate(RainStart = NA,
           RainEnd = NA,
           Rain = NA,
           DidItRain = NA)
  
  event_nu <- 1
  for (event_nu in 1:nrow(flow_df_i)){
    storm_start <- flow_df_i$storm_start[event_nu] -(3600*6)   
    storm_end   <- flow_df_i$storm_end[event_nu]
    
    rain_storm_i <- rain_df_i %>%
      filter(StartDate<storm_end, EndDate>storm_start)
    
    if (nrow(rain_storm_i)>0){
      rain_df_i$DidItFlow[rain_df_i$stormnum %in% rain_storm_i$stormnum] <- 'Yes'
      
      rain_storm_i <- rain_storm_i %>%
        summarize(RainStart = min(StartDate, na.rm=T),
                  RainEnd = max(EndDate, na.rm=T),
                  rain = sum(rain, na.rm=T))
      
      flow_df_i[event_nu, c('RainStart', 'RainEnd', 'Rain')] <- rain_storm_i
    }
  }
  
  rain_df_i$DidItFlow[is.na(rain_df_i$DidItFlow)] <- 'No'
  rain_df_i <- rain_df_i %>%
    mutate(DidItFlow = factor(DidItFlow, c('Yes', 'No')))
  
  # rain_df_i <- mutate(rain_df_i, DidItFlow = factor(DidItFlow, 'Yes', 'No'))
  
  flow_df_i <- flow_df_i %>%
    mutate(DidItRain = case_when(is.na(Rain) ~ 'No',
                                 Rain>0 ~ 'Yes')) %>%
    mutate(DidItRain = factor(DidItRain, c('Yes', 'No')))
  
  flow_ts_i <- ggplot(flow_df_i,  aes(x=storm_start,xend=storm_start,yend=0,y=runoff_volume)) +
    geom_segment(size=0.2, color = 'grey') +
    geom_point(aes(color=DidItRain, shape=as.factor(frozen)), size=1.5, alpha=.3) +
    scale_color_manual(values=c('blue', 'red')) + 
    scale_shape_manual(values=c(16,1), name='frozen') + 
    # facet_wrap(~site, ncol=4, scales = 'free') +
    theme_bw() +
    theme(legend.position = 'bottom') +
    guides(shape = guide_legend(nrow = 1, title.hjust = 0.5)) + 
    ggtitle(site_name) +
    scale_y_sqrt(limits=c(0, max(flow_df_i$runoff_volume, na.rm=T)*1.05), expand=c(0,0)) +
    scale_x_datetime(limits = as.POSIXct(c(start_date, end_date)), 
                     date_breaks = 'years', date_labels ='%Y') +
    theme(strip.background = element_rect(fill=NA, color=NA)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.title.x=element_blank(), legend.margin = margin(t = 0, b = 0, unit='cm'))
  
  rain_ts_i <- ggplot(rain_df_i, aes(x=StartDate, xend=StartDate, yend=0, y=rain)) +
    geom_segment(size=0.2, col='grey') +
    # geom_point(aes(col=state), size=1.5, alpha=0.5) +
    geom_point(aes(color=DidItFlow), size=1.5, alpha=.3) +
    scale_color_manual(values=c('blue', 'red')) + 
    # facet_wrap(~paste(site, rain_site, sep=': '), ncol=2, scales='free_x') +
    theme_bw() +
    theme(legend.position='bottom') +
    # ggtitle('Storm Size: NWIS and raw files') +
    scale_y_sqrt(limits=c(0, max(rain_df_i$rain, na.rm=T)*1.05), expand=c(0, 0)) +
    scale_x_datetime(limits = as.POSIXct(c(start_date, end_date)), 
                     date_breaks = 'years', date_labels ='%Y') +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.title.x=element_blank(), legend.margin=margin(t = 0, b = 0, unit='cm'))
  
  aligned <- align_plots(flow_ts_i, rain_ts_i, align='v')
  plot_out <- plot_grid(aligned[[1]], aligned[[2]], ncol=1)
  
  ggsave(file.path(path_to_results, 'Figures', 'Rain_Runoff_TS', paste(site_name, '.png', sep='')),
         plot_out, height=7, width=6, units='in')
  
  rain_new_list[[site_nu]] <- rain_df_i
  flow_new_list[[site_nu]] <- flow_df_i
  
}

names(rain_new_list) <- names(flow_new_list) <- all_sites

#Bind list together
rain_new_df <- bind_rows(rain_new_list, .id = site_name) %>%
  select(-site) %>%
  rename(site = site_name) %>%
  mutate(month = month(StartDate)) %>%
  mutate(season = case_when(month %in% c(12,1,2) ~ 'winter',
                            month %in% c(3:5) ~ 'spring', 
                            month %in% c(6:9) ~ 'summer', 
                            month %in% c(10:11) ~ 'autumn'),
         season = factor(season, c('winter', 'spring', 'summer', 'autumn')),
         type = substr(site, 4,5)) %>%
  filter(!is.na(StartDate) & !is.na(rain))


flow_new_df <- bind_rows(flow_new_list, .id = site_name) %>%
  select(-site) %>%
  rename(site = site_name) %>%
  mutate(month = month(storm_start)) %>%
  mutate(season = case_when(month %in% c(12,1,2) ~ 'winter',
                            month %in% c(3:5) ~ 'spring', 
                            month %in% c(6:9) ~ 'summer', 
                            month %in% c(10:11) ~ 'autumn'),
         season = factor(season, c('winter', 'spring', 'summer', 'autumn')),
         type = substr(site, 4,5)) %>%
  filter(!is.na(storm_start) & !is.na(runoff_volume))


ggplot(rain_new_df,aes(x=rain, fill=DidItFlow, col=DidItFlow)) + 
  # geom_density(alpha=0.25) +
  # geom_freqpoly(alpha=.5, bins=20, size=2) +
  geom_histogram(alpha=.2, bins=20) +

  scale_x_sqrt(limits=c(0,NA), expand=c(0,0)) +
  scale_y_sqrt(limits=c(0,NA), expand=c(0,0)) +
  facet_wrap(~site, scales='free') +
  theme_bw() +
  theme(strip.background = element_rect(fill=NA, color=NA)) +
  theme(legend.position = 'bottom')


ggplot(flow_new_df,aes(x=runoff_volume, fill=DidItRain, col=DidItRain)) + 
  # geom_density(alpha=0.25) +
  geom_freqpoly(alpha=.5, bins=20, size=2) +
  scale_x_log10() +
  facet_wrap(~site, scales='free') +
  theme_bw() +
  theme(strip.background = element_rect(fill=NA, color=NA)) +
  theme(legend.position = 'bottom')



ggplot(filter(rain_new_df, type=='SW'),aes(x=rain, fill=DidItFlow, col=DidItFlow)) + 
  geom_density(alpha=0.25) +
  scale_x_sqrt(limits=c(0,NA), expand=c(0,0)) +
  facet_grid(season~site, scales='free') +
  theme_bw() +
  theme(strip.background = element_rect(fill=NA, color=NA)) +
  theme(legend.position = 'bottom')


ggplot(filter(flow_new_df, type=='SW'),aes(x=runoff_volume, fill=DidItRain, col=DidItRain)) + 
  geom_density(alpha=0.25) +
  scale_x_log10() +
  facet_grid(season~site, scales='free') +
  theme_bw() +
  theme(strip.background = element_rect(fill=NA, color=NA)) +
  theme(legend.position = 'bottom')


