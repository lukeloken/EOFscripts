

data_df <- readRDS(file=(file_in(file.path(path_to_data, "compiled_data", "storm_event_loads", "storm_event_conc_allsites_model.rds" ))))

all_sites <- as.character(unique(data_df$site))

site_nu <- 15
# for (site_nu in 1:length(all_sites)){
  site_name <- all_sites[site_nu]
  print(site_name)
  state <- substr(site_name, 1,2)
  
  #Subset to only one site and drop all columns with NAs or infinite
  dat <- filter(data_df, site == site_name) %>%
    select_if(not_all_na) %>%
    select_if(function(x) {all(!is.infinite(x))}) %>%
    mutate(period = factor(period, c('before', 'after')))
  
  #Find rain file
  # folders<-list.files(file.path(path_to_data, "field_analysis", "results", site_name))
  



#Rain file for site 1 WI
WI_rain <- read.csv(file.path(path_to_data, 'field_analysis', 'results', 'WI-SW1','2020-03-06-1203','raw', 'SW1_Precip_AQexport_EntireRecord.csv'))

WI_rain_daily <- WI_rain %>%
  mutate(datetime = as.POSIXct(pdate, tz='America/Chicago', format = "%m/%d/%Y %H:%M")) %>%
  mutate(date = as.Date(datetime, tz='America/Chicago')) %>%
  select(datetime, date, rain) %>%
  group_by(date) %>%
  summarize(rain = sum(rain, na.rm=T))

head(WI_rain_daily)  

WI_rain_monthly <- WI_rain_daily %>%
  mutate(month = month(date),
         year = year(date)) %>%
  group_by(month, year) %>%
  summarize(rain = sum(rain, na.rm=T))

WI_rain_monthly$date = as.Date(paste(WI_rain_monthly$year, WI_rain_monthly$month, '01',sep='-'))




ggplot(WI_rain_daily) +
  geom_point(aes(x=date, y=rain))


ggplot(WI_rain_daily, aes(x = date, y=rain)) + 
  geom_histogram(stat='identity') +
  geom_point(aes(x=date, y=rain))


ggplot(WI_rain_daily, aes(x=date, xend=date, y=0, yend=rain))+
  geom_segment()


daterange = range(c(as.Date(dat$storm_middate), as.Date(WI_rain_monthly$date)),na.rm=T)

daily_rainplot <- ggplot(WI_rain_daily, aes(x=date, xend=date, y=0, yend=rain))+
  geom_segment(size=.05) +
  theme_bw() +
  labs(y='daily rain total (in)',
       x='calendar date') +
  scale_y_continuous(limits=c(0,ceiling(max(WI_rain_daily$rain)*10)/10), expand=c(0,0)) +
  scale_x_date(limits=daterange) +
  theme(panel.grid.major = element_blank(), panel.grid.minor =element_blank())

print(daily_rainplot)

ggsave(file.path(path_to_results, 'Figures', 'Rain', 'WI-SW1_DailyRain.png'), daily_rainplot, width=5, height=3, units='in')




monthly_rainplot <- ggplot(WI_rain_monthly, aes(x=date, xend=date, y=0, yend=rain))+
  geom_segment(size=1.3) +
  theme_bw() +
  labs(y='monthly rain total (in)',
       x='calendar date') +
  scale_y_continuous(limits=c(0,ceiling(max(WI_rain_monthly$rain))), expand=c(0,0)) +
  scale_x_date(limits=daterange)

print(monthly_rainplot)

ggsave(file.path(path_to_results, 'Figures', 'Rain', 'WI-SW1_MonthlyRain.png'), monthly_rainplot, width=5, height=3, units='in')



runoffplot <- ggplot(dat, aes(x=as.Date(storm_middate), xend=as.Date(storm_middate), y=runoff_volume, yend=10)) +
  geom_segment(aes(color=period), size=.5) +
  geom_point(aes(col=period, shape=frozen)) + 
  scale_shape_manual(values=c(16,1), guide=F) + 
  scale_color_brewer(type='qual', palette=2) + 
  scale_x_date(limits=daterange, date_breaks = 'years', date_labels = "%Y") +
  # scale_y_continuous(limits=c(0,ceiling(max(dat$runoff_volume)/10000)*10000), expand=c(0,0)) +
  scale_y_log10nice(limits=c(10,ceiling(max(dat$runoff_volume)/1000000)*1000000), expand=c(0,0),
                    name = 'runoff volume per event (cf)') +
  theme_bw() +
  labs(y='runoff volume per event (cf)',
       x='calendar date') +
  theme(panel.grid.major = element_blank(), panel.grid.minor =element_blank(), 
        legend.position=c(1,1), legend.justification = c(1,1), 
        legend.background = element_rect(fill=NA, colour=NA), legend.title=element_blank()) +
        guides(color = guide_legend(ncol=2))

print(runoffplot)

png(file.path(path_to_results, 'Figures', 'Rain', 'WI-SW1_DailyRainandRunoff.png'), width=5, height=6, units='in', res=300)

grid.newpage()
plots<-grid.draw(rbind(ggplotGrob(daily_rainplot), ggplotGrob(runoffplot), size = "last"))

dev.off()




ggplot(dat[which(dat$frozen==F & dat$weq>0),], aes(x=weq, y=runoff_volume/10.3*12/43560, col=period, group=period)) +
  geom_point(size=2) +
  scale_shape_manual(values=c(16,1)) + 
  theme_bw() +
  geom_abline() +
  geom_smooth(method=lm, se=F)

ggplot(dat[which(dat$frozen==F & dat$weq>0),], aes(x=runoff_volume/10.3*12/43560, col=period, group=period)) +
  geom_histogram()

ggplot(dat[which(dat$frozen==F & dat$weq>0),], aes(y=runoff_volume/10.3*12/43560, col=period, group=period)) +
  geom_boxplot()

ggplot(dat[which(dat$frozen==F & dat$weq>0),], aes(x=weq, y=runoff_volume/10.3*12/43560, col=as.factor(month), group=as.factor(month))) +
  geom_point() +
  geom_smooth(method=lm, se=F) +
  theme_bw()

  geom_boxplot()
