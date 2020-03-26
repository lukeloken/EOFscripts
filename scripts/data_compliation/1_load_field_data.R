
library(anytime)
#Combine farm managmenet spreadsheets and summarize by year

#path_to_data

approved_data <- list.files(file_in(file.path(path_to_data, 'field_analysis', 'approved_site_data')))

field_data <- approved_data[grepl('Field Management', approved_data)]
field_data <- field_data[grepl('-SW', field_data)]

field_list <-list()
file_i=1
for (file_i in 1:length(field_data)){
  file <- field_data[file_i]
  path_to_sitefile <- file.path(path_to_data, "field_analysis", "approved_site_data", file)
  
  #Find site name and timezone  for the site
  
  file_site <- unlist(strsplit(file, " Field Management.csv"))[1]
  state <- substr(file_site[1], 1,2)
  site <- substr(file_site[1], 4,6)
  full_site <- paste(state, site, sep='-')
  
  #load and combine all model (mod) files
  data_i <- read.csv(path_to_sitefile, stringsAsFactors = F, header=T) %>%
    mutate(date = anytime(date), 
           water_year2 = getWY(date), 
           calendar_year = year(date)) %>%
    filter(!is.na(date))
  
  data_i[as.logical((rowSums(is.na(data_i))-ncol(data_i))),]
  
  
  field_list[[file_i]]<-data_i
  names(field_list)[[file_i]] <- full_site
}

str(field_list)
#Combine all sites into a single data.frame
field_df <- ldply(field_list, data.frame, .id = "site") %>%
  select(site, calendar_year, water_year2, date, 
         current_crop, last_year_crop, activity_group, activity, X) %>%
  mutate(site = factor(site)) %>%
  mutate(site = factor(site, rev(levels(site))))
  

extrarows <- field_df[which(field_df$activity_group=='planting + fertilizer application'),]
extrarows$activity_group <- 'planting'
field_df$activity_group[which(field_df$activity_group=='planting + fertilizer application')] <- 'fertilizer application'

activity_table <- table(field_df$activity_group)

activities <- names(activity_table)

field_df$activity_group[grepl('plant', field_df$activity_group, ignore.case=T)] <- "planting"
field_df$activity_group[grepl('harvest', field_df$activity_group, ignore.case=T)] <- "harvest"
field_df$activity_group[grepl('herbicide', field_df$activity_group, ignore.case=T)] <- "herbicide applicatoin"
field_df$activity_group[grepl('fert', field_df$activity_group, ignore.case=T)] <- "fertilizer application"
field_df$activity_group[which(field_df$activity_group=='')] <- 'other'
field_df$activity_group[which(field_df$activity_group=='intervention')] <- 'BMP'

field_df$current_crop[grepl('soy', field_df$current_crop, ignore.case=T)] <- 'Soybeans'
field_df$current_crop[grepl('silage', field_df$current_crop, ignore.case=T)] <- 'Cooorn Silage'
field_df$current_crop[grepl('Corn', field_df$current_crop, ignore.case=T)] <- 'Corn'
field_df$current_crop[grepl('silage', field_df$current_crop, ignore.case=T)] <- 'Corn Silage'
field_df$current_crop[grepl('alf', field_df$current_crop, ignore.case=T)] <- 'Alfalfa'


field_crop_byyear <- field_df %>%
  select(site, calendar_year, current_crop) %>%
  distinct() %>%
  arrange(site, calendar_year)

field_harvest_byyear <- field_df %>%
  filter(activity_group %in% c('harvest', 'cutting')) %>%
  select(site, calendar_year, current_crop, last_year_crop, date, activity_group, activity) %>%
  distinct() %>%
  arrange(site, calendar_year)

field_planting_byyear <- field_df %>%
  filter(activity_group %in% c('planting')) %>%
  select(site, calendar_year, current_crop, last_year_crop, date, activity_group, activity) %>%
  distinct() %>%
  arrange(site, calendar_year)


field_cultivation_byyear <- field_df %>%
  filter(activity_group == 'cultivation') %>%
  select(site, calendar_year, current_crop, date, activity_group, activity) %>%
  distinct() %>%
  arrange(site, calendar_year)

field_fertilizer_byyear <- field_df %>%
  filter(activity_group == 'fertilizer application') %>%
  select(site, calendar_year, current_crop, date, activity_group, activity) %>%
  distinct() %>%
  arrange(site, calendar_year)

field_cultivation_byyear

field_harvest_byyear[field_harvest_byyear$site =='NY-SW4',]

field_df[field_df$site == 'WI-SW3',]



harvest_timeline <- ggplot(field_harvest_byyear, aes(x=date, y=site, fill=current_crop, shape=current_crop)) +
  geom_point(size=3, col='black', stroke=1) +
  labs(x='Year', y='Site', fill='Crop', shape='Crop') +
  theme_bw() +
  theme(legend.position='bottom', axis.title =element_blank(), 
        plot.title = element_text(hjust = 0.5, size=12)) +
  scale_shape_manual(values=c(25, 21:24)) +
  ggtitle('Harvest date')


ggsave(file=file_out(file.path(path_to_results, 'Figures', "HavestDates.png")), harvest_timeline, height=6, width=8)


