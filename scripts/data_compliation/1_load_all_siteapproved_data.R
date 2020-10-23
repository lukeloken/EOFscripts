
#Loop through all site folders and load edge of field runoff data

# library(anytime)

#This is where the data live
print(path_to_data)

master_beforeafter_df <- readRDS(file.path(path_to_data, 'compiled_data', 'rain', 'Compiled_Masters.rds'))
gis_combined <- readRDS(file.path(path_to_data, 'compiled_data', 'GIS_Compiled.rds'))


#Create empty list for all site data
allsites_list <-list()

#Identify how many files are in the approved_site_data folder
approved_files <- list.files(file.path(path_to_data, "field_analysis", "approved_site_data"))
approved_files <- approved_files[grepl(".csv", approved_files)]
approved_files <- approved_files[which(grepl("Management", approved_files)==FALSE)]
approved_files <- approved_files[which(grepl("_orig", approved_files)==FALSE)]
approved_files <- approved_files[which(grepl("NYTL", approved_files)==FALSE)]
approved_files <- approved_files[which(grepl("paired", approved_files)==FALSE)]
approved_files <- approved_files[which(grepl("storm event data", approved_files))]



file_nu =1
# for (file_nu in c(1:7, 9:21)){
for (file_nu in 1:length(approved_files)){
  state_tz <- full_site <- state <- site <- datetime_format <- storm_end_na <- storm_start_na <- NA
  
  file <- approved_files[file_nu]
  path_to_sitefile <- file.path(path_to_data, "field_analysis", "approved_site_data", file)
  
  #Find site name and timezone  for the site
  
  file_site_list <- unlist(strsplit(file, "-"))
  state <- substr(file_site_list[1], nchar(file_site_list[1])-1, nchar(file_site_list[1]))
  site <- substr(file_site_list[2], 1,3)
  full_site <- paste(state, site, sep='-')
  
  state_tz <- master_beforeafter_df$site_tz[match(full_site, master_beforeafter_df$site)]
  datetime_format <- master_beforeafter_df$datetime_format[match(full_site, master_beforeafter_df$site)]

  #Prevoiusly did not have master file for MI-TL2  
  # if(full_site == 'MI-TL2'){
  #   state_tz <- master_beforeafter_df$site_tz[match('MI-SW2', master_beforeafter_df$site)]
  #   datetime_format <- master_beforeafter_df$datetime_format[match('MI-SW2', master_beforeafter_df$site)]
  #   }
  
  # if (state %in% c('WI')){
  #   state_tz <- 'America/Chicago'
  # } else if (state %in% c('OH', 'NY', 'IN', 'MI')){
  #   state_tz <- 'America/New_York'
  # }
  
  #load and combine all model (mod) files
  # data_i <- read.csv(path_to_sitefile, stringsAsFactors = F, header=T) %>%
  #   mutate(sample_start = anytime(sample_start, tz=state_tz),
  #          sample_end = anytime(sample_end, tz=state_tz),
  #          storm_start =anytime(storm_start, tz=state_tz),
  #          storm_end = anytime(storm_end, tz=state_tz)) %>%
  #   arrange(storm_start) %>%
  #   # select(-file_id) %>%
  #   distinct()
  
  data_i <- read.csv(path_to_sitefile, stringsAsFactors = F, header=T) %>%
    mutate(across(contains('_start', ignore.case=TRUE), 
                  as.POSIXct, tz=state_tz, format = datetime_format)) %>%
  mutate(across(contains('_end', ignore.case=TRUE), 
                as.POSIXct, tz=state_tz, format = datetime_format))  %>%
    arrange(storm_start) %>%
    # select(-file_id) %>%
    distinct()
  
  #remove rows that are entirely NA
  data_i[as.logical((rowSums(is.na(data_i))-ncol(data_i))),]
  
  storm_end_na <- length(which(is.na(data_i$storm_end)))
  storm_start_na <- length(which(is.na(data_i$storm_start)))
  
  if (storm_end_na == nrow(data_i) | storm_start_na == nrow(data_i)){
    stop(paste0("Site ", toString(full_site), " has all NAs in storm_start or storm_end columns. Check Master file datetime and date formats."))
  } else if (storm_end_na > 0 | storm_start_na > 0){
    storms_missing_dates <- unique(data_i$unique_storm_id[which(is.na(data_i$storm_end))],
           data_i$unique_storm_id[which(is.na(data_i$storm_start))])
    
    warning(paste0("Site ", toString(full_site), " has ", toString(length(storm_end_na) + length(storm_start_na)),
                   " NAs in storm_start or storm_end columns. Dropping storm #'s: ", toString(storms_missing_dates)))
    data_i <- filter(data_i, !is.na(storm_end), !is.na(storm_start))
  }
  
  if ('storm_runoff_cubic_feet' %in% names(data_i)){
    data_i <- dplyr::mutate(data_i, runoff_volume = storm_runoff_cubic_feet) %>%
      select(-storm_runoff_cubic_feet)
    
  }
  
  data_i <- mutate(data_i, runoff_volume = as.numeric(runoff_volume)) %>%
    select(-contains("New_"), -contains("X."), -contains("LoadRatio"))
  
  # data_i <- read.csv(path_to_sitefile, stringsAsFactors = F, header=T) %>%
  #   mutate(sample_start = as.POSIXct(sample_start, tz=state_tz, format='%m/%d/%Y %H:%M'),
  #          sample_end = as.POSIXct(sample_end, tz=state_tz, format='%m/%d/%Y %H:%M'),
  #          storm_start = as.POSIXct(storm_start, tz=state_tz, format='%m/%d/%Y %H:%M'),
  #          storm_end = as.POSIXct(storm_end, tz=state_tz, format='%m/%d/%Y %H:%M')) %>%
  #   arrange(storm_start) %>%
  #   # select(-file_id) %>%
  #   distinct()
  
  #place data into list
  allsites_list[[file_nu]]<-data_i
  names(allsites_list)[[file_nu]] <- full_site
}

#Combine all sites into a single data.frame
data_df <- ldply(allsites_list, data.frame, .id = "site") %>%
  left_join(select(gis_combined, site, Area_acres), by = "site") %>%
  select(!contains("yield"))



# #########################################################
# unite columns that have the same data, but different names
# #########################################################

#Preferred load names
loadvars <- c('suspended_sediment_load_pounds', 
              'chloride_load_pounds',
              'no2_no3n_load_pounds', 
              'ammonium_n_load_pounds',
              'tkn_unfiltered_load_pounds', 
              'orthophosphate_load_pounds',
              'tp_unfiltered_load_pounds',
              'total_nitrogen_load_pounds',
              'organic_nitrogen_load_pounds',
              'doc_load_pounds', 
              'toc_load_pounds')

#Preferred concentration names
concvars <- c('suspended_sediment_conc_mgL', 
              'chloride_conc_mgL',
              'no2_no3_n_conc_mgL', 
              'ammonium_n_conc_mgL',
              'tkn_unfiltered_conc_mgL', 
              'orthophosphate_conc_mgL',
              'total_phosphorus_conc_mgL',
              'total_nitrogen_conc_mgL',
              'organic_nitrogen_conc_mgL',
              'doc_conc_mgL',
              'toc_conc_mgL')

#Preferred yield names
yieldvars <- c('suspended_sediment_yield_pounds_per_acre', 
              'chloride_yield_pounds_per_acre',
              'no2_no3n_yield_pounds_per_acre', 
              'ammonium_n_yield_pounds_per_acre',
              'tkn_unfiltered_yield_pounds_per_acre', 
              'orthophosphate_yield_pounds_per_acre',
              'tp_unfiltered_yield_pounds_per_acre',
              'total_nitrogen_yield_pounds_per_acre',
              'organic_nitrogen_yield_pounds_per_acre',
              'doc_yield_pounds_per_acre', 
              'toc_yield_pounds_per_acre')

flagvars <- c("flag_suspended_sediment", "flag_chloride", "flag_no2_no3n", 
              "flag_ammonium_n", "flag_tkn_unfiltered", "flag_orthophosphate",
              "flag_tp_unfiltered", "flag_tn", "flag_orgN", "flag_doc", "flag_toc" )



#Identify multiple columns for each variable and data type
sediment_names <- names(data_df)[grepl('sediment', names(data_df), ignore.case=T)]
chloride_names <- names(data_df)[grepl('chloride', names(data_df), ignore.case=T)]
nitrate_names <- names(data_df)[grepl('no3', names(data_df), ignore.case=T)]
ammonium_names <- names(data_df)[grepl('Ammonium', names(data_df), ignore.case=T)]
TKN_names <- names(data_df)[grepl('tkn', names(data_df), ignore.case=T)]
SRP_names <- unique(c(names(data_df)[grepl('ortho', names(data_df), ignore.case=T)], 
                      names(data_df)[grepl('Reactive.Phosphorus', names(data_df), ignore.case=T)]))
TP_names <- unique(c(names(data_df)[grepl('tp', names(data_df), ignore.case=T)], 
                     names(data_df)[grepl('total_phosphorus', names(data_df), ignore.case=T)]))
TN_names <- unique(c(names(data_df)[grepl('total_nitrogen', names(data_df), ignore.case=T)], 
                     names(data_df)[grepl('Total.Nitrogen', names(data_df), ignore.case=T)]))
organicN_names <-unique(c(names(data_df)[grepl('organic_nitrogen', names(data_df), ignore.case=T)], 
                          names(data_df)[grepl('organic.nitrogen', names(data_df), ignore.case=T)]))
DOC_names <- names(data_df)[grepl('doc', names(data_df), ignore.case=T)]
TOC_names <- names(data_df)[grepl('toc', names(data_df), ignore.case=T)]

#Combine names into a list. The order of this list must match the good column names listed above
names_list <- list(sediment_names, chloride_names, nitrate_names, ammonium_names, TKN_names,  SRP_names,
                   TP_names, TN_names, organicN_names, DOC_names, TOC_names)

#Load and concentration
load_names <- unique(c(names(data_df)[grepl('Load', names(data_df), ignore.case=T)],
                       names(data_df)[grepl('lound', names(data_df), ignore.case=T)]))
conc_names <- names(data_df)[grepl('mg', names(data_df), ignore.case=T)]
flag_names <- names(data_df)[grepl('flag', names(data_df), ignore.case=T)]


#Loop through each variable and combine concentration and load data
#Calculate yields using area and load
var_i = 1
data_df_new <- data_df
for (var_i in 1:length(names_list)){
  
  data_flag_i_unite <- data_flag_i <- '.dplyr'
  
  #find flags
  flag_names_i <- intersect(flag_names, names_list[[var_i]])
  
  if (length(flag_names_i)>0) {
  
  data_flag_i <- data_df %>%
    select(all_of(flag_names_i)) %>%
    mutate_all(as.character)
  
  data_flag_i_unite <- unite(data_flag_i, col=merge_var, 1:(length(flag_names_i)), na.rm=T, sep='| ')
  
  names(data_flag_i_unite)[1] <- as.character(flagvars[var_i])
  } else { 
    data_flag_i_unite <- data.frame(rep("", nrow(data_df)))
    names(data_flag_i_unite)[1] <- as.character(flagvars[var_i])
  }
  
  
  
  #Identify concentration names to merge/compare
  conc_names_i <- intersect(conc_names, names_list[[var_i]])
  good_conc_name <- concvars[var_i]
  
  data_conc_i <- data_df %>%
    select(all_of(conc_names_i)) %>%
    mutate_all(as.character)
  
  data_conc_i_unite <- unite(data_conc_i, col=merge_var, 1:(length(conc_names_i)), na.rm=T)
  
  names(data_conc_i_unite)[1] <- as.character(good_conc_name)

  data_conc_i_unite <- data_conc_i_unite %>%
    bind_cols(data.frame('site_conc' = data_df[,c('site')])) %>%
    bind_cols(data_flag_i_unite)
  
  data_conc_i_unite$used_conc_numeric <- as.numeric(gsub("<", "", data_conc_i_unite[,1]))
  
  #For values with less than symbol, assign a value of half of MDL
  Below_mdl_conc <- grepl("<", data_conc_i_unite[,3])
  data_conc_i_unite$used_conc_numeric[Below_mdl_conc] <- data_conc_i_unite$used_conc_numeric[Below_mdl_conc]/2


  names(data_conc_i_unite)[which(names(data_conc_i_unite)=='used_conc_numeric')] <- 
    paste0(good_conc_name, '_used')
  
  data_conc_i_unite[,1] <- as.numeric(gsub("<", "", data_conc_i_unite[,1]))

  # data_conc_i_unite <- data_conc_i_unite 
  # 
  # data_conc_i_unite$reported_conc_numeric_half <-   data_conc_i_unite$reported_conc_numeric / 2
  
  #For values with less than symbol, assign a value of half of MDL
  # Below_mdl_conc <- grepl("<", data_conc_i_unite[,1])
  # data_conc_i_unite[Below_mdl_conc,1] <- as.numeric(gsub("<", "", data_conc_i_unite[Below_mdl_conc,1]))/2

  
  #merge load
  load_names_i <- intersect(load_names, names_list[[var_i]])
  good_load_name <- loadvars[var_i]
  good_yield_name <- yieldvars[var_i]
  
  data_load_i <- data_df %>%
    select(all_of(load_names_i), Area_acres) %>%
    mutate_all(as.character) 
  
  data_load_i_unite <- unite(data_load_i, col=merge_var, 1:(length(load_names_i)), na.rm=T)
  
  names(data_load_i_unite)[1] <- as.character(good_load_name)
  
  data_load_i_unite <- mutate_all(data_load_i_unite, as.numeric) %>%
    mutate(yield = !!sym(good_load_name)/as.numeric(Area_acres)) %>%
    mutate(site_load = data_df$site) %>%
    select(-Area_acres)
  
  names(data_load_i_unite)[2] <- as.character(good_yield_name)
  
  
  # Ratio <- data_i_unite[,1] / data_i_unite[,2]
  # Diff <- data_i_unite[,1] - data_i_unite[,2]
  # 
  # plot(Diff, main=good_name)
  # plot(Ratio, main=good_name)
  
  #Compare calculated (from loads) with united column
  # plot_list[[var_i]] <- ggplot(data_i_unite, aes_string(x=names(data_i_unite)[1], y=names(data_i_unite)[2])) +
  #   geom_point(aes(col=site), alpha=.5, size=2) +
  #   geom_abline() +
  #   theme_bw() +
  #   scale_x_log10nice(name='merged concentrations') +
  #   scale_y_log10nice(name='calculated from load') +
  #   ggtitle(good_name)
  # 
  data_df_new <- data_df_new %>%
    select(-all_of(c(conc_names_i, load_names_i, flag_names_i))) %>%
    bind_cols(data_load_i_unite[,1:3]) %>%
    bind_cols(data_conc_i_unite[,1:4]) %>%
    select(-site_load, -site_conc) 
  
  
}

# head(data_df_new)
summary(data_df_new)

saveRDS(data_df_new, file=(file_out(file.path(path_to_data, "compiled_data", "storm_event_loads", "storm_event_loads_allsites_approved_data.rds" ))))

write.csv(data_df_new, file=(file_out(file.path(path_to_data, "compiled_data", "storm_event_loads", "storm_event_loads_allsites_approved_data.csv" ))), row.names=F)


# merged_sites <- data.frame(sites = as.character(unique(data_df_new$site)), stringsAsFactors = F)
# merged_sites <- arrange(merged_sites, sites)

site_summary_report <- data_df_new %>%
  filter(!is.na(storm_start), !is.na(storm_end)) %>%
  group_by(site, estimated) %>%
  dplyr::summarise(first_storm = as.Date(min(storm_start, na.rm=T)),
            last_storm = as.Date(max(storm_start, na.rm=T)),
            number_of_storms = n(),
            runoff_volume = round(sum(runoff_volume, na.rm=T),0)) %>%
  spread(key=estimated, value=runoff_volume) %>%
  rename(Volume_Estimated = '1',
         Volume_Measured = '0') 

site_summary_report$Number_Estimated <- ifelse(is.na(site_summary_report$Volume_Estimated), NA, 
                                               site_summary_report$number_of_storms)

site_summary_report$Number_Measured <- ifelse(is.na(site_summary_report$Volume_Measured), NA, 
                                               site_summary_report$number_of_storms)

site_summary_report2 <- site_summary_report %>%
  group_by(site) %>%
  summarize_at(vars(number_of_storms:Number_Measured), sum, na.rm=T) %>%
  mutate(fraction_volume = round(Volume_Measured / (Volume_Measured + Volume_Estimated),2),
         fraction_number = round(Number_Measured / (Number_Measured + Number_Estimated),2)) %>%
  left_join(select(site_summary_report, site, first_storm, last_storm) %>%
              group_by(site) %>%
              summarize(first_storm = min(first_storm, na.rm=T),
                        last_storm = max(last_storm, na.rm=T))) %>%
  select(site, first_storm, last_storm, number_of_storms, Number_Measured, Number_Estimated, fraction_number, Volume_Measured, Volume_Estimated, fraction_volume, everything())
  
print(data.frame(site_summary_report2))

if (nrow(site_summary_report2) != 21){
  warning(paste0(toString(nrow(site_summary_report)), " sites in dataset. Should be 21"))
  # print(merged_sites)
} else {
  print("21 sites in dataset. Great job!!!")
}


write.csv(site_summary_report2, file=(file_out(file.path(path_to_data, "compiled_data", "compiled_summary_report.csv" ))), row.names=F)


# #############################################
# Plot calculated versus reported concentration
# #############################################


#Calculate concentration using loads and runoff volume

conc_df <- data.frame(sapply(data_df_new[,loadvars], function (x) x/data_df_new$runoff_volume*453592/28.3168))
conc_df <- signif(conc_df, 4)
colnames(conc_df) <- paste0(concvars, '_calculated')

data_df_withconc <- bind_cols(data_df_new, conc_df) %>%
  filter(runoff_volume>0, is.finite(runoff_volume))

data_df_withconc <- bind_cols(data_df_new, conc_df)



calc_vars <- names(data_df_withconc)[grepl('_calculated', names(data_df_withconc))]
used_vars <- names(data_df_withconc)[grepl('_used', names(data_df_withconc))]


# Compare calculated (from loads) with united column
var_i <- 1
plot_list <- plot_list2 <- list()

for (var_i in 1:length(concvars)) {

  sig_digits <- ifelse (var_i == 1, 1, 
                        ifelse(var_i %in% c(2,10,11), 0.1,
                               ifelse(var_i %in% c(3,4,5,8,9), 0.01,
                                      ifelse(var_i %in% c(6,7), 0.001, NA)))) *5
  
  data_df_withconc_i <- data_df_withconc %>% 
    select(site, unique_storm_number, all_of(c(used_vars[var_i], 
                                               calc_vars[var_i], 
                                               flagvars[var_i]))) %>%
    mutate(ratio = data_df_withconc[,used_vars[var_i]] / data_df_withconc[,calc_vars[var_i]],
           diff = abs(data_df_withconc[,used_vars[var_i]] - data_df_withconc[,calc_vars[var_i]])) %>%
    mutate(group = case_when(ratio < 0.9 ~ 'low',
                             ratio > 1.1 ~ 'high',
                             ratio >= 0.9 & ratio <= 1.1 ~ 'good', 
                             is.na(ratio) ~ 'NA'),
           label = case_when(group == 'good' ~ "",
                             group %in% c('low', 'high', 'NA') ~ unique_storm_number)) %>%
  mutate(group2 = case_when(diff > sig_digits ~ 'high',
                           diff <= sig_digits ~ 'good',
                           is.na(diff) ~ 'NA'),
         label2 = case_when(group2 == 'good' ~ "",
                           group2 %in% c('high', 'NA') ~ unique_storm_number)) %>%
    mutate(flag_yes = ifelse(grepl('<', data_df_withconc[,flagvars[var_i]]), TRUE, FALSE)) #%>%
    # filter(site != 'MI-TL2')

 
    
  plot_list[[var_i]] <- ggplot(data_df_withconc_i, 
                               aes_string(x=used_vars[var_i], y=calc_vars[var_i])) + 
    facet_wrap(~site, scales='free', nrow = 3) +
    geom_point(aes(fill=site), alpha=.6, size=.5, shape=21, stroke=NA) +
    geom_text_repel(aes(label = label, colour=flag_yes),
                    alpha=.5, segment.size=.5, size=3, box.padding = 1) +
    scale_color_manual(values=c('black', 'red')) + 
    geom_abline() +
    theme_bw() +
    scale_x_log10nice(name='reported concentrations (if flag contains <, halfed value in conc column') +
    scale_y_log10nice(name='calculated from load') +
    ggtitle(concvars[var_i]) +
    theme(legend.position = 'none')
  
  # print(plot_list[[var_i]])
  
  ggsave(file.path(path_to_results, 'Figures', 'ConcentrationTesting', paste0(concvars[var_i], '.png')),
         plot_list[[var_i]], height=9, width=21, units='in')
  
  
  plot_list2[[var_i]] <- ggplot(data_df_withconc_i, aes(y=diff, x=site, group=site, fill=site)) +
    geom_hline(yintercept = sig_digits) + 
    geom_jitter(aes(color=site), width= .1, height=0, alpha=.4) + 
    # geom_boxplot(aes(group=site), alpha=.6, outlier.shape = NA) +
    scale_y_sqrt(limits=c(0,NA), name='abs difference between calculated and reported') +
    # scale_y_continuous(limits=c(0,5)) +
    geom_text_repel(aes(label = label2, color = site),
                    alpha=.5, segment.size=.5, size=3) +
    theme_bw() +
    theme(legend.position='none', axis.text.x = element_text(angle=90),
          axis.title.x = element_blank()) +
    ggtitle(concvars[var_i])
  
  # print(plot_list2[[var_i]])
  
  ggsave(file.path(path_to_results, 'Figures', 'ConcentrationTesting', paste0(concvars[var_i], '_Diff.png')),
         plot_list2[[var_i]], height=5, width=10, units='in')
  
  
}


# plot_list[[2]]


# end






# 
# ggplot(data_df_withconc, aes(y=total_phosphorus_conc_mgL, x=year(storm_start), group=factor(year(storm_start)), fill=factor(year(storm_start)))) +
#   geom_boxplot() +
#   facet_wrap(~site, scales='free_y') +
#   theme(legend.position='bottom', legend.title = element_blank()) +
#   scale_y_log10nice(name='TP (mg/L)') +
#   labs(x='Year')
# 
# 
# ggplot(data_df_withconc, aes(y=total_phosphorus_conc_mgL/total_phosphorus_conc_mgL_calculated, x=site, group=site, fill=site)) +
#   geom_jitter(aes(color=site), width= .1, height=0, alpha=.4) + 
#   geom_boxplot(aes(group=site), alpha=.6, outlier.shape = NA) +
#   # theme(legend.position='bottom') +
#   # scale_y_log10nice()
#   # scale_y_continuous(limits=c(0,5)) +
#   theme_bw()
# 
# ggplot(data_df_withconc, aes(y=abs(total_phosphorus_conc_mgL-total_phosphorus_conc_mgL_calculated), x=site, group=site, fill=site)) +
#   geom_jitter(aes(color=site), width= .1, height=0, alpha=.4) + 
#   # geom_boxplot(aes(group=site), alpha=.6, outlier.shape = NA) +
#   # theme(legend.position='bottom') +
#   scale_y_sqrt() +
#   # scale_y_continuous(limits=c(0,5)) +
#   theme_bw()
# 


# 
# #End
# 
# #Combine columns with the same name
# data_df_new <- data_df_withconc
# #TP
# var_names <- intersect(conc_names, TP_names)
# good_name <- var_names[which(var_names %in% concvars)]
# 
# data_test <- data_df_withconc %>%
#   select(var_names, -good_name) %>%
#   rowwise() %>%
#   rowMeans(na.rm=T)
# 
# plot_range <- range(c(data_test[is.finite(data_test)], data_df_withconc[,good_name][is.finite(data_df_withconc[,good_name])]), na.rm=T)
# 
# plot_range <- c(0,100)
# 
# plot(data_test, data_df_withconc[,good_name], xlab='new', ylab='old', xlim=plot_range, ylim=plot_range, pch=16, cex=1)
# abline(0,1)
# points(data_test, data_df_withconc[,var_names[1]], col='blue')
# points(data_test, data_df_withconc[,var_names[2]], col='red')
# points(data_test, data_df_withconc[,var_names[3]], col='orange', pch=5)
# 
# 
# data_df_withconc$TP_merged <- as.numeric(data_test)
# data_df_withconc$Ratio <- as.numeric(data_test)/data_df_withconc[,good_name]
# 
# data_df_withconc[which(data_df_withconc$Ratio >1.5 | data_df_withconc$Ratio <0.5),c('site', 'Ratio')]
# 
# data_df_new <- data_df_new %>%
#   dplyr::select(-var_names)
# data_df_new$tp_unfiltered_conc_mgL <- as.numeric(data_test)
# 
# 

# ggplot(data_df_withconc, aes(y=total_phosphorus_conc_mgL, fill=as.factor(year(storm_start)), group=as.factor(year(storm_start)))) + 
#   geom_boxplot() +
#   facet_wrap(~site, scales='free_y') +
#   theme(legend.position='none') +
#   scale_y_log10nice()
# 
# rm(data_test)
# 
# 
# 
# 
# #SRP
# var_names <- intersect(conc_names, SRP_names)
# 
# data_test <- data_df %>%
#   select(var_names) %>%
#   rowwise() %>%
#   rowMeans(na.rm=T)
# 
# plot(data_test, data_df[,var_names[1]], xlim=c(0, max(data_test, na.rm=T)), ylim=c(0, max(data_test, na.rm=T)), xlab='new', ylab='old', main='SRP')
# points(data_test, data_df[,var_names[2]], col='red')
# points(data_test, data_df[,var_names[3]], col='blue')
# 
# data_df_new <- data_df_new %>%
#   dplyr::select(-var_names)
# data_df_new$orthophosphate_conc_mgL <- as.numeric(data_test)
# 
# rm(data_test)
# 
# #TN
# var_names <- intersect(conc_names, TN_names)
# 
# data_test <- data_df %>%
#   select(var_names) %>%
#   rowwise() %>%
#   rowMeans(na.rm=T)
# 
# plot(data_test, data_df[,var_names[1]], xlim=c(0, max(data_test, na.rm=T)), ylim=c(0, max(data_test, na.rm=T)), xlab='new', ylab='old', main='TN')
# points(data_test, data_df[,var_names[2]], col='red')
# points(data_test, data_df[,var_names[3]], col='blue')
# 
# data_df_new <- data_df_new %>%
#   dplyr::select(-var_names)
# data_df_new$total_nitrogen_conc_mgL <- as.numeric(data_test)
# 
# rm(data_test)
# 
