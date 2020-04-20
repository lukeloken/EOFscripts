
#Loop through all site folders and load edge of field runoff data

library(anytime)

#This is where the data live
print(path_to_data)

#Create empty list for all site data
allsites_list <-list()

#Identify how many files are in the approved_site_data folder
approved_files <- list.files(file.path(path_to_data, "field_analysis", "approved_site_data"))
approved_files <- approved_files[grepl(".csv", approved_files)]
approved_files <- approved_files[which(grepl("Management", approved_files)==FALSE)]
approved_files <- approved_files[which(grepl("_orig", approved_files)==FALSE)]
approved_files <- approved_files[which(grepl("NYTL", approved_files)==FALSE)]
approved_files <- approved_files[which(grepl("paired", approved_files)==FALSE)]



file_nu =1
for (file_nu in 1:length(approved_files)){
  file <- approved_files[file_nu]
  path_to_sitefile <- file.path(path_to_data, "field_analysis", "approved_site_data", file)
  
  #Find site name and timezone  for the site
  
  file_site_list <- unlist(strsplit(file, "-"))
  state <- substr(file_site_list[1], nchar(file_site_list[1])-1, nchar(file_site_list[1]))
  site <- substr(file_site_list[2], 1,3)
  full_site <- paste(state, site, sep='-')
  
  if (state %in% c('WI')){
    state_tz <- 'America/Chicago'
  } else if (state %in% c('OH', 'NY', 'IN', 'MI')){
    state_tz <- 'America/New_York'
  }
  
  #load and combine all model (mod) files
  data_i <- read.csv(path_to_sitefile, stringsAsFactors = F, header=T) %>%
    mutate(sample_start = anytime(sample_start, tz=state_tz),
           sample_end = anytime(sample_end, tz=state_tz),
           storm_start =anytime(storm_start, tz=state_tz),
           storm_end = anytime(storm_end, tz=state_tz)) %>%
    arrange(storm_start) %>%
    # select(-file_id) %>%
    distinct()
  
  #remove rows that are entirely NA
  data_i[as.logical((rowSums(is.na(data_i))-ncol(data_i))),]
  
  if ('storm_runoff_cubic_feet' %in% names(data_i)){
    data_i <- dplyr::mutate(data_i, runoff_volume = storm_runoff_cubic_feet) %>%
      select(-storm_runoff_cubic_feet)
    
  }
  
  data_i <- mutate(data_i, runoff_volume = as.numeric(runoff_volume))
  
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
data_df <- ldply(allsites_list, data.frame, .id = "site") 

goodvars <- c('suspended_sediment_load_pounds', 
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

goodcolumns <- which(names(data_df) %in% goodvars)

loadcolumns <- which(grepl("load", names(data_df), ignore.case=T))
badcolumns <- setdiff(loadcolumns, goodcolumns)
badnames<- names(data_df)[badcolumns]

#Combine columns that are runoff
# runoffcolumns <- which(grepl("runoff", names(data_df), ignore.case=T))
# names(data_df)[runoffcolumns]
# 
# data_df$runoff_volume[which(is.na(data_df$runoff_volume))] <- data_df$storm_runoff_cubic_feet[which(is.na(data_df$runoff_volume))]

# head(data_df)
# str(data_df)



if (identical(badnames, c("no2_no3_n_load_pounds", "total_phosphorus_unfiltered_load_pounds",
                          "total_nitrogen_computed_load_pounds", "organic_nitrogen_computed_load_pounds",
                          "Suspended.Sediment.Load..pounds", "Chloride.Load..pounds",
                          "NO2.NO3.N..Load..pounds", "Ammonium..N..Load..pounds",
                          "TKN.Unfiltered.Load..pounds", "Dissolved.Reactive.Phosphorus.Load..pounds",
                          "TP.Unfiltered.Load..pounds", "Total.Nitrogen.Load..in.pounds",
                          "Organic.Nitrogen.Load..pounds", "total_nitrogen_load_in_pounds")
)==FALSE) {
  
  stop("check column names in '1_load_all_siteapproved_data.R'. Trying to merge columns, and more columns need to be identified")
  
} else {
  
  
  replacenames <- c("no2_no3n_load_pounds", "tp_unfiltered_load_pounds", 
                    "total_nitrogen_load_pounds", "organic_nitrogen_load_pounds", 
                    "suspended_sediment_load_pounds", "chloride_load_pounds",
                    "no2_no3n_load_pounds", "ammonium_n_load_pounds",
                    "tkn_unfiltered_load_pounds", "orthophosphate_load_pounds",
                    "tp_unfiltered_load_pounds", "total_nitrogen_load_pounds",
                    "organic_nitrogen_load_pounds", "total_nitrogen_load_pounds")
  
  
  var=1
  for (var in 1:length(badnames)){
    NAs <- which(is.na(data_df[,replacenames[var]]))
    data_df[,replacenames[var]][NAs] <- data_df[,badnames[var]][NAs]
    # print(paste0("Replaced ", toString(length(NAs)), " NAs in col ", toString(loadvars[var]), " with col ", toString(badvars[var])))
    print(paste0("Column ", toString(replacenames[var]), " contained NAs. Replaced with ", toString(length(NAs)), " ",  toString(badnames[var])))
  }
  
  data_df <- data_df %>%
    dplyr::select(-badnames)
  
  #Calculate concentration using loads and runoff volume
  
  conc_df <- data.frame(sapply(data_df[,goodvars[-12]], function (x) x/data_df$runoff_volume*453592/28.3168))
  conc_df <- signif(conc_df, 4)
  colnames(conc_df) <- concvars
  
  # data_df_withconc <- bind_cols(data_df, conc_df) %>%
  #   filter(runoff_volume>0, is.finite(runoff_volume))
  
  data_df_withconc <- bind_cols(data_df, conc_df)
  
  
  #Manually identify multiple columns for each variable and unite
  sediment_names <- names(data_df_withconc)[grepl('sediment', names(data_df_withconc), ignore.case=T)]
  ammonium_names <- names(data_df_withconc)[grepl('Ammonium', names(data_df_withconc), ignore.case=T)]
  nitrate_names <- names(data_df_withconc)[grepl('no3', names(data_df_withconc), ignore.case=T)]
  TKN_names <- names(data_df_withconc)[grepl('tkn', names(data_df_withconc), ignore.case=T)]
  organicN_names <-unique(c(names(data_df_withconc)[grepl('organic_nitrogen', names(data_df_withconc), ignore.case=T)], 
                            names(data_df_withconc)[grepl('organic.nitrogen', names(data_df_withconc), ignore.case=T)]))
  TN_names <- unique(c(names(data_df_withconc)[grepl('total_nitrogen', names(data_df_withconc), ignore.case=T)], 
                       names(data_df_withconc)[grepl('Total.Nitrogen', names(data_df_withconc), ignore.case=T)]))
  TP_names <- unique(c(names(data_df_withconc)[grepl('tp', names(data_df_withconc), ignore.case=T)], 
                       names(data_df_withconc)[grepl('total_phosphorus', names(data_df_withconc), ignore.case=T)]))
  SRP_names <- unique(c(names(data_df_withconc)[grepl('ortho', names(data_df_withconc), ignore.case=T)], 
                        names(data_df_withconc)[grepl('Reactive.Phosphorus', names(data_df_withconc), ignore.case=T)]))
  chloride_names <- names(data_df_withconc)[grepl('chloride', names(data_df_withconc), ignore.case=T)]
  DOC_names <- names(data_df_withconc)[grepl('doc', names(data_df_withconc), ignore.case=T)]
  TOC_names <- names(data_df_withconc)[grepl('toc', names(data_df_withconc), ignore.case=T)]
  
  conc_names_list <- list(sediment_names, ammonium_names, nitrate_names, TKN_names, organicN_names, 
                          TN_names, TP_names, SRP_names, chloride_names, DOC_names, TOC_names)
  
  load_names <- names(data_df_withconc)[grepl('Load', names(data_df_withconc), ignore.case=T)]
  conc_names <- names(data_df_withconc)[grepl('mg', names(data_df_withconc), ignore.case=T)]
  
  
  #Loop through each variable and combine/compare concentration data
  var_i = 1
  data_df_new <- data_df_withconc
  plot_list <- list()
  for (var_i in 1:length(conc_names_list)){
    
    #Identify names to merge/compare
    var_names <- intersect(conc_names, conc_names_list[[var_i]])
    good_name <- var_names[which(var_names %in% concvars)]
    
    data_i <- data_df_withconc %>%
      select(var_names, -good_name) %>%
      mutate_all(as.character) %>%
      bind_cols(data_df_withconc[good_name])
    
    data_i_unite <- unite(data_i, col=merge_var, 1:(length(var_names)-1), na.rm=T)
    
    names(data_i_unite)[1] <- as.character(paste0(good_name, '_unite'))
    
    #For values with less than symbol, assign a value of half of MDL
    Below_mdl <- grepl("<", data_i_unite[,1])
    data_i_unite[Below_mdl,1] <- as.numeric(gsub("<", "", data_i_unite[Below_mdl,1]))/2
    
    data_i_unite <- mutate_all(data_i_unite, as.numeric) %>%
      mutate(site = data_df_withconc$site)
    
    # Ratio <- data_i_unite[,1] / data_i_unite[,2]
    # Diff <- data_i_unite[,1] - data_i_unite[,2]
    # 
    # plot(Diff, main=good_name)
    # plot(Ratio, main=good_name)
    
    #Compare calculated (from loads) with united column
    plot_list[[var_i]] <- ggplot(data_i_unite, aes_string(x=names(data_i_unite)[1], y=names(data_i_unite)[2])) +
      geom_point(aes(col=site), alpha=.5, size=2) +
      geom_abline() +
      theme_bw() +
      scale_x_log10nice(name='merged concentrations') +
      scale_y_log10nice(name='calculated from load') +
      ggtitle(good_name)
    
    data_df_new <- data_df_new %>%
      select(-var_names) %>%
      bind_cols(data_i_unite[,1:2])
    
  }
  
  
  
  saveRDS(data_df_new, file=(file_out(file.path(path_to_data, "compiled_data", "storm_event_loads", "storm_event_loads_allsites_approved_data.rds" ))))
  
  write.csv(data_df_new, file=(file_out(file.path(path_to_data, "compiled_data", "storm_event_loads", "storm_event_loads_allsites_approved_data.csv" ))), row.names=F)
  
  merged_sites <- data.frame(sites = as.character(unique(data_df_new$site)), stringsAsFactors = F)
  merged_sites <- arrange(merged_sites, sites)
  
}

if (nrow(merged_sites) != 20){
  warning(paste0(toString(nrow(merged_sites)), " sites in dataset. Should be 20"))
  print(merged_sites)
} else {
  print("20 sites in dataset. Great job!!!")
}



plot_list

ggplot(data_df_new, aes(y=total_phosphorus_conc_mgL, x=year(storm_start), group=factor(year(storm_start)), fill=factor(year(storm_start)))) +
  geom_boxplot() +
  facet_wrap(~site, scales='free_y') +
  theme(legend.position='bottom', legend.title = element_blank()) +
  scale_y_log10nice() +
  labs(x='Year')

                                                                                                                ggplot(data_df_new, aes(y=total_phosphorus_conc_mgL/total_phosphorus_conc_mgL_unite, x=site, group=site, fill=site)) +
  geom_jitter(aes(color=site), width= .1, height=0, alpha=.4) + 
  geom_boxplot(aes(group=site), alpha=.6, outlier.shape = NA) +
  # theme(legend.position='bottom') +
  # scale_y_log10nice()
  scale_y_continuous(limits=c(0,5)) +
  theme_bw()



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
