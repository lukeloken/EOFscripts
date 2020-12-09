#Check Pioneer Farm concentration and load data
library(lubridate)

project <- "PioneerFarm"

# path_to_site <- "C:/Users/lloken/DOI/GS-UMid GLRI EOF - Data Publication"

#Site data with gage IDs
site_table_i <- site_table %>%
  # mutate(across(c(ApproxStartDate, ApproxEndDate), as.Date)) %>%
  filter(Project == project)


# head(data.frame(site_table_i))

# site_names <- site_table$USGS_Station_Number
# field_names  <- site_table$Field_Name 
# rain_sites <- site_table$USGS_Station_NumberforPrecipitation
# 

#Prepare files for reading
files <- list.files(file.path(path_to_data, "Approved_Site_Data",
                              "Pioneer Farm"), full.names = TRUE)

# files <- files[grepl('compiled', files)]

files <- files[!grepl('compiled', files)]
files <- files[!grepl('site.summary.report', files)]
files <- files[grepl('.csv', files)]

names <- gsub("P:/0301/Approved_Site_Data/Pioneer Farm/", "", files)
names <- gsub(" storm event data.csv", "", names)


file_nu <- 1
data_list <- list()
for (file_nu in 1:length(files)){
  
  data_i <- '.dplyr'
  
  data_i <- read.csv(files[file_nu], stringsAsFactors = FALSE) %>%
    mutate(across(contains("mg_"), function(x) as.numeric(gsub("<", "", x))),
           across(contains("pounds"), as.numeric),
           unique_storm_number = as.character(unique_storm_number)) %>%
    select(-starts_with("X")) %>%
    mutate(across(c("storm_start", "storm_end", "sample_start", "sample_end"),
                  ~as.POSIXct(.x,
                              tz = "America/Chicago", 
                              format = c("%m/%d/%Y %H:%M"))))   
  
  
  # if ('storm_runoff' %in% names(data_i)){
  #   data_i <- dplyr::mutate(data_i, runoff_volume = storm_runoff) %>%
  #     select(-storm_runoff)  
  # 
  # }
  
  
  data_list[[file_nu]] <- data_i %>%
    mutate(across(contains("flag"), as.character))
  
  names(data_list)[file_nu] <- names[file_nu]
  
  
}

data_df <- data_list %>%
  bind_rows(.id = "Field_Name") %>%
  mutate(site = sub("[^0-9.-]", "", site)) %>%
  left_join(select(site_table_i, USGS_Station_Number, Field_Name, Area),
            by = c("Field_Name", "site" = "USGS_Station_Number")) %>%
  select(site, Field_Name, everything())


# data_df <- data_list[[1]] %>%
#   mutate(site = gsub("'", "", site)) %>%
#   left_join(select(site_table_i, USGS_Station_Number, Field_Name, Area),
#             by = c("site" = "USGS_Station_Number"))

head(data_df)
names(data_df)
summary(data_df)
table(data_df$site)

filter(data_df, runoff_volume<0.05)

# #########################################################
# unite columns that have the same data, but different names
# #########################################################


#Identify multiple columns for each variable and data type
sediment_names <- names(data_df)[grepl('sediment', names(data_df), ignore.case=T)]
chloride_names <- names(data_df)[grepl('chloride', names(data_df), ignore.case=T)]
nitrate_names <- names(data_df)[grepl('no3', names(data_df), ignore.case=T)]
ammonium_names <- names(data_df)[grepl('Ammonium', names(data_df), ignore.case=T)]
TKN_names <- names(data_df)[grepl('tkn', names(data_df), ignore.case=T)]
TKN_filtered_names <- TKN_names[grepl("tkn_filtered", TKN_names)==TRUE]
TKN_names <- TKN_names[grepl("tkn_filtered", TKN_names)==FALSE]
SRP_names <- unique(c(names(data_df)[grepl('ortho', names(data_df), ignore.case=T)], 
                      names(data_df)[grepl('Reactive.Phosphorus', names(data_df), ignore.case=T)]))
TP_names <- unique(c(names(data_df)[grepl('tp', names(data_df), ignore.case=T)], 
                     names(data_df)[grepl('total_phosphorus', names(data_df), ignore.case=T)]))
TDP_names <- TP_names[grepl("dissolved", TP_names)==TRUE]
TP_names <- TP_names[grepl("dissolved", TP_names)==FALSE]
TN_names <- unique(c(names(data_df)[grepl('total_nitrogen', names(data_df), ignore.case=T)], 
                     names(data_df)[grepl('Total.Nitrogen', names(data_df), ignore.case=T)]))
organicN_names <-unique(c(names(data_df)[grepl('organic_nitrogen', names(data_df), ignore.case=T)], 
                          names(data_df)[grepl('organic.nitrogen', names(data_df), ignore.case=T)]))
DOC_names <- names(data_df)[grepl('doc', names(data_df), ignore.case=T)]
TOC_names <- names(data_df)[grepl('toc', names(data_df), ignore.case=T)]

volatile_names <- names(data_df)[grepl('volatile', names(data_df), ignore.case=T)]
total_solid_names <- names(data_df)[grepl('total_solid', names(data_df), ignore.case=T)]
total_dissolved_solids_names <- names(data_df)[grepl('total_dissolved_solids', 
                                                     names(data_df), ignore.case=T)]
total_suspended_solid_names <- names(data_df)[grepl('total_suspended_solid', 
                                                    names(data_df), ignore.case=T)]


#Combine names into a list. The order of this list must match the good column names created on the datapublication workflow script
names_list <- list(sediment_names, chloride_names, nitrate_names, 
                   ammonium_names, TKN_names, 
                   SRP_names, TP_names, TN_names, 
                   organicN_names, DOC_names, TOC_names, 
                   TKN_filtered_names, TDP_names,
                   total_dissolved_solids_names, total_solid_names,
                   total_suspended_solid_names, volatile_names)
#Load and concentration
load_names <- unique(c(names(data_df)[grepl('Load', names(data_df), ignore.case=T)],
                       names(data_df)[grepl('lound', names(data_df), ignore.case=T)]))
conc_names <- names(data_df)[grepl('mg', names(data_df), ignore.case=T)]
yield_names <- names(data_df)[grepl('yield', names(data_df), ignore.case=T)]
flag_names <- names(data_df)[grepl('flag', names(data_df), ignore.case=T)]


#Loop through each variable and combine concentration and load data
#Calculate yields using area and load
var_i = 1
data_df_new <- data_df
for (var_i in 1:length(names_list)){
  
  if (length(names_list[[var_i]])>0){
    
    data_flag_i_unite <- data_flag_i <- '.dplyr'
    
    #get list of names and good names
    conc_names_i <- intersect(conc_names, names_list[[var_i]])
    load_names_i <- intersect(load_names, names_list[[var_i]])
    flag_names_i <- intersect(flag_names, names_list[[var_i]])
    yield_names_i <- intersect(yield_names, names_list[[var_i]])
    
    good_conc_name <- concvars[var_i]
    good_load_name <- loadvars[var_i]
    good_yield_name <- yieldvars[var_i]
    good_flag_name <- flagvars[var_i]
    
    #find flags
    if (length(flag_names_i) > 0) {
      
      data_flag_i <- data_df %>%
        select(all_of(flag_names_i)) %>%
        mutate_all(as.character)
      
      data_flag_i_unite <- unite(data_flag_i, col=merge_var, 1:(length(flag_names_i)), na.rm=T, sep=' ') %>%
        mutate(merge_var = gsub(",", "", merge_var))
      names(data_flag_i_unite)[1] <- as.character(good_flag_name)
      
    } else { 
      data_flag_i_unite <- data.frame(rep("", nrow(data_df)))
      names(data_flag_i_unite)[1] <- as.character(good_flag_name)
    }
    
    
    # Merge concentrations and compute new loads
    data_conc_i <- data_df %>%
      select(all_of(conc_names_i)) %>%
      mutate_all(as.character)
    
    data_conc_i_unite <- unite(data_conc_i, col = merge_var, 1:(length(conc_names_i)), na.rm=T)
    
    names(data_conc_i_unite)[1] <- as.character(good_conc_name)
    
    data_conc_i_unite <- data_conc_i_unite %>%
      bind_cols(data_flag_i_unite) %>%
      bind_cols(select(data_df, runoff_volume, Area))
    
    data_conc_i_unite$used_conc_numeric <- as.numeric(gsub("<", "", data_conc_i_unite[,1]))
    
    #For values with less than symbol, assign a value of half of MDL
    Below_mdl_conc <- grepl("<", data_conc_i_unite[,good_flag_name])
    data_conc_i_unite$used_conc_numeric[Below_mdl_conc] <- data_conc_i_unite$used_conc_numeric[Below_mdl_conc]/2
    
    data_conc_i_unite[,1] <- as.numeric(gsub("<", "", data_conc_i_unite[,1]))
    
    #For organic n and total n replace values smaller than 0.50 with "<0.05"
    if (good_conc_name %in% c("total_nitrogen_conc_mgL", "organic_nitrogen_conc_mgL")) {
      
      data_conc_i_unite[,2][which(data_conc_i_unite[,1] < 0.05)] <- 
        paste0("< ", data_conc_i_unite[,2][which(data_conc_i_unite[,1] < 0.05)])
      data_conc_i_unite$used_conc_numeric[which(data_conc_i_unite[,1] < 0.05)] <- 0.025
      data_conc_i_unite[,1][which(data_conc_i_unite[,1] < 0.05)] <- 0.05
      
    }
    
    data_conc_i_unite <- data_conc_i_unite %>%
      mutate(new_load = used_conc_numeric * runoff_volume / 453592 * 28.3168,
             new_yield = new_load / Area) %>%
      select(-runoff_volume, -Area)
    
    names(data_conc_i_unite)[which(names(data_conc_i_unite)=='used_conc_numeric')] <- 
      paste0(good_conc_name, '_used')
    
    names(data_conc_i_unite)[which(names(data_conc_i_unite)=='new_load')] <- 
      paste0(good_load_name, '_new')
    
    names(data_conc_i_unite)[which(names(data_conc_i_unite)=='new_yield')] <- 
      paste0(good_yield_name, '_new')
    
    
    
    
    #merge original load
    data_load_i <- data_df %>%
      select(all_of(load_names_i)) %>%
      mutate_all(as.character) 
    
    data_load_i_unite <- unite(data_load_i, col=merge_var, 1:(length(load_names_i)), na.rm=T)
    
    
    data_load_i_unite <- mutate_all(data_load_i_unite, as.numeric) %>%
      bind_cols(select(data_df, runoff_volume)) %>%
      mutate(used_conc = merge_var / runoff_volume * 453592 / 28.3168) %>%
      select(-runoff_volume)
    
    names(data_load_i_unite)[1] <- as.character(paste0(good_load_name))
    names(data_load_i_unite)[2] <- as.character(paste0(good_conc_name, "_calculated"))
    
    #merge original yield
    data_yield_i <- data_df %>%
      select(all_of(yield_names_i)) %>%
      mutate_all(as.character) 
    
    data_yield_i_unite <- unite(data_yield_i, col=merge_var, 1:(length(yield_names_i)), na.rm=T)
    
    names(data_yield_i_unite)[1] <- as.character(paste0(good_yield_name))
    
    data_yield_i_unite <- mutate_all(data_yield_i_unite, as.numeric) 
    
    data_df_new <- data_df_new %>%
      select(-all_of(c(conc_names_i, load_names_i, flag_names_i, yield_names_i))) %>%
      bind_cols(data_conc_i_unite) %>%
      bind_cols(data_load_i_unite) %>%
      bind_cols(data_yield_i_unite) 
    
  }
}

# head(data_df_new)


# ##############################
#look at data
# ##############################

site_summary_report <- data_df_new %>%
  filter(!is.na(storm_start), !is.na(storm_end)) %>%
  group_by(site, Field_Name, estimated) %>%
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
  group_by(site, Field_Name) %>%
  summarize_at(vars(number_of_storms:Number_Measured), sum, na.rm=T) %>%
  mutate(fraction_volume = round(Volume_Measured / (Volume_Measured + Volume_Estimated),2),
         fraction_number = round(Number_Measured / (Number_Measured + Number_Estimated),2)) %>%
  left_join(select(site_summary_report, site, first_storm, last_storm) %>%
              group_by(site) %>%
              summarize(first_storm = min(first_storm, na.rm=T),
                        last_storm = max(last_storm, na.rm=T))) %>%
  select(site, Field_Name, first_storm, last_storm, number_of_storms, Number_Measured, Number_Estimated, fraction_number, Volume_Measured, Volume_Estimated, fraction_volume, everything()) %>%
  arrange(Field_Name)

print(data.frame(site_summary_report2[,1:4]))
print(data.frame(site_summary_report2))


if (nrow(site_summary_report2) != length(unique(data_df$site))){
  warning(paste0(toString(nrow(site_summary_report2)), " sites in dataset. Should be ", toString(length(unique(data_df$site)))))
  # print(merged_sites)
} else {
  print("Correct number of sites in dataset. Great job!!!")
}


#Save a compiled version and summary report in the data folder. 
# write.csv(data_df_compiled, file=(file.path(path_to_data,
#                                             "Approved_Site_Data",
#                                             "Sand County",
#                                             paste0(project, ".storm.event.data.compiled.csv" ))),
#           row.names=F)


write.csv(site_summary_report2, 
          file=(file.path(path_to_data,
                          "Approved_Site_Data",
                          "Pioneer Farm",
                          paste0(project, ".site.summary.report.csv"))),
          row.names=F)

# #############################################
# Plot calculated versus reported concentration
# #############################################

calc_vars <- paste0(concvars, "_calculated")
used_vars <- paste0(concvars, "_used")
yield_new <- paste0(yieldvars, "_new")   
load_new <- paste0(loadvars, "_new")   


# Compare calculated (from loads) with united column
var_i <- 1
plot_list <- plot_list2 <-  plot_list3 <- plot_list4 <- list()

for (var_i in 1:length(concvars)) {
  
  if(calc_vars[var_i] %in% names(data_df_new) & 
     used_vars[var_i] %in% names(data_df_new)) {
    
    sig_digits <- ifelse (var_i == 1, 1, 
                          ifelse(var_i %in% c(2,10,11), 0.1,
                                 ifelse(var_i %in% c(3,4,5,8,9), 0.01,
                                        ifelse(var_i %in% c(6,7), 0.001, NA)))) *5
    
    
    data_df_new_i <- data_df_new %>% 
      select(site, Field_Name, unique_storm_number, all_of(c(used_vars[var_i], 
                                                             calc_vars[var_i], 
                                                             yield_new[var_i],
                                                             yieldvars[var_i],
                                                             load_new[var_i], 
                                                             loadvars[var_i],
                                                             flagvars[var_i]))) %>%
      mutate(ratio = data_df_new[,used_vars[var_i]] /
               data_df_new[,calc_vars[var_i]],
             ratio_yield = data_df_new[,yield_new[var_i]] /
               data_df_new[,yieldvars[var_i]],
             ratio_load = data_df_new[,load_new[var_i]] /
               data_df_new[,loadvars[var_i]],
             diff = abs(data_df_new[,used_vars[var_i]] - data_df_new[,calc_vars[var_i]])) %>%
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
                                group2 %in% c('high', 'NA') ~ unique_storm_number),
             group3 = case_when(ratio_yield < 0.95 ~ 'low',
                                ratio_yield > 1.05 ~ 'high',
                                ratio_yield >= 0.95 & ratio <= 1.05 ~ 'good', 
                                is.na(ratio_yield) ~ 'NA'),
             label3 = case_when(group3 == 'good' ~ "",
                                group3 %in% c('low', 'high', 'NA') ~ unique_storm_number),
             group4 = case_when(ratio_load < 0.95 ~ 'low',
                                ratio_load > 1.05 ~ 'high',
                                ratio_load >= 0.95 & ratio <= 1.05 ~ 'good', 
                                is.na(ratio_load) ~ 'NA'),
             label4 = case_when(group4 == 'good' ~ "",
                                group4 %in% c('low', 'high', 'NA') ~ unique_storm_number)
      ) %>%
      mutate(flag_yes = ifelse(grepl('<', data_df_new[,flagvars[var_i]]), TRUE, FALSE)) 
    
    mms.cor <- ddply(.data=data_df_new_i, 
                     .(Field_Name), 
                     summarize, 
                     n=paste(" n =", length(which(is.finite(diff)))))
    
    
    plot_list[[var_i]] <- ggplot(data_df_new_i, 
                                 aes_string(x=used_vars[var_i], y=calc_vars[var_i])) + 
      facet_wrap(~Field_Name, ncol = round(length(names)/3)) +
      # facet_wrap(~site, scales='free', nrow = 4) +
      geom_point(aes(fill=Field_Name), alpha=.6, size=.5, shape=21, stroke=NA) +
      geom_text_repel(aes(label = label, colour=flag_yes),
                      alpha=.5, segment.size=.5, size=3, box.padding = 1) +
      scale_color_manual(values=c('black', 'red')) + 
      geom_abline() +
      theme_bw() +
      scale_x_log10(name='reported concentrations (if flag contains <, halfed value in conc column)') +
      scale_y_log10(name='calculated from load') +
      ggtitle(paste(Sys.Date(), concvars[var_i])) +
      theme(legend.position = 'none') +
      geom_text(data=mms.cor, aes(x=0, y=Inf, label=n), 
                colour="black", inherit.aes=FALSE, parse=FALSE, hjust = 0, vjust = 1)
    
    # print(plot_list[[var_i]])
    
    ggsave(file.path(path_to_results, 'Figures', 'ConcentrationTesting', 'PioneerFarm', paste0("PF_", concvars[var_i], '.png')),
           plot_list[[var_i]], height = round(length(names))/2, width = round(length(names))/1.2, units='in')
    
    
    plot_list2[[var_i]] <- ggplot(data_df_new_i, 
                                  aes(y=diff, x=Field_Name, 
                                      group=Field_Name, fill=Field_Name)) +
      geom_hline(yintercept = sig_digits) + 
      geom_jitter(aes(color=Field_Name), width= .1, height=0, alpha=.4) + 
      # geom_boxplot(aes(group=site), alpha=.6, outlier.shape = NA) +
      scale_y_sqrt(limits=c(0,NA), name='abs difference between calculated and reported') +
      # scale_y_continuous(limits=c(0,5)) +
      geom_text_repel(aes(label = label2, color = Field_Name),
                      alpha=.5, segment.size=.5, size=3) +
      theme_bw() +
      theme(legend.position='none', axis.text.x = element_text(angle=90),
            axis.title.x = element_blank()) +
      ggtitle(concvars[var_i])
    
    # print(plot_list2[[var_i]])
    
    ggsave(file.path(path_to_results, 'Figures', 'ConcentrationTesting', 'PioneerFarm', paste0('PF_', concvars[var_i], '_Diff.png')),
           plot_list2[[var_i]], height=4, width=6, units='in')
    
    #yields
    plot_list3[[var_i]] <- ggplot(data_df_new_i, 
                                  aes_string(y=yield_new[var_i], x=yieldvars[var_i])) + 
      facet_wrap(~Field_Name, ncol = round(length(names)/3)) +
      # facet_wrap(~site, scales='free', nrow = 4) +
      geom_point(aes(fill=Field_Name), alpha=.6, size=.5, shape=21, stroke=NA) +
      geom_text_repel(aes(label = label3),
                      alpha=.5, segment.size=.5, size=3, box.padding = 1) +
      # scale_color_manual(values=c('black', 'red')) + 
      geom_abline() +
      theme_bw() +
      scale_x_log10(name='Yield original') +
      scale_y_log10(name='Yield (calculated from conc, runoff, and area)') +
      ggtitle(yieldvars[var_i]) +
      theme(legend.position = 'none') +
      geom_text(data=mms.cor, aes(x=0, y=Inf, label=n), 
                colour="black", inherit.aes=FALSE, parse=FALSE, hjust = 0, vjust = 1)
    
    # print(plot_list3[[var_i]])
    
    ggsave(file.path(path_to_results, 'Figures', 'YieldTesting', 'PioneerFarm', paste0('PF_', yieldvars[var_i], '.png')),
           plot_list3[[var_i]], height = round(length(names))/2, width = round(length(names))/1.2, units='in')
    
    
    plot_list4[[var_i]] <- ggplot(data_df_new_i, 
                                  aes_string(x=loadvars[var_i], y=load_new[var_i])) + 
      facet_wrap(~Field_Name, ncol = round(length(names)/3)) +
      # facet_wrap(~site, scales='free', nrow = 4) +
      geom_point(aes(fill=Field_Name), alpha=.6, size=.5, shape=21, stroke=NA) +
      geom_text_repel(aes(label = label4, colour=flag_yes),
                      alpha=.5, segment.size=.5, size=3, box.padding = 1) +
      scale_color_manual(values=c('black', 'red')) + 
      geom_abline() +
      theme_bw() +
      scale_x_log10(name='reported load') +
      scale_y_log10(name='calculated from concentration') +
      ggtitle(paste(Sys.Date(), loadvars[var_i])) +
      theme(legend.position = 'none') +
      geom_text(data=mms.cor, aes(x=0, y=Inf, label=n),
                colour="black", inherit.aes=FALSE, parse=FALSE, hjust = 0, vjust = 1)
    
    # print(plot_list4[[var_i]])
    
    ggsave(file.path(path_to_results, 'Figures', 'ConcentrationTesting', 'PioneerFarm', paste0("PF_", loadvars[var_i], '.png')),
           plot_list4[[var_i]], height = round(length(names))/2, width = round(length(names))/1.2, units='in')
    
  }
}


data_df_approved <- data_df_new %>% 
  select(-contains('calculated'), -contains('used')) %>%
  select(-intersect(all_of(c(loadvars, yieldvars)),
                    names(data_df_new)))

names(data_df_approved) <- gsub("_new", 
                                "",
                                names(data_df_approved))


#Clean up approved df
data_df_approved2 <- data_df_approved %>%
  select(intersect(all_of(c(common_vars, approved_vars, loadvars, 
                            concvars, flagvars, yieldvars)),
                   names(data_df_approved))) %>%
  filter(exclude == 0) %>%
  mutate(project = project) %>%
  select(site, Field_Name, project, discrete, estimated, frozen, storm, 
         unique_storm_number, storm_start, storm_end, runoff_volume, peak_discharge,
         everything())  


# data_df_approved2$storm[is.na(data_df_approved2$storm)] <- 1


str(data_df_approved2)

saveRDS(data_df_approved2, file.path(path_to_data, "Data Publication", 
                                     "CleanedFinalVersions", 
                                     paste0(project, "_StormEventLoadsFormatted.rds")))
