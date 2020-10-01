#Check Sand County concentration and load data
library(lubridate)

project <- "SandCounty"

path_to_site <- "C:/Users/lloken/DOI/GS-UMid GLRI EOF - Data Publication"

#Site data with gage IDs
site_table <- read_excel(file.path(path_to_site, "EOF_Site_Table.xlsx")) %>%
  filter(!is.na(`USGS Site Number`))

names(site_table) <- gsub(" ", "", names(site_table))

site_table_i <- site_table %>%
  mutate(across(c(ApproxStartDate, ApproxEndDate), as.Date)) %>%
  filter(Project == project)


sand_files <- list.files(file.path(path_to_data, "Approved_Site_Data",
                                    "Sand County"), full.names = TRUE)

sand_files <- sand_files[-grepl('compiled', sand_files)]

sand_names <- list.files(file.path(path_to_data, "Approved_Site_Data",
                                   "Sand County"))
sand_names <-  sand_names[-grepl('compiled', sand_names)]

sand_names <- gsub(" storm event data.csv", "", sand_names)

file_nu <- 1
data_list <- list()
for (file_nu in 1:length(sand_files)){
  
  data_i <- read.csv(sand_files[file_nu], stringsAsFactors = FALSE) %>%
    mutate(across(contains("mg_"), function(x) as.numeric(gsub("<", "", x))),
           across(contains("pounds"), as.numeric),
           unique_storm_number = as.character(unique_storm_number)) %>%
    select(-starts_with("X")) %>%
    mutate(across(c("storm_start", "storm_end", "sample_start", "sample_end"),
                    ~as.POSIXct(.x,
                                tz = "America/Chicago",
                                format = c("%m/%d/%Y %H:%M"))))

  if ('storm_runoff' %in% names(data_i)){
    data_i <- dplyr::mutate(data_i, runoff_volume = storm_runoff) %>%
      select(-storm_runoff)
  
  }

  
  data_list[[file_nu]] <- data_i
  names(data_list)[file_nu] <- sand_names[file_nu]
  
}


    data_df <- data_list %>% 
      bind_rows(.id = "site") %>%
      mutate(site = gsub("WI-", "", site)) %>%
      select(site, everything()) %>%
      left_join(select(site_table_i, FieldName, Area), 
                by = c("site" = "FieldName"))
      
    head(data_df)
    
    summary(data_df)
    

    
    
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
                  'toc_load_pounds', 
                  "tkn_filtered_load_pounds",  
                  "tp_dissolved_filtered_load_pounds",
                  "total_dissolved_solids_load_pounds",    
                  "total_solids_load_pounds",     
                  "total_suspended_solids_load_pounds",   
                  "total_volatile_suspended_solids_load_pounds")
    
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
                  'toc_conc_mgL',    
                  "tkn_filtered_00623_mg_l",    
                  "tp_dissolved_filtered_00666_mg_l",
                  "total_dissolved_solids_70300_mg_l",   
                  "total_solids_00500_mg_l",   
                  "total_suspended_solids_00530_mg_l", 
                  "total_volatile_suspended_solids_00535_mg_l")
    
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
                   'toc_yield_pounds_per_acre',
                   "tkn_filtered_yield_pounds_per_acre",
                   "tp_dissolved_filtered_yield_pounds_per_acre",
                   "total_dissolved_solids_yield_pounds_per_acre",    
                   "total_solids_yield_pounds_per_acre",     
                   "total_suspended_solids_yield_pounds_per_acre",   
                   "total_volatile_suspended_solids_yield_pounds_per_acre")
    
    
    
    
    flagvars <- c("flag_suspended_sediment", "flag_chloride", "flag_no2_no3n", 
                  "flag_ammonium_n", "flag_tkn_unfiltered", "flag_orthophosphate",
                  "flag_tp_unfiltered", "flag_tn", "flag_orgN", "flag_doc", "flag_toc",
                  "flag_tkn_filtered",    
                  "flag_tp_dissolved_filtered",
                  "flag_total_dissolved_solids",   
                  "flag_total_solids",   
                  "flag_total_suspended_solids", 
                  "flag_total_volatile_suspended_solids")
    
    
    
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
    
    
    #Combine names into a list. The order of this list must match the good column names listed above
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
      

      #merge load
      load_names_i <- intersect(load_names, names_list[[var_i]])
      good_load_name <- loadvars[var_i]
      good_yield_name <- yieldvars[var_i]
      
      data_load_i <- data_df %>%
        select(all_of(load_names_i), Area) %>%
        mutate_all(as.character) 
      
      data_load_i_unite <- unite(data_load_i, col=merge_var, 1:(length(load_names_i)), na.rm=T)
      
      names(data_load_i_unite)[1] <- as.character(good_load_name)
      
      data_load_i_unite <- mutate_all(data_load_i_unite, as.numeric) %>%
        mutate(yield = !!sym(good_load_name)/as.numeric(Area)) %>%
        mutate(site_load = data_df$site) %>%
        select(-Area)
      
      names(data_load_i_unite)[2] <- paste0(as.character(good_yield_name), "_new")
      
      data_df_new <- data_df_new %>%
        select(-all_of(c(conc_names_i, load_names_i, flag_names_i))) %>%
        bind_cols(data_load_i_unite[,1:3]) %>%
        bind_cols(data_conc_i_unite[,1:4]) %>%
        select(-site_load, -site_conc) 
      
    }
    }
    
    # head(data_df_new)
        
    
    # ##############################
    #look at data
    # ##############################
    
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
    
    print(data.frame(site_summary_report2[,1:3]))
    print(data.frame(site_summary_report2))
    
    
    if (nrow(site_summary_report2) != length(sand_names)){
      warning(paste0(toString(nrow(site_summary_report2)), " sites in dataset. Should be ", toString(length(sand_names))))
      # print(merged_sites)
    } else {
      print("Correct number of sites in dataset. Great job!!!")
    }
    
    
    # write.csv(site_summary_report2, file=(file_out(file.path(path_to_data, "compiled_data", "compiled_summary_report.csv" ))), row.names=F)
    
    
    # #############################################
    # Plot calculated versus reported concentration
    # #############################################
    
    
    #Calculate concentration using loads and runoff volume
    included_vars <- which(loadvars %in%names(data_df_new))
    
    
    conc_df <- data.frame(sapply(data_df_new[,loadvars[included_vars]], function (x) x/data_df_new$runoff_volume*453592/28.3168))
    conc_df <- signif(conc_df, 4)
    colnames(conc_df) <- paste0(concvars[included_vars], '_calculated')
    
    data_df_withconc <- bind_cols(data_df_new, conc_df) %>%
      filter(runoff_volume>0, is.finite(runoff_volume))
    
    data_df_withconc <- bind_cols(data_df_new, conc_df)
    
    
    calc_vars <- paste0(concvars, "_calculated")
    used_vars <- paste0(concvars, "_used")
    yield_new <- paste0(yieldvars, "_new")   
    
    
    # Compare calculated (from loads) with united column
    var_i <- 1
    plot_list <- plot_list2 <-  plot_list3 <-list()
    
    for (var_i in 1:length(concvars)) {
      
      if(calc_vars[var_i] %in% names(data_df_withconc) & 
         used_vars[var_i] %in% names(data_df_withconc)) {
        
        sig_digits <- ifelse (var_i == 1, 1, 
                              ifelse(var_i %in% c(2,10,11), 0.1,
                                     ifelse(var_i %in% c(3,4,5,8,9), 0.01,
                                            ifelse(var_i %in% c(6,7), 0.001, NA)))) *5
        
        yield_org <- intersect(names(data_df_withconc), 
                               names_list[[var_i]][grepl("yield", names_list[[var_i]])])
        
        data_df_withconc_i <- data_df_withconc %>% 
          select(site, unique_storm_number, all_of(c(used_vars[var_i], 
                                                     calc_vars[var_i], 
                                                     yield_new[var_i],
                                                     yield_org,
                                                     flagvars[var_i]))) %>%
          mutate(ratio = data_df_withconc[,used_vars[var_i]] /
                   data_df_withconc[,calc_vars[var_i]],
                 ratio_yield = data_df_withconc[,yield_new[var_i]] /
                   data_df_withconc[,yield_org],
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
                                    group2 %in% c('high', 'NA') ~ unique_storm_number),
                 group3 = case_when(ratio_yield < 0.95 ~ 'low',
                                    ratio_yield > 1.05 ~ 'high',
                                    ratio_yield >= 0.95 & ratio <= 1.05 ~ 'good', 
                                    is.na(ratio) ~ 'NA'),
                 label3 = case_when(group3 == 'good' ~ "",
                                    group3 %in% c('low', 'high', 'NA') ~ unique_storm_number)
          ) %>%
          mutate(flag_yes = ifelse(grepl('<', data_df_withconc[,flagvars[var_i]]), TRUE, FALSE)) 
        
        mms.cor <- ddply(.data=data_df_withconc_i, 
                         .(site), 
                         summarize, 
                         n=paste(" n =", length(which(is.finite(diff)))))
        
        
      plot_list[[var_i]] <- ggplot(data_df_withconc_i, 
                                   aes_string(x=used_vars[var_i], y=calc_vars[var_i])) + 
        facet_wrap(~site, nrow = 1) +
        # facet_wrap(~site, scales='free', nrow = 4) +
        geom_point(aes(fill=site), alpha=.6, size=.5, shape=21, stroke=NA) +
        geom_text_repel(aes(label = label, colour=flag_yes),
                        alpha=.5, segment.size=.5, size=3, box.padding = 1) +
        scale_color_manual(values=c('black', 'red')) + 
        geom_abline() +
        theme_bw() +
        scale_x_log10nice(name='reported concentrations (if flag contains <, halfed value in conc column') +
        scale_y_log10nice(name='calculated from load') +
        ggtitle(concvars[var_i]) +
        theme(legend.position = 'none') +
        geom_text(data=mms.cor, aes(x=0, y=Inf, label=n), 
                  colour="black", inherit.aes=FALSE, parse=FALSE, hjust = 0, vjust = 1)
      
      print(plot_list[[var_i]])
      
      ggsave(file.path(path_to_results, 'Figures', 'ConcentrationTesting', 'SandCounty', paste0("SC_", concvars[var_i], '.png')),
             plot_list[[var_i]], height=4, width=10, units='in')
      
      
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
      
      ggsave(file.path(path_to_results, 'Figures', 'ConcentrationTesting', 'SandCounty', paste0('SC_', concvars[var_i], '_Diff.png')),
             plot_list2[[var_i]], height=5, width=10, units='in')
      
      
      #yields
      plot_list3[[var_i]] <- ggplot(data_df_withconc_i, 
                                    aes_string(y=yield_new[var_i], x=yield_org)) + 
        facet_wrap(~site, nrow = 1) +
        # facet_wrap(~site, scales='free', nrow = 4) +
        geom_point(aes(fill=site), alpha=.6, size=.5, shape=21, stroke=NA) +
        geom_text_repel(aes(label = label3),
                        alpha=.5, segment.size=.5, size=3, box.padding = 1) +
        # scale_color_manual(values=c('black', 'red')) + 
        geom_abline() +
        theme_bw() +
        scale_x_log10nice(name='Yield original') +
        scale_y_log10nice(name='Yield (calculated from load and area)') +
        ggtitle(yieldvars[var_i]) +
        theme(legend.position = 'none') +
        geom_text(data=mms.cor, aes(x=0, y=Inf, label=n), 
                  colour="black", inherit.aes=FALSE, parse=FALSE, hjust = 0, vjust = 1)
      
      print(plot_list3[[var_i]])
      
      ggsave(file.path(path_to_results, 'Figures', 'YieldTesting', 'SandCounty', paste0('Yields_SC_', concvars[var_i], '.png')),
             plot_list3[[var_i]], height=4, width=10, units='in')
      
    }
    }
    