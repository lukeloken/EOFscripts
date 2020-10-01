#Check Discovery Farms concentration and load data


disco_df <- read.csv(file.path(path_to_data, 'Discovery Farms Database', 'dfarms_database_for_checking.csv'), stringsAsFactors = F)

loadvars_disco <- c("Soil.lb.", "TP.lb.",   "DP.lb.", "PP.lb.",
                    "TN.lb.",   "OrgN.lb.", "NO3.lb.", "NH4.lb.", "TKN.lb.")

concvars_disco <-paste0(gsub('.lb', '', loadvars_disco), 'mgL') 
reported_disco <- gsub('.lb.', '', loadvars_disco)

disco_df <- disco_df %>%
  mutate(PP   = TP  - DOP,
         TN   = TKN + NO3,
         OrgN = TKN - NH3)  %>%
  rename(Soil = TSS,
         DP   = DOP,
         NH4  = NH3)




#Calculate concentration using loads and runoff volume

disco_con <- data.frame(sapply(disco_df[,loadvars_disco], function (x) x/disco_df$Runoff.ft3.*453592/28.3168))
disco_con  <- signif(disco_con , 4)
colnames(disco_con) <- paste0(concvars_disco, '.calculated')

# disco_df_withconc <- bind_cols(disco_df, disco_con) %>%
#   filter(Runoff.ft3.>0, is.finite(Runoff.ft3.))

disco_df_withconc <- bind_cols(disco_df, disco_con)



# saveRDS(data_df_new, file=(file_out(file.path(path_to_data, "compiled_data", "discovery_farms", "storm_event_loads_allsites_approved_data.rds" ))))
# 
# write.csv(data_df_new, file=(file_out(file.path(path_to_data, "compiled_data", "discovery_farms", "storm_event_loads_allsites_approved_data.csv" ))), row.names=F)

merged_sites <- data.frame(sites = as.character(unique(disco_df_withconc$Site)), stringsAsFactors = F)


if (nrow(merged_sites) > 0){
  message(paste0(toString(nrow(merged_sites)), " sites in dataset"))
  print(merged_sites)
} else {
  print("Zero sites. Something is wrong")
}


calc_vars <- names(disco_df_withconc)[grepl('calculated', names(disco_df_withconc))]
# used_vars <- names(disco_df_withconc)[grepl('_used', names(data_df_withconc))]


# Compare calculated (from loads) with united column
var_i <- 1
plot_list <- plot_list2 <- list()

for (var_i in 1:length(calc_vars)) {
  
  # sig_digits <- ifelse (var_i == 1, 1, 
  #                       ifelse(var_i %in% c(2,10,11), 0.1,
  #                              ifelse(var_i %in% c(3,4,5,8,9), 0.01,
  #                                     ifelse(var_i %in% c(6,7), 0.001, NA)))) *5
  
  data_df_withconc_i <- disco_df_withconc %>% 
    select(Site, StartDate, 
           all_of(c(reported_disco[var_i], calc_vars[var_i]))) %>%
    mutate(ratio = disco_df_withconc[,calc_vars[var_i]] / 
             disco_df_withconc[,reported_disco[var_i]],
           diff = abs(disco_df_withconc[,calc_vars[var_i]] - disco_df_withconc[,reported_disco[var_i]])) %>%
    mutate(group = case_when(ratio < 0.9 ~ 'low',
                             ratio > 1.1 ~ 'high',
                             ratio >= 0.9 & ratio <= 1.1 ~ 'good', 
                             is.na(ratio) ~ 'NA'),
           label = case_when(group == 'good' ~ "",
                             group %in% c('low', 'high', 'NA') ~ StartDate)) 
  # mutate(group2 = case_when(diff > sig_digits ~ 'high',
    #                           diff <= sig_digits ~ 'good',
    #                           is.na(diff) ~ 'NA'),
    #        label2 = case_when(group2 == 'good' ~ "",
    #                           group2 %in% c('high', 'NA') ~ StartDate))
  
  
  
  plot_list[[var_i]] <- ggplot(data_df_withconc_i, 
                               aes_string(x=reported_disco[var_i], y=calc_vars[var_i])) + 
    facet_wrap(~Site, scales='free') +
    geom_point(aes(fill=Site), alpha=.6, size=.5, shape=21, stroke=NA) +
    geom_text_repel(aes(label = label), colour='black',
                    alpha=.5, segment.size=.5, size=3, box.padding = 1) +
    scale_color_manual(values=c('black', 'red')) + 
    geom_abline() +
    theme_bw() +
    scale_x_log10nice(name='reported concentrations') +
    scale_y_log10nice(name='calculated from load') +
    ggtitle(concvars_disco[var_i]) +
    theme(legend.position = 'none')
  
  # print(plot_list[[var_i]])
  
  ggsave(file.path(path_to_data, 'Discovery Farms Database', 'Figures', 'ConcentrationTesting', paste0(concvars_disco[var_i], '.png')),
         plot_list[[var_i]], height=15, width=15, units='in')
  
  # 
  # plot_list2[[var_i]] <- ggplot(data_df_withconc_i, aes(y=diff, x=site, group=site, fill=site)) +
  #   geom_hline(yintercept = sig_digits) + 
  #   geom_jitter(aes(color=site), width= .1, height=0, alpha=.4) + 
  #   # geom_boxplot(aes(group=site), alpha=.6, outlier.shape = NA) +
  #   scale_y_sqrt(limits=c(0,NA), name='abs difference between calculated and reported') +
  #   # scale_y_continuous(limits=c(0,5)) +
  #   geom_text_repel(aes(label = label2, color = site),
  #                   alpha=.5, segment.size=.5, size=3) +
  #   theme_bw() +
  #   theme(legend.position='none', axis.text.x = element_text(angle=90),
  #         axis.title.x = element_blank()) +
  #   ggtitle(concvars[var_i])
  # 
  # print(plot_list2[[var_i]])
  # 
  # ggsave(file.path(path_to_results, 'Figures', 'DiscoveryFarms', paste0(concvars[var_i], '_Diff.png')),
  #        plot_list2[[var_i]], height=5, width=10, units='in')
  # 
  # 
}


plot_list[[2]]
