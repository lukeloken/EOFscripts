

#Plot concentration and load data for all sites
#Group by water year
#This script uses

data_df3 <- readRDS(file=(file_in(file.path(path_to_data, "compiled_data", "storm_event_loads", "storm_event_conc_allsites_model.rds" ))))



#Create two r objects for frozen and non-frozen periods
data_df3_frozen <-filter(data_df3, frozen==TRUE)

data_df3_nonfrozen <-filter(data_df3, frozen==FALSE)


# What do you want to plot?
# choices are names(data_df3)

#Identify load variables
loadvars <- c('suspended_sediment_load_pounds', 
              'chloride_load_pounds',
              'no2_no3n_load_pounds', 
              'ammonium_n_load_pounds',
              'tkn_unfiltered_load_pounds', 
              'orthophosphate_load_pounds',
              'tp_unfiltered_load_pounds',
              'total_nitrogen_load_pounds',
              'organic_nitrogen_load_pounds',
              'toc_load_pounds',
              'doc_load_pounds')

#Identify concentration variables
concvars <- c('suspended_sediment_conc_mgL', 
              'chloride_conc_mgL',
              'no2_no3n_conc_mgL', 
              'ammonium_n_conc_mgL',
              'tkn_unfiltered_conc_mgL', 
              'orthophosphate_conc_mgL',
              'tp_unfiltered_conc_mgL',
              'total_nitrogen_conc_mgL',
              'organic_nitrogen_conc_mgL',
              'toc_conc_mgL',
              'doc_conc_mgL',
              'runoff_cubicmeter_percubicmeterWEQ')

#Identify rain variables
rainvars <-c("rain", "duration", "Ievent", "I5", "I10", "I15", "I30", "I60",
             "energy_m1", "erosivity_m1", "energy_m2", "erosivity_m2", "weq")


#Identify sites
EOF_sites <- unique(data_df3$site)

#########################################
# playing with plotting
#########################################

runoffHistorgrams <- ggplot(data=data_df3_nonfrozen, aes(x=sum_runoff, fill=state, alpha=type)) +
  scale_alpha_manual(values = c(1,.3)) + 
  geom_histogram(bins=20, col='grey20') + 
  facet_wrap(~site, nrow=4, scales='free') +
  # scale_x_log10nice() +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(expand=c(0,0)) + 
  theme_bw() +
  theme(legend.position='bottom') +
  labs(x='Total runoff (cf)')
  
print(runoffHistorgrams)

ggsave(file_out(file.path(path_to_results, "Figures", "RunoffVolume_histrograms_nonfrozen.png")), runoffHistorgrams, height=8, width=12, units = 'in', dpi=320)

runoffHistorgrams_postBMP <- ggplot(data=data_df3_nonfrozen[which(data_df3_nonfrozen$period=='after'),], aes(x=sum_runoff/1000, fill=state, alpha=type)) +
  scale_alpha_manual(values = c(1,.3)) + 
  geom_histogram(breaks=seq(0,250, 10), bins=20, col='grey20') + 
  facet_wrap(~site, nrow=4, scales='free') +
  # scale_x_log10nice() +
  scale_x_continuous(labels = scales::comma, limits=c(0,250)) +
  scale_y_continuous(expand=c(0,0)) + 
  theme_bw() +
  theme(legend.position='bottom') +
  labs(x='Total runoff (cf X 1000)') +
  geom_vline(xintercept = 10, col='darkblue', size=1, linetype='dotted') +
  ggtitle('Bin width = 10,000 cf')

pg <- ggplot_build(runoffHistorgrams_postBMP)

pg

print(runoffHistorgrams_postBMP)

ggsave(file_out(file.path(path_to_results, "Figures", "RunoffVolume_histrograms_10000_nonfrozen_postBMP.png")), runoffHistorgrams_postBMP, height=8, width=12, units = 'in', dpi=320)


runoffHistorgrams_postBMP <- ggplot(data=data_df3_nonfrozen[which(data_df3_nonfrozen$period=='after'),], aes(x=sum_runoff/1000, fill=state, alpha=type)) +
  scale_alpha_manual(values = c(1,.3)) + 
  geom_histogram(breaks=seq(0,100, 5), bins=20, col='grey20') + 
  facet_wrap(~site, nrow=4, scales='free') +
  # scale_x_log10nice() +
  scale_x_continuous(labels = scales::comma, limits=c(0,100)) +
  scale_y_continuous(expand=c(0,0)) + 
  theme_bw() +
  theme(legend.position='bottom') +
  labs(x='Total runoff (cf X 1000)') +
  geom_vline(xintercept = 10, col='darkblue', size=1, linetype='dotted') +
  ggtitle('Bin width = 5,000 cf')

# pg <- ggplot_build(runoffHistorgrams_postBMP)
# 
# pg

print(runoffHistorgrams_postBMP)

ggsave(file_out(file.path(path_to_results, "Figures", "RunoffVolume_histrograms_5000_nonfrozen_postBMP.png")), runoffHistorgrams_postBMP, height=8, width=12, units = 'in', dpi=320)


#WI runoff historgrams

runoffHistorgrams_postBMP_WI <- ggplot(data=data_df3_nonfrozen[which(data_df3_nonfrozen$period=='after' & data_df3_nonfrozen$state=='WI'),], aes(x=sum_runoff/1000, alpha=type)) +
  scale_alpha_manual(values = c(1,.3)) + 
  geom_histogram(breaks=seq(0,125, 5), bins=20, col='grey20') + 
  facet_wrap(~site, nrow=2, scales='free') +
  # scale_x_log10nice() +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(expand=c(0,0)) + 
  theme_bw() +
  theme(legend.position='none') +
  labs(x='Total runoff (cf X 1000)') +
  geom_vline(xintercept = 10, col='darkblue', size=1, linetype='dotted') +
  geom_vline(xintercept = 20, col='red', size=1, linetype='dotted') +
  ggtitle('Bin width = 5,000 cf')

print(runoffHistorgrams_postBMP_WI)

ggsave(file_out(file.path(path_to_results, "Figures", "RunoffVolume_histrograms_WI_nonfrozen_postBMP.png")), runoffHistorgrams_postBMP_WI, height=6, width=10, units = 'in', dpi=320)



#Indiana

runoffHistorgrams_postBMP_IN <- ggplot(data=data_df3_nonfrozen[which(data_df3_nonfrozen$period=='after' & data_df3_nonfrozen$state=='IN'),], aes(x=sum_runoff/1000, alpha=type)) +
  scale_alpha_manual(values = c(1,.3)) + 
  geom_histogram(breaks=seq(0,150, 5), bins=20, col='grey20') + 
  facet_wrap(~site, nrow=1, scales='free') +
  # scale_x_log10nice() +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(expand=c(0,0)) + 
  theme_bw() +
  theme(legend.position='none') +
  labs(x='Total runoff (cf X 1000)') +
  # geom_vline(xintercept = 10, col='darkblue', size=1, linetype='dotted') +
  geom_vline(xintercept = 10, col='red', size=1, linetype='dotted') +
  ggtitle('Bin width = 5,000 cf')

print(runoffHistorgrams_postBMP_IN)

ggsave(file_out(file.path(path_to_results, "Figures", "RunoffVolume_histrograms_IN_nonfrozen_postBMP.png")), runoffHistorgrams_postBMP_IN, height=3, width=6, units = 'in', dpi=320)



#Michigan

runoffHistorgrams_postBMP_MI <- ggplot(data=data_df3_nonfrozen[which(data_df3_nonfrozen$period=='after' & data_df3_nonfrozen$state=='MI' & data_df3_nonfrozen$site=='MI-SW2'),], aes(x=sum_runoff/1000, alpha=type)) +
  scale_alpha_manual(values = c(1,.3)) + 
  geom_histogram(breaks=seq(0,50, 1), bins=20, col='grey20') + 
  facet_wrap(~site, nrow=1, scales='free') +
  # scale_x_log10nice() +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(expand=c(0,0)) + 
  theme_bw() +
  theme(legend.position='none') +
  labs(x='Total runoff (cf X 1000)') +
  # geom_vline(xintercept = 10, col='darkblue', size=1, linetype='dotted') +
  geom_vline(xintercept = 5, col='red', size=1, linetype='dotted') +
  ggtitle('Bin width = 1,000 cf')

print(runoffHistorgrams_postBMP_MI)

ggsave(file_out(file.path(path_to_results, "Figures", "RunoffVolume_histrograms_MI_nonfrozen_postBMP.png")), runoffHistorgrams_postBMP_MI, height=3, width=3, units = 'in', dpi=320)

#Ohio

runoffHistorgrams_postBMP_OH <- ggplot(data=data_df3_nonfrozen[which(data_df3_nonfrozen$period=='after' & data_df3_nonfrozen$state=='OH'),], aes(x=sum_runoff/1000, alpha=type)) +
  scale_alpha_manual(values = c(1,.3)) + 
  geom_histogram(breaks=seq(0,90, 2), bins=20, col='grey20') + 
  facet_wrap(~site, nrow=1, scales='free') +
  # scale_x_log10nice() +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(expand=c(0,0)) + 
  theme_bw() +
  theme(legend.position='none') +
  labs(x='Total runoff (cf X 1000)') +
  # geom_vline(xintercept = 4, col='darkblue', size=1, linetype='dotted') +
  geom_vline(xintercept = 6, col='red', size=1, linetype='dotted') +
  ggtitle('Bin width = 2,000 cf')

print(runoffHistorgrams_postBMP_OH)

ggsave(file_out(file.path(path_to_results, "Figures", "RunoffVolume_histrograms_OH_nonfrozen_postBMP.png")), runoffHistorgrams_postBMP_OH, height=3, width=6, units = 'in', dpi=320)


#New York

runoffHistorgrams_postBMP_NY <- ggplot(data=data_df3_nonfrozen[which(data_df3_nonfrozen$period=='after' & data_df3_nonfrozen$state=='NY'),], aes(x=sum_runoff/1000, alpha=type)) +
  scale_alpha_manual(values = c(1,.3)) + 
  geom_histogram(breaks=seq(0,26, 2), bins=20, col='grey20') + 
  facet_wrap(~site, nrow=2, scales='free') +
  # scale_x_log10nice() +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(expand=c(0,0)) + 
  theme_bw() +
  theme(legend.position='none') +
  labs(x='Total runoff (cf X 1000)') +
  # geom_vline(xintercept = 4, col='darkblue', size=1, linetype='dotted') +
  geom_vline(xintercept = 2, col='red', size=1, linetype='dotted') +
  ggtitle('Bin width = 2,000 cf')

print(runoffHistorgrams_postBMP_NY)

ggsave(file_out(file.path(path_to_results, "Figures", "RunoffVolume_histrograms_NY_nonfrozen_postBMP.png")), runoffHistorgrams_postBMP_NY, height=6, width=10, units = 'in', dpi=320)

