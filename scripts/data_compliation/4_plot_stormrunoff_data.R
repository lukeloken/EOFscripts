

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

choice_yields <-  yieldperweqvars[c(1:9,12)]
choice_conc <- concvars[1:9]


#########################################
# playing with plotting
#########################################


LoadByRunnoff_plotlist <- list()

var_i <- 11
for (var_i in 1:length(loadvars)){
  
  LoadByRunnoff_plotlist[[var_i]] <- ggplot(data=data_df3, aes_string(x="sum_runoff", y=loadvars[var_i], group="wateryear", color="wateryear", fill="wateryear")) +
    scale_x_log10() +
    scale_y_log10() +
    geom_point(aes(shape=frozen), size=2) +
    scale_shape_manual(values=c(16, 1))+
    stat_smooth(method = "lm", se=T, alpha=.1) +
    facet_wrap(~site, scales='free') +
    theme_bw() +
    theme(legend.position = 'bottom') +
    guides(color = guide_legend(nrow = 1)) +
    labs(x = "Runoff volume (cubic feet per storm)")
  
  print(LoadByRunnoff_plotlist[[var_i]])
  
  ggsave(file_out(file.path(path_to_results, "Figures", "Loads", paste0(loadvars[var_i], "ByRunoff_plot.png"))), LoadByRunnoff_plotlist[[var_i]], height=8, width=12, units = 'in', dpi=320)
  
}


ConcByRunnoff_plotlist<-list()
var_i <- 1
for (var_i in 1:length(concvars)){
  
  ConcByRunnoff_plotlist[[var_i]] <- ggplot(data=data_df3, aes_string(x="sum_runoff", y=concvars[var_i], group="wateryear", color="wateryear", fill="wateryear")) +
    scale_x_log10() +
    # scale_y_log10() +
    geom_point(aes(shape=frozen), size=2) +
    scale_shape_manual(values=c(16, 1))+
    stat_smooth(method = "lm", se=T, alpha=.1) +
    facet_wrap(~site, scales='free') +
    theme_bw() +
    theme(legend.position = 'bottom') +
    guides(color = guide_legend(nrow = 1)) +
    labs(x = "Runoff volume (cubic feet per storm)")
  
  print(ConcByRunnoff_plotlist[[var_i]])
  
  ggsave(file_out(file.path(path_to_results, "Figures", "Conc", paste0(concvars[var_i], "ByRunoff_plot.png"))), ConcByRunnoff_plotlist[[var_i]], height=8, width=12, units = 'in', dpi=320)
  
}


ConcByRunnoff_log_plotlist<-list()
var_i <- 1
for (var_i in 1:length(concvars)){
  
  ConcByRunnoff_log_plotlist[[var_i]] <- ggplot(data=data_df3, aes_string(x="sum_runoff", y=concvars[var_i], group="wateryear", color="wateryear", fill="wateryear")) +
    scale_x_log10() +
    scale_y_log10() +
    geom_point(aes(shape=frozen), size=2) +
    scale_shape_manual(values=c(16, 1))+
    stat_smooth(method = "lm", se=T, alpha=.1) +
    facet_wrap(~site, scales='free') +
    theme_bw() +
    theme(legend.position = 'bottom') +
    guides(color = guide_legend(nrow = 1)) +
    labs(x = "Runoff volume (cubic feet per storm)")
  
  print(ConcByRunnoff_log_plotlist[[var_i]])
  
  ggsave(file_out(file.path(path_to_results, "Figures", "Conc", paste0(concvars[var_i], "ByRunoff_log_plot.png"))), ConcByRunnoff_log_plotlist[[var_i]], height=8, width=12, units = 'in', dpi=320)
  
}

RainByRunnoff_plotlist <- list()

var_i <- 1
for (var_i in 1:length(rainvars)){
  
  RainByRunnoff_plotlist[[var_i]] <- ggplot(data=data_df3, aes_string(x="sum_runoff", y=rainvars[var_i], group="wateryear", color="wateryear", fill="wateryear")) +
    scale_x_log10() +
    scale_y_log10() +
    geom_point(aes(shape=frozen), size=2) +
    scale_shape_manual(values=c(16, 1))+
    stat_smooth(method = "lm", se=T, alpha=.1) +
    facet_wrap(~site, scales='free') +
    theme_bw() +
    theme(legend.position = 'bottom') +
    guides(color = guide_legend(nrow = 1)) +
    labs(x = "Runoff volume (cubic feet per storm)")
  
  print(RainByRunnoff_plotlist[[var_i]])
  
  ggsave(file_out(file.path(path_to_results, "Figures", "Rain", paste0(rainvars[var_i], "ByRunoff_plot.png"))), RainByRunnoff_plotlist[[var_i]], height=8, width=12, units = 'in', dpi=320)
  
}


RainByRunnoff_plotlist_v2 <- list()

var_i <- 13
for (var_i in 1:length(rainvars)){
  
  RainByRunnoff_plotlist_v2[[var_i]] <- ggplot(data=data_df3[which(data_df3$rain <5 & data_df3$frozen ==FALSE),], aes_string(y="sum_runoff", x=rainvars[var_i], group="wateryear", color="wateryear", fill="wateryear")) +
    # scale_x_log10() +
    # scale_y_log10() +
    geom_point(aes(shape=frozen), size=2) +
    scale_shape_manual(values=c(16, 1))+
    stat_smooth(method = "lm", se=F, alpha=.1) +
    facet_wrap(~site, scales='free') +
    theme_bw() +
    theme(legend.position = 'bottom') +
    guides(color = guide_legend(nrow = 1)) +
    labs(y = "Runoff volume (cubic feet per storm)")
  
  print(RainByRunnoff_plotlist_v2[[var_i]])
  
  ggsave(file_out(file.path(path_to_results, "Figures", "Rain", paste0(rainvars[var_i], "ByRunoff_plot_v2.png"))), RainByRunnoff_plotlist_v2[[var_i]], height=8, width=12, units = 'in', dpi=320)
  
}

RainByRunnoffIndex <- ggplot(data=data_df3[which(data_df3$rain <5 & data_df3$frozen ==FALSE & data_df3$runoff_cubicmeter_percubicmeterWEQ <1 ),], aes_string(y="runoff_cubicmeter_percubicmeterWEQ", x="weq", group="wateryear", color="wateryear", fill="wateryear")) +
  # scale_x_log10() +
  # scale_y_log10() +
  geom_point(aes(shape=frozen), size=2) +
  scale_shape_manual(values=c(16, 1))+
  stat_smooth(method = "lm", se=F, alpha=.1) +
  facet_wrap(~site, scales='free') +
  theme_bw() +
  theme(legend.position = 'bottom') +
  guides(color = guide_legend(nrow = 1)) +
  labs(y = "Runoff index")

print(RainByRunnoffIndex)


#############
# Boxplots
##############



ConcByYear_boxlist<-list()
var_i <- 1
for (var_i in 1:length(concvars)){
  
  ConcByYear_boxlist[[var_i]] <- ggplot(data=data_df3, aes_string(x="wateryear", y=concvars[var_i], group="wateryear", color="wateryear", fill="wateryear")) +
    scale_y_log10() +
    geom_jitter(width = .1, size=1, alpha=.5, shape=16) + 
    geom_boxplot(alpha=0.2, outlier.shape = NA) +
    # scale_shape_manual(values=c(16, 1))+
    # stat_smooth(method = "lm", se=T, alpha=.1) +
    facet_wrap(~site, scales='free_y') +
    theme_bw() +
    theme(legend.position = 'bottom') +
    guides(color = guide_legend(nrow = 1)) +
    labs(x = "Water year") +
    theme(axis.text=element_text(size=8)) +
    ggtitle('All runoff events') +
    theme(plot.title = element_text(hjust = 0.5))
  
  print(ConcByYear_boxlist[[var_i]])
  
  ggsave(file_out(file.path(path_to_results, "Figures", "Boxplots", paste0(concvars[var_i], "ByYear_boxplot.png"))), ConcByYear_boxlist[[var_i]], height=8, width=12, units = 'in', dpi=320)
  
}



ConcByYear_nonFrozen_boxlist<-list()
var_i <- 1
for (var_i in 1:length(concvars)){
  
  ConcByYear_nonFrozen_boxlist[[var_i]] <- ggplot(data=data_df3_nonfrozen, aes_string(x="wateryear", y=concvars[var_i], group="wateryear", color="wateryear", fill="wateryear")) +
    scale_y_log10() +
    geom_jitter(width = .1, size=1, alpha=.5, shape=16) + 
    geom_boxplot(alpha=0.2, outlier.shape = NA) +
    # scale_shape_manual(values=c(16, 1))+
    # stat_smooth(method = "lm", se=T, alpha=.1) +
    facet_wrap(~site, scales='free_y') +
    theme_bw() +
    theme(legend.position = 'bottom') +
    guides(color = guide_legend(nrow = 1)) +
    labs(x = "Water year") +
    theme(axis.text=element_text(size=8)) +
    ggtitle('Non-frozen runoff events') +
    theme(plot.title = element_text(hjust = 0.5))
  
  print(ConcByYear_nonFrozen_boxlist[[var_i]])
  
  ggsave(file_out(file.path(path_to_results, "Figures", "Boxplots", paste0(concvars[var_i], "ByYear_nonFrozen_boxplot.png"))), ConcByYear_nonFrozen_boxlist[[var_i]], height=8, width=12, units = 'in', dpi=320)
  
}



ConcByYear_Frozen_boxlist<-list()
var_i <- 1
for (var_i in 1:length(concvars)){
  
  ConcByYear_Frozen_boxlist[[var_i]] <- ggplot(data=data_df3_frozen, aes_string(x="wateryear", y=concvars[var_i], group="wateryear", color="wateryear", fill="wateryear")) +
    scale_y_log10() +
    geom_jitter(width = .1, size=1, alpha=.5, shape=16) + 
    geom_boxplot(alpha=0.2, outlier.shape = NA) +
    # scale_shape_manual(values=c(16, 1))+
    # stat_smooth(method = "lm", se=T, alpha=.1) +
    facet_wrap(~site, scales='free_y') +
    theme_bw() +
    theme(legend.position = 'bottom') +
    guides(color = guide_legend(nrow = 1)) +
    labs(x = "Water year") +
    theme(axis.text=element_text(size=8)) +
    ggtitle('Frozen runoff events') +
    theme(plot.title = element_text(hjust = 0.5))
  
  print(ConcByYear_Frozen_boxlist[[var_i]])
  
  ggsave(file_out(file.path(path_to_results, "Figures", "Boxplots", paste0(concvars[var_i], "ByYear_Frozen_boxplot.png"))), ConcByYear_Frozen_boxlist[[var_i]], height=8, width=12, units = 'in', dpi=320)
  
}



#Rain
RainByYear_boxlist<-list()
var_i <- 1
for (var_i in 1:length(rainvars)){
  
  
  RainByYear_boxlist[[var_i]] <- ggplot(data=data_df3, aes_string(x="wateryear", y=rainvars[var_i], group="wateryear", color="wateryear", fill="wateryear")) +
    # scale_y_log10() +
    geom_jitter(width = .1, size=1, alpha=.5, shape=16) + 
    geom_boxplot(alpha=0.2, outlier.shape = NA) +
    # scale_shape_manual(values=c(16, 1))+
    # stat_smooth(method = "lm", se=T, alpha=.1) +
    facet_wrap(~site, scales='free_y') +
    theme_bw() +
    theme(legend.position = 'bottom') +
    guides(color = guide_legend(nrow = 1)) +
    labs(x = "Water year") +
    theme(axis.text=element_text(size=8)) +
    ggtitle('All runoff events') +
    theme(plot.title = element_text(hjust = 0.5))
  
  print(RainByYear_boxlist[[var_i]])
  
  ggsave(file_out(file.path(path_to_results, "Figures", "Rain_Boxplots", paste0(rainvars[var_i], "ByYear_boxplot.png"))), RainByYear_boxlist[[var_i]], height=8, width=12, units = 'in', dpi=320)
  
}


RunoffByYear_box <- ggplot(data=data_df3, aes_string(x="wateryear", y="sum_runoff", group="wateryear", color="wateryear", fill="wateryear")) +
  scale_y_log10() +
  geom_jitter(width = .1, size=1, alpha=.5, shape=16) + 
  geom_boxplot(alpha=0.2, outlier.shape = NA) +
  # scale_shape_manual(values=c(16, 1))+
  # stat_smooth(method = "lm", se=T, alpha=.1) +
  facet_wrap(~site, scales='free_y') +
  theme_bw() +
  theme(legend.position = 'bottom') +
  guides(color = guide_legend(nrow = 1)) +
  labs(x = "Water year") +
  theme(axis.text=element_text(size=8)) +
  ggtitle('All runoff events') +
  theme(plot.title = element_text(hjust = 0.5))

print(RunoffByYear_box)

ggsave(file_out(file.path(path_to_results, "Figures", "Rain_Boxplots", paste0("sum_runoff", "ByYear_boxplot.png"))), RunoffByYear_box, height=8, width=12, units = 'in', dpi=320)




# plot all site data in single panel


ConcByYear_boxlist_bysite<-list()
site_i <- 1
for (site_i in 1:length(EOF_sites)){
  
  data_i <- filter(data_df3, site==EOF_sites[site_i]) %>%
    select(c("wateryear", loadvars, concvars, rainvars)) %>%
    gather(key=variable, value=value, 2:(1+length(c(loadvars, concvars, rainvars)))) %>%
    mutate(variable = factor(variable, c(loadvars, concvars, rainvars)))
  
  ConcByYear_boxlist_bysite[[site_i]] <- ggplot(data=data_i, aes_string(x="wateryear", y="value", group="wateryear", color="wateryear", fill="wateryear")) +
    scale_y_log10() +
    geom_jitter(width = .1, size=1, alpha=.5, shape=16) + 
    geom_boxplot(alpha=0.2, outlier.shape = NA) +
    # scale_shape_manual(values=c(16, 1))+
    # stat_smooth(method = "lm", se=T, alpha=.1) +
    facet_wrap(~variable, scales='free_y') +
    theme_bw() +
    theme(legend.position = 'bottom') +
    guides(color = guide_legend(nrow = 1)) +
    labs(x = "Water year") +
    theme(axis.text=element_text(size=8)) +
    ggtitle(paste0(EOF_sites[site_i],  ': All runoff events')) +
    theme(plot.title = element_text(hjust = 0.5))
  
  print(ConcByYear_boxlist_bysite[[site_i]])
  
  ggsave(file_out(file.path(path_to_results, "Figures", "Boxplots_bySite", paste0(EOF_sites[site_i], "_ByYear_boxplot.png"))), ConcByYear_boxlist_bysite[[site_i]], height=8, width=12, units = 'in', dpi=320)
  
}


YieldsByYear_boxlist_bysite<-list()
site_i <- 1
for (site_i in 1:length(EOF_sites)){
  
  data_i <- filter(data_df3, site==EOF_sites[site_i]) %>%
    select(c("wateryear", choice_conc, choice_yields)) %>%
    gather(key=variable, value=value, 2:(1+length(c(choice_conc, choice_yields)))) %>%
    mutate(variable = factor(variable, c(choice_yields, choice_conc)))
  
  YieldsByYear_boxlist_bysite[[site_i]] <- ggplot(data=data_i, aes_string(x="wateryear", y="value", group="wateryear", color="wateryear", fill="wateryear")) +
    scale_y_log10nice() +
    geom_jitter(width = .1, size=1, alpha=.5, shape=16) + 
    geom_boxplot(alpha=0.2, outlier.shape = NA) +
    # scale_shape_manual(values=c(16, 1))+
    # stat_smooth(method = "lm", se=T, alpha=.1) +
    facet_wrap(~variable, scales='free_y') +
    theme_bw() +
    theme(legend.position = 'bottom') +
    guides(color = guide_legend(nrow = 1)) +
    labs(x = "Water year") +
    theme(axis.text=element_text(size=8)) +
    ggtitle(paste0(EOF_sites[site_i],  ': All runoff events')) +
    theme(plot.title = element_text(hjust = 0.5))
  
  print(YieldsByYear_boxlist_bysite[[site_i]])
  
  ggsave(file_out(file.path(path_to_results, "Figures", "Boxplots_bySite", paste0(EOF_sites[site_i], "YieldsConc_ByYear_boxplot.png"))), YieldsByYear_boxlist_bysite[[site_i]], height=8, width=12, units = 'in', dpi=320)
  
}





# boxplot of select variables for SIRs 
selectvars <- c(concvars[c(1,6:7, 3:4,8)],
                'rain', 'weq', 'erosivity_m2',
                'runoff_cubicmeter_percubicmeterWEQ', 'runoff_volume', 'peak_discharge')
# c(loadvars, concvars, rainvars)
selectnames <- c("Suspended sediments (mg/L)", "SRP (mg P/L)", "TP (mg P/L)",
                 "NO2 NO3 (mg N/L)", "NH4 (mg N/L)", "TN (mg N/L)",
                 "Rain (in)", "WEQ (in)", "Erosivity (m2)",
                 'Runoff index', 'Runoff volume (cf)', 'Peak discharge (cfs)')

ConcByYear_boxlist_bysite_select<-list()
ConcByYear_boxlist_bysite_select_nonfrozen <- list()
site_i <- 15
for (site_i in 1:length(EOF_sites)){
  
  data_i <- filter(data_df3, site==EOF_sites[site_i]) %>%
    dplyr::select("wateryear", "period", "frozen", selectvars) %>%
    gather(key=variable, value=value, 4:(3+length(selectvars))) %>%
    mutate(variable = factor(variable, selectvars)) %>%
    mutate(name = selectnames[match(variable, selectvars)]) %>%
    mutate(name = factor(name, selectnames),
           period = factor(period, c('before', 'after'))) #%>%
    # mutate(wateryear = as.numeric(as.character(wateryear)))
  
  line_date = group_by(data_i, period) %>%
    summarize(minyear = min(as.numeric(as.character(wateryear))),
              maxyear = max(as.numeric(as.character(wateryear))))
  
  ConcByYear_boxlist_bysite_select[[site_i]] <- ggplot(data=data_i, aes_string(x="wateryear", y="value", color="period", fill="period")) +
    scale_y_log10() +
    scale_color_brewer(type='qual', palette=2) + 
    scale_fill_brewer(type='qual', palette=2) + 
    
    # geom_point(position=position_jitterdodge(),  width = 1) + 
    geom_point(position=position_jitterdodge(), size=1, alpha=.5, shape=16) +
    # geom_jitter(width = .1, size=1, alpha=.5, shape=16) +
    geom_boxplot(aes(fill=period), alpha=0.2, outlier.shape = NA) +
    # scale_shape_manual(values=c(16, 1))+
    # stat_smooth(method = "lm", se=T, alpha=.1) +
    facet_wrap(~name, scales='free_y', nrow=4) +
    theme_bw() +
    theme(legend.position = 'bottom') +
    guides(color = guide_legend(nrow = 1)) +
    labs(x = "Water year") +
    theme(axis.text=element_text(size=8),
          axis.title.y=element_blank(),
          legend.title=element_blank()) +
    ggtitle(paste0(EOF_sites[site_i],  ': All runoff events')) +
    theme(plot.title = element_text(hjust = 0.5))
  
  print(ConcByYear_boxlist_bysite_select[[site_i]])
  
  ggsave(file_out(file.path(path_to_results, "Figures", "Boxplots_bySite_SelectVars", paste0(EOF_sites[site_i], "_ByYear_boxplot.png"))), ConcByYear_boxlist_bysite_select[[site_i]], height=8, width=8, units = 'in', dpi=320)
 
  
  ConcByYear_boxlist_bysite_select_nonfrozen[[site_i]] <- ggplot(data=data_i[which(data_i$frozen==FALSE),], aes_string(x="wateryear", y="value", color="period", fill="period")) +
    scale_y_log10() +
    # geom_point(position=position_jitterdodge(),  width = 1) + 
    scale_color_brewer(type='qual', palette=2) + 
    scale_fill_brewer(type='qual', palette=2) + 
    geom_point(position=position_jitterdodge(), size=1, alpha=.5, shape=16) +
    # geom_jitter(width = .1, size=1, alpha=.5, shape=16) +
    geom_boxplot(aes(fill=period), alpha=0.2, outlier.shape = NA) +
    # scale_shape_manual(values=c(16, 1))+
    # stat_smooth(method = "lm", se=T, alpha=.1) +
    facet_wrap(~name, scales='free_y', nrow=4) +
    theme_bw() +
    theme(legend.position = 'bottom') +
    guides(color = guide_legend(nrow = 1)) +
    labs(x = "Water year") +
    theme(axis.text=element_text(size=8),
          axis.title.y=element_blank(),
          legend.title=element_blank()) +
    ggtitle(paste0(EOF_sites[site_i],  ': Non-frozen runoff events')) +
    theme(plot.title = element_text(hjust = 0.5))
  
  print(ConcByYear_boxlist_bysite_select_nonfrozen[[site_i]])
  
  ggsave(file_out(file.path(path_to_results, "Figures", "Boxplots_bySite_SelectVars", paste0(EOF_sites[site_i], "_ByYear_NonFrozen_boxplot.png"))), ConcByYear_boxlist_bysite_select_nonfrozen[[site_i]], height=8, width=8, units = 'in', dpi=320)
  
  
}


