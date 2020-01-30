

#Plot concentration and load data







#Create two r objects for frozen and non-frozen periods
data_df4 <-filter(data_df3, frozen==FALSE)

data_df5 <-filter(data_df3, frozen==TRUE)





# From here you can change the group_by columns to select how to perform summary
# select is how to include or exclude (-) columns by name
# filter is how to make the data frame shorter by including certain cases (frozen/non frozen)
# summarize(), summarize_all(), summarize_at() can include a variety of additional metrics
# Medians, means, sum, etc. 





#########################################
# playing with plotting
# this will go to its own script later
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
  
  ConcByYear_nonFrozen_boxlist[[var_i]] <- ggplot(data=data_df4, aes_string(x="wateryear", y=concvars[var_i], group="wateryear", color="wateryear", fill="wateryear")) +
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
  
  ConcByYear_Frozen_boxlist[[var_i]] <- ggplot(data=data_df5, aes_string(x="wateryear", y=concvars[var_i], group="wateryear", color="wateryear", fill="wateryear")) +
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

