
#Loop through all site folders and load edge of field runoff data

#This is where the data live
print(path_to_data)

if (exists("data_df")){
  print("Object data_df exists. Great job!!! You may proceed")
} else {
  stop ("no data loaded into R environment. Check 1_load_all_data.R script")
}


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

concvars <- c('suspended_sediment_conc', 
              'chloride_conc',
              'no2_no3n_conc', 
              'ammonium_n_conc',
              'tkn_unfiltered_conc', 
              'orthophosphate_conc',
              'tp_unfiltered_conc',
              'total_nitrogen_conc',
              'organic_nitrogen_conc',
              'toc_conc',
              'doc_conc')



data_df2 <- data_df %>%
  mutate(site = as.character(site),
         storm_middate = storm_start + difftime(storm_end, storm_start, units='secs')/2) %>%
  mutate(wateryear = as.factor(getWY (storm_middate))) %>%
  select(-file_id, -unique_storm_number, -sub_storms, -rain_startdate, -rain_enddate, -storm_start, -storm_end, -sample_end, -sample_start, -ant_discharge_date)

conc_df <- data.frame(sapply(data_df2[,loadvars], function (x) x/data_df2$runoff_volume))
colnames(conc_df) <- concvars
rownames(conc_df) <- NULL

data_df3 <- bind_cols(data_df2, conc_df)


data_wateryear_summary <- data_df2 %>%
  group_by (site, wateryear, period) %>%
  select(-storm_middate) %>%
  filter(frozen == FALSE) %>% 
  summarize_all(mean, na.rm=T)

data_wateryear_summary


#playing with plotting
#this will go to its own script later
library(ggpubr)

ggplot(data=data_df2, aes_string(x="sum_runoff", y=loadvars[1], group="wateryear", color="wateryear")) +
  geom_point() +
  stat_smooth(method = "lm", se=F) +
  facet_wrap(~site, scales='free') +
  theme_bw()


LoadByRunnoff_plotlist <- list()

var_i <- 11
for (var_i in 1:length(loadvars)){
  
  LoadByRunnoff_plotlist[[var_i]] <- ggplot(data=data_df2, aes_string(x="sum_runoff", y=loadvars[var_i], group="wateryear", color="wateryear", fill="wateryear")) +
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
    scale_y_log10() +
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



# ggsave(file_out(file.path(path_to_results, "Figures", "TPLoadByRunoff_plot.png")), TPLoadByRunoff_plot, height=8, width=12, units = 'in', dpi=320)


ggplot(data=data_df2, aes_string(x="rain", y=loadvars[7], group="wateryear", color="wateryear")) +
  geom_point(aes(shape=frozen), size=2) +
  scale_shape_manual(values=c(16, 1))+
  stat_smooth(method = "lm", se=T, alpha=.1, aes(fill=wateryear)) +
  facet_wrap(~site, scales='free') +
  theme_bw() +
  theme(legend.position = 'bottom') +
  guides(color = guide_legend(nrow = 1)) +
  labs(x = "rain (inches)")


ggplot(data=data_df2, aes_string(x="rain", y=loadvars[7], group="period", color="period")) +
  geom_point(aes(shape=frozen), size=2) +
  scale_shape_manual(values=c(16, 1))+
  stat_smooth(method = "lm", se=T, alpha=.1, aes(fill=period)) +
  facet_wrap(~site, scales='free') +
  theme_bw() +
  theme(legend.position = 'bottom') +
  guides(color = guide_legend(nrow = 1)) +
  labs(x = "rain (inches)")
