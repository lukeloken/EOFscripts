
#load storm data
data_df3 <- readRDS(file=(file_in(file.path(path_to_data, "compiled_data", "storm_event_loads", "storm_event_conc_allsites_model.rds" ))))

# data_df <- readRDS(file=(file_out(file.path(path_to_data, "compiled_data", "storm_event_loads", "storm_event_loads_allsites_approved_data.rds" ))))
# 
# conc_df <- data.frame(sapply(data_df[,loadvars], function (x) x/data_df$runoff_volume*453592/28.3168))
# colnames(conc_df) <- concvars
# 
# #Bind concentration data frame to bigger data frame
# data_df <- bind_cols(data_df, conc_df)

#Load soil data
soil_0_15 <- readRDS(file=file_in(file.path(path_to_data, 'soil', 'cleaned_data', 'Soil2016_0_to_15cm.rds')))

soil_vars <- names(soil_0_15)[5:34]
wq_vars <- c(loadvars, concvars, yieldvars, yieldperweqvars)

#collapse data to only water year with soil data
#Compute annual metrics

data_merge <- data_df3 %>%
  mutate(wateryear = as.numeric(as.character(wateryear))) %>%
  filter(wateryear == unique(soil_0_15$wateryear),
         frozen == 0) %>%
    rename(Site = site) %>%
  dplyr::select(wateryear, Site, wq_vars, peak_discharge, runoff_volume, rain) %>%
  group_by(wateryear, Site) %>%
  summarize_at(wq_vars, .funs=mean, na.rm=T) %>%
  left_join(soil_0_15) %>%
  drop_na(Manure) 
  

# ggplot(data=data_merge, aes(x=Bray_P, y=orthophosphate_yield_poundperAcreperInchWEQ)) +
#   geom_point(aes(color=Manure))
# 
# ggplot(data=data_merge, aes(x=OM_spring, y=orthophosphate_conc_mgL)) +
#   geom_point(aes(color=Manure))
# 
# ggplot(data=data_merge, aes(x=Bray_P, y=tp_unfiltered_yield_poundperAcreperInchWEQ)) +
#   geom_point(aes(color=Manure))
# 
# ggplot(data=data_merge, aes(x=POXC, y=tp_unfiltered_yield_poundperAcreperInchWEQ)) +
#   geom_point(aes(color=Manure))
# 
# 
# ggplot(data=data_merge, aes(x=WE_TN, y=total_nitrogen_conc_mgL)) +
#   geom_point(aes(color=Manure))
# 
# ggplot(data=data_merge, aes(x=Penotrometer_6_18, y=suspended_sediment_yield_poundperAcreperInchWEQ)) +
#   geom_point(aes(color=Manure))
# 
# ggplot(data=data_merge, aes(x=FinalFlux, y=suspended_sediment_yield_poundperAcreperInchWEQ)) +
#   geom_point(aes(color=Manure))
# 
# ggplot(data=data_merge, aes(x=P_Sand, y=suspended_sediment_conc_mgL)) +
#   geom_point(aes(color=Manure))



# choice_yields <-  paste0(yieldvars, "perInchWEQ")[c(1,3,7,8)]
choice_yields <-  yieldperweqvars[c(1:9,12)]
choice_conc <- concvars[1:9]

choice_soil <-  soil_vars[c(1:3,4:6,7:18, 19:23, 25:30)]



wq_var<-1
plot_list <-list()
glm_list <- list()
for (wq_var in 1:length(choice_yields)) {

  var_list <- list()
  soil_var <- 1
  for (soil_var in 1:length(choice_soil)){
    
    lm_model <- lm(data.frame(data_merge[choice_yields[wq_var]])[,1] ~ data.frame(data_merge[choice_soil[soil_var]])[,1])
    pvalue <- summary(lm_model)$coefficients[2,4]
    
    var_list[[soil_var]] <- ggplot(data=data_merge, aes_string(x=choice_soil[soil_var], y=choice_yields[wq_var])) +
      geom_point(aes(color=Manure), size=2)  +
      theme_bw() +
      theme(legend.position = 'none',
            axis.title.y = element_blank())
    
    if (pvalue <=0.1 & pvalue > 0.05) {
      var_list[[soil_var]] <- var_list[[soil_var]] +
        geom_smooth(method="lm", se=F, col='darkgrey', linetype='dashed') +
        geom_point(aes(color=Manure), size=2) 
    } else if (pvalue <=0.05){
      var_list[[soil_var]] <- var_list[[soil_var]] +
        geom_smooth(method="lm", se=F, col='darkgrey') +
        geom_point(aes(color=Manure), size=2) 
    }
    
  }
   

  plot_withLegend <- var_list[[1]] + theme(legend.position = 'bottom', legend.title = element_blank()) + 
    guides(color=guide_legend(ncol=1))

  var_list[[length(choice_soil)+1]] <- g_legend(plot_withLegend)
  
  plot_list[[wq_var]] <- grid.arrange(grobs=var_list, ncol=5, top=choice_yields[wq_var])
  
  ggsave(file=file_out(file.path(path_to_results, 'Figures', 'Soil', 'SoilWQ_Scatter', paste(choice_yields[wq_var], ".png"))) , plot_list[[wq_var]], height=8, width=8)
    

  data.glm <- data_merge[c(choice_soil, choice_yields[wq_var])] %>%
    select_if(~ !any(is.na(.))) %>%
    data.frame()

  glm_list[[wq_var]]<-bestglm(data.glm, family=gaussian, IC='BIC', nvmax=3)
  print(choice_yields[wq_var])
  glm_list[[wq_var]]
  summary(glm_list[[wq_var]])
  
}


#Similar figures for concentration
wq_var<-1
plot_list <-list()
glm_list <- list()
for (wq_var in 1:length(choice_conc)) {
  
  var_list <- list()
  soil_var <- 1
  for (soil_var in 1:length(choice_soil)){
    
    lm_model <- lm(data.frame(data_merge[choice_conc[wq_var]])[,1] ~ data.frame(data_merge[choice_soil[soil_var]])[,1])
    pvalue <- summary(lm_model)$coefficients[2,4]
    
    var_list[[soil_var]] <- ggplot(data=data_merge, aes_string(x=choice_soil[soil_var], y=choice_conc[wq_var])) +
      geom_point(aes(color=Manure), size=2)  +
      theme_bw() +
      theme(legend.position = 'none',
            axis.title.y = element_blank())
    
    if (pvalue <=0.1 & pvalue > 0.05) {
      var_list[[soil_var]] <- var_list[[soil_var]] +
        geom_smooth(method="lm", se=F, col='darkgrey', linetype='dashed') +
        geom_point(aes(color=Manure), size=2) 
    } else if (pvalue <=0.05){
      var_list[[soil_var]] <- var_list[[soil_var]] +
        geom_smooth(method="lm", se=F, col='darkgrey') +
        geom_point(aes(color=Manure), size=2) 
    }
    
  }
  
  
  plot_withLegend <- var_list[[1]] + theme(legend.position = 'bottom', legend.title = element_blank()) + 
    guides(color=guide_legend(ncol=1))
  
  var_list[[length(choice_soil)+1]] <- g_legend(plot_withLegend)
  
  plot_list[[wq_var]] <- grid.arrange(grobs=var_list, ncol=5, top=choice_conc[wq_var])
  
  ggsave(file=file_out(file.path(path_to_results, 'Figures', 'Soil', 'SoilWQ_Scatter', paste(choice_conc[wq_var], ".png"))) , plot_list[[wq_var]], height=8, width=8)
  
  
  data.glm <- data_merge[c(choice_soil, choice_conc[wq_var])] %>%
    select_if(~ !any(is.na(.))) %>%
    data.frame()
  
  glm_list[[wq_var]]<-bestglm(data.glm, family=gaussian, IC='BIC', nvmax=3)
  print(choice_conc[wq_var])
  glm_list[[wq_var]]
  summary(glm_list[[wq_var]])
  
}

wq_var <-1
fig_list <- list()
for (wq_var in 1:length(choice_conc)) {
  
fig_list[[wq_var]] <- ggplot(data=data_merge, aes_string(x=choice_yields[wq_var], y=choice_conc[wq_var])) +
  geom_point(aes(color=Manure), size=2)  +
  theme_bw() +
  theme(legend.position = 'none')
}


yieldversusconc <- grid.arrange(grobs=fig_list, ncol=3, top="Yield per rain VERSUS concentration")

ggsave(file=file_out(file.path(path_to_results, 'Figures', 'Soil', 'SoilWQ_Scatter', "Yield_V_Conc.png")), yieldversusconc, height=8, width=8)







Manure_temp2 <- factor(data_merge$Manure, c('Manure', "No Manure"))
Manure_temp2 <- as.numeric(Manure_temp2)
Manure_temp2[which(Manure_temp2 == 2)] <- 0
data_merge$Manure_binary <- Manure_temp2


merge_pca_df <- data_merge[colSums(!is.na(data_merge)) > 0] %>%
  group_by() %>%
  na.omit()

#For big analysis remove rows with any NAs
merge_pca_df<-na.omit(merge_pca_df)
merge_pca_df2 <- dplyr::select(merge_pca_df, -wateryear, -Site, -Manure, -Depth, -Date)

#Make PCAs
merge_pca <- prcomp(merge_pca_df2, center = TRUE, scale. = TRUE, rank=6) 

rownames(merge_pca_df) <- merge_pca_df$Site


chart.Correlation(merge_pca_df2, histogram=TRUE, pch=19)

corrplot(merge_pca$rotation, is.corr=FALSE, mar=c(0,0,0,1.5), oma=c(0,0,0,0), tl.col='black', cl.pos='r', cl.ratio=0.5, col=brewer.pal(10, 'RdYlBu'), cl.lim=corrange)


res <- cor(merge_pca_df2)
round(res, 2)

corrplot(res, type='upper', cl.lim=c(-1, 1))

