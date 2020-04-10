
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

data_merge_median <- data_df3 %>%
  mutate(wateryear = as.numeric(as.character(wateryear))) %>%
  filter(wateryear %in% c(2016:2017), frozen == 0) %>%
  # filter(wateryear == unique(soil_0_15$wateryear), frozen == 0) %>%
  rename(Site = site) %>%
  dplyr::select(wateryear, Site, wq_vars, peak_discharge, rain) %>%
  group_by(Site) %>%
  summarize_at(wq_vars, .funs=median, na.rm=T) %>%
  left_join(soil_0_15) %>%
  drop_na(Manure) 

data_merge_2016 <- data_df3 %>%
  mutate(wateryear = as.numeric(as.character(wateryear))) %>%
  # filter(wateryear %in% c(2016:2017), frozen == 0) %>%
  filter(wateryear == unique(soil_0_15$wateryear), frozen == 0) %>%
  rename(Site = site) %>%
  dplyr::select(wateryear, Site, wq_vars, peak_discharge, runoff_volume, rain) %>%
  group_by(Site) %>%
  summarize_at(wq_vars, .funs=c(median, mean), na.rm=T) %>%
  left_join(soil_0_15) %>%
  drop_na(Manure) 
  
#Experimenting with flow weighted means
wq_weightedvars <- wq_vars[which(wq_vars %notin% c('runoff_volume', 'runoff_volume_cubicfootperAcre', 
                                                   'runoff_cubicmeter_percubicmeterWEQ'))]
data_merge2 <- data_df3 %>%
  mutate(wateryear = as.numeric(as.character(wateryear))) %>%
  filter(wateryear %in% c(2016:2017), frozen == 0) %>%
  # filter(wateryear == unique(soil_0_15$wateryear), frozen == 0) %>%
  rename(Site = site) %>%
  dplyr::select(wateryear, Site, wq_vars, peak_discharge, rain) %>%
  group_by(Site) %>%
  summarise_at(wq_weightedvars, 
               funs(weighted.mean(., runoff_volume, na.rm=T))) %>%
  left_join(soil_0_15) %>%
  drop_na(Manure) %>%
  left_join(data_merge_median[c('Site', 'runoff_volume', 'runoff_volume_cubicfootperAcre', 
                                'runoff_cubicmeter_percubicmeterWEQ')])

  
data_merge <- data_merge2

# ggplot(data=data_merge_2016_2017, aes(x=Bray_P, y=orthophosphate_yield_poundperAcreperInchWEQ)) +
  # geom_point(aes(color=Manure))
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

predictors.cor <- cor(data_merge[,choice_soil], use = 'complete.obs') 
names.cor <- row.names(predictors.cor)
drop.predictors <- caret::findCorrelation(predictors.cor, cutoff = 0.95, verbose = FALSE, exact = TRUE)
predictors.keep <- c(names.cor[-drop.predictors], 'Manure')


wq_var<-1
plot_list_yield <-list()
glm_list_yield <- list()
model_fig_yield <- list()
model_data_yield <- list()
for (wq_var in 1:length(choice_yields)) {

  var_list <- list()
  soil_var <- 1
  for (soil_var in 1:length(choice_soil)){
    
    lm_model <- lm(data.frame(data_merge[choice_yields[wq_var]])[,1] ~ data.frame(data_merge[choice_soil[soil_var]])[,1])
    pvalue <- summary(lm_model)$coefficients[2,4]
    
    var_list[[soil_var]] <- ggplot(data=data_merge, aes_string(x=choice_soil[soil_var], y=choice_yields[wq_var])) +
      geom_point(aes(color=Manure), size=2, shape=1)  +
      theme_bw() +
      theme(legend.position = 'none',
            axis.title.y = element_blank())
    
    if (choice_soil[soil_var] %in% predictors.keep) {
      var_list[[soil_var]] <- var_list[[soil_var]] +
        geom_point(aes(color=Manure), size=2, shape=16)
    }
    
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
  
  plot_list_yield[[wq_var]] <- grid.arrange(grobs=var_list, ncol=5, top=choice_yields[wq_var])
  
  ggsave(file=file_out(file.path(path_to_results, 'Figures', 'Soil', 'SoilWQ_Scatter', paste(choice_yields[wq_var], ".png"))) , plot_list_yield[[wq_var]], height=8, width=8)
    

  # data.glm <- data_merge[c(choice_soil, choice_yields[wq_var])] %>%
  #   select_if(~ !any(is.na(.))) %>%
  #   data.frame()
  
  data.glm <- data_merge[c(predictors.keep, choice_yields[wq_var])] %>%
    mutate(Manure = factor(Manure, levels=c('No Manure', "Manure"))) %>%
    select_if(~ !any(is.na(.))) %>%
    data.frame()

  glm_list_yield[[wq_var]]<-bestglm(data.glm, family=gaussian, IC='BIC', nvmax=3)
  print(choice_yields[wq_var])
  glm_list_yield[[wq_var]]
  summary(glm_list_yield[[wq_var]])

  model_data_yield[[wq_var]] <- data.frame(obs =glm_list_yield[[wq_var]]$BestModel$model$y,
                           pred=glm_list_yield[[wq_var]]$BestModel$fitted.values)

  model_fig_yield[[wq_var]] <- ggplot(data=model_data_yield[[wq_var]], aes(x=obs,
                                                                   y=pred)) + 
    geom_point() +  
    ggtitle(choice_yields[wq_var]) + 
    labs(x = 'observations', y='predictions' ) + 
    geom_abline() + 
    theme_bw()

  print(model_fig_yield[[wq_var]])
    
}


names(glm_list_yield)<-choice_yields
glm_list_yield
print(model_fig_yield)

#Similar figures for concentration
wq_var<-1
plot_list_conc <-list()
glm_list_conc <- list()
model_fig_conc <- list()
model_data_conc <- list()
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
  
  plot_list_conc[[wq_var]] <- grid.arrange(grobs=var_list, ncol=5, top=choice_conc[wq_var])
  
  ggsave(file=file_out(file.path(path_to_results, 'Figures', 'Soil', 'SoilWQ_Scatter', paste(choice_conc[wq_var], ".png"))) , plot_list_conc[[wq_var]], height=8, width=8)
  
  
  data.glm <- data_merge[c(choice_soil, choice_conc[wq_var])] %>%
    select_if(~ !any(is.na(.))) %>%
    data.frame()
  
  glm_list_conc[[wq_var]]<-bestglm(data.glm, family=gaussian, IC='BIC', nvmax=3)
  print(choice_conc[wq_var])
  glm_list_conc[[wq_var]]
  summary(glm_list_conc[[wq_var]])
  
  
  model_data_conc[[wq_var]] <- data.frame(obs =glm_list_conc[[wq_var]]$BestModel$model$y,
                                           pred=glm_list_conc[[wq_var]]$BestModel$fitted.values)
  
  model_fig_conc[[wq_var]] <- ggplot(data=model_data_conc[[wq_var]], aes(x=obs,
                                                                           y=pred)) + 
    geom_point() +  
    ggtitle(choice_conc[wq_var]) + 
    labs(x = 'observations', y='predictions' ) + 
    geom_abline() + 
    theme_bw()
  
  print(model_fig_conc[[wq_var]])
  
  
}

names(glm_list_conc)<-choice_conc
glm_list_conc
print(model_fig_conc)


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



summary(glm_list_yield$tp_unfiltered_yield_poundperAcreperInchWEQ)
plot(glm_list_yield$tp_unfiltered_yield_poundperAcreperInchWEQ$BestModel$model$y, 
     glm_list_yield$tp_unfiltered_yield_poundperAcreperInchWEQ$BestModel$fitted.values,
     xlab='observations', ylab='predictions', main='tp_unfiltered_yield_poundperAcreperInchWEQ')


