
#load storm data
data_df3 <- readRDS(file=(file_in(file.path(path_to_data, "compiled_data", "storm_event_loads", "storm_event_conc_allsites_model.rds" ))))

#load farm data
farm_area <- read.csv(file_in(file.path(path_to_data, "SiteCharacteristics", "EOF_WatershedAreas.csv")), header=T, stringsAsFactors = F) %>%
  select(-Slope)

#Load soil data
soil_0_15 <- readRDS(file=file_in(file.path(path_to_data, 'soil', 'cleaned_data', 'Soil2016_0_to_15cm.rds'))) %>%
  select(-Manure) %>%
  left_join(farm_area) %>%
  select(-Type)

#Variable groups
soil_vars <- names(soil_0_15)[c(4:25, 26:29,32:37, 39:42)]
wq_vars <- c(loadvars, concvars, yieldvars, yieldperweqvars)

wq_weightedvars <- wq_vars[which(wq_vars %notin% c('runoff_volume', 'runoff_volume_cubicfootperAcre', 
                                                   'runoff_cubicmeter_percubicmeterWEQ'))]

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
                                'runoff_cubicmeter_percubicmeterWEQ')]) %>%
  mutate_if(is.character, as.factor)

#Experimenting with runoff index calculations
data_merge_runofftotal <- data_df3 %>%
  mutate(wateryear = as.numeric(as.character(wateryear))) %>%
  filter(wateryear %in% c(2016:2017), frozen == 0) %>%
  rename(Site = site) %>%
  mutate(weq_volume_m3 = weq*Area_acres*4046.86/39.3701,
         runoff_volume_m3 = runoff_volume/35.3147) %>%
  dplyr::select(Site, Type, weq_volume_m3, runoff_volume_m3) %>%
  group_by(Site, Type) %>%
  na.omit() %>%
  summarize_all(sum) %>%
  mutate(runoff_Index = runoff_volume_m3/weq_volume_m3) %>%
  select(-weq_volume_m3, -runoff_volume_m3)

data_merge_runofftotal

ggplot(data_merge_runofftotal, aes(y=runoff_Index, x=Site, fill=Type)) + 
  geom_bar(stat='identity') +
  theme(axis.text.x=element_text(angle=90))


#Calculating yields by summing first (equivilent to mean then divide)
data_merge_loadtotal <- data_df3 %>%
  mutate(wateryear = as.numeric(as.character(wateryear))) %>%
  filter(wateryear %in% c(2016:2017), frozen == 0) %>%
  rename(Site = site) %>%
  mutate(weq_volume_m3 = weq*Area_acres*4046.86/39.3701) %>%
  select(Site, Type, loadvars, weq_volume_m3) %>%
  group_by(Site, Type) %>%
  # na.omit() %>%
  summarize_all(mean, na.rm=T) 

data_merge_yieldtotal <- data.frame(sapply(data_merge_loadtotal[,loadvars], function (x) x*453.592/data_merge_loadtotal$weq_volume_m3)) %>%
  select(-runoff_volume) 

names(data_merge_yieldtotal) <- gsub("_load_pounds", "_yield_mgL", names(data_merge_yieldtotal))

data_merge_yieldtotal$doc_yield_mgL[which(data_merge_yieldtotal$doc_yield_mgL==0)] <- NaN
data_merge_yieldtotal$toc_yield_mgL[which(data_merge_yieldtotal$toc_yield_mgL==0)] <- NaN

data_merge3 <- bind_cols(data_merge_runofftotal, data_merge_yieldtotal) %>%
  filter(Site %in% data_merge2$Site) %>%
  right_join(data_merge2)

data.frame(data_merge3)


data_merge <- data_merge3

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


choice_yields <- c(names(data_merge_yieldtotal)[1:9], 'runoff_Index')

# choice_yields <-  yieldperweqvars[c(1:9,12)]
choice_conc <- concvars[1:9]
choice_physics <- c("suspended_sediment_conc_mgL", "suspended_sediment_yield_mgL", "runoff_index")
                   
# choice_physics <- c("suspended_sediment_conc_mgL", "suspended_sediment_yield_poundperAcreperInchWEQ", 
                    # "runoff_cubicmeter_percubicmeterWEQ")

#If need to subset soil vars
choice_soil <-  soil_vars[]

predictors.cor <- cor(select_if(data_merge[,choice_soil], is.numeric), use = 'complete.obs') 
names.cor <- row.names(predictors.cor)
drop.predictors <- caret::findCorrelation(predictors.cor, cutoff = 0.95, verbose = FALSE, exact = TRUE)
predictors.keep <- c(names.cor[-drop.predictors], 'Manure')

predictors.keep.physics <- soil_vars[c(19:24, 27:36)]

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
    
    if (is.numeric(data_merge[[choice_soil[soil_var]]])==FALSE){
      var_list[[soil_var]] <- ggplot(data=data_merge, aes_string(x=choice_soil[soil_var], y=choice_yields[wq_var])) +
        geom_point(aes(color=Manure), position=position_jitter(width = 0.1), size=2, shape=16)  +
        scale_color_brewer(palette='Dark2') + 
        theme_bw() +
        theme(legend.position = 'none',
              axis.title.y = element_blank())
    } else if (is.numeric(data_merge[[choice_soil[soil_var]]])){
      
      var_list[[soil_var]] <- ggplot(data=data_merge, aes_string(x=choice_soil[soil_var], y=choice_yields[wq_var])) +
        geom_point(aes(color=Manure), size=2, shape=16)  +
        scale_color_brewer(palette='Dark2') + 
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
    
    
  }
  
  
  plot_withLegend <- var_list[[1]] + theme(legend.position = 'bottom', legend.title = element_blank()) + 
    guides(color=guide_legend(ncol=1))
  
  var_list[[length(choice_soil)+1]] <- g_legend(plot_withLegend)
  
  plot_list_yield[[wq_var]] <- grid.arrange(grobs=var_list, ncol=5, top=choice_yields[wq_var])
  
  ggsave(file=file_out(file.path(path_to_results, 'Figures', 'Soil', 'SoilWQ_Scatter', paste(choice_yields[wq_var], ".png"))) , plot_list_yield[[wq_var]], height=8, width=8)
  
  
  # data.glm <- data_merge[c(choice_soil, choice_yields[wq_var])] %>%
  #   select_if(~ !any(is.na(.))) %>%
  #   data.frame()
  
  data.glm <- data_merge[c(soil_vars[which(soil_vars %notin% c('Texture', 'Soil_Sub_Order'))], choice_yields[wq_var])] %>%
    mutate(Manure = factor(Manure, levels=c('No Manure', "Manure"))) %>%
    select_if(~ !any(is.na(.))) %>%
    data.frame()
  
  if (choice_yields[wq_var] %in% choice_physics){
    data.glm <- data_merge[c(predictors.keep.physics, choice_yields[wq_var])] %>%
      # mutate(Manure = factor(Manure, levels=c('No Manure', "Manure"))) %>%
      # select_if(~ !any(is.na(.))) %>%
      drop_na() %>%
      data.frame()
  }
  
  glm_list_yield[[wq_var]]<-bestglm(data.glm, family=gaussian, IC='BIC', nvmax=2)
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
    
    if (is.numeric(data_merge[[choice_soil[soil_var]]])==FALSE){
      var_list[[soil_var]] <- ggplot(data=data_merge, aes_string(x=choice_soil[soil_var], y=choice_conc[wq_var])) +
        geom_point(aes(color=Manure), position=position_jitter(width = 0.1), size=2, shape=16)  +
        scale_color_brewer(palette='Dark2') + 
        theme_bw() +
        theme(legend.position = 'none',
              axis.title.y = element_blank())
      
    } else if (is.numeric(data_merge[[choice_soil[soil_var]]])){
      
      var_list[[soil_var]] <- ggplot(data=data_merge, aes_string(x=choice_soil[soil_var], y=choice_conc[wq_var])) +
        geom_point(aes(color=Manure), size=2, shape=16)  +
        scale_color_brewer(palette='Dark2') + 
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
    
  }
  
  
  plot_withLegend <- var_list[[1]] + theme(legend.position = 'bottom') + 
    guides(color=guide_legend(ncol=1, title.position="top"))
  
  var_list[[length(choice_soil)+1]] <- g_legend(plot_withLegend)
  
  plot_list_conc[[wq_var]] <- grid.arrange(grobs=var_list, ncol=5, top=choice_conc[wq_var])
  
  ggsave(file=file_out(file.path(path_to_results, 'Figures', 'Soil', 'SoilWQ_Scatter', paste(choice_conc[wq_var], ".png"))) , plot_list_conc[[wq_var]], height=8, width=8)
  
  
  data.glm <- data_merge[c(soil_vars[which(soil_vars %notin% c('Texture', 'Soil_Sub_Order'))], choice_conc[wq_var])] %>%
    select_if(~ !any(is.na(.))) %>%
    data.frame()
  
  if (choice_conc[wq_var] %in% choice_physics){
    data.glm <- data_merge[c(predictors.keep.physics, choice_yields[wq_var])] %>%
      # mutate(Manure = factor(Manure, levels=c('No Manure', "Manure"))) %>%
      select_if(~ !any(is.na(.))) %>%
      data.frame()
  }
  
  glm_list_conc[[wq_var]]<-bestglm(data.glm, family=gaussian, IC='BIC', nvmax=2)
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

print(model_fig_conc[7])
glm_list_conc[7]
summary(glm_list_conc[[7]])

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


