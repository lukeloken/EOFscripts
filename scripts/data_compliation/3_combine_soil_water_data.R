
library(pls)

#load storm data
data_df3 <- readRDS(file=(file_in(file.path(path_to_data, "compiled_data", "storm_event_loads", "storm_event_conc_allsites_model.rds" ))))

#Site order. Put New York and Wisconsin next to each other because they both receive manure
site_order <- as.character(unique(data_df3$Site))
site_order <- c(site_order[grepl('IN', site_order)],
                site_order[grepl('MI', site_order)],
                site_order[grepl('OH', site_order)],
                site_order[grepl('NY', site_order)],
                site_order[grepl('WI', site_order)])

data_WQformerge <- data_df3 %>%
  mutate(wateryear = as.numeric(as.character(wateryear))) %>%
  filter(wateryear %in% c(2016:2018), frozen == 0) %>%
  # rename(Site = site) %>%
  mutate(Site = factor(Site, site_order),
         state = factor(state, c('IN', 'MI', 'OH', 'NY', 'WI'))) %>%
  arrange(Site, storm_middate) 

#Runoff index among sites/years
rain_flow_out2 <- readRDS(file.path(path_to_data, "compiled_data", "rain", "annual_rain_flow_totals.rds")) %>%
  filter(wateryear %in% c(2016:2018)) %>%
  group_by(site, Area_acres) %>%
  summarize(runoff_index_mean = mean(runoff_index, na.rm = TRUE),
            runoff_index_combined = mean(runoff_volume_cf, na.rm = TRUE)/mean(rain_volume_cf, na.rm = TRUE)) %>%
  mutate(diff = runoff_index_mean - runoff_index_combined) %>%
  mutate(log10_Area = log10(Area_acres))

#load farm data
farm_management <- read.csv(file_in(file.path(path_to_data, "SiteCharacteristics", "EOF_WatershedAreas.csv")), header=T, stringsAsFactors = F) %>%
  select(-Area_acres, -Slope)

#load area and slope data
gis_combined <- readRDS(file.path(path_to_data, 'compiled_data', 'GIS_Compiled.rds')) %>%
  select(site, elevation_STD, elevation_RANGE, 
         slope_MEAN, slope_MAX, roughness_MEAN)


#Load soil data
soil_0_15 <- readRDS(file=file_in(file.path(path_to_data, 'soil', 'cleaned_data', 'Soil2016_0_to_15cm.rds'))) %>%
  mutate(TT_class = factor(TT_class, c('SiCl', 'ClLo', 'Lo', 'SiLo'))) %>%
  select(-Manure, -Slope) %>%
  left_join(farm_management, by = c("Site")) %>%
  left_join(gis_combined, by = c("Site" = "site")) %>%
  select(-Type)

#Load new soil data
soil_new <- read.csv(file.path(path_to_results, "Data", "Spring_Fall_medians_16_17_18.csv")) %>%
  filter(Year %in% 2016:2017) %>%
  group_by(Site) %>%
  summarize(across(.cols = -Year, median, na.rm = TRUE)) %>%
  select(-OM, -Fall_Bulk_Den) %>%
  mutate(Site = gsub("-0", "-SW", Site)) # %>%
  # left_join(farm_management, by = c("Site")) %>%
  # left_join(gis_combined, by = c("Site" = "site")) %>%
  # select(-Type)


soil_tomerge <- soil_0_15 %>%
  ungroup() %>%
  select(Site, Residue_Cover:Soil_Sub_Order, wateryear:roughness_MEAN)
    
names(soil_new) <- gsub("_015", "", names(soil_new))

soil_new <- soil_new %>%
  full_join(soil_tomerge) %>%
  left_join(select(rain_flow_out2, site, log10_Area), by = c("Site" = "site"))

head(data.frame(soil_new))

# Variable groups
# good_soil_vars <- c("OM_spring", "total_P", "Bray_P", "POXC",
#                     "percent_N", "percent_C", "Basal_N20", "Basal_CO2",
#                     "SIR1_CO2", "SIR4_CO2", "SIR1_4", "WE_TC",
#                     "WE_TN", "WE_TOC", "WEP", "AP_nmol",
#                     "BG_nmol", "NAG_nmol", "Bulk_Den_spring", "P_Clay",
#                     "P_Sand", "P_Silt", "Residue_Cover", "Texture",
#                     "Soil_Sub_Order", "Kfs", "FinalFlux", "SA_1.2",
#                     "SA_2.8", "Penotrometer_0_6", "Penotrometer_6_18", "TT_class",
#                     "Manure", "Tile", "Till", "elevation_STD",
#                     "elevation_RANGE", "slope_MEAN", "slope_MAX")
# soil_vars <- intersect(good_soil_vars, names(soil_0_15))

good_soil_vars_new <- c("OM", "POXC", "percent_TOC", "WE_TOC", "WE_TC",
                        "total_P", "BrayP", "WEP", 
                        "percent_N", "WE_TN", 
                        "Bulk_Den", "PClay", "PSand", "PSilt", 
                        "TT_class", "Soil_Sub_Order",
                        "Residue_Cover", "log_Kfs", "FinalFlux",
                        "SA_1_2", "SA_2_8",
                        "Compaction_0_15", "Compaction_15_46", 
                        "elevation_STD",  "slope_MEAN", "log10_Area",
                        "SIR1_CO2", "SIR4_CO2", 
                        "Basal_CO2", "NAG", "AcidP", "BetaG", 
                        "Manure", "Tile", "Till")


soil_vars <- intersect(good_soil_vars_new, names(soil_new))

if (length(setdiff(good_soil_vars_new, names(soil_new))) > 0){
  warning("not all good variables are in soil data.frame")
}


wq_vars <- c(loadvars, concvars, yieldvars, yieldperweqvars)

wq_weightedvars <- wq_vars[which(wq_vars %notin% 
                                   c('runoff_volume',
                                     'runoff_volume_cubicfootperAcre', 
                                     'runoff_cubicmeter_percubicmeterWEQ'))]

#collapse data to only water year with soil data
#Compute annual metrics

data_merge_median <- data_WQformerge %>%
  dplyr::select(wateryear, Site, all_of(wq_vars), peak_discharge, rain) %>%
  group_by(Site) %>%
  summarize_at(all_of(wq_vars), .funs=median, na.rm=T) %>%
  left_join(soil_new, by = "Site") #%>%
# drop_na(Manure) 

data_merge_2016 <- data_WQformerge %>%
  dplyr::select(wateryear, Site, all_of(wq_vars),
                peak_discharge, runoff_volume, rain) %>%
  group_by(Site) %>%
  summarize_at(all_of(wq_vars), .funs=c(median, mean), na.rm=T) %>%
  left_join(soil_new, by = "Site") #%>%
# drop_na(Manure) 


#Calculate flow weighted mean concentration
data_merge2 <- data_WQformerge %>%
  dplyr::select(wateryear, Site, all_of(wq_vars), peak_discharge, rain) %>%
  group_by(Site) %>%
  summarise(across(all_of(concvars), 
               ~ weighted.mean(., runoff_volume, na.rm=T)), 
            .groups = "drop") %>%
  left_join(soil_new, by = "Site") %>%
  # drop_na(Manure) %>%
  # left_join(data_merge_median[c('Site', 'runoff_volume', 'runoff_volume_cubicfootperAcre', 
  #                               'runoff_cubicmeter_percubicmeterWEQ')]) %>%
  left_join(data_merge_median[c('Site', 'runoff_volume')], by = "Site") %>%
  mutate_if(is.character, as.factor)

#Experimenting with runoff index calculations
data_merge_runofftotal <- data_WQformerge %>%
  mutate(weq_volume_m3 = weq*Area_acres*4046.86/39.3701,
         runoff_volume_m3 = runoff_volume/35.3147) %>%
  dplyr::select(Site, Type, weq_volume_m3, runoff_volume_m3) %>%
  group_by(Site, Type) %>%
  na.omit() %>%
  summarize_all(sum) %>%
  mutate(Runoff_Index = runoff_volume_m3/weq_volume_m3) %>%
  select(-weq_volume_m3, -runoff_volume_m3)

data_merge_runofftotal

ggplot(data_merge_runofftotal, aes(y=Runoff_Index, x=Site, fill=Type)) + 
  geom_bar(stat='identity') +
  theme(axis.text.x=element_text(angle=90))

data_merge_runofftotal_new <- data_merge_runofftotal %>%
  full_join(rain_flow_out2, by = c("Site" = "site")) %>%
  select(Site, Type, Runoff_Index = runoff_index_combined)

ggplot(data_merge_runofftotal_new, aes(y=Runoff_Index, x=Site, fill = Type)) + 
  geom_bar(stat='identity') +
  theme(axis.text.x=element_text(angle=90))


#Calculating yields by summing first (equivilent to mean then divide)
data_merge_loadtotal <- data_WQformerge %>%
  mutate(weq_volume_m3 = weq*Area_acres*4046.86/39.3701) %>%
  select(Site, Type, all_of(loadvars), weq_volume_m3) %>%
  group_by(Site, Type) %>%
  # na.omit() %>%
  summarize_all(mean, na.rm=T) 

data_merge_yieldtotal <- data.frame(sapply(data_merge_loadtotal[,loadvars], function (x) x*453.592/data_merge_loadtotal$weq_volume_m3)) %>%
  select(-runoff_volume) 

names(data_merge_yieldtotal) <- gsub("_load_pounds", "_yield_mgL", names(data_merge_yieldtotal))

data_merge_yieldtotal$doc_yield_mgL[which(data_merge_yieldtotal$doc_yield_mgL==0)] <- NaN
data_merge_yieldtotal$toc_yield_mgL[which(data_merge_yieldtotal$toc_yield_mgL==0)] <- NaN

data_merge3 <- bind_cols(data_merge_runofftotal_new, data_merge_yieldtotal) %>%
  filter(Site %in% data_merge2$Site) %>%
  right_join(data_merge2, by = "Site") %>%
  group_by() %>%
  mutate(Site = factor(Site, site_order)) %>%
  arrange(Site)

data.frame(data_merge3)

data_merge <- data_merge3 %>%
  filter(Type == 'SW') 

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


choice_yields <- c(names(data_merge_yieldtotal)[1:9], 'Runoff_Index')

# choice_yields <-  yieldperweqvars[c(1:9,12)]
choice_conc <- concvars[1:9]
choice_physics <- c("suspended_sediment_conc_mgL", "suspended_sediment_yield_mgL", "Runoff_Index")

# choice_physics <- c("suspended_sediment_conc_mgL", "suspended_sediment_yield_poundperAcreperInchWEQ", 
# "runoff_cubicmeter_percubicmeterWEQ")

#If need to subset soil vars
choice_soil <-  soil_vars[]

predictors.cor <- cor(select_if(data_merge[,choice_soil], is.numeric), use = 'complete.obs') 
names.cor <- row.names(predictors.cor)
drop.predictors <- caret::findCorrelation(predictors.cor, cutoff = 0.95, verbose = FALSE, exact = TRUE)
predictors.keep <- c(names.cor[-drop.predictors], 'Manure')

# predictors.keep <- c(choice_soil, 'Manure')

# predictors.keep.physics <- soil_vars[c(19:23, 26:31, 34:40)]

predictors.keep.physics <- intersect(good_soil_vars_new, 
                                     c("Bulk_Den", "PClay", "PSand", 
                                       "PSilt", "Residue_Cover", "log_Kfs",
                                       "FinalFlux", "SA_1_2", "SA_2_8",
                                       "Compaction_0_15", "Compaction_15_46", "Tile",
                                       "Till", "elevation_STD",  "slope_MEAN", "log10_Area"))

# Partial least squares (library(pls))
# multi linear regression, glm, etc. 

wq_var<-1
plot_list_yield <- glm_list_yield <- list()
model_fig_yield <- model_data_yield <- list()

pls_list_yield <- pca_list_yield <- pls_plot_yield <- list()
glm_best_yield <- best_fig_plot_yield <- best_var_fig_yield <- list() 
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
    guides(color=guide_legend(ncol=1, title.position="top"))
  
  var_list[[length(choice_soil)+1]] <- g_legend(plot_withLegend)
  
  plot_list_yield[[wq_var]] <- grid.arrange(grobs=var_list, ncol=5, top=choice_yields[wq_var])
  
  ggsave(file=file_out(file.path(path_to_results, 'Figures', 'Soil', 'SoilWQ_Scatter', paste(choice_yields[wq_var], ".png"))) , plot_list_yield[[wq_var]], height=10, width=7)
  
  
  # data.glm <- data_merge[c(choice_soil, choice_yields[wq_var])] %>%
  #   select_if(~ !any(is.na(.))) %>%
  #   data.frame()
  
  data.glm <- data_merge[c(soil_vars[which(soil_vars %notin% c('Texture', 'Soil_Sub_Order', 'TT_class'))], choice_yields[wq_var])] %>%
    mutate(Manure = factor(Manure, levels=c('No Manure', "Manure"))) %>%
    select(all_of(c(predictors.keep, choice_yields[wq_var]))) %>%
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
  
  #Extract glm predictors
  coefficients_i <- glm_list_yield[[wq_var]]$BestModel$coefficients
  names_i <- names(coefficients_i)
  pvalues_i <- as.numeric(print(glm_list_yield[[wq_var]])[,4])
  
  table_i <- data.frame(variable = names_i,
                        estimate = coefficients_i,
                        p_value = pvalues_i, 
                        row.names = NULL)
  glm_best_yield[[wq_var]] <- table_i
  
  original_names <- gsub("TRUE", "", names_i[-1])
  
  best_var_fig_i <- var_list[which(choice_soil %in% original_names)] 
  
  best_fig_plot_yield <- grid.arrange(grobs=best_var_fig_i, nrow=1, left = choice_yields[wq_var])
  
  best_var_fig_yield[[wq_var]] <- best_var_fig_i
  
  ggsave(file=file_out(file.path(path_to_results, 'Figures', 'Soil', 'BestPredictors', paste0("BestPredictors_", choice_yields[wq_var], ".png"))), best_fig_plot_yield, height=4, width=8)
  
  #pls
  pls_formula <- formula(paste(choice_yields[wq_var], " ~ ", 
                               paste(names(data.glm)[1:(ncol(data.glm)-1)], collapse = "+") ))
  pca_list_yield[[wq_var]] <- pcr(pls_formula ,  data=data.glm, ncomp=4, validation='LOO')
  
  pls_list_yield[[wq_var]] <- plsr(pls_formula ,  data=data.glm, ncomp=4, validation='LOO')
  print(choice_yields[wq_var])
  pls_list_yield[[wq_var]]
  summary(pls_list_yield[[wq_var]])
  plot(pls_list_yield[[wq_var]], plottype='validation')
  plot(pls_list_yield[[wq_var]], asp = 1, line=TRUE, ncomp=2)
  plot(pls_list_yield[[wq_var]], plottype = "scores", comps = 1:4)
  explvar(pls_list_yield[[wq_var]])
  
  plot(pls_list_yield[[wq_var]], "loadings", comps = 1:2, legendpos = "topright", xaxt='n', lwd=2)
  # abline(v=1:ncol(data.glm), lty=2, col='grey', lwd=.5)
  axis(1, at=1:(ncol(data.glm)-1), labels = names(data.glm)[1:(ncol(data.glm)-1)], las=2, cex.axis=.75)
  
  ncomp.onesigma <- selectNcomp(pls_list_yield[[wq_var]], method = "onesigma", plot = TRUE)
  ncomp.permut <- selectNcomp(pls_list_yield[[wq_var]], method = "randomization", plot = TRUE)
  
  gas2.cv <- crossval(pls_list_yield[[wq_var]], segments = 10)
  plot(MSEP(gas2.cv), legendpos="topright")
  
  
}


names(glm_list_yield)<-choice_yields
glm_list_yield
print(model_fig_yield)

#Similar figures for concentration
wq_var<-1
plot_list_conc <-list()
glm_list_conc <- list()
glm_best_conc <- list()
model_fig_conc <- list()
model_data_conc <- list()
best_var_fig <- list()
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
  
  ggsave(file=file_out(file.path(path_to_results, 'Figures', 'Soil', 'SoilWQ_Scatter', paste(choice_conc[wq_var], ".png"))) , plot_list_conc[[wq_var]], height=10, width=7)
  
  
  data.glm <- data_merge[c(soil_vars[which(soil_vars %notin% c('Texture', 'Soil_Sub_Order', 'TT_class'))], choice_conc[wq_var])] %>%
    select(all_of(c(predictors.keep, choice_conc[wq_var]))) %>%
    select_if(~ !any(is.na(.))) %>%
    data.frame()
  
  if (choice_conc[wq_var] %in% choice_physics){
    data.glm <- data_merge[c(predictors.keep.physics, choice_conc[wq_var])] %>%
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
  
  #Extract glm predictors
  coefficients_i <- glm_list_conc[[wq_var]]$BestModel$coefficients
  names_i <- names(coefficients_i)
  pvalues_i <- as.numeric(print(glm_list_conc[[wq_var]])[,4])
  
  table_i <- data.frame(variable = names_i,
                        estimate = coefficients_i,
                        p_value = pvalues_i, 
                        row.names = NULL)
  glm_best_conc[[wq_var]] <- table_i
  
  original_names <- gsub("TRUE", "", names_i[-1])
  
  best_var_fig_i <- var_list[which(choice_soil %in% original_names)] 
  
  best_fig_plot <- grid.arrange(grobs=best_var_fig_i, nrow=1, left = choice_conc[wq_var])
  
  best_var_fig[[wq_var]] <- best_var_fig_i
  
  ggsave(file=file_out(file.path(path_to_results, 'Figures', 'Soil', 'BestPredictors', paste0("BestPredictors_", choice_conc[wq_var], ".png"))), best_fig_plot, height=4, width=8)
  
}

names(glm_list_conc)<-choice_conc
glm_list_conc
print(model_fig_conc)

grid.arrange(grobs=best_var_fig[[3]], nrow=1, left = choice_conc[3])

print(model_fig_conc[1])
glm_list_conc[1]
summary(glm_list_conc[[1]])

wq_var <-1
fig_list <- list()
for (wq_var in 1:length(choice_conc)) {
  
  fig_list[[wq_var]] <- ggplot(data=data_merge, aes_string(x=choice_yields[wq_var], y=choice_conc[wq_var])) +
    scale_color_brewer(palette='Dark2') + 
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







png(file_out(file.path(path_to_results, "Figures", "Soil", "Spring2016_TextureTriangle_RI.png")), res=400, width=7, height=7, units='in')

TT.plot(tri.data=data.frame(data_merge3[which(!is.na(data_merge3$P_Clay)),]), class.sys='USDA.TT', css.names=c('P_Clay', 'P_Silt', 'P_Sand'), col='black', main='', z.name='Runoff_Index', tri.sum.tst	= FALSE)

dev.off()

TT.plot(tri.data=data.frame(data_merge3[which(!is.na(data_merge3$P_Clay)),]), class.sys='USDA.TT', css.names=c('P_Clay', 'P_Silt', 'P_Sand'), col='black', main='', z.name='suspended_sediment_yield_mgL', tri.sum.tst	= FALSE)


#Plot water quality data for the time period included with soil links
yieldperweqvars
concvars

#plot runoff index
runoff_index_box <- ggplot(filter(data_WQformerge, type == "SW"), aes(x=Site, y=runoff_cubicmeter_percubicmeterWEQ, fill=state)) +
  geom_hline(yintercept = 1) +
  geom_jitter(width=.1, height=0, aes(col=state), alpha=.2, shape=16) + 
  geom_boxplot(size=.5, aes(alpha=type_binary), outlier.shape=NA) +
  geom_point(data = filter(data_merge3, Type == "SW"), aes(x=Site, y=Runoff_Index), pch=17, size=1.5, inherit.aes = F) + 
  scale_alpha(range=c(1,0), guide='none') + 
  scale_y_log10nice(name = "Runoff Index") +
  scale_color_brewer(palette='Set1') + 
  scale_fill_brewer(palette='Set1') + 
  # scale_y_log10nice(name = "runoff index", limits=c(.0003, 100)) +
  theme_bw() +
  theme(legend.position='bottom', axis.title.x=element_blank(), panel.grid.minor=element_blank()) +
  theme(axis.text.x = element_text(angle=45, hjust=1), plot.title=element_text(size=10)) +
  ggtitle("Wateryears 2016-2018, Non-frozen runoff events")

print(runoff_index_box)

ggsave(file.path(path_to_results, "Figures", "RunoffIndex2016_2018_Boxplots.png"), runoff_index_box,
       width = 4, height = 3, units = "in")


#Loop through yield vars and plot boxplot by site
yieldvar=1
yield_boxlist <- list()
for (yieldvar in 1:length(choice_yields[-10])){
  yield_boxlist[[yieldvar]] <- ggplot(filter(data_WQformerge, type == "SW"), aes_string(x='Site', y=choice_yields[yieldvar], fill='state')) +
    geom_jitter(width=.1, height=0, aes(col=state), alpha=.2, shape=16) + 
    geom_boxplot(size=.5, aes(alpha=type_binary), outlier.shape=NA) +
    geom_point(data=filter(data_merge3, Type == "SW"), aes_string(x='Site', y=choice_yields[yieldvar]),
               pch=17, size=2, inherit.aes = F) + 
    # scale_alpha(range=c(.5,0)) + 
    scale_alpha(range=c(1,0), guide='none') +
    # scale_color_discrete(guide='none') + 
    scale_color_brewer(palette='Set1', guide='none') + 
    scale_fill_brewer(palette='Set1') + 
    # scale_y_log10nice(name = "suspended sediment (mg/L)", limits = c(.5, 20000)) +
    scale_y_log10nice(name = choice_yields[yieldvar]) +
    theme_bw() +
    theme(legend.position='none', panel.grid.minor=element_blank(), axis.title.x=element_blank()) +
    theme(axis.text.x = element_text(angle=45, hjust=1))
  
  # print(yield_boxlist[[yieldvar]])
  
}


#Loop through conc vars and plot boxplot by site
concvar=1
conc_boxlist <- list()
for (concvar in 1:length(choice_conc)){
  conc_boxlist[[concvar]] <-ggplot(filter(data_WQformerge, type == "SW"), aes_string(x='Site', y=choice_conc[concvar], fill='state')) +
    geom_jitter(width=.1, height=0, aes(col=state), alpha=.2, shape=16) + 
    # scale_color_discrete(guide='none') + 
    scale_color_brewer(palette='Set1', guide='none') + 
    scale_fill_brewer(palette='Set1') + 
    geom_boxplot(size=.5, aes(alpha=type_binary), outlier.shape=NA) +
    geom_point(data=filter(data_merge3, Type == "SW"), aes_string(x='Site', y=choice_conc[concvar]), 
               pch=17, size=2, inherit.aes = F) + 
    # scale_alpha(range=c(.5,0)) + 
    scale_alpha(range=c(1,0), guide='none') +
    # scale_y_log10nice(name = "suspended sediment (mg/L)", limits = c(.5, 20000)) +
    scale_y_log10nice(name = choice_conc[concvar]) +
    theme_bw() +
    theme(legend.position='none', panel.grid.minor=element_blank(), axis.title.x=element_blank()) +
    theme(axis.text.x = element_text(angle=45, hjust=1))
  
  # print(conc_boxlist[[concvar]])
  
}


yield_withlegend <- yield_boxlist[[2]] + 
  # theme(legend.position="bottom", legend.title=element_blank()) +
  theme(legend.position="bottom") +
  guides(fill = guide_legend(nrow = 1, title.position='top', title.hjust=0.5)) +
  labs(fill="Wateryears 2016-2018, Non-frozen runoff events")


yieldlegend<-g_legend(yield_withlegend)

rm(yield_withlegend)

yield_box_grid <- grid.arrange(grobs=yield_boxlist, ncol=3, as.table = F)

png(file_out(file.path(path_to_results, "Figures", "Yield2016_2018_Boxplots.png")), res=400, width=12, height=9, units='in')

grid.arrange(grobs=list(yield_box_grid,yieldlegend), ncol=1, heights=c(18,1))

dev.off()




conc_withlegend <- conc_boxlist[[2]] + 
  theme(legend.position="bottom") +
  guides(fill = guide_legend(nrow = 1, title.position='top', title.hjust=0.5)) +
  labs(fill="Wateryears 2016-2018, Non-frozen runoff events")

conclegend<-g_legend(conc_withlegend)

rm(conc_withlegend)

conc_box_grid <- grid.arrange(grobs=conc_boxlist, ncol=3, as.table = F)



png(file_out(file.path(path_to_results, "Figures", "Conc2016_2018_Boxplots.png")), res=400, width=12, height=9, units='in')

grid.arrange(grobs=list(conc_box_grid,conclegend), ncol=1, heights=c(18,1))

dev.off()



yield_boxlist_select <- yield_boxlist[c(1, 7, 8)]

yield_boxlist_select[[1]] <- yield_boxlist_select[[1]] + 
  theme(axis.text.x = element_blank()) +
  scale_y_log10nice(name = expression(paste("Suspended sediment (mg L"^"-1", ")")))

yield_boxlist_select[[2]] <- yield_boxlist_select[[2]] + 
  theme(axis.text.x = element_blank()) +
  scale_y_log10nice(name = expression(paste("Total P (mg P L"^"-1", ")")))
  

yield_boxlist_select[[3]] <- yield_boxlist_select[[3]] + 
  scale_y_log10nice(name = expression(paste("Total N (mg N L"^"-1", ")")))

yield_box_grid_select <- grid.arrange(grobs=yield_boxlist_select, ncol=1, as.table = F)

png(file_out(file.path(path_to_results, "Figures", "yield2016_2018_Boxplots_SelectVars.png")), res=400, width=4, height=8, units='in')

grid.arrange(grobs=list(yield_box_grid_select,yieldlegend), ncol=1, heights=c(18,1), top = "Yield")

dev.off()



conc_boxlist_select <- conc_boxlist[c(1, 7, 8)]

conc_boxlist_select[[1]] <- conc_boxlist_select[[1]] + 
  theme(axis.text.x = element_blank()) +
  scale_y_log10nice(name = expression(paste("Suspended sediment (mg L"^"-1", ")")))

conc_boxlist_select[[2]] <- conc_boxlist_select[[2]] + 
  theme(axis.text.x = element_blank()) +
  scale_y_log10nice(name = expression(paste("Total P (mg P L"^"-1", ")")))


conc_boxlist_select[[3]] <- conc_boxlist_select[[3]] + 
  scale_y_log10nice(name = expression(paste("Total N (mg N L"^"-1", ")")))

conc_box_grid_select <- grid.arrange(grobs=conc_boxlist_select, ncol=1, as.table = F)

png(file_out(file.path(path_to_results, "Figures", "Conc2016_2018_Boxplots_SelectVars.png")), res=400, width=4, height=8, units='in')

grid.arrange(grobs=list(conc_box_grid_select,conclegend), ncol=1, heights=c(18,1), top = "Concentration")

dev.off()


