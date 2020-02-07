# this script will source all scripts required for analyzing percent change for all Edge of Field Sites

#################################
## Get the R packages you need ##
# Not sure which of these are actually needed. 

# load libraries
library(dplyr)
library(Rainmaker)
library(dataRetrieval)
library(USGSHydroTools)
library(lubridate)
library(rnoaa)
library(randomForest)
library(ggplot2)
library(pdp)
library(jtools)
library(caret)
library(bestglm)


#custom function to be moved to other script
not_all_na <- function(x) {!all(is.na(x))}


#Indicate where data are stored
#This currently points at the P:Drive
path_to_data <- "P:/0301"

#Currently outputs are saved here. 
# This can change later when/if results should be saved in a shared location
path_to_results <- "C:/Users/lloken/OneDrive - DOI/EOF_SoilHealth"

# Code largely uses the 'analysis_run_file.R' file that Sam Oliver generated to analyze each site individually
# This code differs in that it processes all the sites serially. 


# load predictors and responses for one site
# Will need to see how variable these are among sites
load(file = 'P:/0301/analysis/WI/WI-SW3/results/2020-01-29-0851/cache/modvars.Rdata')

#Currently the 'clean names' for WI-3 
clean_names <- c('SS load (pounds)', 'Chloride load (pounds)', 
                 'NO2 + NO3 load (pounds)', 'Ammonium load (pounds)', 
                 'TKN load (pounds)', 'Orthophosphate load (pounds)', 
                 'TP load (pounds)', 'TN load (pounds)', 
                 'Org N load (pounds)', 'Peak discharge (cfs)', 'Volume (cf)')


#Load data from P drive. 
#This data is the output from the compilation script
data_df <- readRDS(file=(file_out(file.path(path_to_data, "compiled_data", "storm_event_loads", "storm_event_loads_allsites.rds" ))))

all_sites <- unique(data_df$site)

# Loop through each site and perform the same analysis
#Eventually each of these 'steps' will be there own script. 

site_nu <- "WI-SW3"
# for (site_nu in all_sites){
  
  
  #before step 0
  
  #Subset to only one site and drop all columns with NAs
  dat <- filter(data_df, site == site_nu) %>%
    select_if(not_all_na)
  
  
  #Step 0
  
  # define predictors
  dat.mod <- dat[,predictors]
  dat.mod <- complete.cases(dat.mod)
  
  n.lost <- length(dat.mod[dat.mod == FALSE])
  message(paste0(n.lost, ' observations dropped due to missing predictor data.'))
  
  dat.mod <- dat[dat.mod, ]
  
  # get rid of highly correlated variables
  which.frozen <- which(predictors %in% 'frozen')
  predictors.cor <- cor(dat[,predictors[-which.frozen]], use = 'complete.obs') # drop var "crop" from correlation since it's a categorical var
  names.cor <- row.names(predictors.cor)
  drop.predictors <- caret::findCorrelation(predictors.cor, cutoff = 0.95, verbose = FALSE, exact = TRUE)
  
  predictors.keep <- c(names.cor[-drop.predictors], 'frozen')
  
  # log transform response vars
  dat.mod[,responses] <- log10(dat.mod[,responses])
  sums <- colSums(dat.mod[,responses])
  if(any(is.infinite(sums))) {
    stop('Zeros in the response variables caused values to be infinite when log transformed. Please see code in scripts/data_analysis/0_before_after_datmod_prep.R to debug.', call. = F)
  }
  
  
  
  
  #Step 1
  
  dat.mod.before <- filter(dat.mod, period == 'before')
  
  # save MDC as output from loop
  mdc.perc.nbefore <- c()
  perc.var <- c()
  
  
  # loop through responses to create equation and model
  for (i in 1:length(responses)) {
    
    # create model equation
    mod.equation <- as.formula(paste(responses[i], paste(predictors.keep, collapse = " + "), sep = " ~ "))
    
    # create random forest model - before data
    #mod <- randomForest(mod.equation, data = dat.mod, importance = T, na.action = na.omit)
    mod.before <- randomForest(mod.equation, data = dat.mod.before, importance = T, na.action = na.omit, ntree = 1000)
    
    perc.var[i] <- round(mod.before$rsq[1000]*100, 1)
    
    # calculate minimum detectable change for each constituent based on this model
    
    # pull out mod.before mean squared error
    mse.before <- mod.before$mse[length(mod.before$mse)]
    
    n.before <- nrow(dat.mod.before[dat.mod.before$period == "before", ])
    
    
    
    tval.nbefore <- qt(0.05, n.before + n.before - 2, lower.tail = FALSE)
    
    mdc.nbefore <- tval.nbefore*sqrt((mse.before/n.before) + (mse.before/n.before))
    
    mdc.perc.nbefore[i] <- (1-(10^-mdc.nbefore))*100
    
  }
  # create a dataframe describing the residual models
  mdc <- data.frame(variable = clean_names,
                    model_fit = perc.var,
                    mdc = round(mdc.perc.nbefore, 0))
  

  if (nrow(mdc) == length(responses)) {
    message('Minimum detectable change has been calculated. Results are R object "mdc"')
  } else {
    stop("Somethign went wrong with calculating the minimum detectable change. To debug, see code in scripts/data_analysis/1_mdc_before_after.R.")
  }
  
  
  
  
  
  
  
  # Step 2
  
  dat.mod.before <- filter(dat.mod, period == 'before')
  dat.mod.after <- filter(dat.mod, period == 'after')
  
  # save MDC as output from loop
  
  mdc.perc.nbefore <- c()
  mdc.perc.nafter <- c()
  pval.differences <- c()
  pval.less <- c()
  pval.greater <- c()
  perc.var <- c()
  
  ####################################
  # residual tests - was there a change after BMP implementation?
  # loop through responses to create equation and model
  for (i in 1:length(responses)) {
    
    mod.equation <- as.formula(paste(responses[i], paste(predictors.keep, collapse = " + "), sep = " ~ "))
    
    mod <- randomForest(mod.equation, data = dat.mod, importance = T, na.action = na.omit)
    #mod.before <- randomForest(mod.equation, data = dat.mod.before, importance = T, na.action = na.omit, ntree = 1000)
    
    perc.var[i] <- round(mod$rsq[500]*100, 1)
    
    resid <- dat.mod[, responses[i]] - mod$predicted
    resid.test <- data.frame(resids = resid, 
                             period = dat.mod$period)
    
    #resid.test.after <- data.frame(resids = resid[dat$period == 'after'],
    #                               period = 'after')
    #resid.test.all <- bind_rows(resid.test, resid.test.after)
    
    #diff.test <- t.test(resid.test$resids~resid.test$period,alternative = 'less')
    before.dat <- resid.test$resids[resid.test$period %in% 'before']
    after.dat <- resid.test$resids[resid.test$period %in% 'after']
    diff.test <- wilcox.test(after.dat, before.dat)
    less.test <- wilcox.test(after.dat, before.dat, alternative = 'less')
    greater.test <- wilcox.test(after.dat, before.dat, alternative = 'greater')
    
    pval <- diff.test$p.value
    pval.differences[i] <- pval
    pval.less[i] <- less.test$p.value
    pval.greater[i] <- greater.test$p.value
    
    test.text <- ifelse(pval > 0.05, "No sig. differences between groups", "")
    
    top.vars <- pdp::topPredictors(mod, n = 4)
    
    # pdf(paste0('figures/', site, '_rf_pp_', responses[i], '.pdf'), heigh = 6, width = 6)
    # par(mfcol = c(2,2), mar = c(4,2,2,2), oma = c(2,2,3,0))
    # 
    # for (k in top.vars){
    #   
    #   partialPlot(mod, pred.data = dat.mod, x.var = mod_env$k,
    #               xlab = k, main = "")
    # }
    # mtext(paste0("Partial Dependence plots - ", clean_names[i]), side = 3, outer = T)
    # dev.off()
    
    # change order of levels
    resid.test$period <- factor(resid.test$period, levels = c('before', 'after'))
    ##########
    # now create 4 plots
    # 1-obs vs pred, 2-residual boxplot, 3-residual~fitted, 4-resid~date
    # fig.name = paste0('figures/', site, '_rf_modsum_', responses[i], '.pdf')
    # pdf(fig.name, height = 10, width = 10)
    # layout_matrix <- matrix(c(1:4), nrow=2, ncol=2, byrow=TRUE)
    # layout(layout_matrix)
    # par(mar = c(5,5,3,1), oma = c(0,0,0,0), pch = 16)
    # 
    # ####
    # plot(mod$y ~ mod$predicted,
    #      xlab = "Fitted Values",
    #      ylab = "Observed Values", 
    #      main = paste('log10', clean_names[i]), col = as.factor(resid.test$period))
    # 
    # abline(0,1)
    # 
    # text(x = min(mod$predicted) + 0.2, y = max(mod$y)-0.2, 
    #      labels = paste0('% Var Exp = ', round(mod$rsq[500]*100, 1)), 
    #      col = 'blue', pos = 4)
    # ####
    # temp <- boxplot(resid.test$resids ~ resid.test$period, 
    #                 ylab = 'Residuals', col = c('darkgray', 'red'),
    #                 ylim = c(min(resid), max(resid)*1.3), main = test.text)
    # 
    # # add mdc after calculation back in for plots? 
    # #text(x = 2, y = max(resid)*1.2, labels = paste0("MDC = ", round(mdc.perc.nafter[i],0), "%"), adj=c(0.5, 0))
    # 
    # if (pval < 0.05) {
    #   text(x = 2, y = temp$stats[5,2]*1.1, labels = "*", adj=c(0.5, .5), cex = 3)
    # }
    # 
    # ###
    # plot(resid ~ mod$predicted, 
    #      xlab = "Fitted Values", 
    #      ylab = "Residuals", col = as.factor(resid.test$period))
    # abline(h = 0)
    # ## #
    # plot(resid ~ as.Date(dat.mod$storm_start), col = as.factor(resid.test$period),
    #      xlab = 'Year', ylab = 'Residuals')
    # abline(h = 0, lwd = 2)
    # dev.off()
  }
  
  # create a dataframe describing the residual models
  before_after_resid <- data.frame(variable = clean_names,
                                   perc_var = perc.var,
                                   pvals = round(pval.differences, 2),
                                   pvals_reduction = round(pval.less, 2),
                                   pvals_increase = round(pval.greater, 2))
  # temp_filename <- file.path('data_cached', paste0(site, '_before_after_residual_analysis.csv'))
  # write.csv(file = temp_filename, x = before_after_resid, row.names = F)
  
  # test if the loop actually calculated values
  
  test <- nrow(before_after_resid[!is.na(before_after_resid$perc_var), ])
  
  #if (test != length())
  
  #####################################
  ## now calculate % change if difference
  before.fit <- c()
  after.fit <- c()
  mean.diff <- c()
  mean.diff.sd <- c()
  mean.diff.frozen <- c()
  mean.diff.sd.frozen <- c()
  mean.diff.nonfrozen <- c()
  mean.diff.sd.nonfrozen <- c()
  median.diff <- c()
  diff.sum <- c()
  five.diff <- c()
  ninetyfive.diff <- c()
  pvals.ba <- c()
  load.before <- c()
  load.after <- c()
  
  glmlist<-list()
  mlmlist<-list()
  per.change.list <-list()
  
  for (i in 1:(length(responses)-1)) {
    
    if (pval.differences[i] > 0.1) {
      before.fit[i] <- NA
      after.fit[i] <- NA
      pvals.ba[i] <- NA
      mean.diff[i] <- NA
      median.diff[i] <- NA
      diff.sum[i] <- NA
      five.diff[i] <- NA
      ninetyfive.diff[i] <- NA
      mean.diff.sd[i] <- NA
      mean.diff.frozen[i] <- NA
      mean.diff.sd.frozen[i] <- NA
      mean.diff.nonfrozen[i] <- NA
      mean.diff.sd.nonfrozen[i] <- NA
      load.before[i] <- NA
      load.after[i] <- NA
      next}
    
    mod.equation <- as.formula(paste(responses[i], paste(predictors.keep, collapse = " + "), sep = " ~ "))
    
    mod.before <- randomForest(mod.equation, data = dat.mod.before, importance = T, na.action = na.omit, ntree = 1000)
    mod.after <- randomForest(mod.equation, data = dat.mod.after, importance = T, na.action = na.omit, ntree = 1000)
    
    
    
    
    # #######################################
    # Use top 5 model predictors from before and after to generate a multiple linear model
    # Code is currently nested within the bigger script, which might need to change
    # #########################################
    
    # get top random forest varaibles to use for multilinear models 
    top.vars.before <- pdp::topPredictors(mod.before, n = 5)
    top.vars.after <- pdp::topPredictors(mod.after, n = 5)
    
    top.vars.both <- unique(c(top.vars.before, top.vars.after))
    
    #Build equation for model expression
    mlm.equation <- as.formula(paste(responses[i], paste(top.vars.both, collapse = " + "), sep = " ~ "))

    #Full model all data
    mlm <- lm(mlm.equation, data = dat.mod)

    #subset model
    mlm.step <- step(mlm, direction = 'both', k=3)
    
    #Models based on before/after data separately
    mlm.before <- lm(mlm.equation, data = dat.mod.before)
    mlm.after <- lm(mlm.equation, data = dat.mod.after)
    
    
    #For bestglm, response variable is last column, predictor variables are all except last column
    data.glm <- dat.mod[c(top.vars.both, responses[i])]
    data.glm.before <- dat.mod.before[c(top.vars.both, responses[i])]
    data.glm.after <- dat.mod.after[c(top.vars.both, responses[i])]
    
    glm<-bestglm(data.glm, family=gaussian, IC='BIC', nvmax=5)
    
    glm.before<-bestglm(data.glm.before, family=gaussian, IC='BIC', nvmax=5)
    glm.before
    
    glm.after<-bestglm(data.glm.after, family=gaussian, IC='BIC', nvmax=5)
    glm.after
    
    #Pull R2 from model
    mlm.r2.before <- summary(mlm.before)$r.squared
    glm.r2.before <- summary(glm.before$BestModel)$r.squared
    
    #Use models to predict before and after values
    pred.mlm.before <- predict(mlm.before, dat.mod.before, interval = 'confidence')
    pred.glm.before <- predict(glm.before$BestModel, dat.mod.before, interval='confidence')
    
    pred.mlm.after <- predict(mlm.before, dat.mod.after, interval = 'confidence')
    pred.glm.after <- predict(glm.before$BestModel, dat.mod.after, interval='confidence')
    
    #Bind models predictions with data for plotting
    pred.mlm.before <- data.frame(obs = dat.mod.before[,responses[i]]) %>%
      bind_cols(as.data.frame(pred.mlm.before))
    
    pred.glm.before <- data.frame(obs = dat.mod.before[,responses[i]]) %>%
      bind_cols(as.data.frame(pred.glm.before))
    
    pred.mlm.after <- data.frame(obs = dat.mod.after[,responses[i]]) %>%
      bind_cols(as.data.frame(pred.mlm.after))
    
    pred.glm.after <- data.frame(obs = dat.mod.after[,responses[i]]) %>%
      bind_cols(as.data.frame(pred.glm.after))
    
    #Convert to actual units and make percent change table
    mlm.summary <- colSums(10^pred.mlm.after)
    glm.summary <- colSums(10^pred.glm.after)
    
    per.change.table <- data.frame(matrix(ncol=4, nrow=2))
    names(per.change.table) <- names(mlm.summary)
    names(per.change.table)[1] <- c('model')
    
    per.change.table$model[1] <- 'mlm'
    per.change.table$fit[1] <- round((mlm.summary['obs'] - mlm.summary['fit'])/mlm.summary['obs']*100)
    per.change.table$lwr[1] <- round((-1)*(mlm.summary['upr'] - mlm.summary['obs'])/mlm.summary['obs']*100)
    per.change.table$upr[1] <- round((-1)*(mlm.summary['lwr'] - mlm.summary['obs'])/mlm.summary['obs']*100)
    
    per.change.table$model[2] <- 'glm'
    per.change.table$fit[2] <- round((glm.summary['obs'] - glm.summary['fit'])/glm.summary['obs']*100)
    per.change.table$lwr[2] <- round((-1)*(glm.summary['upr'] - glm.summary['obs'])/glm.summary['obs']*100)
    per.change.table$upr[2] <- round((-1)*(glm.summary['lwr'] - glm.summary['obs'])/glm.summary['obs']*100)
    
    
    
    #Save models and percent change table
    per.change.list[[i]] <- per.change.table
    names(per.change.list)[[i]] <-responses[i]
    
    glmlist[[i]]<-glm
    mlmlist[[i]]<-mlm
    
    
    #Plot simple scatterplots with errorbars to visualize model fits
    mlm.fig.before<-ggplot(data=pred.mlm.before, aes(x=obs, y=fit)) + 
      geom_point() +
      geom_errorbar(aes(x=obs, ymin=lwr, ymax=upr)) +
      geom_abline() +
      theme_bw() +
      # theme(axis.title.x=element_blank()) + 
      labs(y='multi-linear model fit',
           x='') +
      annotate("text", y= min(pred.mlm.before$lwr),
               x =max(pred.mlm.before$obs),
               label="before",hjust=1) +
      annotate("text", y= max(pred.mlm.before$upr),
               x =mean(c(min(pred.mlm.before$obs), max(pred.mlm.before$obs))),label=paste("r2 =", round(mlm.r2.before, 2)),hjust=0.5) 
    
    glm.fig.before<-ggplot(data=pred.glm.before, aes(x=obs, y=fit)) + 
      geom_point() +
      geom_errorbar(aes(x=obs, ymin=lwr, ymax=upr)) +
      geom_abline() +
      theme_bw() +
      labs(y='bestglm fit',
           x='observed value') +
      annotate("text", y= min(pred.glm.before$lwr), 
               x =max(pred.glm.before$obs),
               label="before", hjust=1) +
      annotate("text", y= max(pred.glm.before$upr),
               x =mean(c(min(pred.glm.before$obs), max(pred.glm.before$obs))),label=paste("r2 =", round(glm.r2.before, 2)),hjust=0.5) 
    
    mlm.fig.after<-ggplot(data=pred.mlm.after, aes(x=obs, y=fit)) + 
      geom_point() +
      geom_errorbar(aes(x=obs, ymin=lwr, ymax=upr)) +
      geom_abline() +
      theme_bw() +
      # theme(axis.title.x=element_blank()) + 
      labs(y='multi-linear model fit',
           x='') +
      annotate("text", 
               y= min(pred.mlm.after$lwr), 
               x =max(pred.mlm.after$obs),
               label="after",hjust=1, size=4) +
      annotate("text", 
               y= max(pred.mlm.after$upr),
               x=mean(c(min(pred.mlm.after$obs), max(pred.mlm.after$obs))),
               label=paste0("% change = ", round(per.change.table$fit[1], 2), " (", round(per.change.table$lwr[1], 2), " to ", round(per.change.table$upr[1], 2), ")"),
               hjust=0.5, col="#e41a1c", size=4) 
    
    glm.fig.after<-ggplot(data=pred.glm.after, aes(x=obs, y=fit)) + 
      geom_point() +
      geom_errorbar(aes(x=obs, ymin=lwr, ymax=upr)) +
      geom_abline() +
      theme_bw() +
      labs(y='bestglm fit',
           x='observed value') +
      annotate("text", 
               y= min(pred.glm.after$lwr), 
               x =max(pred.glm.after$obs),
               label="after", hjust=1, size=4) +
      annotate("text", 
               y= max(pred.glm.after$upr),
               x=mean(c(min(pred.glm.after$obs), max(pred.glm.after$obs))),
               label=paste0("% change = ", round(per.change.table$fit[2], 2), " (", round(per.change.table$lwr[2], 2), " to ", round(per.change.table$upr[2], 2), ")"),
               hjust=0.5, col='#e41a1c', size=4) 
    
    
    model.fig.fit <- grid.arrange(grobs=list(mlm.fig.before, mlm.fig.after,  glm.fig.before, glm.fig.after), nrow=2, top=responses[i])
    
    ggsave(paste0('figures/',  responses[i], "_mlmprediction.png"), model.fig.fit, height=6, width=6, units='in')
    

    
    # #############################
    # resume other script
    # ############################
    
    # get residuals from before model for MDC calc
    resid.before <-dat.mod.before[, responses[i]] - mod.before$predicted
    
    pred.before <- predict(mod.before, dat.mod)
    pred.after <- predict(mod.after, dat.mod)
    
    # output model fit stats
    before.fit[i] <- round(mod.before$rsq[1000]*100, 1)
    after.fit[i] <- round(mod.after$rsq[1000]*100, 1)
    
    diff <- (10^pred.before - 10^pred.after)/10^pred.before
    diff.sum[i] <- (sum(10^pred.before) - sum(10^pred.after))/sum(10^pred.before)
    load.before[i] <- sum(10^pred.before)
    load.after[i] <- sum(10^pred.after)
    
    # test if these percent differences are different from zero
    change.test <- t.test(diff, alternative = 'greater')
    pvals.ba[i] <- round(change.test$p.value, 3)
    
    
    diff.frozen <- (pred.before[dat.mod$frozen == TRUE] - pred.after[dat.mod$frozen == TRUE])/pred.before[dat.mod$frozen == TRUE]
    diff.nonfrozen <- (pred.before[dat.mod$frozen == FALSE] - pred.after[dat.mod$frozen == FALSE])/pred.before[dat.mod$frozen == FALSE]
    
    mean.diff[i] <- mean(diff)
    median.diff[i] <- median(diff)
    five.diff[i] <- quantile(diff, 0.05)
    ninetyfive.diff[i] <- quantile(diff, 0.95)
    mean.diff.sd[i] <- sd(diff)
    mean.diff.frozen[i] <- mean(diff.frozen)
    mean.diff.sd.frozen[i] <- sd(diff.frozen)
    mean.diff.nonfrozen[i] <- mean(diff.nonfrozen)
    mean.diff.sd.nonfrozen[i] <- sd(diff.nonfrozen)
  }
  
  # create data frame of values
  perc_reduction <- data.frame(response = responses[-length(responses)],
                               response_clean = clean_names[-length(responses)],
                               before_r2 = before.fit,
                               after_r2 = after.fit,
                               perc_diff = round(mean.diff*100, 1),
                               sd_perc_diff = round(mean.diff.sd*100,1),
                               median_diff = round(median.diff*100, 1),
                               fifth_diff = round(five.diff*100, 1),
                               ninetyfifth_diff = round(ninetyfive.diff*100, 1),
                               diff_sum = round(diff.sum*100, 1),
                               load_before = round(load.before, 0),
                               load_after = round(load.after, 0),
                               pval = pvals.ba,
                               perc_diff_frozen = round(mean.diff.frozen*100, 1),
                               sd_perc_diff_frozen = round(mean.diff.sd.frozen*100,1),
                               perc_diff_nonfrozen = round(mean.diff.nonfrozen*100, 1),
                               sd_perc_diff_nonfrozen = round(mean.diff.sd.nonfrozen*100, 1))
  
  # temp_filename <- file.path('data_cached', paste0(site, '_percent_reduction_before_after.csv'))
  # write.csv(perc_reduction, temp_filename, row.names = F)
  
  
  
  #end
# }

# source the mod.dat prep step that will feed into both MDC calculations and modeling
# source('scripts/data_analysis/0_before_after_datmod_prep.R', echo = F, local = mod_env)
# source('scripts/data_analysis/1_mdc_before_after.R', echo = F, local = mod_env)
# source('scripts/data_analysis/2_before_after_mods.R', echo = F, local = mod_env)


