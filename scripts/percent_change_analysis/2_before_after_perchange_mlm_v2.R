

#Script to estimate percent change
#Multiple methods are included and commented out

#Quantile regression forests

#bestglm

#Multi-linear model
#Use random forest of pre/post data to select highest 5 predictors of each to seed multi linear model
#Also use these variables to seed a bestglm model to reduce number of predictors
#Use these predictors to model 'pre' data
#Visualize model fit in figure and report r2 of model performance
#Using model build on 'pre' data predict loads for 'post' data
#Include confidence intervals using 95% from function predict()
#plot predictions versus observations to visualize differences
#Convert log values to actual loads and sum (observations versus predictions)
#Calculate percent change (obs - pred)/obs
#upper and lower bounds use confidence intervals from predict

#Split data into pre/post
dat.mod.before <- filter(dat.mod, period == 'before')
dat.mod.after <- filter(dat.mod, period == 'after')

#skip analysis if less than 10 observations in either dataset
if (nrow(dat.mod.after)==0 | nrow(dat.mod.before)==0) {
  message(paste0("No Pre/Post analysis for ", toString(site_name), ". Either pre/post data are missing."))
  next
}

if (nrow(dat.mod.before) < 10) {
  message(paste0("Fewer than 10 storm events in 'before' period for ", toString(site_name), " Skipping"))
  next
  }

if (nrow(dat.mod.after) < 10) {
  message(paste0("Fewer than 10 storm events in 'after' period for ", toString(site_name), " Skipping"))
  next
  }

# save MDC as output from loop

mdc.perc.nbefore <- c()
mdc.perc.nafter <- c()
pval.differences <- c()
pval.less <- c()
pval.greater <- c()
perc.var <- c()

####################################
# residual tests - was there a change after BMP implementation?
# Copy and paste from prior data_anaylsis script
# loop through responses to create equation and model
i=11
for (i in 1:length(responses)) {
  
  mod.equation <- as.formula(paste(responses[i], paste(predictors.keep, collapse = " + "), sep = " ~ "))
  
  mod <- randomForest(mod.equation, data = dat.mod, importance = T, na.action = na.omit, ntree=1000)
  #mod.before <- randomForest(mod.equation, data = dat.mod.before, importance = T, na.action = na.omit, ntree = 1000)
  # print(randomForest::varImpPlot(mod))
  
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
  
  pval.differences[i] <- diff.test$p.value
  pval.less[i] <- less.test$p.value
  pval.greater[i] <- greater.test$p.value
  
  top.vars <- pdp::topPredictors(mod, n = 4)
  

  # change order of levels
  resid.test$period <- factor(resid.test$period, levels = c('before', 'after'))

}

# create a dataframe describing the residual models
before_after_resid <- data.frame(variable = responses,
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
#lots of these are copy/paste from orig script
# ####################################
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

# glmlist<-list()
# mlmlist<-list()
# modlist <- list()
quanlist_before <-list()
quanlist_after <-list()
per.change.list <-list()

i=3
for (i in 1:(length(responses))) {
  
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
  
  
  # ###########################
  # Random forest
  # ###########################
  
  mod.equation <- as.formula(paste(responses[i], paste(predictors.keep, collapse = " + "), sep = " ~ "))

  mod.before <- randomForest(mod.equation, data = dat.mod.before, importance = T, na.action = na.omit, ntree = 1000)
  mod.after <- randomForest(mod.equation, data = dat.mod.after, importance = T, na.action = na.omit, ntree = 1000)
  
  
  # ###########################
  # Quantile regression forest
  # ###########################
  
  #Make input data (x are dataframes, y is a vector)
  x.quan <- dat.mod %>%
    select(predictors.keep)
  
  y.quan <- dat.mod %>%
    select(responses[i])
  
  x.quan.before <- dat.mod.before %>%
    select(predictors.keep)
  
  y.quan.before <- dat.mod.before %>%
    select(responses[i])
  
  x.quan.after <- dat.mod.after %>%
    select(predictors.keep)
  
  y.quan.after <- dat.mod.after %>%
    select(responses[i])
  
  #Make quantile regression forest using pre and post data
  quan.before <- quantregForest(x=x.quan.before, y=y.quan.before[,1], importance = T, na.action = na.omit, ntree = 1000)
  quan.after <- quantregForest(x=x.quan.after, y=y.quan.after[,1], importance = T, na.action = na.omit, ntree = 1000)
  
  # print(randomForest::varImpPlot(quan.before))
  # print(randomForest::varImpPlot(quan.after))
  
  quan.r2.before <- round(quan.before$rsq[500], 2)
  quan.r2.after <- round(quan.after$rsq[500], 2)
  
  #Predict loads using before model and all data
  predictConditionalQuantiles.before <- predict(quan.before, x.quan, what=seq(0.05, .95, .05))
  predictConditionalMean.before <- predict(quan.before, x.quan, what=mean)
  predictConditionalSd.before <- predict(quan.before, x.quan, what=sd)

  #Predict loads using after model and all data
  predictConditionalQuantiles.after <- predict(quan.after, x.quan, what=seq(0.05, .95, .05))
  predictConditionalMean.after <- predict(quan.after, x.quan, what=mean)
  predictConditionalSd.after <- predict(quan.after, x.quan, what=sd)
  
  #Organize for plotting
  predict.table.before <- data.frame(predictConditionalQuantiles.before) %>%
    bind_cols(obs = y.quan[,1]) %>%
    bind_cols(mean = predictConditionalMean.before) %>%
    bind_cols(sd = predictConditionalSd.before) 
  
  names(predict.table.before) <- paste0(names(predict.table.before), "_before")
  
  predict.table.after <- data.frame(predictConditionalQuantiles.after) %>%
    bind_cols(obs = y.quan[,1]) %>%
    bind_cols(mean = predictConditionalMean.after) %>%
    bind_cols(sd = predictConditionalSd.after)
  
  names(predict.table.after) <- paste0(names(predict.table.after), "_after")
  
  
  if (nrow(predict.table.before) != nrow(predict.table.after)){
    warning("Before and After data have different lengths. Problem!")
  }
  
  predict_join <- bind_cols(predict.table.before, predict.table.after) %>%
    mutate(period = dat.mod$period)
  
  predict_gather <- predict_join %>%
    select(period, obs_before, quantile..0.5_before, quantile..0.5_after) %>%
    dplyr::rename(obs = obs_before,
           before_mod = quantile..0.5_before, 
           after_mod = quantile..0.5_after) %>%
    gather(key = model, value=pred, 3:4) %>%
    mutate(model = factor(model, c('before_mod', 'after_mod')),
           period = factor(period, c('before', 'after')))
  
  
  
  model_biplots <- ggplot(predict_gather, aes(x=obs, y=pred, color=model, shape = period, linetype=period)) +
    scale_shape_manual(values=c(16,1)) +
    scale_color_discrete(guide=F) + 
    scale_linetype() + 
    geom_smooth(method='lm', se=F) + 
    geom_point(size=2) +
    theme_bw() +
    geom_abline() +
    labs(x='observed load', y='predicted load') +
    guides(linetype = guide_legend(override.aes= list(color = "black"))) + 
    theme(legend.position ='bottom') + 
    facet_wrap(~model) +
    ggtitle(paste(site_name, responses[i]))
  
  
  ggsave(file.path(path_to_results, 'Figures', 'PercentChange', site_name, paste0(site_name, "_", responses[i], "_modelfits.png")), model_biplots, height=3.5, width=5, units='in')
  

  
  #Calculate percent differnece between before and after models for every event 
  #Calcualte the median percent difference among all events (center)
  #Calcualte median absolute deivation (mad) among all events (error bars)
  
  
  # perdiff.mean = (10^predict.table.before$mean - 10^predict.table.after$mean)/
  #   (10^predict.table.before$mean + 10^predict.table.after$mean)*200
  # perdiff.median = (10^predict.table.before$quantile..0.5_before -
  #                     10^predict.table.after$quantile..0.5_after)/
  #   (10^predict.table.before$quantile..0.5_before +
  #      10^predict.table.after$quantile..0.5_after)*200
  
  perdiff.mean = 100*(10^predict.table.after$mean - 10^predict.table.before$mean)/(10^predict.table.before$mean)
  
  perdiff.median = 100*(10^predict.table.after$quantile..0.5_after - 10^predict.table.before$quantile..0.5_before)/
    (10^predict.table.before$quantile..0.5_before)
  
  mean_perdiff <- mean(perdiff.mean)
  sd_perdiff <- sd(perdiff.mean)
  
  median_perdiff <- median(perdiff.median)
  mad_perdiff <- mad(perdiff.median)
  
  
  # Save percent diff in table 
  per.change.table <- data.frame(matrix(ncol=5, nrow=1))
  names(per.change.table) <- c('model', 'fit', 'mad', 'lwr', 'upr')
  per.change.table$model[1] <- 'median_perdiff'
  per.change.table$fit[1] <- median_perdiff
  per.change.table$mad[1] <- mad_perdiff
  per.change.table$upr[1] <- median_perdiff + mad_perdiff
  per.change.table$lwr[1] <- median_perdiff - mad_perdiff
  
  #Save models and percent change table
  per.change.list[[i]] <- per.change.table
  names(per.change.list)[[i]] <-responses[i]
  
  quanlist_before[[i]] <- quan.before
  quanlist_after[[i]] <- quan.after
  
  
  # #Quantile regression  
  # quan.fig.before <- ggplot(data=plot.table.before) + 
  #   geom_errorbar(aes(x=obs, ymin=quantile..0.15, ymax=quantile..0.85), col='grey40', alpha=.5) + 
  #   geom_point(aes(x=obs, y=quantile..0.5), col='red') +
  #   geom_point(aes(x=obs, y=mean), col='black') +
  #   # geom_errorbar(aes(x=obs, ymin=quantile..0.1, ymax=quantile..0.9)) + 
  #   geom_abline() +
  #   theme_bw() + 
  #   labs(y='random forest fit',
  #        x='observed value') +
  #   annotate("text", 
  #            y= min(plot.table.before$quantile..0.15, na.rm=T), 
  #            x =max(plot.table.before$obs),
  #            label="before", hjust=1, size=4) +
  #   annotate("text", y= max(plot.table.before$quantile..0.85),
  #            x =min(plot.table.before$obs),
  #            label=paste("variance explained =", quan.r2),hjust=0) 
  # 
  # quan.fig.after <-  ggplot(data=plot.table.after) + 
  #   geom_errorbar(aes(x=obs, ymin=quantile..0.15, ymax=quantile..0.85), col='grey40', alpha=.5) + 
  #   geom_point(aes(x=obs, y=quantile..0.5), col='red') +
  #   geom_point(aes(x=obs, y=mean), col='black') +
  #   
  #   # geom_errorbar(aes(x=obs, ymin=quantile..0.1, ymax=quantile..0.9)) + 
  #   geom_abline() +
  #   theme_bw() + 
  #   labs(y='random forest prediction',
  #        x='observed value') +
  #   annotate("text", 
  #            y= min(plot.table.after$quantile..0.15, na.rm=T), 
  #            x =max(plot.table.after$obs),
  #            label="after", hjust=1, size=4) +
  #   annotate("text", 
  #            y= max(plot.table.after$quantile..0.85, na.rm=T),
  #            x=min(plot.table.after$obs),
  #            label=paste0("% change = ", round(per.change.table$fit[4], 2), " (", round(per.change.table$lwr[4], 2), " to ", round(per.change.table$upr[4], 2), ")"),
  #            hjust=0, col='#e41a1c', size=4) 
  # 
  # 
  # quan.fig.fit <- grid.arrange(grobs=list(quan.fig.before, quan.fig.after), nrow=1, top=responses[i])
  # 
  # ggsave(file.path(path_to_results, 'Figures', 'PercentChange', site_name, paste0(responses[i], "_QuantileForest_prediction.png")), quan.fig.fit, height=3, width=6, units='in')
  # 
  # 
  # #Six panel figure with multi-linear, glm, and quantile regression
  # fig.fit.6 <- grid.arrange(grobs=list(mlm.fig.before, mlm.fig.after,  glm.fig.before, glm.fig.after, quan.fig.before, quan.fig.after), nrow=3, top=responses[i])
  # 
  # ggsave(file.path(path_to_results, 'Figures', 'PercentChange', site_name, paste0(responses[i], "_3Models_prediction.png")), fig.fit.6, height=9, width=6, units='in')
  # 
  
  
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
perc_reduction <- data.frame(response = responses,
                             # response_clean = clean_names[-length(responses)],
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

if(length(per.change.list) ==0){
  warning("No response variables had detected changes")
  next
} else {

per.change.tableout <-ldply(per.change.list, data.frame, .id = "variable") 


otherresponses_medianperdiff = data.frame(variable = responses[-which(responses %in% per.change.tableout$variable)]) %>%
  mutate(model = 'median_perdiff')

per.change.tableout <- per.change.tableout %>%
  full_join(otherresponses_medianperdiff, by=c('variable', 'model')) %>%
  mutate(variable = factor(variable, rev(responses)))


per.change.tableout$name <- c(clean_names, other_names)[match(per.change.tableout$variable, c(responses_clean, other_responses))]

per.change.tableout$name <- factor(per.change.tableout$name, rev(c(clean_names, other_names)))


per.change.tableout$site <- rep(site_name, nrow(per.change.tableout))

# per.change.table.plot <- per.change.tableout %>%
#   group_by(variable, value=model) %>%
#   tidyr::gather(metric, value, 3:6) 



median.per.change.allvars <- ggplot(per.change.tableout[per.change.tableout$model =='median_perdiff',], aes(x=name)) + 
  geom_hline(yintercept = 0, col='black', linetype='dashed') +
  geom_errorbar(aes(ymin=lwr, ymax=upr), position=position_dodge(width=.5), width=.3) +
  geom_point(aes(y=fit), position=position_dodge(width=.5), size=3, shape=18) +
  theme_bw() +
  labs(y='median percent change per storm event') +
  theme(axis.title.y=element_blank()) + 
  coord_flip() +
  theme(legend.position='bottom',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  ggtitle(site_name) 

ggsave(file=file.path(path_to_results, "Figures", "PercentChange", site_name, "Median_percent_change_allvars.png"), median.per.change.allvars, width=5, height=5, units='in')

ggsave(file=file.path(path_to_results, "Figures", "PercentChange", "SiteSummaries", "IndividualLoad_PercentChange", paste0(site_name, "_Median_percent_change_allvars.png")), median.per.change.allvars, width=5.5, height=5, units='in')


}


Calculated_rows <- which(pval.differences < 0.1)
if (length(Calculated_rows)>0) {
  message(paste0('Percent change calculated for ', toString(responses[Calculated_rows])))
} else {
  message("No responses were found to be different at a p-value less than 0.15")
}

