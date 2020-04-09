
#Step 1 of percent change analysis
#Generate random forest of whole dataset
# library(randomForestExplainer)

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
mdc <- data.frame(variable = responses,
                  model_fit = perc.var,
                  mdc = round(mdc.perc.nbefore, 0))



if (nrow(mdc) == length(responses)) {
  message('Minimum detectable change has been calculated. Results are R object "mdc"')
} else {
  stop("Somethign went wrong with calculating the minimum detectable change. To debug, see code in scripts/data_analysis/1_mdc_before_after.R.")
}

rm(mod.before)
