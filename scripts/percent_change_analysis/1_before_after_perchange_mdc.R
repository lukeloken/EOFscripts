
#Step 1 of percent change analysis
#Generate random forest of whole dataset
library(randomForestExplainer)

dat.mod.before.log10 <- filter(dat.mod.log10, period == 'before')
dat.mod.before <- filter(dat.mod, period == 'before')

# save MDC as output from loop
mdc.perc.nbefore <- c()
mdc.perc.nbefore.log10 <- c()

perc.var <- c()
perc.var.log10 <- c()


# loop through responses to create equation and model
for (i in 1:length(responses)) {
  
  # create model equation
  mod.equation <- as.formula(paste(responses[i], paste(predictors.keep, collapse = " + "), sep = " ~ "))
  
  # create random forest model - before data
  #mod <- randomForest(mod.equation, data = dat.mod, importance = T, na.action = na.omit)
  mod.before <- randomForest(mod.equation, data = dat.mod.before, importance = T, na.action = na.omit, ntree = 1000)
  mod.before.log10 <- randomForest(mod.equation, data = dat.mod.before.log10, importance = T, na.action = na.omit, ntree = 1000)
  
  
  #Look at Random Forest Model
  # print(randomForest::varImpPlot(mod.before))
  # str(mod.before$forest)
  # measure_importance(mod.before)
  # explain_forest(mod.before)
  
  perc.var.log10[i] <- round(mod.before.log10$rsq[1000]*100, 1)
  perc.var[i] <- round(mod.before$rsq[1000]*100, 1)
  
  # calculate minimum detectable change for each constituent based on this model
  
  # pull out mod.before mean squared error
  mse.before <- mod.before$mse[length(mod.before$mse)]
  mse.before.log10 <- mod.before.log10$mse[length(mod.before.log10$mse)]
  
  
  n.before <- nrow(dat.mod.before[dat.mod.before$period == "before", ])
  n.before.log10 <- nrow(dat.mod.before.log10[dat.mod.before.log10$period == "before", ])
  
  
  tval.nbefore <- qt(0.05, n.before + n.before - 2, lower.tail = FALSE)
  
  mdc.nbefore <- tval.nbefore*sqrt((mse.before/n.before) + (mse.before/n.before))
  mdc.nbefore.log10 <- tval.nbefore*sqrt((mse.before.log10/n.before.log10) + (mse.before.log10/n.before.log10))
  
  mdc.perc.nbefore[i] <- (1-(10^-mdc.nbefore))*100
  mdc.perc.nbefore.log10[i] <- (1-(10^-mdc.nbefore.log10))*100
  
}
# create a dataframe describing the residual models
mdc <- data.frame(variable = responses,
                  model_fit = perc.var,
                  mdc = round(mdc.perc.nbefore, 0),
                  model_fit.log10 = perc.var.log10,
                  mdc.log10 = round(mdc.perc.nbefore.log10, 0)
)


if (nrow(mdc) == length(responses)) {
  message('Minimum detectable change has been calculated. Results are R object "mdc"')
} else {
  stop("Somethign went wrong with calculating the minimum detectable change. To debug, see code in scripts/data_analysis/1_mdc_before_after.R.")
}

