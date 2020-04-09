

#prepare data for percent change analysis
predictors <- intersect(predictors, names(dat))

dat.predictors <- dat[,predictors]
dat.mod.complete <- complete.cases(dat.predictors)

n.lost <- length(dat.mod.complete[dat.mod.complete == FALSE])
message(paste0(n.lost, ' observations dropped due to missing predictor data.'))

#Drop rows that are not complete
dat.mod <- dat[dat.mod.complete, ] 

#new list of predictors
predictors.new <- intersect(predictors, names(dat.mod))

# get rid of highly correlated variables and those that are a single value
which.frozen <- which(predictors.new %in% 'frozen')

cor.vars <- predictors[-which.frozen]
cor.vars <- cor.vars[which(sapply(dat[cor.vars], sd) >0)]
predictors.cor <- cor(dat.mod[,cor.vars], use = 'complete.obs') 

names.cor <- row.names(predictors.cor)
drop.predictors <- caret::findCorrelation(predictors.cor, cutoff = 0.95, verbose = FALSE, exact = TRUE)

#final list of predictors
predictors.keep <- c(names.cor[-drop.predictors], 'frozen')

# log transform response vars
dat.mod[,responses] <- log10(dat.mod[,responses])
sums <- colSums(dat.mod[,responses])
if(any(is.infinite(sums))) {
  stop('Zeros in the response variables caused values to be infinite when log transformed. Please see code in scripts/data_analysis/0_before_after_datmod_prep.R to debug.', call. = F)
}


