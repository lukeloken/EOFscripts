

#prepare data for percent change analysis

# define predictors
dat.mod <- dat[,predictors]
dat.mod <- complete.cases(dat.mod)

n.lost <- length(dat.mod[dat.mod == FALSE])
message(paste0(n.lost, ' observations dropped due to missing predictor data.'))

dat.mod <- dat[dat.mod, ]

# get rid of highly correlated variables
which.frozen <- which(predictors %in% 'frozen')

cor.vars <- predictors[-which.frozen]
cor.vars <- cor.vars[which(sapply(dat[cor.vars], sd) >0)]
predictors.cor <- cor(dat[,cor.vars], use = 'complete.obs') # drop var "crop" from correlation since it's a categorical var
names.cor <- row.names(predictors.cor)
drop.predictors <- caret::findCorrelation(predictors.cor, cutoff = 0.95, verbose = FALSE, exact = TRUE)

predictors.keep <- c(names.cor[-drop.predictors], 'frozen')

# log transform response vars
dat.mod[,responses] <- log10(dat.mod[,responses])

sums <- colSums(dat.mod[,responses])
if(any(is.infinite(sums))) {
  stop('Zeros in the response variables caused values to be infinite when log transformed. Please see code in scripts/data_analysis/0_before_after_datmod_prep.R to debug.', call. = F)
}


