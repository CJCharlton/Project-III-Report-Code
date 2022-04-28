#install.packages("mice")
library(mice)

method_vector <- c("norm.predict", "norm.predict", "norm.predict", "norm.predict", 
                   "norm.predict", "norm.predict", "norm.predict", "norm.predict", 
                   "norm.predict", "norm.predict", "logreg")

mice_MCAR10_samples <- mice(MCAR10_sample, m = 10, method_vector, maxit = 10)
mice_MCAR20_samples <- mice(MCAR20_sample, m = 10, method_vector, maxit = 10)
mice_MCAR30_samples <- mice(MCAR30_sample, m = 10, method_vector, maxit = 10)
mice_MCAR40_samples <- mice(MCAR40_sample, m = 10, method_vector, maxit = 10)
mice_MCAR50_samples <- mice(MCAR50_sample, m = 10, method_vector, maxit = 10)
mice_MCAR60_samples <- mice(MCAR60_sample, m = 10, method_vector, maxit = 10)
mice_MCAR70_samples <- mice(MCAR70_sample, m = 10, method_vector, maxit = 10)
mice_MCAR80_samples <- mice(MCAR80_sample, m = 10, method_vector, maxit = 10)