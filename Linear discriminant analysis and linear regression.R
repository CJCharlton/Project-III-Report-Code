#install.packages("mice")
library(mice)

method_vector <- c("norm.predict", "norm.predict", "norm.predict", "norm.predict", 
                   "norm.predict", "norm.predict", "norm.predict", "norm.predict", 
                   "norm.predict", "norm.predict", "lda")

LDA_LinReg_MCAR10_sample <- mice(MCAR10_sample, m = 1, method_vector, maxit = 1)
LDA_LinReg_MCAR20_sample <- mice(MCAR20_sample, m = 1, method_vector, maxit = 1)
LDA_LinReg_MCAR30_sample <- mice(MCAR30_sample, m = 1, method_vector, maxit = 1)
LDA_LinReg_MCAR40_sample <- mice(MCAR40_sample, m = 1, method_vector, maxit = 1)
LDA_LinReg_MCAR50_sample <- mice(MCAR50_sample, m = 1, method_vector, maxit = 1)
LDA_LinReg_MCAR60_sample <- mice(MCAR60_sample, m = 1, method_vector, maxit = 1)
LDA_LinReg_MCAR70_sample <- mice(MCAR70_sample, m = 1, method_vector, maxit = 1)
LDA_LinReg_MCAR80_sample <- mice(MCAR80_sample, m = 1, method_vector, maxit = 1)

LDA_LinReg_MNAR10_sample <- complete(mice(MNAR10_sample, m = 1, method_vector, maxit = 1))
LDA_LinReg_MNAR20_sample <- complete(mice(MNAR20_sample, m = 1, method_vector, maxit = 1))
LDA_LinReg_MNAR30_sample <- complete(mice(MNAR30_sample, m = 1, method_vector, maxit = 1))
LDA_LinReg_MNAR40_sample <- complete(mice(MNAR40_sample, m = 1, method_vector, maxit = 1))
LDA_LinReg_MNAR50_sample <- complete(mice(MNAR50_sample, m = 1, method_vector, maxit = 1))
LDA_LinReg_MNAR60_sample <- complete(mice(MNAR60_sample, m = 1, method_vector, maxit = 1))
LDA_LinReg_MNAR70_sample <- complete(mice(MNAR70_sample, m = 1, method_vector, maxit = 1))
LDA_LinReg_MNAR80_sample <- complete(mice(MNAR80_sample, m = 1, method_vector, maxit = 1))

setwd("C:/Users/conno/Downloads/Datasets")
write.csv(complete(LDA_LinReg_MCAR10_sample), "simulatedDatasetMCAR10_LDA_LinReg.csv")
write.csv(complete(LDA_LinReg_MCAR20_sample), "simulatedDatasetMCAR20_LDA_LinReg.csv")
write.csv(complete(LDA_LinReg_MCAR30_sample), "simulatedDatasetMCAR30_LDA_LinReg.csv")
write.csv(complete(LDA_LinReg_MCAR40_sample), "simulatedDatasetMCAR40_LDA_LinReg.csv")
write.csv(complete(LDA_LinReg_MCAR50_sample), "simulatedDatasetMCAR50_LDA_LinReg.csv")
write.csv(complete(LDA_LinReg_MCAR60_sample), "simulatedDatasetMCAR60_LDA_LinReg.csv")
write.csv(complete(LDA_LinReg_MCAR70_sample), "simulatedDatasetMCAR70_LDA_LinReg.csv")
write.csv(complete(LDA_LinReg_MCAR80_sample), "simulatedDatasetMCAR80_LDA_LinReg.csv")