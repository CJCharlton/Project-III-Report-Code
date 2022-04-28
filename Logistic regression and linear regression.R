#install.packages("mice")
library(mice)

method_vector <- c("norm.predict", "norm.predict", "norm.predict", "norm.predict", 
                   "norm.predict", "norm.predict", "norm.predict", "norm.predict", 
                   "norm.predict", "norm.predict", "logreg")

LogReg_LinReg_MCAR10_sample <- mice(MCAR10_sample, m = 1, method_vector, maxit = 1)
LogReg_LinReg_MCAR20_sample <- mice(MCAR20_sample, m = 1, method_vector, maxit = 1)
LogReg_LinReg_MCAR30_sample <- mice(MCAR30_sample, m = 1, method_vector, maxit = 1)
LogReg_LinReg_MCAR40_sample <- mice(MCAR40_sample, m = 1, method_vector, maxit = 1)
LogReg_LinReg_MCAR50_sample <- mice(MCAR50_sample, m = 1, method_vector, maxit = 1)
LogReg_LinReg_MCAR60_sample <- mice(MCAR60_sample, m = 1, method_vector, maxit = 1)
LogReg_LinReg_MCAR70_sample <- mice(MCAR70_sample, m = 1, method_vector, maxit = 1)
LogReg_LinReg_MCAR80_sample <- mice(MCAR80_sample, m = 1, method_vector, maxit = 1)

LogReg_LinReg_MNAR10_sample <- mice(MNAR10_sample, m = 1, method_vector, maxit = 1)
LogReg_LinReg_MNAR20_sample <- mice(MNAR20_sample, m = 1, method_vector, maxit = 1)
LogReg_LinReg_MNAR30_sample <- mice(MNAR30_sample, m = 1, method_vector, maxit = 1)
LogReg_LinReg_MNAR40_sample <- mice(MNAR40_sample, m = 1, method_vector, maxit = 1)
LogReg_LinReg_MNAR50_sample <- mice(MNAR50_sample, m = 1, method_vector, maxit = 1)
LogReg_LinReg_MNAR60_sample <- mice(MNAR60_sample, m = 1, method_vector, maxit = 1)
LogReg_LinReg_MNAR70_sample <- mice(MNAR70_sample, m = 1, method_vector, maxit = 1)
LogReg_LinReg_MNAR80_sample <- mice(MNAR80_sample, m = 1, method_vector, maxit = 1)

setwd("C:/Users/conno/Downloads/Datasets")
write.csv(complete(LogReg_LinReg_MCAR10_sample), "simulatedDatasetMCAR10_LogReg_LinReg.csv")
write.csv(complete(LogReg_LinReg_MCAR20_sample), "simulatedDatasetMCAR20_LogReg_LinReg.csv")
write.csv(complete(LogReg_LinReg_MCAR30_sample), "simulatedDatasetMCAR30_LogReg_LinReg.csv")
write.csv(complete(LogReg_LinReg_MCAR40_sample), "simulatedDatasetMCAR40_LogReg_LinReg.csv")
write.csv(complete(LogReg_LinReg_MCAR50_sample), "simulatedDatasetMCAR50_LogReg_LinReg.csv")
write.csv(complete(LogReg_LinReg_MCAR60_sample), "simulatedDatasetMCAR60_LogReg_LinReg.csv")
write.csv(complete(LogReg_LinReg_MCAR70_sample), "simulatedDatasetMCAR70_LogReg_LinReg.csv")
write.csv(complete(LogReg_LinReg_MCAR80_sample), "simulatedDatasetMCAR80_LogReg_LinReg.csv")