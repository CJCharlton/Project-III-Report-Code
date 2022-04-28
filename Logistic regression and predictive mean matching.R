#install.packages("mice")
library(mice)

method_vector <- c("pmm", "pmm", "pmm", "pmm", 
                   "pmm", "pmm", "pmm", "pmm", 
                   "pmm", "pmm", "logreg")

LogReg_PMM_MCAR10_sample <- mice(MCAR10_sample, m = 1, method_vector, maxit = 1)
LogReg_PMM_MCAR20_sample <- mice(MCAR20_sample, m = 1, method_vector, maxit = 1)
LogReg_PMM_MCAR30_sample <- mice(MCAR30_sample, m = 1, method_vector, maxit = 1)
LogReg_PMM_MCAR40_sample <- mice(MCAR40_sample, m = 1, method_vector, maxit = 1)
LogReg_PMM_MCAR50_sample <- mice(MCAR50_sample, m = 1, method_vector, maxit = 1)
LogReg_PMM_MCAR60_sample <- mice(MCAR60_sample, m = 1, method_vector, maxit = 1)
LogReg_PMM_MCAR70_sample <- mice(MCAR70_sample, m = 1, method_vector, maxit = 1)
LogReg_PMM_MCAR80_sample <- mice(MCAR80_sample, m = 1, method_vector, maxit = 1)

LogReg_PMM_MNAR10_sample <- complete(mice(MNAR10_sample, m = 1, method_vector, maxit = 1))
LogReg_PMM_MNAR20_sample <- complete(mice(MNAR20_sample, m = 1, method_vector, maxit = 1))
LogReg_PMM_MNAR30_sample <- complete(mice(MNAR30_sample, m = 1, method_vector, maxit = 1))
LogReg_PMM_MNAR40_sample <- complete(mice(MNAR40_sample, m = 1, method_vector, maxit = 1))
LogReg_PMM_MNAR50_sample <- complete(mice(MNAR50_sample, m = 1, method_vector, maxit = 1))
LogReg_PMM_MNAR60_sample <- complete(mice(MNAR60_sample, m = 1, method_vector, maxit = 1))
LogReg_PMM_MNAR70_sample <- complete(mice(MNAR70_sample, m = 1, method_vector, maxit = 1))
LogReg_PMM_MNAR80_sample <- complete(mice(MNAR80_sample, m = 1, method_vector, maxit = 1))

setwd("C:/Users/conno/Downloads/Datasets")
write.csv(complete(LogReg_PMM_MCAR10_sample), "simulatedDatasetMCAR10_LogReg_PMM.csv")
write.csv(complete(LogReg_PMM_MCAR20_sample), "simulatedDatasetMCAR20_LogReg_PMM.csv")
write.csv(complete(LogReg_PMM_MCAR30_sample), "simulatedDatasetMCAR30_LogReg_PMM.csv")
write.csv(complete(LogReg_PMM_MCAR40_sample), "simulatedDatasetMCAR40_LogReg_PMM.csv")
write.csv(complete(LogReg_PMM_MCAR50_sample), "simulatedDatasetMCAR50_LogReg_PMM.csv")
write.csv(complete(LogReg_PMM_MCAR60_sample), "simulatedDatasetMCAR60_LogReg_PMM.csv")
write.csv(complete(LogReg_PMM_MCAR70_sample), "simulatedDatasetMCAR70_LogReg_PMM.csv")
write.csv(complete(LogReg_PMM_MCAR80_sample), "simulatedDatasetMCAR80_LogReg_PMM.csv")