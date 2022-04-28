#install.packages("mice")
library(mice)

method_vector <- c("sample", "sample", "sample", "sample", 
                   "sample", "sample", "sample", "sample", 
                   "sample", "sample", "sample")

RanSam_MCAR10_sample <- mice(MCAR10_sample, m = 1, method_vector, maxit = 1)
RanSam_MCAR20_sample <- mice(MCAR20_sample, m = 1, method_vector, maxit = 1)
RanSam_MCAR30_sample <- mice(MCAR30_sample, m = 1, method_vector, maxit = 1)
RanSam_MCAR40_sample <- mice(MCAR40_sample, m = 1, method_vector, maxit = 1)
RanSam_MCAR50_sample <- mice(MCAR50_sample, m = 1, method_vector, maxit = 1)
RanSam_MCAR60_sample <- mice(MCAR60_sample, m = 1, method_vector, maxit = 1)
RanSam_MCAR70_sample <- mice(MCAR70_sample, m = 1, method_vector, maxit = 1)
RanSam_MCAR80_sample <- mice(MCAR80_sample, m = 1, method_vector, maxit = 1)

RanSam_MNAR10_sample <- complete(mice(MNAR10_sample, m = 1, method_vector, maxit = 1))
RanSam_MNAR20_sample <- complete(mice(MNAR20_sample, m = 1, method_vector, maxit = 1))
RanSam_MNAR30_sample <- complete(mice(MNAR30_sample, m = 1, method_vector, maxit = 1))
RanSam_MNAR40_sample <- complete(mice(MNAR40_sample, m = 1, method_vector, maxit = 1))
RanSam_MNAR50_sample <- complete(mice(MNAR50_sample, m = 1, method_vector, maxit = 1))
RanSam_MNAR60_sample <- complete(mice(MNAR60_sample, m = 1, method_vector, maxit = 1))
RanSam_MNAR70_sample <- complete(mice(MNAR70_sample, m = 1, method_vector, maxit = 1))
RanSam_MNAR80_sample <- complete(mice(MNAR80_sample, m = 1, method_vector, maxit = 1))

setwd("C:/Users/conno/Downloads/Datasets")
write.csv(complete(RanSam_MCAR10_sample), "simulatedDatasetMCAR10_RanSam.csv")
write.csv(complete(RanSam_MCAR20_sample), "simulatedDatasetMCAR20_RanSam.csv")
write.csv(complete(RanSam_MCAR30_sample), "simulatedDatasetMCAR30_RanSam.csv")
write.csv(complete(RanSam_MCAR40_sample), "simulatedDatasetMCAR40_RanSam.csv")
write.csv(complete(RanSam_MCAR50_sample), "simulatedDatasetMCAR50_RanSam.csv")
write.csv(complete(RanSam_MCAR60_sample), "simulatedDatasetMCAR60_RanSam.csv")
write.csv(complete(RanSam_MCAR70_sample), "simulatedDatasetMCAR70_RanSam.csv")
write.csv(complete(RanSam_MCAR80_sample), "simulatedDatasetMCAR80_RanSam.csv")
