
setwd("C:/Users/conno/Downloads/Datasets")
sample <- read.csv("simulatedDataset.csv", row.names = 1)
sample_ID <- read.csv("simulatedDatasetWithLabels.csv", row.names = 1)
MCAR10_sample <- read.csv("simulatedDatasetMCAR10.csv", row.names = 1)
MCAR20_sample <- read.csv("simulatedDatasetMCAR20.csv", row.names = 1)
MCAR30_sample <- read.csv("simulatedDatasetMCAR30.csv", row.names = 1)
MCAR40_sample <- read.csv("simulatedDatasetMCAR40.csv", row.names = 1)
MCAR50_sample <- read.csv("simulatedDatasetMCAR50.csv", row.names = 1)
MCAR60_sample <- read.csv("simulatedDatasetMCAR60.csv", row.names = 1)
MCAR70_sample <- read.csv("simulatedDatasetMCAR70.csv", row.names = 1)
MCAR80_sample <- read.csv("simulatedDatasetMCAR80.csv", row.names = 1)

sample[,11] <- as.factor(sample[,11])
sample_ID[,11] <- as.factor(sample_ID[,11])
MCAR10_sample[,11] <- as.factor(MCAR10_sample[,11])
MCAR20_sample[,11] <- as.factor(MCAR20_sample[,11])
MCAR30_sample[,11] <- as.factor(MCAR30_sample[,11])
MCAR40_sample[,11] <- as.factor(MCAR40_sample[,11])
MCAR50_sample[,11] <- as.factor(MCAR50_sample[,11])
MCAR60_sample[,11] <- as.factor(MCAR60_sample[,11])
MCAR70_sample[,11] <- as.factor(MCAR70_sample[,11])
MCAR80_sample[,11] <- as.factor(MCAR80_sample[,11])
