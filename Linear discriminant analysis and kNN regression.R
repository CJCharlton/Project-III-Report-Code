#install.packages("VIM")
#install.packages("mice")
library(VIM)
library(mice)
library(english)

film_names <- function(films, prem_sub = TRUE, ID = FALSE) {
  output <- c()
  for (idx in 1:films) {
    output <- append(output, paste("film", english(idx), sep = "_"))
  }
  if (prem_sub) {
    output <- append(output, "premium_subscription")
  }
  if (ID) {
    output <- append(output, "type")
  }
  return(output)
}

method_vector <- c("sample", "sample", "sample", "sample", 
                   "sample", "sample", "sample", "sample", 
                   "sample", "sample", "lda")

LDA_kNN_MCAR10_sample <- kNN(MCAR10_sample[,1:10], k = sqrt( 500 - sum(is.na(MCAR10_sample[,1:10]))/10 ))[,1:10]
LDA_kNN_MCAR10_sample <- cbind( LDA_kNN_MCAR10_sample, complete(mice(MCAR10_sample, m = 1, method_vector, maxit = 1))[,11] )

LDA_kNN_MCAR20_sample <- kNN(MCAR20_sample[,1:10], k = sqrt( 500 - sum(is.na(MCAR20_sample[,1:10]))/10 ))[,1:10]
LDA_kNN_MCAR20_sample <- cbind( LDA_kNN_MCAR20_sample, complete(mice(MCAR20_sample, m = 1, method_vector, maxit = 1))[,11] )

LDA_kNN_MCAR30_sample <- kNN(MCAR30_sample[,1:10], k = sqrt( 500 - sum(is.na(MCAR30_sample[,1:10]))/10 ))[,1:10]
LDA_kNN_MCAR30_sample <- cbind( LDA_kNN_MCAR30_sample, complete(mice(MCAR30_sample, m = 1, method_vector, maxit = 1))[,11] )

LDA_kNN_MCAR40_sample <- kNN(MCAR40_sample[,1:10], k = sqrt( 500 - sum(is.na(MCAR40_sample[,1:10]))/10 ))[,1:10]
LDA_kNN_MCAR40_sample <- cbind( LDA_kNN_MCAR40_sample, complete(mice(MCAR40_sample, m = 1, method_vector, maxit = 1))[,11] )

LDA_kNN_MCAR50_sample <- kNN(MCAR50_sample[,1:10], k = sqrt( 500 - sum(is.na(MCAR50_sample[,1:10]))/10 ))[,1:10]
LDA_kNN_MCAR50_sample <- cbind( LDA_kNN_MCAR50_sample, complete(mice(MCAR50_sample, m = 1, method_vector, maxit = 1))[,11] )

LDA_kNN_MCAR60_sample <- kNN(MCAR60_sample[,1:10], k = sqrt( 500 - sum(is.na(MCAR60_sample[,1:10]))/10 ))[,1:10]
LDA_kNN_MCAR60_sample <- cbind( LDA_kNN_MCAR60_sample, complete(mice(MCAR60_sample, m = 1, method_vector, maxit = 1))[,11] )

LDA_kNN_MCAR70_sample <- kNN(MCAR70_sample[,1:10], k = sqrt( 500 - sum(is.na(MCAR70_sample[,1:10]))/10 ))[,1:10]
LDA_kNN_MCAR70_sample <- cbind( LDA_kNN_MCAR70_sample, complete(mice(MCAR70_sample, m = 1, method_vector, maxit = 1))[,11] )

LDA_kNN_MCAR80_sample <- kNN(MCAR80_sample[,1:10], k = sqrt( 500 - sum(is.na(MCAR80_sample[,1:10]))/10 ))[,1:10]
LDA_kNN_MCAR80_sample <- cbind( LDA_kNN_MCAR80_sample, complete(mice(MCAR80_sample, m = 1, method_vector, maxit = 1))[,11] )


colnames(LDA_kNN_MCAR10_sample) <- film_names(10)
colnames(LDA_kNN_MCAR20_sample) <- film_names(10)
colnames(LDA_kNN_MCAR30_sample) <- film_names(10)
colnames(LDA_kNN_MCAR40_sample) <- film_names(10)
colnames(LDA_kNN_MCAR50_sample) <- film_names(10)
colnames(LDA_kNN_MCAR60_sample) <- film_names(10)
colnames(LDA_kNN_MCAR70_sample) <- film_names(10)
colnames(LDA_kNN_MCAR80_sample) <- film_names(10)

setwd("C:/Users/conno/Downloads/Datasets")
write.csv(complete(LDA_kNN_MCAR10_sample), "simulatedDatasetMCAR10_LDA_kNN.csv")
write.csv(complete(LDA_kNN_MCAR20_sample), "simulatedDatasetMCAR20_LDA_kNN.csv")
write.csv(complete(LDA_kNN_MCAR30_sample), "simulatedDatasetMCAR30_LDA_kNN.csv")
write.csv(complete(LDA_kNN_MCAR40_sample), "simulatedDatasetMCAR40_LDA_kNN.csv")
write.csv(complete(LDA_kNN_MCAR50_sample), "simulatedDatasetMCAR50_LDA_kNN.csv")
write.csv(complete(LDA_kNN_MCAR60_sample), "simulatedDatasetMCAR60_LDA_kNN.csv")
write.csv(complete(LDA_kNN_MCAR70_sample), "simulatedDatasetMCAR70_LDA_kNN.csv")
write.csv(complete(LDA_kNN_MCAR80_sample), "simulatedDatasetMCAR80_LDA_kNN.csv")

LDA_kNN_MNAR10_sample <- kNN(MNAR10_sample[,1:10], k = sqrt( 500 - sum(is.na(MNAR10_sample[,1:10]))/10 ))[,1:10]
LDA_kNN_MNAR10_sample <- cbind( LDA_kNN_MNAR10_sample, complete(mice(MNAR10_sample, m = 1, method_vector, maxit = 1))[,11] )

LDA_kNN_MNAR20_sample <- kNN(MNAR20_sample[,1:10], k = sqrt( 500 - sum(is.na(MNAR20_sample[,1:10]))/10 ))[,1:10]
LDA_kNN_MNAR20_sample <- cbind( LDA_kNN_MNAR20_sample, complete(mice(MNAR20_sample, m = 1, method_vector, maxit = 1))[,11] )

LDA_kNN_MNAR30_sample <- kNN(MNAR30_sample[,1:10], k = sqrt( 500 - sum(is.na(MNAR30_sample[,1:10]))/10 ))[,1:10]
LDA_kNN_MNAR30_sample <- cbind( LDA_kNN_MNAR30_sample, complete(mice(MNAR30_sample, m = 1, method_vector, maxit = 1))[,11] )

LDA_kNN_MNAR40_sample <- kNN(MNAR40_sample[,1:10], k = sqrt( 500 - sum(is.na(MNAR40_sample[,1:10]))/10 ))[,1:10]
LDA_kNN_MNAR40_sample <- cbind( LDA_kNN_MNAR40_sample, complete(mice(MNAR40_sample, m = 1, method_vector, maxit = 1))[,11] )

LDA_kNN_MNAR50_sample <- kNN(MNAR50_sample[,1:10], k = sqrt( 500 - sum(is.na(MNAR50_sample[,1:10]))/10 ))[,1:10]
LDA_kNN_MNAR50_sample <- cbind( LDA_kNN_MNAR50_sample, complete(mice(MNAR50_sample, m = 1, method_vector, maxit = 1))[,11] )

LDA_kNN_MNAR60_sample <- kNN(MNAR60_sample[,1:10], k = sqrt( 500 - sum(is.na(MNAR60_sample[,1:10]))/10 ))[,1:10]
LDA_kNN_MNAR60_sample <- cbind( LDA_kNN_MNAR60_sample, complete(mice(MNAR60_sample, m = 1, method_vector, maxit = 1))[,11] )

LDA_kNN_MNAR70_sample <- kNN(MNAR70_sample[,1:10], k = sqrt( 500 - sum(is.na(MNAR70_sample[,1:10]))/10 ))[,1:10]
LDA_kNN_MNAR70_sample <- cbind( LDA_kNN_MNAR70_sample, complete(mice(MNAR70_sample, m = 1, method_vector, maxit = 1))[,11] )

LDA_kNN_MNAR80_sample <- kNN(MNAR80_sample[,1:10], k = sqrt( 500 - sum(is.na(MNAR80_sample[,1:10]))/10 ))[,1:10]
LDA_kNN_MNAR80_sample <- cbind( LDA_kNN_MNAR80_sample, complete(mice(MNAR80_sample, m = 1, method_vector, maxit = 1))[,11] )
