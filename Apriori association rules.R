#install.packages("arules")
library(arules)
#install.packages("arulesViz")
library(arulesViz)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("readxml")
library(readxl)
#install.packages("knitr")
library(knitr)
library(ggplot2)
#install.packages("lubridate")
library(lubridate)
#install.packages("plyr")
library(plyr)
#install.packages("dplyr")
library(dplyr)
#install.packages("broom.mixed")
library(broom.mixed)

association_rules <- function(sample, supp=0.25, conf=0.75, maxlen=10) {
  methods = list(film_one = list(method = "fixed", breaks = c(0,1,2,3,4,5)),
                 film_two = list(method = "fixed", breaks = c(0,1,2,3,4,5)), 
                 film_three = list(method = "fixed", breaks = c(0,1,2,3,4,5)), 
                 film_four = list(method = "fixed", breaks = c(0,1,2,3,4,5)), 
                 film_five = list(method = "fixed", breaks = c(0,1,2,3,4,5)), 
                 film_six = list(method = "fixed", breaks = c(0,1,2,3,4,5)), 
                 film_seven = list(method = "fixed", breaks = c(0,1,2,3,4,5)), 
                 film_eight = list(method = "fixed", breaks = c(0,1,2,3,4,5)), 
                 film_nine = list(method = "fixed", breaks = c(0,1,2,3,4,5)), 
                 film_ten = list(method = "fixed", breaks = c(0,1,2,3,4,5)))
  sample[,1:10] <- discretizeDF(sample[,1:10], methods=methods)
  #sample[,1:10] <- discretize( sample[,1:10], disc="equalfreq", nbins=5 )
  sample[,11] <- as.factor(sample[,11])
  return(apriori(sample, parameter = list(supp=supp, conf=conf, maxlen=maxlen)))
}

check_against_true_rules <- function(rules, estimated_rules) {
  n_rules <- nrow(rules@quality)
  n_found_rules <- nrow(estimated_rules@quality)
  n_right_rules <- nrow((generics::intersect(rules, estimated_rules))@quality)
  n_wrong_rules <- n_found_rules - n_right_rules
  error_rate <- 1 + (n_wrong_rules - n_right_rules)/n_rules
  return(min(error_rate, 1))
}

combine_rules <- function(set_of_rules_sets) {
  for (idx in 1:10) {
    ref_rules_set <- set_of_rules_sets[[idx]]
    filter()
  }
  return(combined_rules)
}

apriori_output <- association_rules(sample)
summary(apriori_output)
inspect(apriori_output)

apriori_output_large <- association_rules(sample, supp = 0.01, conf = 0.05)
plot(apriori_output_large, method = "two-key plot")



RanSam10_apriori_output <- association_rules(complete(RanSam_MCAR10_sample))
RanSam_error10 <- check_against_true_rules(apriori_output, RanSam10_apriori_output)

RanSam20_apriori_output <- association_rules(complete(RanSam_MCAR20_sample))
RanSam_error20 <- check_against_true_rules(apriori_output, RanSam20_apriori_output)

RanSam30_apriori_output <- association_rules(complete(RanSam_MCAR30_sample))
RanSam_error30 <- check_against_true_rules(apriori_output, RanSam30_apriori_output)

RanSam40_apriori_output <- association_rules(complete(RanSam_MCAR40_sample))
RanSam_error40 <- check_against_true_rules(apriori_output, RanSam40_apriori_output)

RanSam50_apriori_output <- association_rules(complete(RanSam_MCAR50_sample))
RanSam_error50 <- check_against_true_rules(apriori_output, RanSam50_apriori_output)

RanSam60_apriori_output <- association_rules(complete(RanSam_MCAR60_sample))
RanSam_error60 <- check_against_true_rules(apriori_output, RanSam60_apriori_output)

RanSam70_apriori_output <- association_rules(complete(RanSam_MCAR70_sample))
RanSam_error70 <- check_against_true_rules(apriori_output, RanSam70_apriori_output)

RanSam80_apriori_output <- association_rules(complete(RanSam_MCAR80_sample))
RanSam_error80 <- check_against_true_rules(apriori_output, RanSam80_apriori_output)

RanSam_error_rates <- c(RanSam_error10, RanSam_error20, 
                        RanSam_error30, RanSam_error40, RanSam_error50, 
                        RanSam_error60, RanSam_error70, RanSam_error80)
names(RanSam_error_rates) <- c("10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%")

barplot(RanSam_error_rates, xlab="Percentage of Values Removed", ylab="Error Rates")



PPS10_apriori_output <- association_rules(PPS_MCAR10_sample)
PPS_error10 <- check_against_true_rules(apriori_output, PPS10_apriori_output)

PPS20_apriori_output <- association_rules(PPS_MCAR20_sample)
PPS_error20 <- check_against_true_rules(apriori_output, PPS20_apriori_output)

PPS30_apriori_output <- association_rules(PPS_MCAR30_sample)
PPS_error30 <- check_against_true_rules(apriori_output, PPS30_apriori_output)

PPS40_apriori_output <- association_rules(PPS_MCAR40_sample)
PPS_error40 <- check_against_true_rules(apriori_output, PPS40_apriori_output)

PPS50_apriori_output <- association_rules(PPS_MCAR50_sample)
PPS_error50 <- check_against_true_rules(apriori_output, PPS50_apriori_output)

PPS60_apriori_output <- association_rules(PPS_MCAR60_sample)
PPS_error60 <- check_against_true_rules(apriori_output, PPS60_apriori_output)

PPS70_apriori_output <- association_rules(PPS_MCAR70_sample)
PPS_error70 <- check_against_true_rules(apriori_output, PPS70_apriori_output)

PPS80_apriori_output <- association_rules(PPS_MCAR80_sample)
PPS_error80 <- check_against_true_rules(apriori_output, PPS80_apriori_output)

PPS_error_rates <- c(PPS_error10, PPS_error20, 
                     PPS_error30, PPS_error40, PPS_error50, 
                     PPS_error60, PPS_error70, PPS_error80)
names(PPS_error_rates) <- c("10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%")

barplot(PPS_error_rates, xlab="Percentage of Values Removed", ylab="Error Rates")



LogReg_LinReg10_apriori_output <- association_rules(complete(LogReg_LinReg_MCAR10_sample))
LogReg_LinReg_error10 <- check_against_true_rules(apriori_output, LogReg_LinReg10_apriori_output)

LogReg_LinReg20_apriori_output <- association_rules(complete(LogReg_LinReg_MCAR20_sample))
LogReg_LinReg_error20 <- check_against_true_rules(apriori_output, LogReg_LinReg20_apriori_output)

LogReg_LinReg30_apriori_output <- association_rules(complete(LogReg_LinReg_MCAR30_sample))
LogReg_LinReg_error30 <- check_against_true_rules(apriori_output, LogReg_LinReg30_apriori_output)

LogReg_LinReg40_apriori_output <- association_rules(complete(LogReg_LinReg_MCAR40_sample))
LogReg_LinReg_error40 <- check_against_true_rules(apriori_output, LogReg_LinReg40_apriori_output)

LogReg_LinReg50_apriori_output <- association_rules(complete(LogReg_LinReg_MCAR50_sample))
LogReg_LinReg_error50 <- check_against_true_rules(apriori_output, LogReg_LinReg50_apriori_output)

LogReg_LinReg60_apriori_output <- association_rules(complete(LogReg_LinReg_MCAR60_sample))
LogReg_LinReg_error60 <- check_against_true_rules(apriori_output, LogReg_LinReg60_apriori_output)

LogReg_LinReg70_apriori_output <- association_rules(complete(LogReg_LinReg_MCAR70_sample))
LogReg_LinReg_error70 <- check_against_true_rules(apriori_output, LogReg_LinReg70_apriori_output)

LogReg_LinReg80_apriori_output <- association_rules(complete(LogReg_LinReg_MCAR80_sample))
LogReg_LinReg_error80 <- check_against_true_rules(apriori_output, LogReg_LinReg80_apriori_output)

LogReg_LinReg_error_rates <- c(LogReg_LinReg_error10, LogReg_LinReg_error20, 
                               LogReg_LinReg_error30, LogReg_LinReg_error40, LogReg_LinReg_error50, 
                               LogReg_LinReg_error60, LogReg_LinReg_error70, LogReg_LinReg_error80)
names(LogReg_LinReg_error_rates) <- c("10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%")

barplot(LogReg_LinReg_error_rates, xlab="Percentage of Values Removed", ylab="Error Rates")



LogReg_kNN10_apriori_output <- association_rules(LogReg_kNN_MCAR10_sample)
LogReg_kNN_error10 <- check_against_true_rules(apriori_output, LogReg_kNN10_apriori_output)

LogReg_kNN20_apriori_output <- association_rules(LogReg_kNN_MCAR20_sample)
LogReg_kNN_error20 <- check_against_true_rules(apriori_output, LogReg_kNN20_apriori_output)

LogReg_kNN30_apriori_output <- association_rules(LogReg_kNN_MCAR30_sample)
LogReg_kNN_error30 <- check_against_true_rules(apriori_output, LogReg_kNN30_apriori_output)

LogReg_kNN40_apriori_output <- association_rules(LogReg_kNN_MCAR40_sample)
LogReg_kNN_error40 <- check_against_true_rules(apriori_output, LogReg_kNN40_apriori_output)

LogReg_kNN50_apriori_output <- association_rules(LogReg_kNN_MCAR50_sample)
LogReg_kNN_error50 <- check_against_true_rules(apriori_output, LogReg_kNN50_apriori_output)

LogReg_kNN60_apriori_output <- association_rules(LogReg_kNN_MCAR60_sample)
LogReg_kNN_error60 <- check_against_true_rules(apriori_output, LogReg_kNN60_apriori_output)

LogReg_kNN70_apriori_output <- association_rules(LogReg_kNN_MCAR70_sample)
LogReg_kNN_error70 <- check_against_true_rules(apriori_output, LogReg_kNN70_apriori_output)

LogReg_kNN80_apriori_output <- association_rules(LogReg_kNN_MCAR80_sample)
LogReg_kNN_error80 <- check_against_true_rules(apriori_output, LogReg_kNN80_apriori_output)

LogReg_kNN_error_rates <- c(LogReg_kNN_error10, LogReg_kNN_error20, 
                            LogReg_kNN_error30, LogReg_kNN_error40, LogReg_kNN_error50, 
                            LogReg_kNN_error60, LogReg_kNN_error70, LogReg_kNN_error80)
names(LogReg_kNN_error_rates) <- c("10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%")

barplot(LogReg_kNN_error_rates, xlab="Percentage of Values Removed", ylab="Error Rates")



LogReg_PMM10_apriori_output <- association_rules(complete(LogReg_PMM_MCAR10_sample))
LogReg_PMM_error10 <- check_against_true_rules(apriori_output, LogReg_PMM10_apriori_output)

LogReg_PMM20_apriori_output <- association_rules(complete(LogReg_PMM_MCAR20_sample))
LogReg_PMM_error20 <- check_against_true_rules(apriori_output, LogReg_PMM20_apriori_output)

LogReg_PMM30_apriori_output <- association_rules(complete(LogReg_PMM_MCAR30_sample))
LogReg_PMM_error30 <- check_against_true_rules(apriori_output, LogReg_PMM30_apriori_output)

LogReg_PMM40_apriori_output <- association_rules(complete(LogReg_PMM_MCAR40_sample))
LogReg_PMM_error40 <- check_against_true_rules(apriori_output, LogReg_PMM40_apriori_output)

LogReg_PMM50_apriori_output <- association_rules(complete(LogReg_PMM_MCAR50_sample))
LogReg_PMM_error50 <- check_against_true_rules(apriori_output, LogReg_PMM50_apriori_output)

LogReg_PMM60_apriori_output <- association_rules(complete(LogReg_PMM_MCAR60_sample))
LogReg_PMM_error60 <- check_against_true_rules(apriori_output, LogReg_PMM60_apriori_output)

LogReg_PMM70_apriori_output <- association_rules(complete(LogReg_PMM_MCAR70_sample))
LogReg_PMM_error70 <- check_against_true_rules(apriori_output, LogReg_PMM70_apriori_output)

LogReg_PMM80_apriori_output <- association_rules(complete(LogReg_PMM_MCAR80_sample))
LogReg_PMM_error80 <- check_against_true_rules(apriori_output, LogReg_PMM80_apriori_output)

LogReg_PMM_error_rates <- c(LogReg_PMM_error10, LogReg_PMM_error20, 
                               LogReg_PMM_error30, LogReg_PMM_error40, LogReg_PMM_error50, 
                               LogReg_PMM_error60, LogReg_PMM_error70, LogReg_PMM_error80)
names(LogReg_PMM_error_rates) <- c("10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%")

barplot(LogReg_PMM_error_rates, xlab="Percentage of Values Removed", ylab="Error Rates")



LDA_LinReg10_apriori_output <- association_rules(complete(LDA_LinReg_MCAR10_sample))
LDA_LinReg_error10 <- check_against_true_rules(apriori_output, LDA_LinReg10_apriori_output)

LDA_LinReg20_apriori_output <- association_rules(complete(LDA_LinReg_MCAR20_sample))
LDA_LinReg_error20 <- check_against_true_rules(apriori_output, LDA_LinReg20_apriori_output)

LDA_LinReg30_apriori_output <- association_rules(complete(LDA_LinReg_MCAR30_sample))
LDA_LinReg_error30 <- check_against_true_rules(apriori_output, LDA_LinReg30_apriori_output)

LDA_LinReg40_apriori_output <- association_rules(complete(LDA_LinReg_MCAR40_sample))
LDA_LinReg_error40 <- check_against_true_rules(apriori_output, LDA_LinReg40_apriori_output)

LDA_LinReg50_apriori_output <- association_rules(complete(LDA_LinReg_MCAR50_sample))
LDA_LinReg_error50 <- check_against_true_rules(apriori_output, LDA_LinReg50_apriori_output)

LDA_LinReg60_apriori_output <- association_rules(complete(LDA_LinReg_MCAR60_sample))
LDA_LinReg_error60 <- check_against_true_rules(apriori_output, LDA_LinReg60_apriori_output)

LDA_LinReg70_apriori_output <- association_rules(complete(LDA_LinReg_MCAR70_sample))
LDA_LinReg_error70 <- check_against_true_rules(apriori_output, LDA_LinReg70_apriori_output)

LDA_LinReg80_apriori_output <- association_rules(complete(LDA_LinReg_MCAR80_sample))
LDA_LinReg_error80 <- check_against_true_rules(apriori_output, LDA_LinReg80_apriori_output)

LDA_LinReg_error_rates <- c(LDA_LinReg_error10, LDA_LinReg_error20, 
                            LDA_LinReg_error30, LDA_LinReg_error40, LDA_LinReg_error50, 
                            LDA_LinReg_error60, LDA_LinReg_error70, LDA_LinReg_error80)
names(LDA_LinReg_error_rates) <- c("10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%")

barplot(LDA_LinReg_error_rates, xlab="Percentage of Values Removed", ylab="Error Rates")



LDA_kNN10_apriori_output <- association_rules(LDA_kNN_MCAR10_sample)
LDA_kNN_error10 <- check_against_true_rules(apriori_output, LDA_kNN10_apriori_output)

LDA_kNN20_apriori_output <- association_rules(LDA_kNN_MCAR20_sample)
LDA_kNN_error20 <- check_against_true_rules(apriori_output, LDA_kNN20_apriori_output)

LDA_kNN30_apriori_output <- association_rules(LDA_kNN_MCAR30_sample)
LDA_kNN_error30 <- check_against_true_rules(apriori_output, LDA_kNN30_apriori_output)

LDA_kNN40_apriori_output <- association_rules(LDA_kNN_MCAR40_sample)
LDA_kNN_error40 <- check_against_true_rules(apriori_output, LDA_kNN40_apriori_output)

LDA_kNN50_apriori_output <- association_rules(LDA_kNN_MCAR50_sample)
LDA_kNN_error50 <- check_against_true_rules(apriori_output, LDA_kNN50_apriori_output)

LDA_kNN60_apriori_output <- association_rules(LDA_kNN_MCAR60_sample)
LDA_kNN_error60 <- check_against_true_rules(apriori_output, LDA_kNN60_apriori_output)

LDA_kNN70_apriori_output <- association_rules(LDA_kNN_MCAR70_sample)
LDA_kNN_error70 <- check_against_true_rules(apriori_output, LDA_kNN70_apriori_output)

LDA_kNN80_apriori_output <- association_rules(LDA_kNN_MCAR80_sample)
LDA_kNN_error80 <- check_against_true_rules(apriori_output, LDA_kNN80_apriori_output)

LDA_kNN_error_rates <- c(LDA_kNN_error10, LDA_kNN_error20, 
                         LDA_kNN_error30, LDA_kNN_error40, LDA_kNN_error50, 
                         LDA_kNN_error60, LDA_kNN_error70, LDA_kNN_error80)
names(LDA_kNN_error_rates) <- c("10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%")

barplot(LDA_kNN_error_rates, xlab="Percentage of Values Removed", ylab="Error Rates")



LDA_PMM10_apriori_output <- association_rules(complete(LDA_PMM_MCAR10_sample))
LDA_PMM_error10 <- check_against_true_rules(apriori_output, LDA_PMM10_apriori_output)

LDA_PMM20_apriori_output <- association_rules(complete(LDA_PMM_MCAR20_sample))
LDA_PMM_error20 <- check_against_true_rules(apriori_output, LDA_PMM20_apriori_output)

LDA_PMM30_apriori_output <- association_rules(complete(LDA_PMM_MCAR30_sample))
LDA_PMM_error30 <- check_against_true_rules(apriori_output, LDA_PMM30_apriori_output)

LDA_PMM40_apriori_output <- association_rules(complete(LDA_PMM_MCAR40_sample))
LDA_PMM_error40 <- check_against_true_rules(apriori_output, LDA_PMM40_apriori_output)

LDA_PMM50_apriori_output <- association_rules(complete(LDA_PMM_MCAR50_sample))
LDA_PMM_error50 <- check_against_true_rules(apriori_output, LDA_PMM50_apriori_output)

LDA_PMM60_apriori_output <- association_rules(complete(LDA_PMM_MCAR60_sample))
LDA_PMM_error60 <- check_against_true_rules(apriori_output, LDA_PMM60_apriori_output)

LDA_PMM70_apriori_output <- association_rules(complete(LDA_PMM_MCAR70_sample))
LDA_PMM_error70 <- check_against_true_rules(apriori_output, LDA_PMM70_apriori_output)

LDA_PMM80_apriori_output <- association_rules(complete(LDA_PMM_MCAR80_sample))
LDA_PMM_error80 <- check_against_true_rules(apriori_output, LDA_PMM80_apriori_output)

LDA_PMM_error_rates <- c(LDA_PMM_error10, LDA_PMM_error20, 
                         LDA_PMM_error30, LDA_PMM_error40, LDA_PMM_error50, 
                         LDA_PMM_error60, LDA_PMM_error70, LDA_PMM_error80)
names(LDA_PMM_error_rates) <- c("10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%")

barplot(LDA_PMM_error_rates, xlab="Percentage of Values Removed", ylab="Error Rates")

ara_pool <- function(list_of_sets_of_rules) {
  pooled_rules <- union(list_of_sets_of_rules[[1]], 
                        list_of_sets_of_rules[[2]], 
                        list_of_sets_of_rules[[3]])
  for (i in 1:10) {
    for (j in 1:10) {
      for (k in 1:10) {
        if (i!=j & i!=k & j!=k) {
          new_rules <- intersect(list_of_sets_of_rules[[i]], 
                                 list_of_sets_of_rules[[j]], 
                                 list_of_sets_of_rules[[k]])
          pooled_rules <- union(pooled_rules, new_rules)
        }
      }
    }
  }
  return(pooled_rules)
}



mice10_apriori_output1 <- association_rules(complete(mice_MCAR10_samples, action = 1))
mice_error10_1 <- check_against_true_rules(apriori_output, mice10_apriori_output1)

mice10_apriori_output2 <- association_rules(complete(mice_MCAR10_samples, action = 2))
mice_error10_2 <- check_against_true_rules(apriori_output, mice10_apriori_output2)

mice10_apriori_output3 <- association_rules(complete(mice_MCAR10_samples, action = 3))
mice_error10_3 <- check_against_true_rules(apriori_output, mice10_apriori_output3)

mice10_apriori_output4 <- association_rules(complete(mice_MCAR10_samples, action = 4))
mice_error10_4 <- check_against_true_rules(apriori_output, mice10_apriori_output4)

mice10_apriori_output5 <- association_rules(complete(mice_MCAR10_samples, action = 5))
mice_error10_5 <- check_against_true_rules(apriori_output, mice10_apriori_output5)

mice10_apriori_output6 <- association_rules(complete(mice_MCAR10_samples, action = 6))
mice_error10_6 <- check_against_true_rules(apriori_output, mice10_apriori_output6)

mice10_apriori_output7 <- association_rules(complete(mice_MCAR10_samples, action = 7))
mice_error10_7 <- check_against_true_rules(apriori_output, mice10_apriori_output7)

mice10_apriori_output8 <- association_rules(complete(mice_MCAR10_samples, action = 8))
mice_error10_8 <- check_against_true_rules(apriori_output, mice10_apriori_output8)

mice10_apriori_output9 <- association_rules(complete(mice_MCAR10_samples, action = 9))
mice_error10_9 <- check_against_true_rules(apriori_output, mice10_apriori_output9)

mice10_apriori_output10 <- association_rules(complete(mice_MCAR10_samples, action = 10))
mice_error10_10 <- check_against_true_rules(apriori_output, mice10_apriori_output10)

mice10_apriori_output <- ara_pool(list(mice10_apriori_output1, 
                                       mice10_apriori_output2, 
                                       mice10_apriori_output3, 
                                       mice10_apriori_output4, 
                                       mice10_apriori_output5, 
                                       mice10_apriori_output6, 
                                       mice10_apriori_output7, 
                                       mice10_apriori_output8, 
                                       mice10_apriori_output9, 
                                       mice10_apriori_output10))
mice_error10 <- check_against_true_rules(apriori_output, mice10_apriori_output)



mice20_apriori_output1 <- association_rules(complete(mice_MCAR20_samples, action = 1))
mice_error20_1 <- check_against_true_rules(apriori_output, mice20_apriori_output1)

mice20_apriori_output2 <- association_rules(complete(mice_MCAR20_samples, action = 2))
mice_error20_2 <- check_against_true_rules(apriori_output, mice20_apriori_output2)

mice20_apriori_output3 <- association_rules(complete(mice_MCAR20_samples, action = 3))
mice_error20_3 <- check_against_true_rules(apriori_output, mice20_apriori_output3)

mice20_apriori_output4 <- association_rules(complete(mice_MCAR20_samples, action = 4))
mice_error20_4 <- check_against_true_rules(apriori_output, mice20_apriori_output4)

mice20_apriori_output5 <- association_rules(complete(mice_MCAR20_samples, action = 5))
mice_error10_5 <- check_against_true_rules(apriori_output, mice10_apriori_output5)

mice20_apriori_output6 <- association_rules(complete(mice_MCAR20_samples, action = 6))
mice_error20_6 <- check_against_true_rules(apriori_output, mice20_apriori_output6)

mice20_apriori_output7 <- association_rules(complete(mice_MCAR20_samples, action = 7))
mice_error20_7 <- check_against_true_rules(apriori_output, mice20_apriori_output7)

mice20_apriori_output8 <- association_rules(complete(mice_MCAR20_samples, action = 8))
mice_error20_8 <- check_against_true_rules(apriori_output, mice20_apriori_output8)

mice20_apriori_output9 <- association_rules(complete(mice_MCAR20_samples, action = 9))
mice_error20_9 <- check_against_true_rules(apriori_output, mice20_apriori_output9)

mice20_apriori_output10 <- association_rules(complete(mice_MCAR20_samples, action = 10))
mice_error20_10 <- check_against_true_rules(apriori_output, mice20_apriori_output10)

mice20_apriori_output <- ara_pool(list(mice20_apriori_output1, 
                                       mice20_apriori_output2, 
                                       mice20_apriori_output3, 
                                       mice20_apriori_output4, 
                                       mice20_apriori_output5, 
                                       mice20_apriori_output6, 
                                       mice20_apriori_output7, 
                                       mice20_apriori_output8, 
                                       mice20_apriori_output9, 
                                       mice20_apriori_output10))
mice_error20 <- check_against_true_rules(apriori_output, mice20_apriori_output)



mice30_apriori_output1 <- association_rules(complete(mice_MCAR30_samples, action = 1))
mice_error30_1 <- check_against_true_rules(apriori_output, mice30_apriori_output1)

mice30_apriori_output2 <- association_rules(complete(mice_MCAR30_samples, action = 2))
mice_error30_2 <- check_against_true_rules(apriori_output, mice30_apriori_output2)

mice30_apriori_output3 <- association_rules(complete(mice_MCAR30_samples, action = 3))
mice_error30_3 <- check_against_true_rules(apriori_output, mice30_apriori_output3)

mice30_apriori_output4 <- association_rules(complete(mice_MCAR30_samples, action = 4))
mice_error30_4 <- check_against_true_rules(apriori_output, mice30_apriori_output4)

mice30_apriori_output5 <- association_rules(complete(mice_MCAR30_samples, action = 5))
mice_error30_5 <- check_against_true_rules(apriori_output, mice30_apriori_output5)

mice30_apriori_output6 <- association_rules(complete(mice_MCAR30_samples, action = 6))
mice_error30_6 <- check_against_true_rules(apriori_output, mice30_apriori_output6)

mice30_apriori_output7 <- association_rules(complete(mice_MCAR30_samples, action = 7))
mice_error30_7 <- check_against_true_rules(apriori_output, mice30_apriori_output7)

mice30_apriori_output8 <- association_rules(complete(mice_MCAR30_samples, action = 8))
mice_error30_8 <- check_against_true_rules(apriori_output, mice30_apriori_output8)

mice30_apriori_output9 <- association_rules(complete(mice_MCAR30_samples, action = 9))
mice_error30_9 <- check_against_true_rules(apriori_output, mice30_apriori_output9)

mice30_apriori_output10 <- association_rules(complete(mice_MCAR30_samples, action = 10))
mice_error30_10 <- check_against_true_rules(apriori_output, mice30_apriori_output10)

mice30_apriori_output <- ara_pool(list(mice30_apriori_output1, 
                                       mice30_apriori_output2, 
                                       mice30_apriori_output3, 
                                       mice30_apriori_output4, 
                                       mice30_apriori_output5, 
                                       mice30_apriori_output6, 
                                       mice30_apriori_output7, 
                                       mice30_apriori_output8, 
                                       mice30_apriori_output9, 
                                       mice30_apriori_output10))
mice_error30 <- check_against_true_rules(apriori_output, mice30_apriori_output)



mice40_apriori_output1 <- association_rules(complete(mice_MCAR40_samples, action = 1))
mice_error40_1 <- check_against_true_rules(apriori_output, mice40_apriori_output1)

mice40_apriori_output2 <- association_rules(complete(mice_MCAR40_samples, action = 2))
mice_error40_2 <- check_against_true_rules(apriori_output, mice40_apriori_output2)

mice40_apriori_output3 <- association_rules(complete(mice_MCAR40_samples, action = 3))
mice_error40_3 <- check_against_true_rules(apriori_output, mice40_apriori_output3)

mice40_apriori_output4 <- association_rules(complete(mice_MCAR40_samples, action = 4))
mice_error40_4 <- check_against_true_rules(apriori_output, mice140_apriori_output4)

mice40_apriori_output5 <- association_rules(complete(mice_MCAR40_samples, action = 5))
mice_error40_5 <- check_against_true_rules(apriori_output, mice40_apriori_output5)

mice40_apriori_output6 <- association_rules(complete(mice_MCAR40_samples, action = 6))
mice_error40_6 <- check_against_true_rules(apriori_output, mice40_apriori_output6)

mice40_apriori_output7 <- association_rules(complete(mice_MCAR40_samples, action = 7))
mice_error40_7 <- check_against_true_rules(apriori_output, mice40_apriori_output7)

mice40_apriori_output8 <- association_rules(complete(mice_MCAR40_samples, action = 8))
mice_error40_8 <- check_against_true_rules(apriori_output, mice40_apriori_output8)

mice40_apriori_output9 <- association_rules(complete(mice_MCAR40_samples, action = 9))
mice_error40_9 <- check_against_true_rules(apriori_output, mice40_apriori_output9)

mice40_apriori_output10 <- association_rules(complete(mice_MCAR40_samples, action = 10))
mice_error40_10 <- check_against_true_rules(apriori_output, mice40_apriori_output10)

mice40_apriori_output <- ara_pool(list(mice40_apriori_output1, 
                                       mice40_apriori_output2, 
                                       mice40_apriori_output3, 
                                       mice40_apriori_output4, 
                                       mice40_apriori_output5, 
                                       mice40_apriori_output6, 
                                       mice40_apriori_output7, 
                                       mice40_apriori_output8, 
                                       mice40_apriori_output9, 
                                       mice40_apriori_output10))
mice_error40 <- check_against_true_rules(apriori_output, mice40_apriori_output)



mice50_apriori_output1 <- association_rules(complete(mice_MCAR50_samples, action = 1))
mice_error50_1 <- check_against_true_rules(apriori_output, mice50_apriori_output1)

mice50_apriori_output2 <- association_rules(complete(mice_MCAR50_samples, action = 2))
mice_error50_2 <- check_against_true_rules(apriori_output, mice50_apriori_output2)

mice50_apriori_output3 <- association_rules(complete(mice_MCAR50_samples, action = 3))
mice_error50_3 <- check_against_true_rules(apriori_output, mice50_apriori_output3)

mice50_apriori_output4 <- association_rules(complete(mice_MCAR50_samples, action = 4))
mice_error50_4 <- check_against_true_rules(apriori_output, mice50_apriori_output4)

mice50_apriori_output5 <- association_rules(complete(mice_MCAR50_samples, action = 5))
mice_error50_5 <- check_against_true_rules(apriori_output, mice50_apriori_output5)

mice50_apriori_output6 <- association_rules(complete(mice_MCAR50_samples, action = 6))
mice_error50_6 <- check_against_true_rules(apriori_output, mice50_apriori_output6)

mice50_apriori_output7 <- association_rules(complete(mice_MCAR50_samples, action = 7))
mice_error50_7 <- check_against_true_rules(apriori_output, mice50_apriori_output7)

mice50_apriori_output8 <- association_rules(complete(mice_MCAR50_samples, action = 8))
mice_error50_8 <- check_against_true_rules(apriori_output, mice50_apriori_output8)

mice50_apriori_output9 <- association_rules(complete(mice_MCAR50_samples, action = 9))
mice_error50_9 <- check_against_true_rules(apriori_output, mice50_apriori_output9)

mice50_apriori_output10 <- association_rules(complete(mice_MCAR50_samples, action = 10))
mice_error50_10 <- check_against_true_rules(apriori_output, mice50_apriori_output10)

mice50_apriori_output <- ara_pool(list(mice50_apriori_output1, 
                                       mice50_apriori_output2, 
                                       mice50_apriori_output3, 
                                       mice50_apriori_output4, 
                                       mice50_apriori_output5, 
                                       mice50_apriori_output6, 
                                       mice50_apriori_output7, 
                                       mice50_apriori_output8, 
                                       mice50_apriori_output9, 
                                       mice50_apriori_output10))
mice_error50 <- check_against_true_rules(apriori_output, mice50_apriori_output)



mice60_apriori_output1 <- association_rules(complete(mice_MCAR60_samples, action = 1))
mice_error60_1 <- check_against_true_rules(apriori_output, mice60_apriori_output1)

mice60_apriori_output2 <- association_rules(complete(mice_MCAR60_samples, action = 2))
mice_error60_2 <- check_against_true_rules(apriori_output, mice60_apriori_output2)

mice60_apriori_output3 <- association_rules(complete(mice_MCAR60_samples, action = 3))
mice_error60_3 <- check_against_true_rules(apriori_output, mice60_apriori_output3)

mice60_apriori_output4 <- association_rules(complete(mice_MCAR60_samples, action = 4))
mice_error60_4 <- check_against_true_rules(apriori_output, mice60_apriori_output4)

mice60_apriori_output5 <- association_rules(complete(mice_MCAR60_samples, action = 5))
mice_error60_5 <- check_against_true_rules(apriori_output, mice60_apriori_output5)

mice60_apriori_output6 <- association_rules(complete(mice_MCAR60_samples, action = 6))
mice_error60_6 <- check_against_true_rules(apriori_output, mice60_apriori_output6)

mice60_apriori_output7 <- association_rules(complete(mice_MCAR60_samples, action = 7))
mice_error60_7 <- check_against_true_rules(apriori_output, mice60_apriori_output7)

mice60_apriori_output8 <- association_rules(complete(mice_MCAR60_samples, action = 8))
mice_error60_8 <- check_against_true_rules(apriori_output, mice60_apriori_output8)

mice60_apriori_output9 <- association_rules(complete(mice_MCAR60_samples, action = 9))
mice_error60_9 <- check_against_true_rules(apriori_output, mice60_apriori_output9)

mice60_apriori_output10 <- association_rules(complete(mice_MCAR60_samples, action = 10))
mice_error60_10 <- check_against_true_rules(apriori_output, mice60_apriori_output10)

mice60_apriori_output <- ara_pool(list(mice60_apriori_output1, 
                                       mice60_apriori_output2, 
                                       mice60_apriori_output3, 
                                       mice60_apriori_output4, 
                                       mice60_apriori_output5, 
                                       mice60_apriori_output6, 
                                       mice60_apriori_output7, 
                                       mice60_apriori_output8, 
                                       mice60_apriori_output9, 
                                       mice60_apriori_output10))
mice_error60 <- check_against_true_rules(apriori_output, mice60_apriori_output)



mice70_apriori_output1 <- association_rules(complete(mice_MCAR70_samples, action = 1))
mice_error70_1 <- check_against_true_rules(apriori_output, mice70_apriori_output1)

mice70_apriori_output2 <- association_rules(complete(mice_MCAR70_samples, action = 2))
mice_error70_2 <- check_against_true_rules(apriori_output, mice70_apriori_output2)

mice70_apriori_output3 <- association_rules(complete(mice_MCAR70_samples, action = 3))
mice_error70_3 <- check_against_true_rules(apriori_output, mice70_apriori_output3)

mice70_apriori_output4 <- association_rules(complete(mice_MCAR70_samples, action = 4))
mice_error70_4 <- check_against_true_rules(apriori_output, mice70_apriori_output4)

mice70_apriori_output5 <- association_rules(complete(mice_MCAR70_samples, action = 5))
mice_error70_5 <- check_against_true_rules(apriori_output, mice70_apriori_output5)

mice70_apriori_output6 <- association_rules(complete(mice_MCAR70_samples, action = 6))
mice_error70_6 <- check_against_true_rules(apriori_output, mice70_apriori_output6)

mice70_apriori_output7 <- association_rules(complete(mice_MCAR70_samples, action = 7))
mice_error70_7 <- check_against_true_rules(apriori_output, mice70_apriori_output7)

mice70_apriori_output8 <- association_rules(complete(mice_MCAR70_samples, action = 8))
mice_error70_8 <- check_against_true_rules(apriori_output, mice70_apriori_output8)

mice70_apriori_output9 <- association_rules(complete(mice_MCAR70_samples, action = 9))
mice_error70_9 <- check_against_true_rules(apriori_output, mice70_apriori_output9)

mice70_apriori_output10 <- association_rules(complete(mice_MCAR70_samples, action = 10))
mice_error70_10 <- check_against_true_rules(apriori_output, mice70_apriori_output10)

mice70_apriori_output <- ara_pool(list(mice70_apriori_output1, 
                                       mice70_apriori_output2, 
                                       mice70_apriori_output3, 
                                       mice70_apriori_output4, 
                                       mice70_apriori_output5, 
                                       mice70_apriori_output6, 
                                       mice70_apriori_output7, 
                                       mice70_apriori_output8, 
                                       mice70_apriori_output9, 
                                       mice70_apriori_output10))
mice_error70 <- check_against_true_rules(apriori_output, mice70_apriori_output)



mice80_apriori_output1 <- association_rules(complete(mice_MCAR80_samples, action = 1))
mice_error80_1 <- check_against_true_rules(apriori_output, mice80_apriori_output1)

mice80_apriori_output2 <- association_rules(complete(mice_MCAR80_samples, action = 2))
mice_error80_2 <- check_against_true_rules(apriori_output, mice80_apriori_output2)

mice80_apriori_output3 <- association_rules(complete(mice_MCAR80_samples, action = 3))
mice_error80_3 <- check_against_true_rules(apriori_output, mice80_apriori_output3)

mice80_apriori_output4 <- association_rules(complete(mice_MCAR80_samples, action = 4))
mice_error80_4 <- check_against_true_rules(apriori_output, mice80_apriori_output4)

mice80_apriori_output5 <- association_rules(complete(mice_MCAR80_samples, action = 5))
mice_error80_5 <- check_against_true_rules(apriori_output, mice80_apriori_output5)

mice80_apriori_output6 <- association_rules(complete(mice_MCAR80_samples, action = 6))
mice_error80_6 <- check_against_true_rules(apriori_output, mice80_apriori_output6)

mice80_apriori_output7 <- association_rules(complete(mice_MCAR80_samples, action = 7))
mice_error80_7 <- check_against_true_rules(apriori_output, mice80_apriori_output7)

mice80_apriori_output8 <- association_rules(complete(mice_MCAR80_samples, action = 8))
mice_error80_8 <- check_against_true_rules(apriori_output, mice80_apriori_output8)

mice80_apriori_output9 <- association_rules(complete(mice_MCAR80_samples, action = 9))
mice_error80_9 <- check_against_true_rules(apriori_output, mice80_apriori_output9)

mice80_apriori_output10 <- association_rules(complete(mice_MCAR80_samples, action = 10))
mice_error80_10 <- check_against_true_rules(apriori_output, mice80_apriori_output10)

mice80_apriori_output <- ara_pool(list(mice80_apriori_output1, 
                                       mice80_apriori_output2, 
                                       mice80_apriori_output3, 
                                       mice80_apriori_output4, 
                                       mice80_apriori_output5, 
                                       mice80_apriori_output6, 
                                       mice80_apriori_output7, 
                                       mice80_apriori_output8, 
                                       mice80_apriori_output9, 
                                       mice80_apriori_output10))
mice_error80 <- check_against_true_rules(apriori_output, mice80_apriori_output)

mice_error_rates <- c(mice_error10, mice_error20, 
                      mice_error30, mice_error40, mice_error50, 
                      mice_error60, mice_error70, mice_error80)
names(mice_error_rates) <- c("10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%")

barplot(mice_error_rates, xlab="Percentage of Values Removed", ylab="Error Rates")
