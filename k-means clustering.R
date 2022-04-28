library(dplyr)
library(comprehenr)

means <- function(df) {
  output <- c()
  for (idx in 1:ncol(df)) {
    output <- append(output, mean(df[,idx]))
  }
  return(output)
}

Mode_full <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}
#Mode_full function code taken from www.tutorialspoint.com

Mode <- function(x) {
  y <- Mode_full(x)
  if (length(y) == 1) {
    return(y)
  } else {
    return(y[1])
  }
}

stan_devs <- function(df, population = FALSE) {
  output <- c()
  for (idx in 1:ncol(df)) {
    next_stan_dev <- sd(df[,idx])
    if (population) {
      n <- ncol(df)
      next_stan_dev <- (((n-1)/n)**0.5) * next_stan_dev
    }
    output <- append(output, next_stan_dev)
  }
  return(output)
}

standardise <- function(df) {
  #df_numeric <- 
  #df_categoric <- 
  means <- means(df)
  stan_devs <- stan_devs(df)
  for (col in 1:ncol(df)) {
    for (row in 1:nrow(df)) {
      df[row, col] <- (df[row,col] - means[col])/stan_devs[col]
    }
  }
  return(df)
}

scree_plot <- function(prcomp_output, pc_ub = 1, pc_lb = 1, plot = TRUE) {
  total_variance <- sum(prcomp_output[[1]]**2)
  poev <- prcomp_output[[1]]/total_variance
  if (plot) {
    plot(poev)
  }
  return(sum(poev[pc_lb:pc_ub]))
}

stan_euc_dist <- function(df, inst1, inst2){
  stand_df <- standardise(df)
  output <- 0
  for (idx in 1:ncol(stand_df)) {
    output <- output + ((stand_df[inst1,idx])-(stand_df[inst2,idx]))**2
  }
  return(output**0.5)
}

euc_dist <- function(df, inst1, inst2){
  output <- 0
  for (idx in 1:ncol(df)) {
    output <- output + ((df[inst1,idx])-(df[inst2,idx]))**2
  }
  return(output**0.5)
}

output_convert <- function(df, k, km.out) {
  assignment_col <- as.data.frame(km.out[[1]])
  colnames(assignment_col) = c("assignment")
  output1 <- as.data.frame(append(df, assignment_col))
  centroids <- as.data.frame(km.out[[2]])
  output <- list(output1, centroids)
  error <- 0
  for (cluster in 1:k) {
    for (inst in 1:nrow(df)) {
      if (assignment_col[inst, 1] == cluster) {
        error <- error + stan_euc_dist(rbind(df,centroids[cluster,]), inst, nrow(df)+1)
      }
    }
  }
  output <- list(output1, as.data.frame(km.out[[2]]), error)
  return(output)
}

multi_k_means <- function(df, k, runs) {
  df[,11] <- as.numeric(df[,11])
  km.out <- kmeans(df,k,nstart=runs)
  output <- output_convert(df, k, km.out)
  output_compact <- output[[1]][1:(ncol(output[[1]])-1)]
  prcomp_output <- prcomp(standardise(output_compact))
  pc1 <- (as.matrix(standardise(output_compact)) %*% as.matrix(prcomp_output[[2]]))[,1]
  pc2 <- (as.matrix(standardise(output_compact)) %*% as.matrix(prcomp_output[[2]]))[,2]
  if (ncol(df) >= 6) {
    pc3 <- (as.matrix(standardise(output_compact)) %*% as.matrix(prcomp_output[[2]]))[,3]
    pc4 <- (as.matrix(standardise(output_compact)) %*% as.matrix(prcomp_output[[2]]))[,4]
    pc5 <- (as.matrix(standardise(output_compact)) %*% as.matrix(prcomp_output[[2]]))[,5]
    pc6 <- (as.matrix(standardise(output_compact)) %*% as.matrix(prcomp_output[[2]]))[,6]
  }
  Assignment <- output[[1]]$assignment
  if (ncol(df) >= 6) {
    grid <- matrix(c(1,2,1,3), nrow = 2)
    layout(grid)
  }
  x_lab <- "First principal component"
  y_lab <- "Second principal component"
  perc_oev2 <- signif(100*scree_plot(prcomp_output, pc_ub = 2, pc_lb =1, plot = FALSE), digits = 3)
  note <- paste("(The first two principal components explain ", as.character(perc_oev2), "% of the total variance in the data)")
  plot(pc1, pc2, col=Assignment, pch=15+Assignment%%5, cex=0.75, xlab=x_lab, ylab=y_lab, sub=note)
  if (ncol(df) >= 6) {
    x_lab <- "Third principal component"
    y_lab <- "Fourth principal component"
    plot(pc3, pc4, col=Assignment, pch=15+Assignment%%5, cex=0.5, xlab=x_lab, ylab=y_lab)
    x_lab <- "Fifth principal component"
    y_lab <- "Sixth principal component"
    plot(pc5, pc6, col=Assignment, pch=15+Assignment%%5, cex=0.5, xlab=x_lab, ylab=y_lab)
  }
  par(mfrow = c(1, 1))
  #text(Age, Height, labels=rownames(output[[1]]), cex=0.5, font=2)
  return(output)
}

type_convert <- function(vec_string) {
  output <- c()
  for (string in vec_string) {
    if (string == "Type I") {
      output <- append(output, 1)
    }
    if (string == "Type II") {
      output <- append(output, 2)
    }
    if (string == "Type III") {
      output <- append(output, 3)
    }
  }
  return(output)
}

centroid_match <- function(Centroids, centroids) {
  perms <- c()
  for (i in 1:length(Centroids)) {
    distances <- to_vec(for(j in 1:length(centroids)) euc_dist( rbind(Centroids[[i]], centroids[[j]]), 1, 2 ))
    perms <- append(perms, which.min(distances))
  }
  perm <- function(x) {
    return(perms[x])
  }
  return(perm)
}

combined_assign <- function(outputs) {
  ref_output <- outputs[[1]]
  Centroid1 <- ref_output[[2]][1,]
  Centroid2 <- ref_output[[2]][2,]
  Centroid3 <- ref_output[[2]][3,]
  Centroids <- list(Centroid1, Centroid2, Centroid3)
  for (imp in 2:10) {
    output_imp_cluster1 <- outputs[[imp]][[1]] %>% filter(assignment == 1)
    output_imp_cluster2 <- outputs[[imp]][[1]] %>% filter(assignment == 2)
    output_imp_cluster3 <- outputs[[imp]][[1]] %>% filter(assignment == 3)
    centroidI <- as.data.frame(t(as.data.frame(colMeans(output_imp_cluster1))))[,1:11]
    centroidII <- as.data.frame(t(as.data.frame(colMeans(output_imp_cluster2))))[,1:11]
    centroidIII <- as.data.frame(t(as.data.frame(colMeans(output_imp_cluster3))))[,1:11]
    centroids <- list(centroidI, centroidII, centroidIII)
    perm <- centroid_match(centroids, Centroids)
    outputs[[imp]][[1]][,12] <- perm(outputs[[imp]][[1]][,12])
  }
  combined_assignment <- ref_output[[1]]
  for (row in 1:nrow(combined_assignment)) {
    assignment_list <- c(combined_assignment[row,12])
    for (imp in 2:10) {
      assignment_list <- append( assignment_list, outputs[[imp]][[1]][row,12] )
    }
    combined_assignment[row,12] <- Mode(assignment_list)
  }
  combined_assignment <- list( combined_assignment, ref_output[[2]], ref_output[[3]] )
  return(combined_assignment)
}

check_against_true_assignment <- function(sample, output) {
  sample[,11] <- as.numeric(sample[,11])
  sample[,12] <- type_convert(sample[,12])
  sample1 <- sample %>% filter(type == 1)
  sample2 <- sample %>% filter(type == 2)
  sample3 <- sample %>% filter(type == 3)
  CentroidI <- as.data.frame(t(as.data.frame(colMeans(sample1))))[,1:11]
  CentroidII <- as.data.frame(t(as.data.frame(colMeans(sample2))))[,1:11]
  CentroidIII <- as.data.frame(t(as.data.frame(colMeans(sample3))))[,1:11]
  Centroids <- list(CentroidI, CentroidII, CentroidIII)
  centroid1 <- output[[2]][1,]
  centroid2 <- output[[2]][2,]
  centroid3 <- output[[2]][3,]
  centroids <- list(centroid1, centroid2, centroid3)
  perm <- centroid_match(Centroids, centroids)
  error_count <- sum(perm(sample[,12]) != output[[1]][,12])
  return(error_count/nrow(sample))
}

k_means_output <- multi_k_means(sample[,1:11], 3, 3)
original_error <- check_against_true_assignment(sample_ID, k_means_output)



RanSam10_k_means_output <- multi_k_means(complete(RanSam_MCAR10_sample), 3, 3)
RanSam20_k_means_output <- multi_k_means(complete(RanSam_MCAR20_sample), 3, 3)
RanSam30_k_means_output <- multi_k_means(complete(RanSam_MCAR30_sample), 3, 3)
RanSam40_k_means_output <- multi_k_means(complete(RanSam_MCAR40_sample), 3, 3)
RanSam50_k_means_output <- multi_k_means(complete(RanSam_MCAR50_sample), 3, 3)
RanSam60_k_means_output <- multi_k_means(complete(RanSam_MCAR60_sample), 3, 3)
RanSam70_k_means_output <- multi_k_means(complete(RanSam_MCAR70_sample), 3, 3)
RanSam80_k_means_output <- multi_k_means(complete(RanSam_MCAR80_sample), 3, 3)

RanSam_error10 <- check_against_true_assignment(sample_ID, RanSam10_k_means_output)
RanSam_error20 <- check_against_true_assignment(sample_ID, RanSam20_k_means_output)
RanSam_error30 <- check_against_true_assignment(sample_ID, RanSam30_k_means_output)
RanSam_error40 <- check_against_true_assignment(sample_ID, RanSam40_k_means_output)
RanSam_error50 <- check_against_true_assignment(sample_ID, RanSam50_k_means_output)
RanSam_error60 <- check_against_true_assignment(sample_ID, RanSam60_k_means_output)
RanSam_error70 <- check_against_true_assignment(sample_ID, RanSam70_k_means_output)
RanSam_error80 <- check_against_true_assignment(sample_ID, RanSam80_k_means_output)

RanSam_error_rates <- c(original_error, RanSam_error10, RanSam_error20, 
                     RanSam_error30, RanSam_error40, RanSam_error50, 
                     RanSam_error60, RanSam_error70, RanSam_error80)
names(RanSam_error_rates) <- c("0% (Original)", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%")

barplot(RanSam_error_rates, xlab="Percentage of Values Removed", ylab="Error Rates")



PPS10_k_means_output <- multi_k_means(complete(PPS_MCAR10_sample), 3, 3)
PPS20_k_means_output <- multi_k_means(complete(PPS_MCAR20_sample), 3, 3)
PPS30_k_means_output <- multi_k_means(complete(PPS_MCAR30_sample), 3, 3)
PPS40_k_means_output <- multi_k_means(complete(PPS_MCAR40_sample), 3, 3)
PPS50_k_means_output <- multi_k_means(complete(PPS_MCAR50_sample), 3, 3)
PPS60_k_means_output <- multi_k_means(complete(PPS_MCAR60_sample), 3, 3)
PPS70_k_means_output <- multi_k_means(complete(PPS_MCAR70_sample), 3, 3)
PPS80_k_means_output <- multi_k_means(complete(PPS_MCAR80_sample), 3, 3)

PPS_error10 <- check_against_true_assignment(sample_ID, PPS10_k_means_output)
PPS_error20 <- check_against_true_assignment(sample_ID, PPS20_k_means_output)
PPS_error30 <- check_against_true_assignment(sample_ID, PPS30_k_means_output)
PPS_error40 <- check_against_true_assignment(sample_ID, PPS40_k_means_output)
PPS_error50 <- check_against_true_assignment(sample_ID, PPS50_k_means_output)
PPS_error60 <- check_against_true_assignment(sample_ID, PPS60_k_means_output)
PPS_error70 <- check_against_true_assignment(sample_ID, PPS70_k_means_output)
PPS_error80 <- check_against_true_assignment(sample_ID, PPS80_k_means_output)

PPS_error_rates <- c(original_error, PPS_error10, PPS_error20, 
                     PPS_error30, PPS_error40, PPS_error50, 
                     PPS_error60, PPS_error70, PPS_error80)
names(PPS_error_rates) <- c("0% (Original)", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%")

barplot(PPS_error_rates, xlab="Percentage of Values Removed", ylab="Error Rates")



LDA_LinReg10_k_means_output <- multi_k_means(complete(LDA_LinReg_MCAR10_sample, action = 1), 3, 3)
LDA_LinReg20_k_means_output <- multi_k_means(complete(LDA_LinReg_MCAR20_sample, action = 1), 3, 3)
LDA_LinReg30_k_means_output <- multi_k_means(complete(LDA_LinReg_MCAR30_sample, action = 1), 3, 3)
LDA_LinReg40_k_means_output <- multi_k_means(complete(LDA_LinReg_MCAR40_sample, action = 1), 3, 3)
LDA_LinReg50_k_means_output <- multi_k_means(complete(LDA_LinReg_MCAR50_sample, action = 1), 3, 3)
LDA_LinReg60_k_means_output <- multi_k_means(complete(LDA_LinReg_MCAR60_sample, action = 1), 3, 3)
LDA_LinReg70_k_means_output <- multi_k_means(complete(LDA_LinReg_MCAR70_sample, action = 1), 3, 3)
LDA_LinReg80_k_means_output <- multi_k_means(complete(LDA_LinReg_MCAR80_sample, action = 1), 3, 3)

LDA_LinReg_error10 <- check_against_true_assignment(sample_ID, LDA_LinReg10_k_means_output)
LDA_LinReg_error20 <- check_against_true_assignment(sample_ID, LDA_LinReg20_k_means_output)
LDA_LinReg_error30 <- check_against_true_assignment(sample_ID, LDA_LinReg30_k_means_output)
LDA_LinReg_error40 <- check_against_true_assignment(sample_ID, LDA_LinReg40_k_means_output)
LDA_LinReg_error50 <- check_against_true_assignment(sample_ID, LDA_LinReg50_k_means_output)
LDA_LinReg_error60 <- check_against_true_assignment(sample_ID, LDA_LinReg60_k_means_output)
LDA_LinReg_error70 <- check_against_true_assignment(sample_ID, LDA_LinReg70_k_means_output)
LDA_LinReg_error80 <- check_against_true_assignment(sample_ID, LDA_LinReg80_k_means_output)

LDA_LinReg_error_rates <- c(original_error, LDA_LinReg_error10, LDA_LinReg_error20, 
                      LDA_LinReg_error30, LDA_LinReg_error40, LDA_LinReg_error50, 
                      LDA_LinReg_error60, LDA_LinReg_error70, LDA_LinReg_error80)
names(LDA_LinReg_error_rates) <- c("0% (Original)", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%")

barplot(LDA_LinReg_error_rates, xlab="Percentage of Values Removed", ylab="Error Rates")



LDA_kNN10_k_means_output <- multi_k_means(LDA_kNN_MCAR10_sample, 3, 3)
LDA_kNN20_k_means_output <- multi_k_means(LDA_kNN_MCAR20_sample, 3, 3)
LDA_kNN30_k_means_output <- multi_k_means(LDA_kNN_MCAR30_sample, 3, 3)
LDA_kNN40_k_means_output <- multi_k_means(LDA_kNN_MCAR40_sample, 3, 3)
LDA_kNN50_k_means_output <- multi_k_means(LDA_kNN_MCAR50_sample, 3, 3)
LDA_kNN60_k_means_output <- multi_k_means(LDA_kNN_MCAR60_sample, 3, 3)
LDA_kNN70_k_means_output <- multi_k_means(LDA_kNN_MCAR70_sample, 3, 3)
LDA_kNN80_k_means_output <- multi_k_means(LDA_kNN_MCAR80_sample, 3, 3)

LDA_kNN_error10 <- check_against_true_assignment(sample_ID, LDA_kNN10_k_means_output)
LDA_kNN_error20 <- check_against_true_assignment(sample_ID, LDA_kNN20_k_means_output)
LDA_kNN_error30 <- check_against_true_assignment(sample_ID, LDA_kNN30_k_means_output)
LDA_kNN_error40 <- check_against_true_assignment(sample_ID, LDA_kNN40_k_means_output)
LDA_kNN_error50 <- check_against_true_assignment(sample_ID, LDA_kNN50_k_means_output)
LDA_kNN_error60 <- check_against_true_assignment(sample_ID, LDA_kNN60_k_means_output)
LDA_kNN_error70 <- check_against_true_assignment(sample_ID, LDA_kNN70_k_means_output)
LDA_kNN_error80 <- check_against_true_assignment(sample_ID, LDA_kNN80_k_means_output)

LDA_kNN_error_rates <- c(original_error, LDA_kNN_error10, LDA_kNN_error20, 
                            LDA_kNN_error30, LDA_kNN_error40, LDA_kNN_error50, 
                            LDA_kNN_error60, LDA_kNN_error70, LDA_kNN_error80)
names(LDA_kNN_error_rates) <- c("0% (Original)", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%")

barplot(LDA_kNN_error_rates, xlab="Percentage of Values Removed", ylab="Error Rates")



LDA_PMM10_k_means_output <- multi_k_means(complete(LDA_PMM_MCAR10_sample, action = 1), 3, 3)
LDA_PMM20_k_means_output <- multi_k_means(complete(LDA_PMM_MCAR20_sample, action = 1), 3, 3)
LDA_PMM30_k_means_output <- multi_k_means(complete(LDA_PMM_MCAR30_sample, action = 1), 3, 3)
LDA_PMM40_k_means_output <- multi_k_means(complete(LDA_PMM_MCAR40_sample, action = 1), 3, 3)
LDA_PMM50_k_means_output <- multi_k_means(complete(LDA_PMM_MCAR50_sample, action = 1), 3, 3)
LDA_PMM60_k_means_output <- multi_k_means(complete(LDA_PMM_MCAR60_sample, action = 1), 3, 3)
LDA_PMM70_k_means_output <- multi_k_means(complete(LDA_PMM_MCAR70_sample, action = 1), 3, 3)
LDA_PMM80_k_means_output <- multi_k_means(complete(LDA_PMM_MCAR80_sample, action = 1), 3, 3)

LDA_PMM_error10 <- check_against_true_assignment(sample_ID, LDA_PMM10_k_means_output)
LDA_PMM_error20 <- check_against_true_assignment(sample_ID, LDA_PMM20_k_means_output)
LDA_PMM_error30 <- check_against_true_assignment(sample_ID, LDA_PMM30_k_means_output)
LDA_PMM_error40 <- check_against_true_assignment(sample_ID, LDA_PMM40_k_means_output)
LDA_PMM_error50 <- check_against_true_assignment(sample_ID, LDA_PMM50_k_means_output)
LDA_PMM_error60 <- check_against_true_assignment(sample_ID, LDA_PMM60_k_means_output)
LDA_PMM_error70 <- check_against_true_assignment(sample_ID, LDA_PMM70_k_means_output)
LDA_PMM_error80 <- check_against_true_assignment(sample_ID, LDA_PMM80_k_means_output)

LDA_PMM_error_rates <- c(original_error, LDA_PMM_error10, LDA_PMM_error20, 
                         LDA_PMM_error30, LDA_PMM_error40, LDA_PMM_error50, 
                         LDA_PMM_error60, LDA_PMM_error70, LDA_PMM_error80)
names(LDA_PMM_error_rates) <- c("0% (Original)", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%")

barplot(LDA_PMM_error_rates, xlab="Percentage of Values Removed", ylab="Error Rates")



LogReg_LinReg10_k_means_output <- multi_k_means(complete(LogReg_LinReg_MCAR10_sample), 3, 3)
LogReg_LinReg20_k_means_output <- multi_k_means(complete(LogReg_LinReg_MCAR20_sample), 3, 3)
LogReg_LinReg30_k_means_output <- multi_k_means(complete(LogReg_LinReg_MCAR30_sample), 3, 3)
LogReg_LinReg40_k_means_output <- multi_k_means(complete(LogReg_LinReg_MCAR40_sample), 3, 3)
LogReg_LinReg50_k_means_output <- multi_k_means(complete(LogReg_LinReg_MCAR50_sample), 3, 3)
LogReg_LinReg60_k_means_output <- multi_k_means(complete(LogReg_LinReg_MCAR60_sample), 3, 3)
LogReg_LinReg70_k_means_output <- multi_k_means(complete(LogReg_LinReg_MCAR70_sample), 3, 3)
LogReg_LinReg80_k_means_output <- multi_k_means(complete(LogReg_LinReg_MCAR80_sample), 3, 3)

LogReg_LinReg_error10 <- check_against_true_assignment(sample_ID, LogReg_LinReg10_k_means_output)
LogReg_LinReg_error20 <- check_against_true_assignment(sample_ID, LogReg_LinReg20_k_means_output)
LogReg_LinReg_error30 <- check_against_true_assignment(sample_ID, LogReg_LinReg30_k_means_output)
LogReg_LinReg_error40 <- check_against_true_assignment(sample_ID, LogReg_LinReg40_k_means_output)
LogReg_LinReg_error50 <- check_against_true_assignment(sample_ID, LogReg_LinReg50_k_means_output)
LogReg_LinReg_error60 <- check_against_true_assignment(sample_ID, LogReg_LinReg60_k_means_output)
LogReg_LinReg_error70 <- check_against_true_assignment(sample_ID, LogReg_LinReg70_k_means_output)
LogReg_LinReg_error80 <- check_against_true_assignment(sample_ID, LogReg_LinReg80_k_means_output)

LogReg_LinReg_error_rates <- c(original_error, LogReg_LinReg_error10, LogReg_LinReg_error20, 
                               LogReg_LinReg_error30, LogReg_LinReg_error40, LogReg_LinReg_error50, 
                               LogReg_LinReg_error60, LogReg_LinReg_error70, LogReg_LinReg_error80)
names(LogReg_LinReg_error_rates) <- c("0% (Original)", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%")

barplot(LogReg_LinReg_error_rates, xlab="Percentage of Values Removed", ylab="Error Rates")



LogReg_kNN10_k_means_output <- multi_k_means(LogReg_kNN_MCAR10_sample, 3, 3)
LogReg_kNN20_k_means_output <- multi_k_means(LogReg_kNN_MCAR20_sample, 3, 3)
LogReg_kNN30_k_means_output <- multi_k_means(LogReg_kNN_MCAR30_sample, 3, 3)
LogReg_kNN40_k_means_output <- multi_k_means(LogReg_kNN_MCAR40_sample, 3, 3)
LogReg_kNN50_k_means_output <- multi_k_means(LogReg_kNN_MCAR50_sample, 3, 3)
LogReg_kNN60_k_means_output <- multi_k_means(LogReg_kNN_MCAR60_sample, 3, 3)
LogReg_kNN70_k_means_output <- multi_k_means(LogReg_kNN_MCAR70_sample, 3, 3)
LogReg_kNN80_k_means_output <- multi_k_means(LogReg_kNN_MCAR80_sample, 3, 3)

LogReg_kNN_error10 <- check_against_true_assignment(sample_ID, LogReg_kNN10_k_means_output)
LogReg_kNN_error20 <- check_against_true_assignment(sample_ID, LogReg_kNN20_k_means_output)
LogReg_kNN_error30 <- check_against_true_assignment(sample_ID, LogReg_kNN30_k_means_output)
LogReg_kNN_error40 <- check_against_true_assignment(sample_ID, LogReg_kNN40_k_means_output)
LogReg_kNN_error50 <- check_against_true_assignment(sample_ID, LogReg_kNN50_k_means_output)
LogReg_kNN_error60 <- check_against_true_assignment(sample_ID, LogReg_kNN60_k_means_output)
LogReg_kNN_error70 <- check_against_true_assignment(sample_ID, LogReg_kNN70_k_means_output)
LogReg_kNN_error80 <- check_against_true_assignment(sample_ID, LogReg_kNN80_k_means_output)

LogReg_kNN_error_rates <- c(original_error, LogReg_kNN_error10, LogReg_kNN_error20, 
                               LogReg_kNN_error30, LogReg_kNN_error40, LogReg_kNN_error50, 
                               LogReg_kNN_error60, LogReg_kNN_error70, LogReg_kNN_error80)
names(LogReg_kNN_error_rates) <- c("0% (Original)", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%")

barplot(LogReg_kNN_error_rates, xlab="Percentage of Values Removed", ylab="Error Rates")



LogReg_PMM10_k_means_output <- multi_k_means(complete(LogReg_PMM_MCAR10_sample), 3, 3)
LogReg_PMM20_k_means_output <- multi_k_means(complete(LogReg_PMM_MCAR20_sample), 3, 3)
LogReg_PMM30_k_means_output <- multi_k_means(complete(LogReg_PMM_MCAR30_sample), 3, 3)
LogReg_PMM40_k_means_output <- multi_k_means(complete(LogReg_PMM_MCAR40_sample), 3, 3)
LogReg_PMM50_k_means_output <- multi_k_means(complete(LogReg_PMM_MCAR50_sample), 3, 3)
LogReg_PMM60_k_means_output <- multi_k_means(complete(LogReg_PMM_MCAR60_sample), 3, 3)
LogReg_PMM70_k_means_output <- multi_k_means(complete(LogReg_PMM_MCAR70_sample), 3, 3)
LogReg_PMM80_k_means_output <- multi_k_means(complete(LogReg_PMM_MCAR80_sample), 3, 3)

LogReg_PMM_error10 <- check_against_true_assignment(sample_ID, LogReg_PMM10_k_means_output)
LogReg_PMM_error20 <- check_against_true_assignment(sample_ID, LogReg_PMM20_k_means_output)
LogReg_PMM_error30 <- check_against_true_assignment(sample_ID, LogReg_PMM30_k_means_output)
LogReg_PMM_error40 <- check_against_true_assignment(sample_ID, LogReg_PMM40_k_means_output)
LogReg_PMM_error50 <- check_against_true_assignment(sample_ID, LogReg_PMM50_k_means_output)
LogReg_PMM_error60 <- check_against_true_assignment(sample_ID, LogReg_PMM60_k_means_output)
LogReg_PMM_error70 <- check_against_true_assignment(sample_ID, LogReg_PMM70_k_means_output)
LogReg_PMM_error80 <- check_against_true_assignment(sample_ID, LogReg_PMM80_k_means_output)

LogReg_PMM_error_rates <- c(original_error, LogReg_PMM_error10, LogReg_PMM_error20, 
                            LogReg_PMM_error30, LogReg_PMM_error40, LogReg_PMM_error50, 
                            LogReg_PMM_error60, LogReg_PMM_error70, LogReg_PMM_error80)
names(LogReg_PMM_error_rates) <- c("0% (Original)", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%")

barplot(LogReg_PMM_error_rates, xlab="Percentage of Values Removed", ylab="Error Rates")



mice10_k_means_output1 <- multi_k_means(complete(mice_MCAR10_samples, action = 1), 3, 3)
mice10_k_means_output2 <- multi_k_means(complete(mice_MCAR10_samples, action = 2), 3, 3)
mice10_k_means_output3 <- multi_k_means(complete(mice_MCAR10_samples, action = 3), 3, 3)
mice10_k_means_output4 <- multi_k_means(complete(mice_MCAR10_samples, action = 4), 3, 3)
mice10_k_means_output5 <- multi_k_means(complete(mice_MCAR10_samples, action = 5), 3, 3)
mice10_k_means_output6 <- multi_k_means(complete(mice_MCAR10_samples, action = 6), 3, 3)
mice10_k_means_output7 <- multi_k_means(complete(mice_MCAR10_samples, action = 7), 3, 3)
mice10_k_means_output8 <- multi_k_means(complete(mice_MCAR10_samples, action = 8), 3, 3)
mice10_k_means_output9 <- multi_k_means(complete(mice_MCAR10_samples, action = 9), 3, 3)
mice10_k_means_output10 <- multi_k_means(complete(mice_MCAR10_samples, action = 10), 3, 3)
mice10_k_means_avg_output <- combined_assign( list(mice10_k_means_output1, mice10_k_means_output2, 
                                                   mice10_k_means_output3, mice10_k_means_output4, 
                                                   mice10_k_means_output5, mice10_k_means_output6, 
                                                   mice10_k_means_output7, mice10_k_means_output8, 
                                                   mice10_k_means_output9, mice10_k_means_output10) )

mice20_k_means_output1 <- multi_k_means(complete(mice_MCAR20_samples, action = 1), 3, 3)
mice20_k_means_output2 <- multi_k_means(complete(mice_MCAR20_samples, action = 2), 3, 3)
mice20_k_means_output3 <- multi_k_means(complete(mice_MCAR20_samples, action = 3), 3, 3)
mice20_k_means_output4 <- multi_k_means(complete(mice_MCAR20_samples, action = 4), 3, 3)
mice20_k_means_output5 <- multi_k_means(complete(mice_MCAR20_samples, action = 5), 3, 3)
mice20_k_means_output6 <- multi_k_means(complete(mice_MCAR20_samples, action = 6), 3, 3)
mice20_k_means_output7 <- multi_k_means(complete(mice_MCAR20_samples, action = 7), 3, 3)
mice20_k_means_output8 <- multi_k_means(complete(mice_MCAR20_samples, action = 8), 3, 3)
mice20_k_means_output9 <- multi_k_means(complete(mice_MCAR20_samples, action = 9), 3, 3)
mice20_k_means_output10 <- multi_k_means(complete(mice_MCAR20_samples, action = 10), 3, 3)
mice20_k_means_avg_output <- combined_assign( list(mice20_k_means_output1, mice20_k_means_output2, 
                                                   mice20_k_means_output3, mice20_k_means_output4, 
                                                   mice20_k_means_output5, mice20_k_means_output6, 
                                                   mice20_k_means_output7, mice20_k_means_output8, 
                                                   mice20_k_means_output9, mice20_k_means_output10) )

mice30_k_means_output1 <- multi_k_means(complete(mice_MCAR30_samples, action = 1), 3, 3)
mice30_k_means_output2 <- multi_k_means(complete(mice_MCAR30_samples, action = 2), 3, 3)
mice30_k_means_output3 <- multi_k_means(complete(mice_MCAR30_samples, action = 3), 3, 3)
mice30_k_means_output4 <- multi_k_means(complete(mice_MCAR30_samples, action = 4), 3, 3)
mice30_k_means_output5 <- multi_k_means(complete(mice_MCAR30_samples, action = 5), 3, 3)
mice30_k_means_output6 <- multi_k_means(complete(mice_MCAR30_samples, action = 6), 3, 3)
mice30_k_means_output7 <- multi_k_means(complete(mice_MCAR30_samples, action = 7), 3, 3)
mice30_k_means_output8 <- multi_k_means(complete(mice_MCAR30_samples, action = 8), 3, 3)
mice30_k_means_output9 <- multi_k_means(complete(mice_MCAR30_samples, action = 9), 3, 3)
mice30_k_means_output10 <- multi_k_means(complete(mice_MCAR30_samples, action = 10), 3, 3)
mice30_k_means_avg_output <- combined_assign( list(mice30_k_means_output1, mice30_k_means_output2, 
                                                   mice30_k_means_output3, mice30_k_means_output4, 
                                                   mice30_k_means_output5, mice30_k_means_output6, 
                                                   mice30_k_means_output7, mice30_k_means_output8, 
                                                   mice30_k_means_output9, mice30_k_means_output10) )

mice40_k_means_output1 <- multi_k_means(complete(mice_MCAR40_samples, action = 1), 3, 3)
mice40_k_means_output2 <- multi_k_means(complete(mice_MCAR40_samples, action = 2), 3, 3)
mice40_k_means_output3 <- multi_k_means(complete(mice_MCAR40_samples, action = 3), 3, 3)
mice40_k_means_output4 <- multi_k_means(complete(mice_MCAR40_samples, action = 4), 3, 3)
mice40_k_means_output5 <- multi_k_means(complete(mice_MCAR40_samples, action = 5), 3, 3)
mice40_k_means_output6 <- multi_k_means(complete(mice_MCAR40_samples, action = 6), 3, 3)
mice40_k_means_output7 <- multi_k_means(complete(mice_MCAR40_samples, action = 7), 3, 3)
mice40_k_means_output8 <- multi_k_means(complete(mice_MCAR40_samples, action = 8), 3, 3)
mice40_k_means_output9 <- multi_k_means(complete(mice_MCAR40_samples, action = 9), 3, 3)
mice40_k_means_output10 <- multi_k_means(complete(mice_MCAR40_samples, action = 10), 3, 3)
mice40_k_means_avg_output <- combined_assign( list(mice40_k_means_output1, mice40_k_means_output2, 
                                                   mice40_k_means_output3, mice40_k_means_output4, 
                                                   mice40_k_means_output5, mice40_k_means_output6, 
                                                   mice40_k_means_output7, mice40_k_means_output8, 
                                                   mice40_k_means_output9, mice40_k_means_output10) )

mice50_k_means_output1 <- multi_k_means(complete(mice_MCAR50_samples, action = 1), 3, 3)
mice50_k_means_output2 <- multi_k_means(complete(mice_MCAR50_samples, action = 2), 3, 3)
mice50_k_means_output3 <- multi_k_means(complete(mice_MCAR50_samples, action = 3), 3, 3)
mice50_k_means_output4 <- multi_k_means(complete(mice_MCAR50_samples, action = 4), 3, 3)
mice50_k_means_output5 <- multi_k_means(complete(mice_MCAR50_samples, action = 5), 3, 3)
mice50_k_means_output6 <- multi_k_means(complete(mice_MCAR50_samples, action = 6), 3, 3)
mice50_k_means_output7 <- multi_k_means(complete(mice_MCAR50_samples, action = 7), 3, 3)
mice50_k_means_output8 <- multi_k_means(complete(mice_MCAR50_samples, action = 8), 3, 3)
mice50_k_means_output9 <- multi_k_means(complete(mice_MCAR50_samples, action = 9), 3, 3)
mice50_k_means_output10 <- multi_k_means(complete(mice_MCAR50_samples, action = 10), 3, 3)
mice50_k_means_avg_output <- combined_assign( list(mice50_k_means_output1, mice50_k_means_output2, 
                                                   mice50_k_means_output3, mice50_k_means_output4, 
                                                   mice50_k_means_output5, mice50_k_means_output6, 
                                                   mice50_k_means_output7, mice50_k_means_output8, 
                                                   mice50_k_means_output9, mice50_k_means_output10) )

mice60_k_means_output1 <- multi_k_means(complete(mice_MCAR60_samples, action = 1), 3, 3)
mice60_k_means_output2 <- multi_k_means(complete(mice_MCAR60_samples, action = 2), 3, 3)
mice60_k_means_output3 <- multi_k_means(complete(mice_MCAR60_samples, action = 3), 3, 3)
mice60_k_means_output4 <- multi_k_means(complete(mice_MCAR60_samples, action = 4), 3, 3)
mice60_k_means_output5 <- multi_k_means(complete(mice_MCAR60_samples, action = 5), 3, 3)
mice60_k_means_output6 <- multi_k_means(complete(mice_MCAR60_samples, action = 6), 3, 3)
mice60_k_means_output7 <- multi_k_means(complete(mice_MCAR60_samples, action = 7), 3, 3)
mice60_k_means_output8 <- multi_k_means(complete(mice_MCAR60_samples, action = 8), 3, 3)
mice60_k_means_output9 <- multi_k_means(complete(mice_MCAR60_samples, action = 9), 3, 3)
mice60_k_means_output10 <- multi_k_means(complete(mice_MCAR60_samples, action = 10), 3, 3)
mice60_k_means_avg_output <- combined_assign( list(mice60_k_means_output1, mice60_k_means_output2, 
                                                   mice60_k_means_output3, mice60_k_means_output4, 
                                                   mice60_k_means_output5, mice60_k_means_output6, 
                                                   mice60_k_means_output7, mice60_k_means_output8, 
                                                   mice60_k_means_output9, mice60_k_means_output10) )

mice70_k_means_output1 <- multi_k_means(complete(mice_MCAR70_samples, action = 1), 3, 3)
mice70_k_means_output2 <- multi_k_means(complete(mice_MCAR70_samples, action = 2), 3, 3)
mice70_k_means_output3 <- multi_k_means(complete(mice_MCAR70_samples, action = 3), 3, 3)
mice70_k_means_output4 <- multi_k_means(complete(mice_MCAR70_samples, action = 4), 3, 3)
mice70_k_means_output5 <- multi_k_means(complete(mice_MCAR70_samples, action = 5), 3, 3)
mice70_k_means_output6 <- multi_k_means(complete(mice_MCAR70_samples, action = 6), 3, 3)
mice70_k_means_output7 <- multi_k_means(complete(mice_MCAR70_samples, action = 7), 3, 3)
mice70_k_means_output8 <- multi_k_means(complete(mice_MCAR70_samples, action = 8), 3, 3)
mice70_k_means_output9 <- multi_k_means(complete(mice_MCAR70_samples, action = 9), 3, 3)
mice70_k_means_output10 <- multi_k_means(complete(mice_MCAR70_samples, action = 10), 3, 3)
mice70_k_means_avg_output <- combined_assign( list(mice70_k_means_output1, mice70_k_means_output2, 
                                                   mice70_k_means_output3, mice70_k_means_output4, 
                                                   mice70_k_means_output5, mice70_k_means_output6, 
                                                   mice70_k_means_output7, mice70_k_means_output8, 
                                                   mice70_k_means_output9, mice70_k_means_output10) )

mice80_k_means_output1 <- multi_k_means(complete(mice_MCAR80_samples, action = 1), 3, 3)
mice80_k_means_output2 <- multi_k_means(complete(mice_MCAR80_samples, action = 2), 3, 3)
mice80_k_means_output3 <- multi_k_means(complete(mice_MCAR80_samples, action = 3), 3, 3)
mice80_k_means_output4 <- multi_k_means(complete(mice_MCAR80_samples, action = 4), 3, 3)
mice80_k_means_output5 <- multi_k_means(complete(mice_MCAR80_samples, action = 5), 3, 3)
mice80_k_means_output6 <- multi_k_means(complete(mice_MCAR80_samples, action = 6), 3, 3)
mice80_k_means_output7 <- multi_k_means(complete(mice_MCAR80_samples, action = 7), 3, 3)
mice80_k_means_output8 <- multi_k_means(complete(mice_MCAR80_samples, action = 8), 3, 3)
mice80_k_means_output9 <- multi_k_means(complete(mice_MCAR80_samples, action = 9), 3, 3)
mice80_k_means_output10 <- multi_k_means(complete(mice_MCAR80_samples, action = 10), 3, 3)
mice80_k_means_avg_output <- combined_assign( list(mice80_k_means_output1, mice80_k_means_output2, 
                                                   mice80_k_means_output3, mice80_k_means_output4, 
                                                   mice80_k_means_output5, mice80_k_means_output6, 
                                                   mice80_k_means_output7, mice80_k_means_output8, 
                                                   mice80_k_means_output9, mice80_k_means_output10) )

mice_error10_1 <- check_against_true_assignment(sample_ID, mice10_k_means_output1)
mice_error10_2 <- check_against_true_assignment(sample_ID, mice10_k_means_output2)
mice_error10_3 <- check_against_true_assignment(sample_ID, mice10_k_means_output3)
mice_error10_4 <- check_against_true_assignment(sample_ID, mice10_k_means_output4)
mice_error10_5 <- check_against_true_assignment(sample_ID, mice10_k_means_output5)
mice_error10_6 <- check_against_true_assignment(sample_ID, mice10_k_means_output6)
mice_error10_7 <- check_against_true_assignment(sample_ID, mice10_k_means_output7)
mice_error10_8 <- check_against_true_assignment(sample_ID, mice10_k_means_output8)
mice_error10_9 <- check_against_true_assignment(sample_ID, mice10_k_means_output9)
mice_error10_10 <- check_against_true_assignment(sample_ID, mice10_k_means_output10)
mice_error10 <- mean( c(mice_error10_1, mice_error10_2, mice_error10_3, 
                        mice_error10_4, mice_error10_5, mice_error10_6, 
                        mice_error10_7, mice_error10_8, mice_error10_9, 
                        mice_error10_10) )
mice_error10_combined <- check_against_true_assignment(sample_ID, mice10_k_means_avg_output)

mice_error20_1 <- check_against_true_assignment(sample_ID, mice20_k_means_output1)
mice_error20_2 <- check_against_true_assignment(sample_ID, mice20_k_means_output2)
mice_error20_3 <- check_against_true_assignment(sample_ID, mice20_k_means_output3)
mice_error20_4 <- check_against_true_assignment(sample_ID, mice20_k_means_output4)
mice_error20_5 <- check_against_true_assignment(sample_ID, mice20_k_means_output5)
mice_error20_6 <- check_against_true_assignment(sample_ID, mice20_k_means_output6)
mice_error20_7 <- check_against_true_assignment(sample_ID, mice20_k_means_output7)
mice_error20_8 <- check_against_true_assignment(sample_ID, mice20_k_means_output8)
mice_error20_9 <- check_against_true_assignment(sample_ID, mice20_k_means_output9)
mice_error20_10 <- check_against_true_assignment(sample_ID, mice20_k_means_output10)
mice_error20 <- mean( c(mice_error20_1, mice_error20_2, mice_error20_3, 
                        mice_error20_4, mice_error20_5, mice_error20_6, 
                        mice_error20_7, mice_error20_8, mice_error20_9, 
                        mice_error20_10) )
mice_error20_combined <- check_against_true_assignment(sample_ID, mice20_k_means_avg_output)

mice_error30_1 <- check_against_true_assignment(sample_ID, mice30_k_means_output1)
mice_error30_2 <- check_against_true_assignment(sample_ID, mice30_k_means_output2)
mice_error30_3 <- check_against_true_assignment(sample_ID, mice30_k_means_output3)
mice_error30_4 <- check_against_true_assignment(sample_ID, mice30_k_means_output4)
mice_error30_5 <- check_against_true_assignment(sample_ID, mice30_k_means_output5)
mice_error30_6 <- check_against_true_assignment(sample_ID, mice30_k_means_output6)
mice_error30_7 <- check_against_true_assignment(sample_ID, mice30_k_means_output7)
mice_error30_8 <- check_against_true_assignment(sample_ID, mice30_k_means_output8)
mice_error30_9 <- check_against_true_assignment(sample_ID, mice30_k_means_output9)
mice_error30_10 <- check_against_true_assignment(sample_ID, mice30_k_means_output10)
mice_error30 <- mean( c(mice_error30_1, mice_error30_2, mice_error30_3, 
                        mice_error30_4, mice_error30_5, mice_error30_6, 
                        mice_error30_7, mice_error30_8, mice_error30_9, 
                        mice_error30_10) )
mice_error30_combined <- check_against_true_assignment(sample_ID, mice30_k_means_avg_output)

mice_error40_1 <- check_against_true_assignment(sample_ID, mice40_k_means_output1)
mice_error40_2 <- check_against_true_assignment(sample_ID, mice40_k_means_output2)
mice_error40_3 <- check_against_true_assignment(sample_ID, mice40_k_means_output3)
mice_error40_4 <- check_against_true_assignment(sample_ID, mice40_k_means_output4)
mice_error40_5 <- check_against_true_assignment(sample_ID, mice40_k_means_output5)
mice_error40_6 <- check_against_true_assignment(sample_ID, mice40_k_means_output6)
mice_error40_7 <- check_against_true_assignment(sample_ID, mice40_k_means_output7)
mice_error40_8 <- check_against_true_assignment(sample_ID, mice40_k_means_output8)
mice_error40_9 <- check_against_true_assignment(sample_ID, mice40_k_means_output9)
mice_error40_10 <- check_against_true_assignment(sample_ID, mice40_k_means_output10)
mice_error40 <- mean( c(mice_error40_1, mice_error40_2, mice_error40_3, 
                        mice_error40_4, mice_error40_5, mice_error40_6, 
                        mice_error40_7, mice_error40_8, mice_error40_9, 
                        mice_error40_10) )
mice_error40_combined <- check_against_true_assignment(sample_ID, mice40_k_means_avg_output)

mice_error50_1 <- check_against_true_assignment(sample_ID, mice50_k_means_output1)
mice_error50_2 <- check_against_true_assignment(sample_ID, mice50_k_means_output2)
mice_error50_3 <- check_against_true_assignment(sample_ID, mice50_k_means_output3)
mice_error50_4 <- check_against_true_assignment(sample_ID, mice50_k_means_output4)
mice_error50_5 <- check_against_true_assignment(sample_ID, mice50_k_means_output5)
mice_error50_6 <- check_against_true_assignment(sample_ID, mice50_k_means_output6)
mice_error50_7 <- check_against_true_assignment(sample_ID, mice50_k_means_output7)
mice_error50_8 <- check_against_true_assignment(sample_ID, mice50_k_means_output8)
mice_error50_9 <- check_against_true_assignment(sample_ID, mice50_k_means_output9)
mice_error50_10 <- check_against_true_assignment(sample_ID, mice50_k_means_output10)
mice_error50 <- mean( c(mice_error50_1, mice_error50_2, mice_error50_3, 
                        mice_error50_4, mice_error50_5, mice_error50_6, 
                        mice_error50_7, mice_error50_8, mice_error50_9, 
                        mice_error50_10) )
mice_error50_combined <- check_against_true_assignment(sample_ID, mice50_k_means_avg_output)

mice_error60_1 <- check_against_true_assignment(sample_ID, mice60_k_means_output1)
mice_error60_2 <- check_against_true_assignment(sample_ID, mice60_k_means_output2)
mice_error60_3 <- check_against_true_assignment(sample_ID, mice60_k_means_output3)
mice_error60_4 <- check_against_true_assignment(sample_ID, mice60_k_means_output4)
mice_error60_5 <- check_against_true_assignment(sample_ID, mice60_k_means_output5)
mice_error60_6 <- check_against_true_assignment(sample_ID, mice60_k_means_output6)
mice_error60_7 <- check_against_true_assignment(sample_ID, mice60_k_means_output7)
mice_error60_8 <- check_against_true_assignment(sample_ID, mice60_k_means_output8)
mice_error60_9 <- check_against_true_assignment(sample_ID, mice60_k_means_output9)
mice_error60_10 <- check_against_true_assignment(sample_ID, mice60_k_means_output10)
mice_error60 <- mean( c(mice_error60_1, mice_error60_2, mice_error60_3, 
                        mice_error60_4, mice_error60_5, mice_error60_6, 
                        mice_error60_7, mice_error60_8, mice_error60_9, 
                        mice_error60_10) )
mice_error60_combined <- check_against_true_assignment(sample_ID, mice60_k_means_avg_output)

mice_error70_1 <- check_against_true_assignment(sample_ID, mice70_k_means_output1)
mice_error70_2 <- check_against_true_assignment(sample_ID, mice70_k_means_output2)
mice_error70_3 <- check_against_true_assignment(sample_ID, mice70_k_means_output3)
mice_error70_4 <- check_against_true_assignment(sample_ID, mice70_k_means_output4)
mice_error70_5 <- check_against_true_assignment(sample_ID, mice70_k_means_output5)
mice_error70_6 <- check_against_true_assignment(sample_ID, mice70_k_means_output6)
mice_error70_7 <- check_against_true_assignment(sample_ID, mice70_k_means_output7)
mice_error70_8 <- check_against_true_assignment(sample_ID, mice70_k_means_output8)
mice_error70_9 <- check_against_true_assignment(sample_ID, mice70_k_means_output9)
mice_error70_10 <- check_against_true_assignment(sample_ID, mice70_k_means_output10)
mice_error70 <- mean( c(mice_error70_1, mice_error70_2, mice_error70_3, 
                        mice_error70_4, mice_error70_5, mice_error70_6, 
                        mice_error70_7, mice_error70_8, mice_error70_9, 
                        mice_error70_10) )
mice_error70_combined <- check_against_true_assignment(sample_ID, mice70_k_means_avg_output)

mice_error80_1 <- check_against_true_assignment(sample_ID, mice80_k_means_output1)
mice_error80_2 <- check_against_true_assignment(sample_ID, mice80_k_means_output2)
mice_error80_3 <- check_against_true_assignment(sample_ID, mice80_k_means_output3)
mice_error80_4 <- check_against_true_assignment(sample_ID, mice80_k_means_output4)
mice_error80_5 <- check_against_true_assignment(sample_ID, mice80_k_means_output5)
mice_error80_6 <- check_against_true_assignment(sample_ID, mice80_k_means_output6)
mice_error80_7 <- check_against_true_assignment(sample_ID, mice80_k_means_output7)
mice_error80_8 <- check_against_true_assignment(sample_ID, mice80_k_means_output8)
mice_error80_9 <- check_against_true_assignment(sample_ID, mice80_k_means_output9)
mice_error80_10 <- check_against_true_assignment(sample_ID, mice80_k_means_output10)
mice_error80 <- mean( c(mice_error80_1, mice_error80_2, mice_error80_3, 
                        mice_error80_4, mice_error80_5, mice_error80_6, 
                        mice_error80_7, mice_error80_8, mice_error80_9, 
                        mice_error80_10) )
mice_error80_combined <- check_against_true_assignment(sample_ID, mice80_k_means_avg_output)

mice_error_rates <- c(original_error, mice_error10_combined, mice_error20_combined, 
                      mice_error30_combined, mice_error40_combined, mice_error50_combined, 
                      mice_error60_combined, mice_error70_combined, mice_error80_combined)
names(mice_error_rates) <- c("0% (Original)", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%")

barplot(mice_error_rates, xlab="Percentage of Values Removed", ylab="Error Rates")

#error_rates <- c(original_error, error10, error20, error30, error40, error50, error60, error70, error80)
#names(error_rates) <- c(0, 10, 20, 30, 40, 50, 60, 70, 80)

#barplot(error_rates, main="Error Rates vs Percentage of Values Removed", xlab="Percentage of Values Removed")
