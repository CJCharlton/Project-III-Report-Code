#install.packages('purrr')
#install.packages('english')
library(purrr)
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

PPS_normal_prior <- function(data, mu_0, tau_0, alpha_0, beta_0) {
  n <- length(data) - sum(is.na(data))
  x_n_bar <- mean(data, na.rm = TRUE)
  s_n_squared <- var(data, na.rm = TRUE)
  mu_n <- (n*x_n_bar + tau_0*mu_0) / (n + tau_0)
  tau_n <- n + tau_0
  alpha_n <- n + alpha_0
  beta_n <- beta_0 + (1/2)*n*(s_n_squared) + (1/2)*(tau_0*n*(mu_0 - x_n_bar)^2)/(n + tau_0)
  #return(c(mu_n, tau_n, alpha_n, beta_n))
  for (row in 1:length(data)) {
    if (is.na(as.vector(data)[row])) {
      data[row] <- min(5, max(0, mu_n + (beta_n/alpha_n)*(1 + 1/tau_n) * rt(1, 2*alpha_n)))
    }
  }
  return(data)
}

PPS_beta_prior <- function(data, alpha_0, beta_0) {
  n <- length(data) - sum(is.na(data))
  sum_x_i <- sum(data, na.rm = TRUE)
  alpha_n <- alpha_0 + sum_x_i - 1
  beta_n <- beta_0 + n - 1 - sum_x_i
  for (row in 1:length(data)) {
    if (is.na(as.vector(data)[row])) {
      data[row] <- rbernoulli(1, alpha_n/(alpha_n + beta_n))
    }
  }
  return(data)
}

PPS_specific_df <- function(df, mu_0, tau_0, norm_alpha_0, norm_beta_0, beta_alpha_0, beta_beta_0) {
  for (col in 1:10) {
    df[,col] <- matrix( PPS_normal_prior(df[,col], mu_0, tau_0, norm_alpha_0, norm_beta_0), ncol = 1 )
  }
  df[,11] <- as.factor(matrix( PPS_beta_prior(as.numeric(df[,11])-1, beta_alpha_0, beta_beta_0), ncol = 1 ))
  colnames(df) <- film_names(10)
  return(df)
}

PPS_MCAR10_sample <- PPS_specific_df(MCAR10_sample, 3, 2, 10, 10, 12, 8)
PPS_MCAR20_sample <- PPS_specific_df(MCAR20_sample, 3, 2, 10, 10, 12, 8)
PPS_MCAR30_sample <- PPS_specific_df(MCAR30_sample, 3, 2, 10, 10, 12, 8)
PPS_MCAR40_sample <- PPS_specific_df(MCAR40_sample, 3, 2, 10, 10, 12, 8)
PPS_MCAR50_sample <- PPS_specific_df(MCAR50_sample, 3, 2, 10, 10, 12, 8)
PPS_MCAR60_sample <- PPS_specific_df(MCAR60_sample, 3, 2, 10, 10, 12, 8)
PPS_MCAR70_sample <- PPS_specific_df(MCAR70_sample, 3, 2, 10, 10, 12, 8)
PPS_MCAR80_sample <- PPS_specific_df(MCAR80_sample, 3, 2, 10, 10, 12, 8)

PPS_MNAR10_sample <- PPS_specific_df(MNAR10_sample, 3, 2, 10, 10, 12, 8)
PPS_MNAR20_sample <- PPS_specific_df(MNAR20_sample, 3, 2, 10, 10, 12, 8)
PPS_MNAR30_sample <- PPS_specific_df(MNAR30_sample, 3, 2, 10, 10, 12, 8)
PPS_MNAR40_sample <- PPS_specific_df(MNAR40_sample, 3, 2, 10, 10, 12, 8)
PPS_MNAR50_sample <- PPS_specific_df(MNAR50_sample, 3, 2, 10, 10, 12, 8)
PPS_MNAR60_sample <- PPS_specific_df(MNAR60_sample, 3, 2, 10, 10, 12, 8)
PPS_MNAR70_sample <- PPS_specific_df(MNAR70_sample, 3, 2, 10, 10, 12, 8)
PPS_MNAR80_sample <- PPS_specific_df(MNAR80_sample, 3, 2, 10, 10, 12, 8)

setwd("C:/Users/conno/Downloads/Datasets")
write.csv(complete(PPS_MCAR10_sample), "simulatedDatasetMCAR10_PPS.csv")
write.csv(complete(PPS_MCAR20_sample), "simulatedDatasetMCAR20_PPS.csv")
write.csv(complete(PPS_MCAR30_sample), "simulatedDatasetMCAR30_PPS.csv")
write.csv(complete(PPS_MCAR40_sample), "simulatedDatasetMCAR40_PPS.csv")
write.csv(complete(PPS_MCAR50_sample), "simulatedDatasetMCAR50_PPS.csv")
write.csv(complete(PPS_MCAR60_sample), "simulatedDatasetMCAR60_PPS.csv")
write.csv(complete(PPS_MCAR70_sample), "simulatedDatasetMCAR70_PPS.csv")
write.csv(complete(PPS_MCAR80_sample), "simulatedDatasetMCAR80_PPS.csv")