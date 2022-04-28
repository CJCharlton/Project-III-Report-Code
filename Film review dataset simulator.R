#install.packages('purrr')
#install.packages('english')
library(purrr)
library(english)

rating_select <- function() {
  return(1 + rbernoulli(1) + rbernoulli(1))
}

random_rating1 <- function(film) {
  output <- round(rnorm(1,5-film/3,1.5), digits = 1)
  if (output > 5) {
    output <- 5
  }
  if (output < 0) {
    output <- 0
  }
  return(output)
}

random_rating2 <- function(film) {
  output <- round(rnorm(1,1.5+film/2,1), digits = 1)
  if (output > 5) {
    output <- 5
  }
  if (output < 0) {
    output <- 0
  }
  return(output)
}

random_rating3 <- function(film) {
  output <- round(rexp(1,6/(11-film)), digits = 1)
  if (output > 5) {
    output <- 5
  }
  return(output)
}

premium_subscription <- function(reviews) {
  p <- mean(reviews)/5
  indicator <- rbernoulli(1, p)
  if (indicator == 1) {
    return(TRUE)
  } else{
    return(FALSE)
  }
}

random_ratings <- function(films, prem_sub = TRUE, ID = FALSE) {
  selection <- rating_select()
  output <- c()
  if (selection < 1.5) {
    for (idx in 1:films) {
      output <- append(output, random_rating1(idx))
    }
    if (prem_sub) {
      output <- append(output, premium_subscription(output))
    }
    output <- t(data.frame(output))
    if (ID) {
      type_col <- data.frame(c("Type I"))
      rownames(type_col) <- c("output")
      output <- cbind(output, type_col)
      colnames(output) <- 1:(films+1)
    }
    return(output)
  }
  if (selection < 2.5) {
    for (idx in 1:films) {
      output <- append(output, random_rating2(idx))
    }
    if (prem_sub) {
      output <- append(output, premium_subscription(output))
    }
    output <- t(data.frame(output))
    if (ID) {
      type_col <- data.frame(c("Type II"))
      rownames(type_col) <- c("output")
      output <- cbind(output, type_col)
      colnames(output) <- 1:(films+1)
    }
    return(output)
  } else {
    for (idx in 1:films) {
      output <- append(output, random_rating3(idx))
    }
    if (prem_sub) {
      output <- append(output, premium_subscription(output))
    }
    output <- t(data.frame(output))
    if (ID) {
      type_col <- data.frame(c("Type III"))
      rownames(type_col) <- c("output")
      output <- cbind(output, type_col)
      colnames(output) <- 1:(films+1)
    }
    return(output)
  }
}

viewer_names <- function(viewers) {
  output <- c()
  for (idx in 1:viewers) {
    output <- append(output, paste("viewer",as.character(idx)))
  }
  return(output)
}

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

random_ratings_df <- function(films, viewers, prem_sub = TRUE, ID = FALSE) {
  output <- random_ratings(films, prem_sub = prem_sub, ID = ID)
  for (idx in 2:viewers) {
    output <- rbind(output, random_ratings(films, prem_sub = prem_sub, ID = ID))
  }
  #output[,films+1] <- as.logical(output[,films+1])
  colnames(output) <- film_names(films, prem_sub = prem_sub, ID = ID)
  rownames(output) <- viewer_names(viewers)
  output$prem_sub <- as.logical(output$prem_sub)
  return(output)
}

sample_ID <- random_ratings_df(10, 500, prem_sub = TRUE, ID = TRUE)
sample <- sample_ID[,1:11]

MCAR <- function(df, p, exclude_last_column = FALSE) {
  nr <- nrow(df)
  if (exclude_last_column) {
    nr <- nr-1
  }
  for (i in 1:nr) {
    for (j in 1:ncol(df)) {
      if (rbernoulli(1, p) == 1) {
        df[i, j] <- NA
      }
    }
  }
  return(df)
}

MCAR10_sample <- MCAR(sample, 0.1)
MCAR20_sample <- MCAR(sample, 0.2)
MCAR30_sample <- MCAR(sample, 0.3)
MCAR40_sample <- MCAR(sample, 0.4)
MCAR50_sample <- MCAR(sample, 0.5)
MCAR60_sample <- MCAR(sample, 0.6)
MCAR70_sample <- MCAR(sample, 0.7)
MCAR80_sample <- MCAR(sample, 0.8)

#MNAR <- function(df, p, exclude_last_column = TRUE) {
#  mu_reviews <- mean(df[,1:10])
#  sigma_reviews <- sd(df[,1:10])
#  mu_subscription <- mean(df[,11])
#  sigma_subscription <- sd(df[,11])
#  P_reviews <- (df[,1:10] - mu_reviews)/sigma_reviews
#  P_subscription <- (df[,11] - mu_subscription)/sigma_subscription
#  P <- cbind(P_reviews, P_subscription)
#  for (idx in 1:20) {
#    P <- P * p/mean(P)
#    P <- 1/(1 + exp(-P))
#  }
#  return(P)
 # nr <- nrow(df)
 # if (exclude_last_column) {
 #   nr <- nr-1
 # }
 # for (i in 1:nr) {
 #   for (j in 1:ncol(df)) {
 #     if (rbernoulli(1, P[i,j]) == 1) {
 #       df[i, j] <- NA
 #     }
#    }
#  }
#  return(df)
#}

max(MNAR(sample, 0.8))
min(MNAR(sample, 0.8))
mean(MNAR(sample, 0.8))

MNAR10_sample <- MNAR(sample, 0.1)
MNAR20_sample <- MNAR(sample, 0.2)
MNAR30_sample <- MNAR(sample, 0.3)
MNAR40_sample <- MNAR(sample, 0.4)
MNAR50_sample <- MNAR(sample, 0.5)
MNAR60_sample <- MNAR(sample, 0.6)
MNAR70_sample <- MNAR(sample, 0.7)
MNAR80_sample <- MNAR(sample, 0.8)