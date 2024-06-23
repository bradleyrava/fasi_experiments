require(ggplot2)
require(dplyr)
require(tibble)
require(tidyr)
require(gam)
require(MASS)
require(mvtnorm)
require(parallel)
require(ggpubr)

## Source function file (assumed to be in the same directory)
source("Appendix Figure 9 Function File.R")

## Define parameters
## Sample size for test and train data
n_train <- 2500
n_test <- 1000

## Probability of having protected class A=1
pi <- 0.5

## Probability of having the label Y=1 (indicies respectively for the race A)
p <- c(0.5, 0.5)
names(p) <- c("p1", "p2")

## We have 4 distributions. They are fixed as normal for now but user can define their mean and sd.
mean_vec <- c(2,3,7,0,1,6,2,3,7,0,1,6)
means <- matrix(mean_vec, ncol=4, nrow=3, byrow = F)
colnames(means) <- c("f_white_y2", "f_white_y1", "f_black_y2", "f_black_y1")
rownames(means) <- c("x1", "x2", "x3")

sds <- diag(3)
diag(sds) <- c(2,2,2)
rownames(sds) <- rownames(means)
colnames(sds) <- rownames(means)

alpha_param <- 0.1

### Simulation Section ###
nits <- 1000

p_iter <- seq(0.15, 0.85, by=0.05)

avg_matrix <- matrix(nrow=length(p_iter), ncol=2)
sd_matrix <- matrix(nrow=length(p_iter), ncol=2)
p_iter_fullrun <- list()

set.seed(1)

for (i in 1:length(p_iter)) {
  p[2] <- p_iter[i]
  diagnostics_list <- mclapply(1:nits, function(dummy_iter) {
    dta_obs <- gen_obs(n_train, pi, p, means, sds, oracle_indicator = F)
    obs_split_index <- sort(sample(1:nrow(dta_obs), 0.6*nrow(dta_obs)))

    dta_test <- gen_obs(n_test, pi, p, means, sds, oracle_indicator = F)

    ## separate training dataset for the oracle procedure
    data_oracle_train <- gen_obs(n_test, pi, p, means, sds, oracle_indicator = F)

    ## Calculate gamma
    pi_cal <- length(which(data_oracle_train$true_label == 1 & data_oracle_train$protected_class == 0)) / length(which(data_oracle_train$protected_class == 0))
    pi_test <- length(which(dta_test$true_label == 1 & dta_test$protected_class == 0)) / length(which(dta_test$protected_class == 0))
    gamma_0 <- pi_test / pi_cal

    pi_cal <- length(which(data_oracle_train$true_label == 1 & data_oracle_train$protected_class == 1)) / length(which(data_oracle_train$protected_class == 1))
    pi_test <- length(which(dta_test$true_label == 1 & dta_test$protected_class == 1)) / length(which(dta_test$protected_class == 1))
    gamma_1 <- pi_test / pi_cal

    ####################

    return_object <- c(gamma_0, gamma_1)

    return(return_object)
  }, mc.cores = 8, mc.set.seed = TRUE)
  diagnostics <- Reduce(cbind, diagnostics_list)
  p_iter_fullrun[[i]] <- diagnostics
  avg_matrix[i,] <- apply(diagnostics, 1, mean)
  sd_matrix[i,] <- apply(diagnostics, 1, sd)
  ## See where we are
  print(i)
}


## Save results
save(avg_matrix, sd_matrix, p_iter_fullrun, file = "gamma_sim.rda")

