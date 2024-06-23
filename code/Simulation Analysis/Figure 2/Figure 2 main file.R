require(ggplot2)
require(dplyr)
require(tibble)
require(tidyr)
require(MASS)
require(mvtnorm)
require(parallel)
require(naivebayes)

## Source in the function file (assumed to be in the same directory)
source("Figure 2 function file.R")

## Define parameters
## Sample size for test and train data
n <- 1000

## Probability of having protected class A=1
pi <- 0.5

## Probability of having the label Y=1 (indices respectively for the race A)
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

### Simulation Section ####
nits <- 100

p_iter <- seq(0.15, 0.85, by=0.05)
nb_est <- T
avg_matrix <- matrix(nrow=length(p_iter), ncol=48)
sd_matrix <- matrix(nrow=length(p_iter), ncol=48)
p_iter_fullrun <- list()

set.seed(1)

for (i in 1:length(p_iter)) {
  p[2] <- p_iter[i]
  diagnostics_list <- lapply(1:nits, function(dummy_iter) {
  # diagnostics_list <- mclapply(1:nits, function(dummy_iter) {
    dta_train <- gen_obs(n, pi, p, means, sds, oracle_indicator = F)
    dta_test <- gen_obs(50, pi, p, means, sds, oracle_indicator = T)
    data_oracle_train <- gen_obs(n, pi, p, means, sds, oracle_indicator = T)

    ## oracle
    aci_class_umi <- aci_naive(data_oracle_train, dta_test, alpha_param, method = "s_naive")
    aci_class_ami <- aci_rank(data_oracle_train, dta_test, alpha_param, method = "s_naive")
    aci_class_ci <- aci_naive(data_oracle_train, dta_test, alpha_param, method = "s_sideinfo")
    aci_class_fasi <- aci_rank(data_oracle_train, dta_test, alpha_param, method = "s_sideinfo")

    dta_test <- dta_test %>% tibble::add_column(aci_class_umi, aci_class_ami,
                                                aci_class_ci, aci_class_fasi)

    ## Oracle error and power calculation
    oracle_power <- sapply(colnames(dta_test)[grep('aci_class',colnames(dta_test))], function(name) {
      power_est(dta_test, method = name)})
    oracle_err <- sapply(colnames(dta_test)[grep('aci_class',colnames(dta_test))], function(name) {
      fasi_err_full(dta_test, method = name)})

    ## Number of Decisions Made for every method
    num_decisions <- sapply(colnames(dta_test)[grep('aci_class',colnames(dta_test))], function(m) num_dec_allgroup(dta_test, m))
    num_decisions_vec <- as.vector(num_decisions)
    names(num_decisions_vec) <- as.vector(outer(rownames(num_decisions), colnames(num_decisions), function(x,y) paste(x,y,sep=" ")))

    error_vec <- as.vector(oracle_err)
    names(error_vec) <- as.vector(outer(rownames(oracle_err), colnames(oracle_err), function(x,y) paste(x,y,sep=" ")))

    return_object <- c(error_vec, num_decisions_vec)

    return(return_object)
  })
  # }, mc.cores = 8, mc.set.seed = TRUE)
  diagnostics <- Reduce(cbind, diagnostics_list)
  p_iter_fullrun[[i]] <- diagnostics
  avg_matrix[i,] <- apply(diagnostics, 1, mean)
  sd_matrix[i,] <- apply(diagnostics, 1, sd)
  ## See where we are
  print(i)
}

colnames(avg_matrix) <- names(diagnostics_list[[1]])

