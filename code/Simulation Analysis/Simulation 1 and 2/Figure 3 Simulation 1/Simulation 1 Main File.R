############################
## Simulation 1 main file ##
############################

#######################
## Packages Required ##
#######################
require(ggplot2)
require(dplyr)
require(tibble)
require(tidyr)
require(gam)
require(MASS)
require(mvtnorm)
require(parallel)
#######################

## Load in functions from the function file (assumed to be in the same directory) ##
source("Simulation 1 Function File")

## Define parameters
## Sample size for test and train data
n_train <- 2500
n_test <- 1000

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

## Level of group-wise FSR control (assumed equal for both classes)
alpha_param <- 0.1

### Simulation parameters ####

## Number of random iterations averaged over for each setting.
nits <- 1000

## Number of cores for parallel processing
n_cores <- detectCores() - 1

## Grid for iterating over \pi_{2|F}
p_iter <- seq(0.15, 0.85, by=0.05)

avg_matrix <- matrix(nrow=length(p_iter), ncol=144)
sd_matrix <- matrix(nrow=length(p_iter), ncol=144)
p_iter_fullrun <- list()

set.seed(1)

for (i in 1:length(p_iter)) {
  p[2] <- p_iter[i]
  diagnostics_list <- mclapply(1:nits, function(dummy_iter) {
    dta_obs <- gen_obs(n_train, pi, p, means, sds, oracle_indicator = F)
    obs_split_index <- sort(sample(1:nrow(dta_obs), 0.6*nrow(dta_obs)))
    dta_train <- dta_obs[obs_split_index,]
    dta_calibrate <- dta_obs[(1:nrow(dta_obs))[-obs_split_index],]

    dta_test <- gen_obs(n_test, pi, p, means, sds, oracle_indicator = T)

    ## separate training dataset for the oracle procedure
    data_oracle_train <- gen_obs(n_test, pi, p, means, sds, oracle_indicator = T)

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

    ## GAM
    s_gam_fasi_calibrate <- fasi_fcn(dta_train, dta_calibrate, naive_indicator = F, method = "GAM")
    s_gam_fasi_test <- fasi_fcn(dta_obs, dta_test, naive_indicator = F, method = "GAM")
    s_gam_naive_calibrate <- fasi_fcn(dta_train, dta_calibrate, naive_indicator = T, method = "GAM")
    s_gam_naive_test <- fasi_fcn(dta_obs, dta_test, naive_indicator = T, method = "GAM")


    dta_calibrate <- as_tibble(cbind.data.frame(dta_calibrate, s_gam_fasi_calibrate_c1=s_gam_fasi_calibrate[,1], s_gam_fasi_calibrate_c2=s_gam_fasi_calibrate[,2],
                                                s_gam_naive_calibrate_c1=s_gam_naive_calibrate[,1], s_gam_naive_calibrate_c2=s_gam_naive_calibrate[,2]))
    dta_test <- as_tibble(cbind.data.frame(dta_test, s_gam_fasi_test_c1=s_gam_fasi_test[,1], s_gam_fasi_test_c2=s_gam_fasi_test[,2],
                                           s_gam_naive_test_c1=s_gam_naive_test[,1], s_gam_naive_test_c2=s_gam_naive_test[,2]))

    gam_classification_umi <- aci_naive(dta_calibrate, dta_test, alpha_param, method = "s_gam_naive")
    gam_classification_ami <- aci_rank(dta_calibrate, dta_test, alpha_param, method = "s_gam_naive")
    gam_classification_ci <- aci_naive(dta_calibrate, dta_test, alpha_param, method = "s_gam_fasi")
    gam_classification_fasi <- aci_rank(dta_calibrate, dta_test, alpha_param, method = "s_gam_fasi")

    dta_test <- dta_test %>% add_column(gam_classification_umi, gam_classification_ami,
                                        gam_classification_ci, gam_classification_fasi)

    ## Gam error and power calculation
    gam_power <- sapply(colnames(dta_test)[grep('gam_classification',colnames(dta_test))], function(name) {
      power_est(dta_test, method = name)})
    gam_err <- sapply(colnames(dta_test)[grep('gam_classification',colnames(dta_test))], function(name) {
      fasi_err_full(dta_test, method = name)})

    ## Number of Decisions Made for every method
    num_decisions <- sapply(colnames(dta_test)[grep('aci_class|gam_class',colnames(dta_test))], function(m) num_dec_allgroup(dta_test, m))
    num_decisions_vec <- as.vector(num_decisions)
    names(num_decisions_vec) <- as.vector(outer(rownames(num_decisions), colnames(num_decisions), function(x,y) paste(x,y,sep=" ")))

    error_vec <- c(as.vector(oracle_err), as.vector(gam_err))
    names(error_vec) <- c(as.vector(outer(rownames(oracle_err), colnames(oracle_err), function(x,y) paste(x,y,sep=" "))),
                          as.vector(outer(rownames(gam_err), colnames(gam_err), function(x,y) paste(x,y,sep=" "))))
    power_sim <- c(as.vector(oracle_power), as.vector(gam_power))
    names(power_sim) <- c(as.vector(outer(rownames(oracle_power), colnames(oracle_power), function(x,y) paste(x,y,sep=" "))),
                          as.vector(outer(rownames(gam_power), colnames(gam_power), function(x,y) paste(x,y,sep=" "))))

    return_object <- c(error_vec, power_sim, num_decisions_vec)

    return(return_object)
  }, mc.cores = n_cores, mc.set.seed = TRUE)
  diagnostics <- Reduce(cbind, diagnostics_list)
  p_iter_fullrun[[i]] <- diagnostics
  avg_matrix[i,] <- apply(diagnostics, 1, mean)
  sd_matrix[i,] <- apply(diagnostics, 1, sd)
  ## See where we are
  print(i)
}

colnames(avg_matrix) <- names(diagnostics_list[[1]])

## Save results ##
save(avg_matrix, sd_matrix, p_iter_fullrun, file = "sim_1_gam.rda")

