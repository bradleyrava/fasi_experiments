## Adult Income Dataset Analysis FASI ##

### Packages #######
require(tidyverse)
require(parallel)
require(JOUSBoost)
####################

## Source the functions from an external file (assumed to be in the same directory)
source("Adult Function File.R")

z <- read_csv("adult", col_names = F)

##################################
#### Clean / set-up the data #####
##################################
colnames(z) <- c("age", "workclass", "fnlwgt", "education", "education-num", "marital-status", "occupation", "relationship", "race", "sex", "capital-gain", "capital-loss", "hours-per-week", 'native-country', 'y')
z$y <- ifelse(z$y == "<=50K", 1, 2)
z$sex[z$sex == "Female"] <- "female"
z$sex[z$sex == "Male"] <- "male"

z$`native-country` <- as.factor(z$`native-country`)

names <- c(2,4,6,7,8,9,10,14)
z[,names] <- lapply(z[,names] , factor)

z$orig_rows <- 1:nrow(z)

########################
## Parameters to tune ##
########################

## Grid of alpha's
alpha_grid <- seq(0.005, 0.1, by=0.005)

## Number of random iterations per-alpha
n_iter <- 100

## Percentage of data split for the observed data set
obs_test_split <- 0.7

## Percentage of data split for the training data set (from observed)
obs_train_split <- 0.5

## Storage matrices for results
err_matrix_storage <- matrix(NA, nrow=length(alpha_grid), ncol=51)
rownames(err_matrix_storage) <- alpha_grid

var_matrix_storage <- matrix(NA, nrow=length(alpha_grid), ncol=51)
rownames(var_matrix_storage) <- alpha_grid

err_overall <- matrix(NA, nrow=length(alpha_grid), ncol=9)
rownames(err_overall) <- alpha_grid

#### Body of the main analysis ####

## If running parallel, set the number of cores below
n_cores <- detectCores() - 1

for (ii in 1:length(alpha_grid)) {
  alpha_param <- alpha_grid[ii]
  ## Uncomment below if you want to use lapply instead of mclapply (running in parallel)
  # err_reps <- lapply(1:n_iter, function(dummy_iter) {
  err_reps <- mclapply(1:n_iter, function(dummy_iter) {

    ## Split data into observed and test, making sure to sample evenly from each protected group and class label y.

    ## creates a list of size 4 that splits the data by the class label y and protected attribute.
    df_split_y <- lapply(split(z, z$y), function(l) split(l, l$sex))
    obs_rows_1 <- sort(sample(df_split_y[[1]][[1]]$orig_rows, obs_test_split*nrow(df_split_y[[1]][[1]])))
    obs_rows_2 <- sort(sample(df_split_y[[1]][[2]]$orig_rows, obs_test_split*nrow(df_split_y[[1]][[2]])))
    obs_rows_3 <- sort(sample(df_split_y[[2]][[1]]$orig_rows, obs_test_split*nrow(df_split_y[[2]][[1]])))
    obs_rows_4 <- sort(sample(df_split_y[[2]][[2]]$orig_rows, obs_test_split*nrow(df_split_y[[2]][[2]])))
    obs_rows <- sort(c(obs_rows_1,obs_rows_2,obs_rows_3,obs_rows_4))

    test_rows <- (1:nrow(z))[-obs_rows]
    observed_data <- z[obs_rows,]
    test_data <- z[test_rows, ]

    ## Split observed data into train and calibrate
    train_split_index <- sample(1:nrow(observed_data), obs_train_split*nrow(observed_data))
    calibrate_split_index <- (1:nrow(observed_data))[-train_split_index]
    train_data <- observed_data[train_split_index,]
    calibrate_data <- observed_data[calibrate_split_index,]

    ##########################
    #### Estimate scores #####
    ##########################

    ## Naive train and testing scores using NB
    s_nb_naive_calibrate <- fasi_fcn(train_data, calibrate_data, naive_indicator = T, n_iter=n_iter_xg, max_depth = max_depth, max_delta_step = max_delta_step)
    s_nb_naive_test <- fasi_fcn(observed_data, test_data, naive_indicator = T, n_iter=n_iter_xg, max_depth = max_depth, max_delta_step = max_delta_step)

    ## FASI train and testing scores using NB
    s_nb_fasi_calibrate <- fasi_fcn(train_data, calibrate_data, naive_indicator = F,  n_iter=n_iter_xg, max_depth = max_depth, max_delta_step = max_delta_step)
    s_nb_fasi_test <- fasi_fcn(observed_data, test_data, naive_indicator = F, n_iter=n_iter_xg, max_depth = max_depth, max_delta_step = max_delta_step)

    ## Add these scores to the dataframes
    calibrate_data <- calibrate_data %>% tibble::add_column(s_nb_naive_calibrate, s_nb_fasi_calibrate)
    test_data <- test_data %>% tibble::add_column(s_nb_naive_test, s_nb_fasi_test)

    ## Classify using decile, naive and fasi method
    test_data$gam_classification_ci <- aci_naive(calibrate_data, test_data, alpha_param, method = "s_nb_fasi")
    test_data$gam_classification_fasi <- aci_rank(calibrate_data, test_data, alpha_param, method = "s_nb_fasi")
    test_data$gam_classification_nr <- aci_naive(calibrate_data, test_data, alpha_param, method = "s_nb_naive")

    gam_err_naive <- fasi_err_full(test_data, "gam_classification_ci")
    gam_err_fasi <- fasi_err_full(test_data, "gam_classification_fasi")
    gam_err_nr <- fasi_err_full(test_data, "gam_classification_nr")

    ## Number Selected
    sex_unique_factor <- factor(test_data$sex, levels=c("male", "female"))

    gam_ci_num_nofull <- as.data.frame(aggregate(factor(test_data$gam_classification_ci, levels = 0:2), by=list(sex_unique_factor), table))[, 2]
    gam_ci_num <- as.vector(rbind(apply(as.data.frame(aggregate(factor(test_data$gam_classification_ci, levels = 0:2), by=list(sex_unique_factor), table))[2],2,sum), gam_ci_num_nofull))
    names(gam_ci_num) <- paste(paste("numselect_gam_classification_ci", rep(c("Full", c(1,2)), 3), sep = "_"), rep(0:2, each=2 + 1), sep="_")

    gam_fasi_num_nofull <- as.data.frame(aggregate(factor(test_data$gam_classification_fasi, levels = 0:2), by=list(sex_unique_factor), table))[, 2]
    gam_fasi_num <- as.vector(rbind(apply(as.data.frame(aggregate(factor(test_data$gam_classification_fasi, levels = 0:2), by=list(sex_unique_factor), table))[2],2,sum), gam_fasi_num_nofull))
    names(gam_fasi_num) <- paste(paste("numselect_gam_classification_fasi", rep(c("Full", c(1,2)), 3), sep = "_"), rep(0:2, each=2 + 1), sep="_")

    gam_nr_num_nofull <- as.data.frame(aggregate(factor(test_data$gam_classification_nr, levels = 0:2), by=list(sex_unique_factor), table))[, 2]
    gam_nr_num <- as.vector(rbind(apply(as.data.frame(aggregate(factor(test_data$gam_classification_nr, levels = 0:2), by=list(sex_unique_factor), table))[2],2,sum), gam_nr_num_nofull))
    names(gam_nr_num) <- paste(paste("numselect_nb_classification_nr", rep(c("Full", c(1,2)), 3), sep = "_"), rep(0:2, each=2 + 1), sep="_")

    avg_indecision_fasi <- gam_fasi_num[2:3] / c(length(which(test_data$sex == "male")), length(which(test_data$sex == "female")))
    names(avg_indecision_fasi) <- c("Indecision percent 1 fasi", "Indecision percent 2 fasi")

    avg_indecision_naive <- gam_ci_num[2:3] / c(length(which(test_data$sex == "male")), length(which(test_data$sex == "female")))
    names(avg_indecision_naive) <- c("Indecision percent 1 naive", "Indecision percent 2 naive")

    avg_indecision_nr <- gam_nr_num[2:3] / c(length(which(test_data$sex == "male")), length(which(test_data$sex == "female")))
    names(avg_indecision_nr) <-  c("Indecision percent 1 nr", "Indecision percent 2 nr")

    gam_info <- c(gam_err_naive, gam_err_nr, gam_err_fasi,
                  gam_ci_num, gam_fasi_num, gam_nr_num,
                  avg_indecision_fasi, avg_indecision_naive, avg_indecision_nr)

    return(gam_info)
    # })
  }, mc.cores = n_cores, mc.set.seed = TRUE)

  ## for averaging over each rep
  err_reps_bind <- bind_rows(err_reps)

  err_avg <- apply(err_reps_bind, 2,  mean)
  err_var <- apply(err_reps_bind, 2,  sd)
  err_matrix_storage[ii,] <- err_avg
  var_matrix_storage[ii,] <- err_var
}

colnames(err_matrix_storage) <- names(err_avg)
err_matrix_storage <- as.data.frame(err_matrix_storage)
err_matrix_storage_do_not_delete <- err_matrix_storage

##############

## Save results to your computer ##
save(err_matrix_storage, var_matrix_storage, file = "adult_large_run.RData")


