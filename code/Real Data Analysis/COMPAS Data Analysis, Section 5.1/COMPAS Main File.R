############################
## Load required packages ##
############################
require(ggplot2)
require(tidyverse)
require(mgcv)
require(gridExtra)
require(parallel)
############################

## Source the functions from an external file (assumed to be in the same directory)
source("COMPAS Function File.R")


## Clean the data ##
raw_data <- read_csv("compas-scores-two-years.csv")
raw_data$priors_count <- raw_data$priors_count...15
raw_data$decile_score <- raw_data$decile_score...12
df <- dplyr::select(raw_data, age, c_charge_degree, race, age_cat, score_text, sex, priors_count,
                    days_b_screening_arrest, decile_score, is_recid, two_year_recid, c_jail_in, c_jail_out) %>%
  filter(days_b_screening_arrest <= 30) %>%
  filter(days_b_screening_arrest >= -30) %>%
  filter(is_recid != -1) %>%
  filter(c_charge_degree != "O") %>%
  filter(score_text != 'N/A')

df$row_num <- 1:nrow(df)
df$score_prob <- 1/df$decile_score
df$is_recid_new <- df$is_recid+1

## Double checking the type of the protected group
df$sex <- as.numeric(as.factor((df$sex)))

## Combine protected groups.
race <- df$race
race[race=="Asian"] <- "Other"
race[race=="Native American"] <- "Other"
race[race=="Hispanic"] <- "Other"
race[race=="Caucasian"] <- "Other"

df$race <- race

## Race factor
df$race_factor <- as.numeric(as.factor(df$race))

## Add original row order
df$orig_rows <- 1:nrow(df)

## Storage
alpha_full_storage <- list()
iii <- 1

race_unique <- c("Other", "African-American")

## Create some interaction variables
df$age_priors <- with(df, age * priors_count)
df$days_decile <- with(df, days_b_screening_arrest * decile_score)
df$sex_race <- with(df, sex*race_factor)
df$days_btwn_arrest <- as.numeric(df$c_jail_in - df$c_jail_out)

## Parameters to tune
alpha_grid <- seq(0.15, 0.3, by=0.05)

## Number of random iterations averaged over for each value of alpha considered
n_iter <- 100

## Proportion of the data split for the observation data set & test data set.
obs_test_split <- 0.9
## Proportion of the observed data set that is split for the training data set.
obs_train_split <- 0.5

## Random seed
set.seed(1)

## Number of cores for parallel processsing
## (replace mclapply with lapply you do not want to run the code in parallel )
n_cores <- detectCores() - 1

mult_alpha_err <- lapply(alpha_grid, function(alpha_param) {

  iters <- mclapply(1:n_iter, function(dummy_iter) {

    ## Split data into observed and test, across each class label and protected group
      ## List of 4, splitting the data by each protected group and class label.
    df_split_y <- lapply(split(df, df$is_recid_new), function(l) split(l, l$race_factor))
    obs_rows_1 <- sort(sample(df_split_y[[1]][[1]]$orig_rows, obs_test_split*nrow(df_split_y[[1]][[1]])))
    obs_rows_2 <- sort(sample(df_split_y[[1]][[2]]$orig_rows, obs_test_split*nrow(df_split_y[[1]][[2]])))
    obs_rows_3 <- sort(sample(df_split_y[[2]][[1]]$orig_rows, obs_test_split*nrow(df_split_y[[2]][[1]])))
    obs_rows_4 <- sort(sample(df_split_y[[2]][[2]]$orig_rows, obs_test_split*nrow(df_split_y[[2]][[2]])))
    obs_rows <- sort(c(obs_rows_1,obs_rows_2,obs_rows_3,obs_rows_4))

    test_rows <- (1:nrow(df))[-obs_rows]
    observed_data <- df[obs_rows,]
    test_data <- df[test_rows, ]

    ## Split observed data into train and calibrate
    train_split_index <- sample(1:nrow(observed_data), obs_train_split*nrow(observed_data))
    calibrate_split_index <- (1:nrow(observed_data))[-train_split_index]
    train_data <- observed_data[train_split_index,]
    calibrate_data <- observed_data[calibrate_split_index,]

    #####################
    #### GAM   ##########
    #####################

    ## Naive train and testing scores using NB
    s_nb_naive_calibrate <- fasi_fcn(train_data, calibrate_data, naive_indicator = T)
    s_nb_naive_test <- fasi_fcn(observed_data, test_data, naive_indicator = T)

    ## FASI train and testing scores using NB
    s_nb_fasi_calibrate <- fasi_fcn(train_data, calibrate_data, naive_indicator = F)
    s_nb_fasi_test <- fasi_fcn(observed_data, test_data, naive_indicator = F)

    ## Add these scores to the dataframes
    calibrate_data <- calibrate_data %>% tibble::add_column(s_nb_naive_calibrate, s_nb_fasi_calibrate)
    test_data <- test_data %>% tibble::add_column(s_nb_naive_test, s_nb_fasi_test)

    ## Classify using decile, naive and fasi method
    test_data$nb_classification_ci <- aci_naive(calibrate_data, test_data, alpha_param, method = "s_nb_fasi")
    test_data$nb_classification_fasi <- aci_rank(calibrate_data, test_data, alpha_param, method = "s_nb_fasi")
    test_data$decile_classification_fasi <- aci_naive(calibrate_data, test_data, alpha_param, method = "decile_score")
    test_data$nb_classification_nr <- aci_naive(calibrate_data, test_data, alpha_param, method = "s_nb_naive")

    ## Calculate errors in protected classes
    nb_err_naive <- fasi_err_full(test_data, "nb_classification_ci")
    nb_err_fasi <- fasi_err_full(test_data, "nb_classification_fasi")
    decile_err_fasi <- fasi_err_full(test_data, "decile_classification_fasi")
    nb_err_nr <- fasi_err_full(test_data, "nb_classification_nr")

    ## Calculates num of errors fasi_err_num
    nb_err_naive_num <- fasi_err_num(test_data, "nb_classification_ci")
    nb_err_fasi_num <- fasi_err_num(test_data, "nb_classification_fasi")
    decile_err_fasi_num <- fasi_err_num(test_data, "decile_classification_fasi")
    nb_err_nr_num <- fasi_err_num(test_data, "nb_classification_nr")

    ## Number Selected
    race_unique_factor <- factor(test_data$race, levels=c("Other", "African-American"))

    nb_ci_num_nofull <- as.data.frame(aggregate(factor(test_data$nb_classification_ci, levels = 0:2), by=list(race_unique_factor), table))[, 2]
    nb_ci_num <- as.vector(rbind(apply(as.data.frame(aggregate(factor(test_data$nb_classification_ci, levels = 0:2), by=list(race_unique_factor), table))[2],2,sum), nb_ci_num_nofull))
    names(nb_ci_num) <- paste(paste("numselect_nb_classification_ci", rep(c("Full", race_unique), 3), sep = "_"), rep(0:2, each=length(race_unique) + 1), sep="_")

    nb_fasi_num_nofull <- as.data.frame(aggregate(factor(test_data$nb_classification_fasi, levels = 0:2), by=list(race_unique_factor), table))[, 2]
    nb_fasi_num <- as.vector(rbind(apply(as.data.frame(aggregate(factor(test_data$nb_classification_fasi, levels = 0:2), by=list(race_unique_factor), table))[2],2,sum), nb_fasi_num_nofull))
    names(nb_fasi_num) <- paste(paste("numselect_nb_classification_fasi", rep(c("Full", race_unique), 3), sep = "_"), rep(0:2, each=length(race_unique) + 1), sep="_")

    nb_nr_num_nofull <- as.data.frame(aggregate(factor(test_data$nb_classification_nr, levels = 0:2), by=list(race_unique_factor), table))[, 2]
    nb_nr_num <- as.vector(rbind(apply(as.data.frame(aggregate(factor(test_data$nb_classification_nr, levels = 0:2), by=list(race_unique_factor), table))[2],2,sum), nb_nr_num_nofull))
    names(nb_nr_num) <- paste(paste("numselect_nb_classification_nr", rep(c("Full", race_unique), 3), sep = "_"), rep(0:2, each=length(race_unique) + 1), sep="_")

    decile_fasi_num_nofull <- as.data.frame(aggregate(factor(test_data$decile_classification_fasi, levels = 0:2), by=list(race_unique_factor), table))[, 2]
    decile_fasi_num <- as.vector(rbind(apply(as.data.frame(aggregate(factor(test_data$decile_classification_fasi, levels = 0:2), by=list(race_unique_factor), table))[2],2,sum), decile_fasi_num_nofull))
    names(decile_fasi_num) <- paste(paste("numselect_nb_classification_decile", rep(c("Full", race_unique), 3), sep = "_"), rep(0:2, each=length(race_unique) + 1), sep="_")

    avg_indecision_fasi <- nb_fasi_num[2:3] / c(length(which(test_data$race == "Other")), length(which(test_data$race == "African-American")))
    names(avg_indecision_fasi) <- c("Indecision percent Other fasi", "Indecision percent African American fasi")

    avg_indecision_naive <- nb_ci_num[2:3] / c(length(which(test_data$race == "Other")), length(which(test_data$race == "African-American")))
    names(avg_indecision_naive) <- c("Indecision percent Other naive", "Indecision percent African American naive")

    avg_indecision_nr <- nb_nr_num[2:3] / c(length(which(test_data$race == "Other")), length(which(test_data$race == "African-American")))
    names(avg_indecision_nr) <- c("Indecision percent Other nr", "Indecision percent African American nr")

    avg_indecision_decile <- decile_fasi_num[2:3] / c(length(which(test_data$race == "Other")), length(which(test_data$race == "African-American")))
    names(avg_indecision_decile) <- c("Indecision percent Other decile", "Indecision percent African American decile")

    ## Return Object
    nb_info <- c(nb_err_naive, nb_err_fasi, decile_err_fasi, nb_err_nr,
                 nb_ci_num, nb_fasi_num, decile_fasi_num, nb_nr_num,
                 nb_err_naive_num, nb_err_fasi_num, decile_err_fasi_num, nb_err_nr_num,
                 avg_indecision_fasi, avg_indecision_naive, avg_indecision_decile, avg_indecision_nr)
    return(nb_info)
  }, mc.cores = n_cores, mc.set.seed = T)
  iters_bind <- dplyr::bind_rows(iters)
  alpha_full_storage[[iii]] <- iters_bind
  iii <- iii+1

  avg_err <- apply(iters_bind, 2, mean)
  sd_err <- apply(iters_bind, 2, sd)

  return_object <- list(avg_err, sd_err)
  return(return_object)
})

mult_alpha_err_list_avg <- lapply(mult_alpha_err, function(x) x[[1]])
mult_alpha_err_mat <- dplyr::bind_rows(mult_alpha_err_list_avg)

mult_alpha_err_mat <- as.data.frame(mult_alpha_err_mat)
rownames(mult_alpha_err_mat) <- alpha_grid

## Save results ##
save(mult_alpha_err, file = "mult_alpha_compass.rda")
