############################################
## Function file for FASI COMPAS Analysis ##
############################################


aci_rank <- function(train_data, test_data, alpha_param, method) {
  ## Get column the method is in
  score_col_train <- grep(method, colnames(train_data))
  score_col_test <- grep(method, colnames(test_data))

  race_factor <- 1:2
  rscore_list <- lapply(race_factor, function(pt_c) {
    index_rows <- which(test_data$race_factor == pt_c)

    ## Subset the data by protected class
    train_df <- train_data %>%
      dplyr::filter(race_factor == pt_c)
    test_df <- test_data %>%
      dplyr::filter(race_factor == pt_c)

    class_2_score_test_r1 <- 1 - dplyr::pull(test_df[, score_col_test])
    class_2_score_train_r1 <- 1 - dplyr::pull(train_df[,score_col_train])
    class_2_score_test_r2 <- dplyr::pull(test_df[, score_col_test])
    class_2_score_train_r2 <- dplyr::pull(train_df[,score_col_train])

    conf_scores_r1 <- c(class_2_score_train_r1, class_2_score_test_r1)
    r1_score_unadj <- sapply(conf_scores_r1, function(test_cur) {
      return(rscore_fcn(test_cur, class_2_score_train_r1, class_2_score_test_r1, train_df$is_recid_new, pt_c, train_data$race_factor, test_data$race_factor, T))
    })

    conf_scores_r2 <- c(class_2_score_train_r2, class_2_score_test_r2)
    r2_score_unadj <- sapply(conf_scores_r2, function(test_cur) {
      return(rscore_fcn(test_cur, class_2_score_train_r2, class_2_score_test_r2, train_df$is_recid_new, pt_c, train_data$race_factor, test_data$race_factor, F))
    })

    ## Preform a monotonicity adjustment
    r1_score <- sapply(1:length(class_2_score_test_r1), function(iii) {
      conf_score_cur <- class_2_score_test_r1[iii]
      r_mono_adj <- min(r1_score_unadj[which(conf_scores_r1 <= conf_score_cur)])
      return(r_mono_adj)
    })
    r2_score <- sapply(1:length(class_2_score_test_r2), function(iii) {
      conf_score_cur <- class_2_score_test_r2[iii]
      r_mono_adj <- min(r2_score_unadj[which(conf_scores_r2 <= conf_score_cur)])
      return(r_mono_adj)
    })

    return_scores <- cbind.data.frame(r1_score, r2_score, index_rows)
    return(return_scores)
  })
  names(rscore_list) <- 1:2

  r_scores <- as_tibble(Reduce(rbind, rscore_list))
  r_scores_order <- r_scores %>%
    dplyr::arrange(index_rows)

  classification_vec <- vector(mode="numeric", length = nrow(r_scores_order))
  classification_vec[which(r_scores_order$r1_score <= alpha_param)] <- 1
  classification_vec[which(r_scores_order$r2_score <= alpha_param)] <- 2

  return(classification_vec)
}

aci_naive <- function(train_data, test_data, alpha_param, method) {
  ## Get column the method is in
  score_col_train <- grep(method, colnames(train_data))
  score_col_test <- grep(method, colnames(test_data))

  train_score <- train_data %>%
    dplyr::select(contains(method)) %>%
    dplyr::pull()

  test_score <- test_data %>%
    dplyr::select(contains(method)) %>%
    dplyr::pull()

  ## Protected Class
  train_ptc <- train_data %>%
    dplyr::select(contains("race_factor")) %>%
    pull()
  test_ptc <- test_data %>%
    dplyr::select(contains("race_factor")) %>%
    pull()
  all_ptc <- c(train_ptc, test_ptc)

  conf_scores_r1 <- c((1-train_score), (1-test_score))
  r1_score_unadj <- sapply(1:length(conf_scores_r1), function(ii) {
    return(rscore_fcn(conf_scores_r1[ii], (1-train_score), (1-test_score), train_data$is_recid_new, all_ptc[ii], train_ptc, test_ptc, T))
  })

  conf_scores_r2 <- c(train_score, test_score)
  r2_score_unadj <- sapply(1:length(conf_scores_r2), function(ii) {
    return(rscore_fcn(conf_scores_r2[ii], train_score, test_score, train_data$is_recid_new, all_ptc[ii], train_ptc, test_ptc, F))
  })

  ## Preform a monotonicity adjustment
  r1_score <- sapply(1:length(test_score), function(iii) {
    conf_score_cur <- (1-test_score)[iii]
    r_mono_adj <- min(r1_score_unadj[which(conf_scores_r1 <= conf_score_cur)])
    return(r_mono_adj)
  })
  r2_score <- sapply(1:length(test_score), function(iii) {
    conf_score_cur <- test_score[iii]
    r_mono_adj <- min(r2_score_unadj[which(conf_scores_r2 <= conf_score_cur)])
    return(r_mono_adj)
  })

  return_scores <- cbind.data.frame(r1_score, r2_score)

  ###############################
  ## Classify the observations ##
  ###############################

  classification_vec <- vector(mode="numeric", length = nrow(return_scores))
  classification_vec[which(return_scores$r1_score <= alpha_param)] <- 1
  classification_vec[which(return_scores$r2_score <= alpha_param)] <- 2

  return(classification_vec)
}

rscore_fcn <- function(test_score_cur, train_scores, test_scores, train_true_labels, pt_class_cur, pt_class_train, pt_class_test, c1_ind) {
  m <- length(which(pt_class_test == pt_class_cur)) + length(which(pt_class_train == pt_class_cur))

  n <- length(which(pt_class_train == pt_class_cur))
  index_numerator <- which(train_scores >= test_score_cur)
  index_denominator <- which(test_scores >= test_score_cur)
  if (c1_ind == T) {
    r_elem <- ( (1/(n+1)) * (sum(train_true_labels[index_numerator] != 1)+1) )  / ( (1/m) * (length(index_denominator) +  length(index_numerator)) )
  } else {
    r_elem <- ( (1/(n+1)) * (sum(train_true_labels[index_numerator] != 2)+1) )  / ( (1/m) * (length(index_denominator) +  length(index_numerator)) )
  }
  return(r_elem)
}

fasi_err_full <- function(test_data, method) {
  # the input
  # test_data: The test data used for classification. This must include all 4 classifications.
  # method: A character vector of the method that allows the function to obtain the correct classification vector.
  # the output
  # err: A vector of length 2. The P(Y=0|Y_hat=1, a). Probability the true label is 0 given that
  #      we assigned it a label of 1 for each race.

  ## Get the correct method
  m_index <- grep(method, colnames(test_data))

  all_groups <- 2
  error_rates <- sapply(all_groups, function(class_cur) {
    ## Only look at instances where we have selected someone into the favorable group
    x <- test_data %>%
      dplyr::filter(test_data[,m_index]==class_cur)

    err_full <- ifelse(sum(x$is_recid_new != class_cur)==0, 0, mean(x$is_recid_new != class_cur))

    ## Protected Class Error
    pt_class_unique <- c("Other", "African-American")

    err_pt <- sapply(pt_class_unique, function(pt_c) {
      ind <- which(x$race==pt_c)
      group2_err <- ifelse(length(ind)==0, 0, mean(x$is_recid_new[ind] != class_cur))
      return(group2_err)
    })

    ### return object
    err_all <- c(err_full, err_pt)
    names(err_all) <- paste(method, c("Full Error", paste(pt_class_unique, "Error")))
    return(err_all)
  })
  pt_class_unique <- c("Other", "African-American")

  err_rates_vec <- as.vector(error_rates)
  names(err_rates_vec) <- paste(method, c("Full Error", paste(pt_class_unique, "Error")))

  return(err_rates_vec)
}

fasi_err_num <- function(test_data, method) {
  # the input
  # test_data: The test data used for classification. This must include all 4 classifications.
  # method: A character vector of the method that allows the function to obtain the correct classification vector.
  # the output
  # err: A vector of length 2. The P(Y=0|Y_hat=1, a). Probability the true label is 0 given that
  #      we assigned it a label of 1 for each race.

  ## Get the correct method
  m_index <- grep(method, colnames(test_data))

  all_groups <- 2
  error_rates <- sapply(all_groups, function(class_cur) {
    ## Only look at instances where we have selected someone into the favorable group
    x <- test_data %>%
      dplyr::filter(test_data[,m_index]==class_cur)

    err_full <- ifelse(sum(x$is_recid_new != class_cur)==0, 0, sum(x$is_recid_new != class_cur))

    ## Protected Class Error
    pt_class_unique <- c("Other", "African-American")

    err_pt <- sapply(pt_class_unique, function(pt_c) {
      ind <- which(x$race==pt_c)
      group2_err <- ifelse(length(ind)==0, 0, sum(x$is_recid_new[ind] != class_cur))
      return(group2_err)
    })

    ### return object
    err_all <- c(err_full, err_pt)
    names(err_all) <- paste(method, c("Full Num", paste(pt_class_unique, "Num")))
    return(err_all)
  })
  pt_class_unique <- c("Other", "African-American")

  err_rates_vec <- as.vector(error_rates)
  names(err_rates_vec) <- paste(method, c("Full Num", paste(pt_class_unique, "Num")))

  return(err_rates_vec)
}


fasi_fcn <- function(train_data, test_data, naive_indicator=T) {

  y_train <-  train_data$is_recid
  vars <- colnames(train_data[c(1,6:8,9,18,19,20,21,22)])
  keep_vars <- if(naive_indicator) {vars} else {c(vars, "race_factor")}

  rows <- 1:nrow(train_data)

  ## Subset the train / test data on the vars we need to model
  dta_train <- train_data %>%
    dplyr::select(all_of(keep_vars))
  dta_test <- test_data %>%
    dplyr::select(all_of(keep_vars))

  ## GAM Model ##
  dta_gam <- cbind.data.frame(y=y_train, dta_train)

  if (naive_indicator==T) {
    gam_formula_mgcv <- as.formula("y ~ s(age, k=4) +
                           s(priors_count, k=3) +
                           s(days_b_screening_arrest, k=2) +
                           s(decile_score, k=3) +
                           sex +
                           age_priors +
                           s(days_decile, k=2) +
                           s(days_btwn_arrest, k=2)")
    gam_formula_mgcv <- as.formula("y ~ s(age, k=2.557) +
                           s(priors_count, k=3.739) +
                           s(days_b_screening_arrest, k=1.000) +
                           s(decile_score, k=1.001) +
                           sex +
                           age_priors +
                           s(days_decile, k=2.204) +
                           s(days_btwn_arrest, k=3.364)")
  } else {
    gam_formula_mgcv <- as.formula("y ~ s(age, k=4) +
                           s(priors_count, k=3) +
                           s(days_b_screening_arrest, k=2) +
                           s(decile_score, k=3) +
                           sex +
                           race_factor +
                           age_priors +
                           s(days_decile, k=2) +
                           sex_race +
                           s(days_btwn_arrest, k=2)")
    gam_formula_mgcv <- as.formula("y ~ s(age, k=2.557) +
                           s(priors_count, k=3.739) +
                           s(days_b_screening_arrest, k=1.000) +
                           s(decile_score, k=1.001) +
                           sex +
                           race_factor +
                           age_priors +
                           s(days_decile, k=2.204) +
                           sex_race +
                           s(days_btwn_arrest, k=3.364)")
  }
  model_gam_mgcv <- mgcv::gam(gam_formula_mgcv, data=dta_gam,
                              family=binomial)

  logit <- predict(model_gam_mgcv, newdata = dta_test)
  score_2 <- exp(logit) / (1+exp(logit))

  return(score_2)
}

