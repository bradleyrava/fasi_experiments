## Adult Function File

cv_rows <- function(rows, n.group) {
  rows_rand <- rows[sample(rows)]
  ## Return a list with 10 approx equal vectors of rows.
  partitions <- split(rows_rand,
                      cut(rows_rand,quantile(rows_rand,(0:n.group)/n.group),
                          include.lowest=TRUE, labels=FALSE))

  for(jjj in 1:n.group) {
    partitions[[jjj]] <- rows_rand[partitions[[jjj]]]
  }

  return(partitions)
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
    dplyr::select(contains("sex")) %>%
    pull()
  test_ptc <- test_data %>%
    dplyr::select(contains("sex")) %>%
    pull()
  all_ptc <- c(train_ptc, test_ptc)

  conf_scores_r1 <- c((1-train_score), (1-test_score))
  r1_score_unadj <- sapply(1:length(conf_scores_r1), function(ii) {
    return(rscore_fcn(conf_scores_r1[ii], (1-train_score), (1-test_score), train_data$y, all_ptc[ii], train_ptc, test_ptc, T))
  })

  conf_scores_r2 <- c(train_score, test_score)
  r2_score_unadj <- sapply(1:length(conf_scores_r2), function(ii) {
    return(rscore_fcn(conf_scores_r2[ii], train_score, test_score, train_data$y, all_ptc[ii], train_ptc, test_ptc, F))
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


  ###############################
  ## Classify the observations ##
  ###############################
  r_scores_mat <- rbind.data.frame(r1=r1_score, r2=r2_score)
  classification_vec <- apply(r_scores_mat, 2, function(r_vals_cur) {
    c_candidate <- which.min(r_vals_cur)
    r_val_smallest <- r_vals_cur[c_candidate]
    class_cur <- ifelse(r_val_smallest <= alpha_param, c_candidate, 0)
    return(class_cur)
  })

  return(classification_vec)
}

aci_rank <- function(train_data, test_data, alpha_param, method) {
  ## Get column the method is in
  score_col_train <- grep(method, colnames(train_data))
  score_col_test <- grep(method, colnames(test_data))

  ptc_unique <- c("male", "female")
  rscore_list <- lapply(ptc_unique, function(pt_c) {
    index_rows <- which(test_data$sex == pt_c)

    ## Subset the data by protected class
    train_df <- train_data %>%
      filter(sex == pt_c)
    test_df <- test_data %>%
      filter(sex == pt_c)

    class_2_score_test_r1 <- 1 - dplyr::pull(test_df[, score_col_test])
    class_2_score_train_r1 <- 1 - dplyr::pull(train_df[,score_col_train])
    class_2_score_test_r2 <- dplyr::pull(test_df[, score_col_test])
    class_2_score_train_r2 <- dplyr::pull(train_df[,score_col_train])


    conf_scores_r1 <- c(class_2_score_train_r1, class_2_score_test_r1)
    r1_score_unadj <- sapply(conf_scores_r1, function(test_cur) {
      return(rscore_fcn(test_cur, class_2_score_train_r1, class_2_score_test_r1,
                        train_df$y, pt_c, train_data$sex, test_data$sex, T))
    })

    conf_scores_r2 <- c(class_2_score_train_r2, class_2_score_test_r2)
    r2_score_unadj <- sapply(conf_scores_r2, function(test_cur) {
      return(rscore_fcn(test_cur, class_2_score_train_r2, class_2_score_test_r2,
                        train_df$y, pt_c, train_data$sex, test_data$sex, F))
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
  names(rscore_list) <- ptc_unique

  r_scores <- as_tibble(Reduce(rbind, rscore_list))
  r_scores_order <- r_scores %>%
    dplyr::arrange(index_rows)

  r_scores_mat <- rbind.data.frame(r1=r_scores_order$r1_score, r2=r_scores_order$r2_score)
  classification_vec <- apply(r_scores_mat, 2, function(r_vals_cur) {
    c_candidate <- which.min(r_vals_cur)
    r_val_smallest <- r_vals_cur[c_candidate]
    class_cur <- ifelse(r_val_smallest <= alpha_param, c_candidate, 0)
    return(class_cur)
  })

  return(classification_vec)
}


rscore_fcn <- function(test_score_cur, train_scores, test_scores, train_true_labels,
                       pt_class_cur, pt_class_train, pt_class_test, c1_ind) {
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

  all_groups <- c(1,2)
  error_rates <- lapply(all_groups, function(class_cur) {
    ## Only look at instances where we have selected someone into the favorable group
    x <- test_data %>%
      dplyr::filter(test_data[,m_index]==class_cur)

    err_full <- ifelse(sum(x$y != class_cur)==0, 0, mean(x$y != class_cur))

    ## Protected Class Error
    pt_class_unique <- c("male", "female")

    err_pt <- sapply(pt_class_unique, function(pt_c) {
      ind <- which(x$sex==pt_c)
      group2_err <- ifelse(length(ind)==0, 0, mean(x$y[ind] != class_cur))
      return(group2_err)
    })

    ### return object
    err_all <- c(err_full, err_pt)
    names(err_all) <- paste(paste(method, c("Full Error", paste(pt_class_unique, "Error"))), "Class", class_cur)
    return(err_all)
  })
  pt_class_unique <- c("male", "female")

  err_rates_vec <- unlist(error_rates)
  return(err_rates_vec)
}

fasi_fcn <- function(train_data, test_data, naive_indicator=T, n_iter, max_depth, max_delta_step) {
  label_train <- as.factor(train_data$y - 1)
  label_test <- as.factor(test_data$y - 1)

  y_train <- label_train
  vars <- colnames(train_data[c(1:9,11:14)])
  keep_vars <- if(naive_indicator) {vars} else {c(vars, "sex")}

  rows <- 1:nrow(train_data)

  ## Subset the train / test data on the vars we need to model
  dta_train <- train_data %>%
    dplyr::select(all_of(keep_vars))
  dta_test <- test_data %>%
    dplyr::select(all_of(keep_vars))

  ## Adaboost Model ##
  dta_train_model <- cbind.data.frame(data.matrix(dta_train), y = as.factor(y_train))
  label_ada <- vector(mode="numeric", length=length(train_data$y))
  label_ada[train_data$y==2] <- 1
  label_ada[train_data$y==1] <- -1
  adaboost_model <- JOUSBoost::adaboost(X=data.matrix(dta_train), y=label_ada, n_rounds=100, tree_depth = 5)
  score_2 <- predict(adaboost_model, X = data.matrix(dta_test), type="prob")

  return(score_2)
}
