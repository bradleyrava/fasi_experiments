## Function File

gen_obs <- function(n, pi, p, means, sds, oracle_indicator=F) {
  # the input
  # n: The number of observations you want to generate
  # p: A 2 dimensional vector p=[p_1, p_0] denoting the probabilities of
  #   having true label Y=1 for each protected class respectively.
  # pi: A probability (single number) denoting the probability of being from protected class 1.
  # means: A 4 dimensional vector denoting the means of the conditional distrbutions
  #        f_11, f_10, f_01, f_00, in that order.
  # sds: A 4 dimensional vector denoting the standard deviations of the conditional distrbutions
  #        f_11, f_10, f_01, f_00, in that order.
  # the output
  # dta: A data frame that contains the univariate feature x, the protected class of x, the true label Y of x,
  #      along with the naive and side information scores used for classification

  ## Assign the protected class (flip coin with prob pi)
  ## pi = P(white) => 1=white and 0=black
  prot_class <- rbinom(n, 1, pi)

  ## Assign Y=0 and Y=1
  true_labels <- rep(NA, length=n)
  n_ptclass0 <- sum(prot_class==0)
  true_labels[prot_class==0] <- ifelse(rbinom(n_ptclass0, 1, p[2])==1, 2, 1)
  true_labels[prot_class==1] <- ifelse(rbinom(n-n_ptclass0, 1, p[1])==1, 2, 1)
  true_labels[is.na(true_labels)] <- 0

  ## Add the observations
  ## Here we have 4 cases to consider
  x <- matrix(NA, nrow=n, ncol=nrow(sds))
  x[which(true_labels==2 & prot_class==1),] <- mvrnorm(n = sum(true_labels==2 & prot_class==1), means[,1],
                                                       Sigma=sds, tol = 1e-6, empirical = FALSE)
  x[which(true_labels==1 & prot_class==1),] <- mvrnorm(n = sum(true_labels==1 & prot_class==1), means[,2],
                                                       Sigma=sds, tol = 1e-6, empirical = FALSE)
  x[which(true_labels==2 & prot_class==0),] <- mvrnorm(n = sum(true_labels==2 & prot_class==0), means[,3],
                                                       Sigma=sds, tol = 1e-6, empirical = FALSE)
  x[which(true_labels==1 & prot_class==0),] <- mvrnorm(n = sum(true_labels==1 & prot_class==0), means[,4],
                                                       Sigma=sds, tol = 1e-6, empirical = FALSE)
  colnames(x) <- c("x1", "x2", "x3")

  ## Combine the data
  if (oracle_indicator == F) {
    dta <- cbind.data.frame(protected_class=as.character(prot_class), true_label=as.character(true_labels), x)
    dta <- as_tibble(dta)
  }
  else {
    ## Add in oracle classification scores
    a1_index <- which(prot_class==1)
    a0_index <- which(prot_class==0)
    sideinfo <- score_sideinfo(x[,1:3], p, means, sds, a0_index, a1_index)
    naive <- score_naive(x[,1:3], p, pi, means, sds)

    ## Combine the data
    dta <- cbind.data.frame(protected_class=as.character(prot_class), true_label=as.character(true_labels), x,
                            naive, sideinfo)
    dta <- as_tibble(dta)
  }
  return(dta)
}

score_naive <- function(x, p, pi, means, sds) {
  # the input
  # x: A univariate vector of observations from our sample.
  # p: A 2 dimensional vector p=[p_1, p_0] denoting the probabilities of
  #   having true label Y=1 for each protected class respectively.
  # pi: A probability (single number) denoting the probability of being from protected class 1.
  # means: A 4 dimensional vector denoting the means of the conditional distrbutions
  #        f_11, f_10, f_01, f_00, in that order.
  # sds: A 4 dimensional vector denoting the standard deviations of the conditional distrbutions
  #        f_11, f_10, f_01, f_00, in that order.
  # the output
  # score: A vector of length x that denotes the probability of being from class 1 given x. P(Y=1|x).

  denom <- apply(x, 1, function(x_cur) {
    (pi*p[1]*dmvnorm(x_cur, mean=means[,1], sigma=sds) + (1-pi)*p[2]*dmvnorm(x_cur, mean=means[,3], sigma=sds) +
       pi*(1-p[1])*dmvnorm(x_cur, mean=means[,2], sigma=sds) + (1-pi)*(1-p[2])*dmvnorm(x_cur, mean=means[,4], sigma=sds))
  })

  score_c2 <- apply(x, 1, function(x_cur) {
    (pi*p[1]*dmvnorm(x_cur, mean=means[,1], sigma=sds) + (1-pi)*p[2]*dmvnorm(x_cur, mean=means[,3], sigma=sds))
  }) / denom

  score_c1 <- 1-score_c2

  scores_return <- cbind.data.frame(s_naive_c1=score_c1, s_naive_c2=score_c2)
  return(scores_return)
}

score_sideinfo <- function(x, p, means, sds, a0_index, a1_index) {
  # the input
  # x: A single feature from our sample (i.e. one number).
  # p: A 2 dimensional vector p=[p_1, p_0] denoting the probabilities of
  #   having true label Y=1 for each protected class respectively.
  # pi: A probability (single number) denoting the probability of being from protected class 1.
  # means: A 4 dimensional vector denoting the means of the conditional distrbutions
  #        f_11, f_10, f_01, f_00, in that order.
  # sds: A 4 dimensional vector denoting the standard deviations of the conditional distrbutions
  #        f_11, f_10, f_01, f_00, in that order.
  # a: The protected class that corresponds to feature x.
  # the output
  # score: A vector of length x that denotes the probability of being from class 1 given x and a. P(Y=1|x,a).
  a0_index <- (1:nrow(x))[-a1_index]

  ## P(Y=2|X,A)
  denom_a0 <- apply(x[a0_index,], 1, function(x_cur) {
    return(p[2]*dmvnorm(x_cur, mean=means[,3], sigma=sds) +
             (1-p[2])*dmvnorm(x_cur, mean=means[,4], sigma=sds))
  })
  score_a0 <- apply(x[a0_index,], 1, function(x_cur) {
    return(p[2]*dmvnorm(x_cur, mean=means[,3], sigma=sds))
  }) / denom_a0

  denom_a1 <-  apply(x[a1_index,], 1, function(x_cur) {
    return(p[1]*dmvnorm(x_cur, mean=means[,1], sigma=sds) +
             (1-p[1])*dmvnorm(x_cur, mean=means[,2], sigma=sds))
  })
  score_a1 <- apply(x[a1_index,], 1, function(x_cur) {
    return(p[1]*dmvnorm(x_cur, mean=means[,1], sigma=sds))
  }) / denom_a1

  score_c2 <- vector(mode="numeric", length=nrow(x))
  score_c2[a0_index] <- score_a0
  score_c2[a1_index] <- score_a1

  ## P(Y=2|X,A)
  score_a0 <- apply(x[a0_index,], 1, function(x_cur) {
    return((1-p[2])*dmvnorm(x_cur, mean=means[,4], sigma=sds))
  }) / denom_a0

  score_a1 <- apply(x[a1_index,], 1, function(x_cur) {
    return((1-p[1])*dmvnorm(x_cur, mean=means[,2], sigma=sds))
  }) / denom_a1

  score_c1 <- vector(mode="numeric", length=nrow(x))
  score_c1[a0_index] <- score_a0
  score_c1[a1_index] <- score_a1

  ## Return object
  scores_return <- cbind.data.frame(s_sideinfo_c1=score_c1, s_sideinfo_c2=score_c2)
  return(scores_return)
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
  error_rates <- sapply(all_groups, function(class_cur) {
    ## Only look at instances where we have selected someone into the favorable group
    x <- test_data %>%
      dplyr::filter(test_data[,m_index]==class_cur)

    err_full <- ifelse(sum(x$true_label != class_cur)==0, 0, mean(x$true_label != class_cur))

    ## Protected Class Error
    pt_class_unique <- c(0,1)

    err_pt <- sapply(pt_class_unique, function(pt_c) {
      ind <- which(x$protected_class==pt_c)
      group2_err <- ifelse(length(ind)==0, 0, mean(x$true_label[ind] != class_cur))
      return(group2_err)
    })

    ### return object
    err_all <- c(err_full, err_pt)
    names(err_all) <- paste(method, c("Full Error", paste(pt_class_unique, "Error")))
    return(err_all)
  })
  err_rates_vec <- as.vector(error_rates)
  names(err_rates_vec) <- paste("FSR Protected Class", c("Full", "0", "1"), "Class", c(1,1,1,2,2,2), sep = " ")
  return(err_rates_vec)
}

rscore_fcn <- function(test_score_cur, train_scores, test_scores, train_true_labels, pt_class_cur, pt_class_train, pt_class_test, c1_ind) {
  m <- length(which(pt_class_train == pt_class_cur))

  n <- length(which(pt_class_train == pt_class_cur))
  index_numerator <- which(train_scores >= test_score_cur)
  index_denominator <- which(test_scores >= test_score_cur)
  if (c1_ind == T) {
    r_elem <- ( (1/(n+1)) * (sum(train_true_labels[index_numerator] != 1)+1) )  / ( (1/m) * length(index_numerator) )
  } else {
    r_elem <- ( (1/(n+1)) * (sum(train_true_labels[index_numerator] != 2)+1) )  / ( (1/m) * length(index_numerator) )
  }
  return(r_elem)
}


aci_rank <- function(train_data, test_data, alpha_param, method) {
  ## Get column the method is in
  score_col_train <- grep(method, colnames(train_data))
  score_col_test <- grep(method, colnames(test_data))

  rscore_list <- lapply(c(0,1), function(pt_c) {
    index_rows <- which(test_data$protected_class == pt_c)

    ## Subset the data by protected class
    train_df <- train_data %>%
      dplyr::filter(protected_class == pt_c)
    test_df <- test_data %>%
      dplyr::filter(protected_class == pt_c)

    class_2_score_test_r1 <- dplyr::pull(test_df[,score_col_test[1]])
    class_2_score_train_r1 <- dplyr::pull(train_df[,score_col_train[1]])
    class_2_score_test_r2 <- dplyr::pull(test_df[,score_col_test[2]])
    class_2_score_train_r2 <- dplyr::pull(train_df[,score_col_train[2]])

    r1_score <- sapply(class_2_score_test_r1, function(test_cur) {
      return(rscore_fcn(test_cur, class_2_score_train_r1, class_2_score_test_r1, train_df$true_label, pt_c, train_data$protected_class, test_data$protected_class, T))
    })

    r2_score <- sapply(class_2_score_test_r2, function(test_cur) {
      return(rscore_fcn(test_cur, class_2_score_train_r2, class_2_score_test_r2, train_df$true_label, pt_c, train_data$protected_class, test_data$protected_class, F))
    })

    return_scores <- cbind.data.frame(r1_score, r2_score, index_rows)
    return(return_scores)
  })
  names(rscore_list) <- c(0, 1)

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
    dplyr::select(contains(method))

  test_score <- test_data %>%
    dplyr::select(contains(method))

  test_score_iter_r1 <- dplyr::pull(test_score[,1])
  test_score_iter_r2 <- dplyr::pull(test_score[,2])

  r1_score <- sapply(1:length(test_score_iter_r1), function(ii) {
    return(rscore_fcn(test_score_iter_r1[ii], dplyr::pull(train_score[,1]), dplyr::pull(test_score[,1]), train_data$true_label, test_data$protected_class[ii], train_data$protected_class, test_data$protected_class, T))
  })

  r2_score <- sapply(1:length(test_score_iter_r2), function(ii) {
    return(rscore_fcn(test_score_iter_r2[ii], dplyr::pull(train_score[,2]), dplyr::pull(test_score[,2]), train_data$true_label, test_data$protected_class[ii], train_data$protected_class, test_data$protected_class, F))
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

naivebayes_rd <- function(dta_train, dta_test, naive_indicator=T) {
  ## This function extracts the scores from the xgboost model
  # the input
  # train_data: A dataset from the gen_obs() function that contains the feature x, protected,
  #             class, true label, and naive / side info scores. This data will be used to
  #             determine the thresholds.
  # test_data: A dataset from the gen_obs() function that contains the feature x, protected,
  #             class, true label, and naive / side info scores. This data will be classified
  #             according to the rule determined from train_data.
  # the output
  # scores: A vector of probabilities that are from a model trained on the entire train dataset
  #         and separately predicted on each class.

  if (naive_indicator == T) {
    dta_train_model <- data.matrix(dta_train[,grep("protected|x", colnames(dta_train))])
    dta_test_model <- data.matrix(dta_test[,grep("protected|x", colnames(dta_test))])

    model_nb <- nonparametric_naive_bayes(x = dta_train_model, y = as.character(dta_train$true_label))
    scores <- predict(model_nb, newdata = dta_test_model, type = "prob")
    score_class2 <- scores[,2]
    return (score_class2)
  } else {
    dta_train_model <- data.matrix(dta_train[,grep("x", colnames(dta_train))])
    dta_test_model <- data.matrix(dta_test[,grep("x", colnames(dta_test))])

    model_nb <- nonparametric_naive_bayes(x = dta_train_model, y = as.character(dta_train$true_label))
    scores <- predict(model_nb, newdata = dta_test_model, type = "prob")
    score_class2 <- scores[,2]
    return (score_class2)
  }
}


num_dec_allgroup <- function(dta, method) {
  all_pt_class <- c(0,1)

  pt_numdec <- sapply(all_pt_class, function(pt_cur) {
    method_loc <- grep(method,colnames(dta))
    info <- pull(dta[dta$protected_class==pt_cur, method_loc])
    num_dec <- table(factor(info, levels = 0:2))
    return(num_dec)
  })

  num_dec_vec <- as.vector(pt_numdec)
  names(num_dec_vec) <- paste("Number of Decisions for Class", rep(rownames(pt_numdec), 2), "Protected Group", c(0,0,0,1,1,1))
  return(num_dec_vec)
}

fasi_fcn <- function(train_data, test_data, naive_indicator=T, n_iter) {
  label_train <- train_data$true_label
  label_test <- test_data$true_label

  class_scores <- sapply(1:2, function(ii) {
    if (ii == 1) {
      ## Change all 2's to 1
      label_train[train_data$true_label == 1] <- 2
      label_train[train_data$true_label == 2] <- 1
      label_test[test_data$true_label == 1] <- 2
      label_test[test_data$true_label == 2] <- 1
    }

    y_train <- label_train
    vars <- colnames(train_data[,grep("x", colnames(train_data))])
    keep_vars <- if(naive_indicator) {vars} else {c(vars, "protected_class")}

    rows <- 1:nrow(train_data)

    ## Subset the train / test data on the vars we need to model
    dta_train <- train_data %>%
      dplyr::select(all_of(keep_vars))
    dta_test <- test_data %>%
      dplyr::select(all_of(keep_vars))

    dta_train_model <- cbind.data.frame(data.matrix(dta_train), y = as.character(y_train))
    adaboost_model <- adaboost(y ~ ., data=as.data.frame(dta_train_model), nIter = n_iter)
    score_2 <- predict(adaboost_model, newdata = data.matrix(dta_test))$prob[,2]
  })

  colnames(class_scores) <- c("class_1", "class_2")
  return(class_scores)
}


