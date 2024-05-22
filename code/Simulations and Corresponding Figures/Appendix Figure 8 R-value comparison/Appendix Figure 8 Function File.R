## Function file for testing out different r-scores

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

sim_function <- function(nits, n_train, n_test, pi, p, means, sds, alpha_param, p_iter, rscore_bothdenom=F, num_cores) {

  set.seed(1)
  ## Preload containers
  avg_matrix <- matrix(nrow=length(p_iter), ncol=48)
  sd_matrix <- matrix(nrow=length(p_iter), ncol=48)
  p_iter_fullrun <- list()

  if (rscore_bothdenom == T) {
    rscore_fcn <- function(test_score_cur, train_scores, test_scores, train_true_labels, pt_class_cur, pt_class_train, pt_class_test, c1_ind) {
      m <- length(which(pt_class_test == pt_class_cur)) + length(which(pt_class_train == pt_class_cur))

      n <- length(which(pt_class_train == pt_class_cur))
      index_numerator <- which(train_scores >= test_score_cur)
      index_denominator <- which(test_scores >= test_score_cur)
      if (c1_ind == T) {
        r_elem <- ( (1/(n+1)) * (sum(train_true_labels[index_numerator] != 1)+1) )  / ( (1/(m+n+1)) * (length(index_denominator) +  length(index_numerator)) )
      } else {
        r_elem <- ( (1/(n+1)) * (sum(train_true_labels[index_numerator] != 2)+1) )  / ( (1/m) * (length(index_denominator) +  length(index_numerator)) )
      }
      return(r_elem)
    }
  } else {
    rscore_fcn <- function(test_score_cur, train_scores, test_scores, train_true_labels, pt_class_cur, pt_class_train, pt_class_test, c1_ind) {
      m <- length(which(pt_class_test == pt_class_cur))

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
  }

  for (i in 1:length(p_iter)) {
    p[2] <- p_iter[i]
    # diagnostics_list <- lapply(1:nits, function(dummy_iter) {
    diagnostics_list <- mclapply(1:nits, function(dummy_iter) {

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
      # })
    }, mc.cores = num_cores, mc.set.seed = TRUE)
    diagnostics <- Reduce(cbind, diagnostics_list)
    p_iter_fullrun[[i]] <- diagnostics
    avg_matrix[i,] <- apply(diagnostics, 1, function(v) mean(v, na.rm = T))
    sd_matrix[i,] <- apply(diagnostics, 1, function(v) sd(v, na.rm = T))
    ## See where we are
    print(i)
  }

  colnames(avg_matrix) <- names(diagnostics_list[[1]])

  return_object <- list(avg_matrix, sd_matrix, p_iter_fullrun)
  return(return_object)
}

rscore_plt_gen <- function(dta_current, p_iter, title="") {
  names_save <- c("FSR Protected Class Full Class 1 aci_class_umi",                   "FSR Protected Class 0 Class 1 aci_class_umi",
                  "FSR Protected Class 1 Class 1 aci_class_umi",                      "FSR Protected Class Full Class 2 aci_class_umi",
                  "FSR Protected Class 0 Class 2 aci_class_umi",                      "FSR Protected Class 1 Class 2 aci_class_umi",
                  "FSR Protected Class Full Class 1 aci_class_ami",                   "FSR Protected Class 0 Class 1 aci_class_ami",
                  "FSR Protected Class 1 Class 1 aci_class_ami",                      "FSR Protected Class Full Class 2 aci_class_ami",
                  "FSR Protected Class 0 Class 2 aci_class_ami",                      "FSR Protected Class 1 Class 2 aci_class_ami",
                  "FSR Protected Class Full Class 1 aci_class_ci",                    "FSR Protected Class 0 Class 1 aci_class_ci",
                  "FSR Protected Class 1 Class 1 aci_class_ci",                       "FSR Protected Class Full Class 2 aci_class_ci",
                  "FSR Protected Class 0 Class 2 aci_class_ci",                       "FSR Protected Class 1 Class 2 aci_class_ci",
                  "FSR Protected Class Full Class 1 aci_class_fasi",                  "FSR Protected Class 0 Class 1 aci_class_fasi",
                  "FSR Protected Class 1 Class 1 aci_class_fasi",                     "FSR Protected Class Full Class 2 aci_class_fasi",
                  "FSR Protected Class 0 Class 2 aci_class_fasi",                     "FSR Protected Class 1 Class 2 aci_class_fasi",
                  "Number of Decisions for Class 0 Protected Group 0 aci_class_umi",  "Number of Decisions for Class 1 Protected Group 0 aci_class_umi",
                  "Number of Decisions for Class 2 Protected Group 0 aci_class_umi",  "Number of Decisions for Class 0 Protected Group 1 aci_class_umi",
                  "Number of Decisions for Class 1 Protected Group 1 aci_class_umi",  "Number of Decisions for Class 2 Protected Group 1 aci_class_umi",
                  "Number of Decisions for Class 0 Protected Group 0 aci_class_ami",  "Number of Decisions for Class 1 Protected Group 0 aci_class_ami",
                  "Number of Decisions for Class 2 Protected Group 0 aci_class_ami",  "Number of Decisions for Class 0 Protected Group 1 aci_class_ami",
                  "Number of Decisions for Class 1 Protected Group 1 aci_class_ami",  "Number of Decisions for Class 2 Protected Group 1 aci_class_ami",
                  "Number of Decisions for Class 0 Protected Group 0 aci_class_ci",   "Number of Decisions for Class 1 Protected Group 0 aci_class_ci",
                  "Number of Decisions for Class 2 Protected Group 0 aci_class_ci",   "Number of Decisions for Class 0 Protected Group 1 aci_class_ci",
                  "Number of Decisions for Class 1 Protected Group 1 aci_class_ci",   "Number of Decisions for Class 2 Protected Group 1 aci_class_ci",
                  "Number of Decisions for Class 0 Protected Group 0 aci_class_fasi", "Number of Decisions for Class 1 Protected Group 0 aci_class_fasi",
                  "Number of Decisions for Class 2 Protected Group 0 aci_class_fasi", "Number of Decisions for Class 0 Protected Group 1 aci_class_fasi",
                  "Number of Decisions for Class 1 Protected Group 1 aci_class_fasi", "Number of Decisions for Class 2 Protected Group 1 aci_class_fasi")
  ## FASI Adjustment
  avg_matrix <- dta_current[[1]]
  colnames(avg_matrix) <- names_save
  avg_matrix2 <- cbind.data.frame(avg_matrix, p_iter)
  plt_wide <- as_tibble(avg_matrix2[,grep("^FSR .*aci_class_fasi|p_iter", colnames(avg_matrix2))])
  plt_data <- gather(plt_wide, method_type, average_error, "FSR Protected Class Full Class 1 aci_class_fasi":"FSR Protected Class 1 Class 2 aci_class_fasi", factor_key=TRUE)

  plt_dta_overall <- plt_data[grep("Protected Class Full Class 2|Protected Class 0 Class 2|Protected Class 1 Class 2", plt_data$method_type),]
  plt_dta_overall$Race <- rep("Female", nrow(plt_dta_overall))
  plt_dta_overall$Race[grep("Protected Class 1", plt_dta_overall$method_type)] <- "Male"
  plt_dta_overall$Race[grep("Full", plt_dta_overall$method_type)] <- "Female and Male"
  plt_dta_overall$Race <- factor(plt_dta_overall$Race, levels = c("Female", "Male", "Female and Male"))

  intro_plt_class1_fasi <- ggplot(plt_dta_overall, aes(x=p_iter, y=average_error)) +
    geom_line(aes(color=Race, linetype=Race), size=1) +
    ylim(0, 0.22) +
    xlim(0.15,0.85) +
    theme_classic(base_size = 7) +
    scale_color_manual(values=c("#009E73", "#0072B2", "#CC79A7")) +
    xlab("$p_F$") +
    ylab("$FSR^{1,a}$") +
    theme(legend.title = element_blank(), legend.position = c(.7, .85), plot.subtitle = element_text(hjust = 0.5)) +
    scale_linetype_manual(values=c("longdash", "twodash", "solid")) +
    labs(subtitle = title)

  return(intro_plt_class1_fasi)
}

plot_gen_new <- function(ggplot_data, title) {
  p_iter <- seq(0.05, 0.95, by=0.05)

  ggplt <- ggplot(ggplot_data, aes(x=p_iter, y=average_error)) +
    geom_line(aes(color=Race, linetype=Race), size=1) +
    ylim(0.05, 0.15) +
    theme_classic(base_size = 7) +
    scale_color_manual(values=c("#009E73", "#0072B2", "#CC79A7")) +
    xlab("$\\pi_{2,F}$") +
    ylab("$FSR^{1,a}$") +
    theme(legend.title = element_blank(), legend.position = c(.7, .85), plot.subtitle = element_text(hjust = 0.5)) +
    scale_linetype_manual(values=c("longdash", "twodash", "solid")) +
    labs(subtitle = title) +
    xlim(0.15,0.85) +
    geom_hline(yintercept=0.1, linetype="dashed")

  return(ggplt)
}


