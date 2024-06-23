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

  n1 <- sum(true_labels==2 & prot_class==1)
  n2 <- sum(true_labels==1 & prot_class==1)
  n3 <- sum(true_labels==2 & prot_class==0)
  n4 <- sum(true_labels==1 & prot_class==0)

  if (n1 != 0) {
    x[which(true_labels==2 & prot_class==1),] <- mvrnorm(n = n1, means[,1],
                                                         Sigma=sds, tol = 1e-6, empirical = FALSE)
  }
  if (n2 != 0) {
    x[which(true_labels==1 & prot_class==1),] <- mvrnorm(n = n2, means[,2],
                                                         Sigma=sds, tol = 1e-6, empirical = FALSE)
  }
  if (n3 != 0) {
    x[which(true_labels==2 & prot_class==0),] <- mvrnorm(n = n3, means[,3],
                                                         Sigma=sds, tol = 1e-6, empirical = FALSE)
  }
  if (n4 != 0) {
    x[which(true_labels==1 & prot_class==0),] <- mvrnorm(n = n4, means[,4],
                                                         Sigma=sds, tol = 1e-6, empirical = FALSE)
  }

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
  x_cur_1 <- x[a0_index,]
  if(is.matrix(x_cur_1) == FALSE) {
    x_cur_1 <- t(as.matrix(x_cur_1))
  }
  denom_a0 <- apply(x_cur_1, 1, function(x_cur) {
    return(p[2]*dmvnorm(x_cur, mean=means[,3], sigma=sds) +
             (1-p[2])*dmvnorm(x_cur, mean=means[,4], sigma=sds))
  })
  score_a0 <- apply(x_cur_1, 1, function(x_cur) {
    return(p[2]*dmvnorm(x_cur, mean=means[,3], sigma=sds))
  }) / denom_a0

  x_cur_2 <- x[a1_index,]
  if(is.matrix(x_cur_2) == FALSE) {
    x_cur_2 <- t(as.matrix(x_cur_2))
  }
  denom_a1 <- apply(x_cur_2, 1, function(x_cur) {
    return(p[1]*dmvnorm(x_cur, mean=means[,1], sigma=sds) +
             (1-p[1])*dmvnorm(x_cur, mean=means[,2], sigma=sds))
  })
  score_a1 <- apply(x_cur_2, 1, function(x_cur) {
    return(p[1]*dmvnorm(x_cur, mean=means[,1], sigma=sds))
  }) / denom_a1

  score_c2 <- vector(mode="numeric", length=nrow(x))
  if (length(score_a0) != 0) {
    score_c2[a0_index] <- score_a0
  }
  if (length(score_a1) != 0) {
    score_c2[a1_index] <- score_a1
  }

  ## P(Y=2|X,A)
  if (length(score_a0) != 0) {
    x_cur_1 <- x[a1_index,]
    if(is.matrix(x_cur_1) == FALSE) {
      x_cur_1 <- t(as.matrix(x_cur_1))
    }
    score_a0 <- apply(x_cur_1, 1, function(x_cur) {
      return((1-p[2])*dmvnorm(x_cur, mean=means[,4], sigma=sds))
    }) / denom_a0
  }

  if (length(score_a1) != 0) {
    x_cur_2 <- x[a1_index,]
    if(is.matrix(x_cur_2) == FALSE) {
      x_cur_2 <- t(as.matrix(x_cur_2))
    }
    score_a1 <- apply(x_cur_2, 1, function(x_cur) {
      return((1-p[1])*dmvnorm(x_cur, mean=means[,2], sigma=sds))
    }) / denom_a1
  }
  score_c1 <- vector(mode="numeric", length=nrow(x))

  if (length(a0_index) != 0) {
    score_c1[a0_index] <- score_a0
  }
  if (length(a1_index) != 0) {
    score_c1[a1_index] <- score_a1
  }

  ## Return object
  scores_return <- cbind.data.frame(s_sideinfo_c1=score_c1, s_sideinfo_c2=score_c2)
  return(scores_return)
}

