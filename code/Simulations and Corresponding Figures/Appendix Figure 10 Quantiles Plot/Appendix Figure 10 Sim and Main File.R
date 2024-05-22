library(ggplot2)
library(dplyr)
library(tibble)
library(tidyr)
library(randomForest)
library(gam)
library(MASS)
library(mvtnorm)
library(parallel)
library(tikzDevice)

## Define parameters
## Sample size for test and train data
n_train <- 2500
n_test <- 2500

## Probability of having protected class A=1
pi <- 0.5

## Probability of having the label Y=1 (indicies respectively for the race A)
p <- c(0.5, 0.5)
names(p) <- c("p1", "p2")

## We have 4 distributions. They are fixed as normal for now but user can define their mean and sd.
mean_vec <- c(2,3,7,0,1,6,2,3,7,0,1,6)
# mean_vec <- c(2,3,7,0,1,6,3,4,8,1,2,7)
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
num_iter_adaboost <- 100

avg_matrix <- matrix(nrow=length(p_iter), ncol=144)
sd_matrix <- matrix(nrow=length(p_iter), ncol=144)
quantile_low <- matrix(nrow=length(p_iter), ncol=144)
quantile_up <- matrix(nrow=length(p_iter), ncol=144)

p_iter_fullrun <- list()

set.seed(1)

for (i in 1:length(p_iter)) {
  p[2] <- p_iter[i]
  # diagnostics_list <- lapply(1:nits, function(dummy_iter) {
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

    ## Naive Bayes
    s_nb_fasi_calibrate <- fasi_fcn(dta_train, dta_calibrate, naive_indicator = F, num_iter_adaboost)
    s_nb_fasi_test <- fasi_fcn(dta_obs, dta_test, naive_indicator = F, num_iter_adaboost)
    s_nb_naive_calibrate <- fasi_fcn(dta_train, dta_calibrate, naive_indicator = T, num_iter_adaboost)
    s_nb_naive_test <- fasi_fcn(dta_obs, dta_test, naive_indicator = T, num_iter_adaboost)

    dta_calibrate <- as_tibble(cbind.data.frame(dta_calibrate, s_nb_fasi_calibrate_c1=s_nb_fasi_calibrate[,1], s_nb_fasi_calibrate_c2=s_nb_fasi_calibrate[,2],
                                                s_nb_naive_calibrate_c1=s_nb_naive_calibrate[,1], s_nb_naive_calibrate_c2=s_nb_naive_calibrate[,2]))
    dta_test <- as_tibble(cbind.data.frame(dta_test, s_nb_fasi_test_c1=s_nb_fasi_test[,1], s_nb_fasi_test_c2=s_nb_fasi_test[,2],
                                           s_nb_naive_test_c1=s_nb_naive_test[,1], s_nb_naive_test_c2=s_nb_naive_test[,2]))

    nb_classification_umi <- aci_naive(dta_calibrate, dta_test, alpha_param, method = "s_nb_naive")
    nb_classification_ami <- aci_rank(dta_calibrate, dta_test, alpha_param, method = "s_nb_naive")
    nb_classification_ci <- aci_naive(dta_calibrate, dta_test, alpha_param, method = "s_nb_fasi")
    nb_classification_fasi <- aci_rank(dta_calibrate, dta_test, alpha_param, method = "s_nb_fasi")

    dta_test <- dta_test %>% add_column(nb_classification_umi, nb_classification_ami,
                                        nb_classification_ci, nb_classification_fasi)

    ## Gam error and power calculation
    nb_power <- sapply(colnames(dta_test)[grep('nb_classification',colnames(dta_test))], function(name) {
      power_est(dta_test, method = name)})
    nb_err <- sapply(colnames(dta_test)[grep('nb_classification',colnames(dta_test))], function(name) {
      fasi_err_full(dta_test, method = name)})

    ## Number of Decisions Made for every method
    num_decisions <- sapply(colnames(dta_test)[grep('aci_class|nb_class',colnames(dta_test))], function(m) num_dec_allgroup(dta_test, m))
    num_decisions_vec <- as.vector(num_decisions)
    names(num_decisions_vec) <- as.vector(outer(rownames(num_decisions), colnames(num_decisions), function(x,y) paste(x,y,sep=" ")))

    error_vec <- c(as.vector(oracle_err), as.vector(nb_err))
    names(error_vec) <- c(as.vector(outer(rownames(oracle_err), colnames(oracle_err), function(x,y) paste(x,y,sep=" "))),
                          as.vector(outer(rownames(nb_err), colnames(nb_err), function(x,y) paste(x,y,sep=" "))))
    power_sim <- c(as.vector(oracle_power), as.vector(nb_power))
    names(power_sim) <- c(as.vector(outer(rownames(oracle_power), colnames(oracle_power), function(x,y) paste(x,y,sep=" "))),
                          as.vector(outer(rownames(nb_power), colnames(nb_power), function(x,y) paste(x,y,sep=" "))))

    return_object <- c(error_vec, power_sim, num_decisions_vec)

    return(return_object)
  }, mc.cores = 10, mc.set.seed = TRUE)
  # })
  diagnostics <- Reduce(cbind, diagnostics_list)
  p_iter_fullrun[[i]] <- diagnostics
  avg_matrix[i,] <- apply(diagnostics, 1, mean)
  sd_matrix[i,] <- apply(diagnostics, 1, sd)
  quantile_up[i,] <- apply(diagnostics, 1, function(r) quantile(r, probs=c(0.95)))
  quantile_low[i,] <- apply(diagnostics, 1, function(r) quantile(r, probs=c(0.05)))
  ## See where we are
  print(i)
  print( apply(diagnostics, 1, mean))
}

colnames(avg_matrix) <- names(diagnostics_list[[1]])

p_iter <- seq(0.15, 0.85, by=0.05)

#######################################
####### FSR Plot Class 1 ##############
#######################################

avg_matrix2 <- as.data.frame(avg_matrix[,grep("FSR", colnames(avg_matrix))])
avg_matrix2 <- as.data.frame(avg_matrix2[,grep("[0-1] Class 1 |Full Class 1 ", colnames(avg_matrix2))])
avg_matrix2 <- as.data.frame(avg_matrix2[,grep("nb_classification_ci|nb_classification_fasi", colnames(avg_matrix2))])
avg_matrix2$p_iter <- p_iter
plot_matrix <- gather(avg_matrix2, method_type, average_error, "FSR Protected Class Full Class 1 nb_classification_ci":"FSR Protected Class 1 Class 1 nb_classification_fasi", factor_key=TRUE)

## Repeat for lower quantile
colnames(quantile_low) <- names(diagnostics_list[[1]])
q_low <- as.data.frame(quantile_low[,grep("FSR", colnames(quantile_low))])
q_low <- as.data.frame(q_low[,grep("[0-1] Class 1 |Full Class 1 ", colnames(q_low))])
q_low <- as.data.frame(q_low[,grep("nb_classification_ci|nb_classification_fasi", colnames(q_low))])
q_low$p_iter <- p_iter
q_low_mat <- gather(q_low, method_type, quantile_low, "FSR Protected Class Full Class 1 nb_classification_ci":"FSR Protected Class 1 Class 1 nb_classification_fasi", factor_key=TRUE)
plot_matrix$q_low <- q_low_mat$quantile_low

## Repeat for upper quantile
colnames(quantile_up) <- names(diagnostics_list[[1]])
q_up <- as.data.frame(quantile_up[,grep("FSR", colnames(quantile_up))])
q_up <- as.data.frame(q_up[,grep("[0-1] Class 1 |Full Class 1 ", colnames(q_up))])
q_up <- as.data.frame(q_up[,grep("nb_classification_ci|nb_classification_fasi", colnames(q_up))])
q_up$p_iter <- p_iter
q_up_mat <- gather(q_up, method_type, quantile_up, "FSR Protected Class Full Class 1 nb_classification_ci":"FSR Protected Class 1 Class 1 nb_classification_fasi", factor_key=TRUE)
plot_matrix$q_up <- q_up_mat$quantile_up


protected_class <- c()
protected_class[grep("Protected Class 0", plot_matrix$method_type)] <- "Female"
protected_class[grep("Protected Class 1", plot_matrix$method_type)] <- "Male"
protected_class[grep("Protected Class Full", plot_matrix$method_type)] <- "Female and Male"
plot_matrix$protected_class <- protected_class


plt_dta_err <- plot_matrix

## CI error plot
plt_dta_ci <- plt_dta_err[grep("FSR Protected Class [0-9]", plt_dta_err$method_type),]
fasi_indicator <- vector(mode="character", length=nrow(plt_dta_ci))
fasi_indicator[grep("classification_fasi", plt_dta_ci$method_type)] <- "FASI"
fasi_indicator[grep("classification_ci", plt_dta_ci$method_type)] <- "FCC"
plt_dta_ci$fasi_indicator <- fasi_indicator

plt_dta_ci$protected_class <-  factor(plt_dta_ci$protected_class, levels = c("Female and Male", "Female", "Male"))

plt_dta_quantiles_fasi <- plt_dta_ci[plt_dta_ci$fasi_indicator=="FASI",]
err_plot_class1_oracle <- ggplot(plt_dta_quantiles_fasi, aes(x=p_iter, y=average_error)) +
  geom_line(aes(color = protected_class, linetype = protected_class), size=1, show.legend = T) +
  theme_classic(base_size = 7) +
  scale_color_manual(values=c("#CC79A7", "#0072B2"),
                     labels = c("Female", "Male")) +
  scale_linetype_manual(values=c("twodash", "solid", "dashed")) +
  xlab('$\\pi_{2,F}$') +
  ylab("$FSR^{1,a}$") +
  labs(linetype="Race", colour="Method") +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), plot.subtitle = element_text(hjust = 0.5), legend.position="none") +
  # ylim(0.06,0.16) +
  xlim(0.15, 0.85) +
  geom_hline(yintercept=0.1, linetype="dotted",
             color = "black", size=0.5) +
  labs(subtitle = " ") +
  geom_ribbon(aes(x=p_iter, ymin=q_low, ymax=q_up, fill=protected_class), alpha= 0.1, show.legend = F) +
  geom_path(aes(x=p_iter, y=q_low, color=protected_class), linetype="dashed", size=0.5, show.legend = F) +
  geom_path(aes(x=p_iter, y=q_up, color=protected_class), linetype="dashed", size=0.5, , show.legend = F)




