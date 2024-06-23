require(ggplot2)
require(dplyr)
require(tibble)
require(tidyr)
require(e1071)
require(gam)
require(xgboost)
require(MASS)
require(mvtnorm)
require(parallel)
require(naivebayes)

## Load in the function file
source("Appendix Figure 11 Function File.R")

## Define parameters
## Sample size for test and train data
n_train <- 2500
n_test <- 1000

## Probability of having protected class A=1
pi <- 0.5

## Probability of having the label Y=1 (indicies respectively for the race A)
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

## Number of random iterations
nits <- 1000

p_iter <- seq(0.15, 0.85, by=0.05)
num_iter_adaboost <- 100

avg_matrix <- matrix(nrow=length(p_iter), ncol=48)
sd_matrix <- matrix(nrow=length(p_iter), ncol=48)
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

    ## Save master copies of these data sets
    dta_obs_model <- dta_obs
    dta_train_model <- dta_train
    dta_calibrate_model <- dta_calibrate
    dta_test_model <- dta_test

    ## Logistic Regression
    s_nb_fasi_calibrate <- fasi_fcn_logit(dta_train_model, dta_calibrate_model, naive_indicator = F, num_iter_adaboost)
    s_nb_fasi_test <- fasi_fcn_logit(dta_obs_model, dta_test_model, naive_indicator = F, num_iter_adaboost)

    dta_calibrate <- as_tibble(cbind.data.frame(dta_calibrate, s_logit_fasi_calibrate_c1=s_nb_fasi_calibrate[,1], s_logit_fasi_calibrate_c2=s_nb_fasi_calibrate[,2]))
    dta_test <- as_tibble(cbind.data.frame(dta_test, s_logit_fasi_test_c1=s_nb_fasi_test[,1], s_logit_fasi_test_c2=s_nb_fasi_test[,2]))

    logit_class <- aci_rank(dta_calibrate, dta_test, alpha_param, method = "s_logit_fasi")

    ## GAM
    s_nb_fasi_calibrate <- fasi_fcn_gam(dta_train_model, dta_calibrate_model, naive_indicator = F, num_iter_adaboost)
    s_nb_fasi_test <- fasi_fcn_gam(dta_obs_model, dta_test_model, naive_indicator = F, num_iter_adaboost)

    dta_calibrate <- as_tibble(cbind.data.frame(dta_calibrate, s_gam_fasi_calibrate_c1=s_nb_fasi_calibrate[,1], s_gam_fasi_calibrate_c2=s_nb_fasi_calibrate[,2]))
    dta_test <- as_tibble(cbind.data.frame(dta_test, s_gam_fasi_test_c1=s_nb_fasi_test[,1], s_gam_fasi_test_c2=s_nb_fasi_test[,2]))

    gam_class <- aci_rank(dta_calibrate, dta_test, alpha_param, method = "s_gam_fasi")

    ## XG Boost
    s_nb_fasi_calibrate <- fasi_fcn_xgboost(dta_train_model, dta_calibrate_model, naive_indicator = F, num_iter_adaboost)
    s_nb_fasi_test <- fasi_fcn_xgboost(dta_obs_model, dta_test_model, naive_indicator = F, num_iter_adaboost)

    dta_calibrate <- as_tibble(cbind.data.frame(dta_calibrate, s_xg_fasi_calibrate_c1=s_nb_fasi_calibrate[,1], s_xg_fasi_calibrate_c2=s_nb_fasi_calibrate[,2]))
    dta_test <- as_tibble(cbind.data.frame(dta_test, s_xg_fasi_test_c1=s_nb_fasi_test[,1], s_xg_fasi_test_c2=s_nb_fasi_test[,2]))

    xg_class <- aci_rank(dta_calibrate, dta_test, alpha_param, method = "s_xg_fasi")

    ## Naive Bayes
    s_nb_fasi_calibrate <- fasi_fcn_npnb(dta_train_model, dta_calibrate_model, naive_indicator = F, num_iter_adaboost)
    s_nb_fasi_test <- fasi_fcn_npnb(dta_obs_model, dta_test_model, naive_indicator = F, num_iter_adaboost)

    dta_calibrate <- as_tibble(cbind.data.frame(dta_calibrate, s_npnb_fasi_calibrate_c1=s_nb_fasi_calibrate[,1], s_npnb_fasi_calibrate_c2=s_nb_fasi_calibrate[,2]))
    dta_test <- as_tibble(cbind.data.frame(dta_test, s_npnb_fasi_test_c1=s_nb_fasi_test[,1], s_npnb_fasi_test_c2=s_nb_fasi_test[,2]))

    npnb_class <- aci_rank(dta_calibrate, dta_test, alpha_param, method = "s_npnb_fasi")

    ## Add classifications to the data frame
    dta_test <- dta_test %>% add_column(logit_class, gam_class, xg_class, npnb_class)
    ## Gam error and power calculation
    ## FSR
    logit_err <- fasi_err_full(dta_test, method = "logit_class")
    names(logit_err) <- paste("Logit", names(logit_err))

    gam_err <- fasi_err_full(dta_test, method = "gam_class")
    names(gam_err) <- paste("GAM", names(gam_err))

    xg_err <- fasi_err_full(dta_test, method = "xg_class")
    names(xg_err) <- paste("XG Boost", names(xg_err))

    nb_err <- fasi_err_full(dta_test, method = "nb_class")
    names(nb_err) <- paste("NB", names(nb_err))

    err_all <- c(logit_err, gam_err, xg_err, nb_err)

    ## Num Indecisions
    logit_epi <- num_dec_allgroup(dta_test, method = "logit_class")
    names(logit_epi) <- paste("Logit", names(logit_epi))

    gam_epi <- num_dec_allgroup(dta_test, method = "gam_class")
    names(gam_epi) <- paste("GAM", names(gam_epi))

    xg_epi <- num_dec_allgroup(dta_test, method = "xg_class")
    names(xg_epi) <- paste("XG Boost", names(xg_epi))

    nb_epi <- num_dec_allgroup(dta_test, method = "nb_class")
    names(nb_epi) <- paste("NB", names(nb_epi))

    epi_all <- c(logit_epi, gam_epi, xg_epi, nb_epi)

    return_object <- c(err_all, epi_all)

    return(return_object)
  }, mc.cores = 8, mc.set.seed = TRUE)
  diagnostics <- Reduce(cbind, diagnostics_list)
  p_iter_fullrun[[i]] <- diagnostics
  avg_matrix[i,] <- apply(diagnostics, 1, mean)
  sd_matrix[i,] <- apply(diagnostics, 1, sd)
  ## See where we are
  print(i)
}

colnames(avg_matrix) <- names(diagnostics_list[[1]])


## Save results
save(avg_matrix, sd_matrix, p_iter_fullrun, file = "appendix_mult_ml.rda")












#
# ## Save results-
# # save(avg_matrix, sd_matrix, p_iter_fullrun, file = "/Users/bradleyrava/Documents/Research Projects/Fairness Fdr/Simulations/Main Simulation New Rank Method/saved runs/rank_sim_1.rda")
#
#
#
# ## Save everything
# # save(avg_matrix, sd_matrix, file = "main_sim_paper_1.RData")
#
#
# # ## FDR Plot
# # avg_matrix2 <- cbind.data.frame(p_iter=p_iter, avg_matrix[,1:12])
# # colnames(avg_matrix2) <- c("p_iter", "ACI White Oracle", "ACI Black Oracle", "ACI White Logit", "ACI Black Logit",
# #                            "ACI White Random Forest", "ACI Black Random Forest", "ACI White Adaboost", "ACI Black Adaboost",
# #                            "ACI White GAM", "ACI Black GAM", "ACI White XGboost", "ACI Black XGboost")
# #
# # plot_matrix <- gather(avg_matrix2, method_type, average_error, "ACI White Oracle":"ACI Black XGboost", factor_key=TRUE)
# #
# # protected_class <- c()
# # protected_class[grep("Black", plot_matrix$method_type)] <- "Black"
# # protected_class[grep("White", plot_matrix$method_type)] <- "White"
# # plot_matrix$protected_class <- protected_class
# #
# # err_plot <- ggplot(plot_matrix, aes(x=p_iter, y=average_error)) +
# #   geom_line(aes(color = method_type, linetype = protected_class)) +
# #   theme_minimal() +
# #   scale_color_manual(values=c('#009E73','#009E73', '#CC79A7','#CC79A7',
# #                               "#D55E00", "#D55E00", "#0072B2", "#0072B2",
# #                               "darkgoldenrod2", "darkgoldenrod2", "purple4", "purple4")) +
# #   xlab(paste(paste("Fixed p0 =", p[2]), "against a fine grid of p1")) +
# #   ylab("P(Y=0 | Y_hat=1, A=a)")
#
# ## FSR Plot
# avg_matrix2 <- as.data.frame(avg_matrix[,grep("aci_class_ci error|nb_class_ci error|aci_class_fasi error|nb_class_fasi error", colnames(avg_matrix))])
# avg_matrix2$p_iter <- p_iter
# plot_matrix <- gather(avg_matrix2, method_type, average_error, "aci_class_ci error Full":"nb_class_fasi error 1", factor_key=TRUE)
#
# protected_class <- c()
# protected_class[grep("0", plot_matrix$method_type)] <- "Black"
# protected_class[grep("1", plot_matrix$method_type)] <- "White"
# protected_class[grep("Full", plot_matrix$method_type)] <- "Black and White"
# plot_matrix$protected_class <- protected_class
#
# ## subsetting the plot
# plot_matrix <- plot_matrix[plot_matrix$method_type %in% c("aci_class_ci error 0", "aci_class_ci error 1", "aci_class_fasi error Full",
#                                                           "aci_class_fasi error 0", "aci_class_fasi error 1", "nb_class_ci error 0",
#                                                           "nb_class_ci error 1", "nb_class_fasi error 0", "nb_class_fasi error 1"),]
# err_plot <- ggplot(plot_matrix, aes(x=p_iter, y=average_error)) +
#   geom_line(aes(color = method_type, linetype = protected_class), size=1) +
#   theme_minimal() +
#   scale_color_manual(values=c("#D55E00", "#D55E00", "#D55E00",
#                               "#56B4E9", "#56B4E9",
#                               "#E69F00", "#E69F00",
#                               "#0072B2", "#0072B2"),
#                      breaks=c("oracle ci err 0", "oracle fasi err 0", "nb ci err 0", "nb fasi err 0"),
#                      labels = c("Naive Oracle", "FASI Oracle", "Naive NB", "FASI NB")) +
#   scale_linetype_manual(values=c("dashed", "dotted", "solid"))+
#   xlab('Fixed $p_1=0.5$ with $p_2$ ranging from $0$ to $1$') +
#   ylab("$P(Y=0 | \\hat{Y}=1, A=a)$") +
#   labs(linetype="Race", colour="Method")
#
# # ## Full FSR plot
# # avg_matrix2 <- cbind.data.frame(p_iter=p_iter, avg_matrix[,17:24])
# # colnames(avg_matrix2) <- c("p_iter", "UMI Oracle", "AMI Oracle", "CI Oracle", "FASI Oracle",
# #                            "UMI GAM", "AMI GAM", "CI GAM", "FASI GAM")
# #
# # plot_matrix <- gather(avg_matrix2, method_type, average_error, "UMI Oracle":"FASI GAM", factor_key=TRUE)
# #
# # protected_class <- c()
# # protected_class[grep("Black", plot_matrix$method_type)] <- "Black"
# # protected_class[grep("White", plot_matrix$method_type)] <- "White"
# # plot_matrix$protected_class <- protected_class
# #
# # err_plot_full <- ggplot(plot_matrix, aes(x=p_iter, y=average_error)) +
# #   geom_line(aes(color = method_type), size=1) +
# #   theme_minimal() +
# #   scale_color_manual(values=c('#009E73', '#56B4E9', "#D55E00", "CC79A7"), labels = c("Oracle", "FASI GAM", "Naive GAM")) +
# #   ylim(0.095,0.12) +
# #   geom_hline(yintercept=0.1, linetype="dashed",
# #              color = "black") +
# #   xlab(paste(paste("Fixed p1 =", p[2]), "against a fine grid of p2")) +
# #   ylab("$FSR^{2}$") +
# #   labs(colour="Method")
#
#
# ## Power Plot
# avg_matrix2 <- cbind.data.frame(p_iter=p_iter, avg_matrix[,17:24])
# colnames(avg_matrix2) <- c("p_iter", "UMI Oracle", "AMI Oracle", "CI Oracle", "FASI Oracle",
#                            "UMI NB", "AMI NB", "CI NB", "FASI NB")
#
# avg_matrix2 <- avg_matrix2[,c(1,4:5,8:9)]
#
# avg_matrix2$rf <- rep(1, nrow(avg_matrix2))
# avg_matrix2$rf2 <- rep(1, nrow(avg_matrix2))
#
# # colnames(avg_matrix2)[6] <- "Random Forext"
#
# plot_matrix <- gather(avg_matrix2, method_type, average_error, "CI Oracle":"rf2", factor_key=TRUE)
# plot_matrix$oracle_type <- rep(c(rep("Ignore Protected Class", nrow(plot_matrix)/2),
#                                  rep("Include Protected Class", nrow(plot_matrix)/2)))
#
# power_plot <- ggplot(plot_matrix, aes(x=p_iter, y=average_error)) +
#   geom_line(aes(x=p_iter, y=average_error, color = method_type), size=1) +
#   theme_minimal() +
#   scale_color_manual(values=c("#D55E00",
#                               "#56B4E9",
#                               "#E69F00",
#                               "#0072B2",
#                               "white",
#                               "white"),
#                      breaks=c("CI Oracle", "FASI Oracle", "CI NB", "FASI NB"),
#                      labels = c("Naive Oracle", "FASI Oracle", "Naive NB", "FASI NB"))  +
#   xlab(paste(paste("Fixed p1 =", p[2]), "against a fine grid of p2")) +
#   labs(colour="Method") +
#   ylab("Power - P(Y_hat=1|Y=1)")
#
# ## Number of selections for each method
# selection_matrix <- avg_matrix[,17:24]
# colnames(selection_matrix) <- c("oracle_nd_naive_0", "oracle_nd_naive_1", "oracle_nd_fasi_0", "oracle_nd_fasi_1",
#                                 "gam_nd_naive_0", "gam_nd_naive_1", "gam_nd_fasi_0", "gam_nd_fasi_1")
#
#
# ### Export in tikz format
# ## Tikz code
#
# ## Error plot
# err_dta <- err_plot$data
# # err_dta <- err_dta %>%
# #   filter(grepl("aci|gam", method_type))
# err_plot <- ggplot(err_dta, aes(x=p_iter, y=average_error)) +
#   geom_line(aes(color = method_type, linetype = protected_class),  size=1) +
#   theme_minimal()  +
#   labs(linetype="Race", colour="Method") +
#   scale_color_manual(values=c("#D55E00", "#D55E00",
#                               "#56B4E9", "#56B4E9",
#                               "#E69F00", "#E69F00",
#                               "#0072B2", "#0072B2",
#                               "#D55E00"),
#                      breaks=c("oracle ci err 0", "oracle fasi err 0", "gam ci err 0", "gam fasi err 0"),
#                      labels = c("Naive Oracle", "FASI Oracle", "Naive GAM", "FASI GAM")) +
#   scale_linetype_manual(values=c("dashed", "dotdash", "solid")) +
#   xlab(paste(paste0("$p_1 =", p[1]), "$ against $p_2$ from $0.15$ to $0.85$")) +
#   ylab("$FSR^{2,a}$") +
#   theme(axis.text.x = element_text(vjust = 5),
#         axis.text.y = element_text(hjust = 20),
#         text = element_text(size=8)) +
#   theme(legend.position = "none")
#
# ## Power Plot
# power_dta <- power_plot$data
# # power_dta <- power_dta %>%
# #   filter(grepl("Oracle|GAM|Naive GAM|Random Forext", method_type))
# power_dta$protected_class <- "White"
# power_dta$protected_class[power_dta$method_type == "rf"] <- "Black"
# power_dta$protected_class[power_dta$method_type == "rf2"] <- "Black and White"
# power_dta$protected_class <- factor(power_dta$protected_class, levels = c("Black", "White", "Black and White"))
#
# power_plot <- ggplot(power_dta, aes(x=p_iter, y=average_error)) +
#   geom_line(aes(x=p_iter, y=average_error, color = method_type, linetype = protected_class), size=1) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(vjust = 5),
#         axis.text.y = element_text(hjust = 20),
#         text = element_text(size=8)) +
#   labs(linetype="Race", colour="Method") +
#   scale_color_manual(values=c("#D55E00",
#                               "#56B4E9",
#                               "#E69F00",
#                               "#0072B2",
#                               "white",
#                               "white"),
#                      breaks=c("CI Oracle", "FASI Oracle", "CI NB", "FASI NB"),
#                      labels = c("Naive Oracle", "FASI Oracle", "Naive NB", "FASI NB")) +
#   scale_linetype_manual(values=c("dashed", "solid", "dotdash")) +
#   xlab(paste(paste0("$p_1 =", p[2]), "$ against $p_2$ from $0$ to $1$")) +
#   ylab("$P(\\hat{Y}=2|Y=2)$")
#
# ## Export as tikz file
# tikz(file = "Simulation_1_plot_err_rank.tex", width = 2.75, height = 2.8)
# err_plot
# dev.off()
#
# tikz(file = "Simulation_1_plot_power_rank.tex", width = 3.25, height = 2.8)
# power_plot
# dev.off()
#
# tikz(file = "Simulation_1_plot_errfull_gf.tex", width = 4.5, height = 2.5)
# err_plot_full
# dev.off()
#
