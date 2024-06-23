## Figure 5 Plot File ##

## Load data
load("quantile_sim_1_gam.rda")

p_iter <- seq(0.15, 0.85, by=0.05)

#######################################
####### FSR Plot Class 1 ##############
#######################################

avg_matrix2 <- as.data.frame(avg_matrix[,grep("FSR", colnames(avg_matrix))])
avg_matrix2 <- as.data.frame(avg_matrix2[,grep("[0-1] Class 1 |Full Class 1 ", colnames(avg_matrix2))])
avg_matrix2 <- as.data.frame(avg_matrix2[,grep("gam_classification_ci|gam_classification_fasi", colnames(avg_matrix2))])
avg_matrix2$p_iter <- p_iter
plot_matrix <- gather(avg_matrix2, method_type, average_error, "FSR Protected Class Full Class 1 gam_classification_ci":"FSR Protected Class 1 Class 1 gam_classification_fasi", factor_key=TRUE)

## Repeat for lower quantile
colnames(quantile_low) <- names(diagnostics_list[[1]])
q_low <- as.data.frame(quantile_low[,grep("FSR", colnames(quantile_low))])
q_low <- as.data.frame(q_low[,grep("[0-1] Class 1 |Full Class 1 ", colnames(q_low))])
q_low <- as.data.frame(q_low[,grep("gam_classification_ci|gam_classification_fasi", colnames(q_low))])
q_low$p_iter <- p_iter
q_low_mat <- gather(q_low, method_type, quantile_low, "FSR Protected Class Full Class 1 gam_classification_ci":"FSR Protected Class 1 Class 1 gam_classification_fasi", factor_key=TRUE)
plot_matrix$q_low <- q_low_mat$quantile_low

## Repeat for upper quantile
colnames(quantile_up) <- names(diagnostics_list[[1]])
q_up <- as.data.frame(quantile_up[,grep("FSR", colnames(quantile_up))])
q_up <- as.data.frame(q_up[,grep("[0-1] Class 1 |Full Class 1 ", colnames(q_up))])
q_up <- as.data.frame(q_up[,grep("gam_classification_ci|gam_classification_fasi", colnames(q_up))])
q_up$p_iter <- p_iter
q_up_mat <- gather(q_up, method_type, quantile_up, "FSR Protected Class Full Class 1 gam_classification_ci":"FSR Protected Class 1 Class 1 gam_classification_fasi", factor_key=TRUE)
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
  xlab('$\\pi_{2|F}$') +
  ylab("$\\mbox{FSR}^{1}_a$") +
  labs(linetype="Race", colour="Method") +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), plot.subtitle = element_text(hjust = 0.5), legend.position="none") +
  xlim(0.15, 0.85) +
  geom_hline(yintercept=0.1, linetype="dotted",
             color = "black", size=0.5) +
  labs(subtitle = " ") +
  geom_ribbon(aes(x=p_iter, ymin=q_low, ymax=q_up, fill=protected_class), alpha= 0.1, show.legend = F) +
  geom_path(aes(x=p_iter, y=q_low, color=protected_class), linetype="dashed", size=0.5, show.legend = F) +
  geom_path(aes(x=p_iter, y=q_up, color=protected_class), linetype="dashed", size=0.5, , show.legend = F)

