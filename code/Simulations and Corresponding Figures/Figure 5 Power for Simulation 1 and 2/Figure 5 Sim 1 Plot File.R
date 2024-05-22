##################################
## Plots for paper simulation ####
##################################
library(ggpubr)
library(ggplot2)
library(tidyverse)

p_iter <- seq(0.15, 0.85, by=0.05)

#######################################
####### FSR Plot Class 1 ##############
#######################################

avg_matrix2 <- as.data.frame(avg_matrix[,grep("FSR", colnames(avg_matrix))])
avg_matrix2 <- as.data.frame(avg_matrix2[,grep("[0-1] Class 1 |Full Class 1 ", colnames(avg_matrix2))])
avg_matrix2 <- as.data.frame(avg_matrix2[,grep("nb_classification_ci|nb_classification_fasi", colnames(avg_matrix2))])
avg_matrix2$p_iter <- p_iter
plot_matrix <- gather(avg_matrix2, method_type, average_error, "FSR Protected Class Full Class 1 nb_classification_ci":"FSR Protected Class 1 Class 1 nb_classification_fasi", factor_key=TRUE)

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

plt_dta_ci <- rbind.data.frame(plt_dta_ci,
                               cbind.data.frame(p_iter, method_type=rep("random", length(p_iter)), average_error=rep(0.5, length(p_iter)), protected_class=rep("Female and Male", length(p_iter)), fasi_indicator=rep("FASI", length(p_iter))))

plt_dta_ci$protected_class <-  factor(plt_dta_ci$protected_class, levels = c("Female and Male", "Female", "Male"))

err_plot_class1_oracle <- ggplot(plt_dta_ci, aes(x=p_iter, y=average_error)) +
  geom_line(aes(color = fasi_indicator, linetype = protected_class), size=1) +
  theme_classic(base_size = 7) +
  scale_color_manual(values=c("#CC79A7", "#0072B2"),
                     labels = c("FASI", "FCC")) +
  scale_linetype_manual(values=c("twodash", "solid", "dashed")) +
  xlab('$\\pi_{2,F}$') +
  ylab("$FSR^{1,a}$") +
  labs(linetype="Race", colour="Method") +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), plot.subtitle = element_text(hjust = 0.5), legend.position = "none") +
  ylim(0.06,0.16) +
  xlim(0.15, 0.85) +
  geom_hline(yintercept=0.1, linetype="dotted",
             color = "black", size=0.5) +
  labs(subtitle = " ")


avg_matrix2 <- as.data.frame(avg_matrix[,grep("FSR", colnames(avg_matrix))])
avg_matrix2 <- as.data.frame(avg_matrix2[,grep("[0-1] Class 1 |Full Class 1 ", colnames(avg_matrix2))])
avg_matrix2 <- as.data.frame(avg_matrix2[,grep("nb_classification_ci|nb_classification_fasi", colnames(avg_matrix2))])
avg_matrix2$p_iter <- p_iter
plot_matrix <- gather(avg_matrix2, method_type, average_error, "FSR Protected Class Full Class 1 nb_classification_ci":"FSR Protected Class 1 Class 1 nb_classification_fasi", factor_key=TRUE)

protected_class <- c()
protected_class[grep("Protected Class 0", plot_matrix$method_type)] <- "Female"
protected_class[grep("Protected Class 1", plot_matrix$method_type)] <- "Male"
protected_class[grep("Protected Class Full", plot_matrix$method_type)] <- "Female and Male"
plot_matrix$protected_class <- protected_class


plt_dta_err <- plot_matrix

## CI error plot
plt_dta_ci <- plt_dta_err[grep("FSR Protected Class [0-9]", plt_dta_err$method_type),]
fasi_indicator <- vector(mode="character", length=nrow(plt_dta_ci))
fasi_indicator[grep("fasi", plt_dta_ci$method_type)] <- "FASI"
fasi_indicator[grep("ci", plt_dta_ci$method_type)] <- "FCC"
plt_dta_ci$fasi_indicator <- fasi_indicator
err_plot_class1 <- ggplot(plt_dta_ci, aes(x=p_iter, y=average_error)) +
  geom_line(aes(color = fasi_indicator, linetype = protected_class), size=1) +
  theme_classic(base_size = 7) +
  scale_color_manual(values=c("#CC79A7", "#0072B2"),
                     labels = c("FASI", "FCC")) +
  scale_linetype_manual(values=c("solid", "dashed")) +
  xlab('$\\pi_{2,F}$') +
  ylab("$FSR^{1,a}$") +
  labs(linetype="Race", colour="Method") +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), plot.subtitle = element_text(hjust = 0.5), legend.position = "none") +
  ylim(0.06,0.16) +
  xlim(0.15, 0.85) +
  geom_hline(yintercept=0.1, linetype="dotted",
             color = "black", size=0.5) +
  labs(subtitle = " ")


#######################################
####### FSR Plot Class 2 ##############
#######################################

avg_matrix2 <- as.data.frame(avg_matrix[,grep("FSR", colnames(avg_matrix))])
avg_matrix2 <- as.data.frame(avg_matrix2[,grep("[0-1] Class 2 |Full Class 2 ", colnames(avg_matrix2))])
avg_matrix2 <- as.data.frame(avg_matrix2[,grep("nb_classification_ci|nb_classification_fasi", colnames(avg_matrix2))])
avg_matrix2$p_iter <- p_iter
plot_matrix <- gather(avg_matrix2, method_type, average_error, "FSR Protected Class Full Class 2 nb_classification_ci":"FSR Protected Class 1 Class 2 nb_classification_fasi", factor_key=TRUE)

protected_class <- c()
protected_class[grep("Protected Class 0", plot_matrix$method_type)] <- "Female"
protected_class[grep("Protected Class 1", plot_matrix$method_type)] <- "Male"
protected_class[grep("Protected Class Full", plot_matrix$method_type)] <- "Female and Male"
plot_matrix$protected_class <- protected_class


plt_dta_err <- plot_matrix

## CI error plot
plt_dta_ci <- plt_dta_err[grep("FSR Protected Class [0-9]", plt_dta_err$method_type),]
fasi_indicator <- vector(mode="character", length=nrow(plt_dta_ci))
fasi_indicator[grep("fasi", plt_dta_ci$method_type)] <- "FASI"
fasi_indicator[grep("ci", plt_dta_ci$method_type)] <- "FCC"
plt_dta_ci$fasi_indicator <- fasi_indicator
err_plot_class2 <- ggplot(plt_dta_ci, aes(x=p_iter, y=average_error)) +
  geom_line(aes(color = fasi_indicator, linetype = protected_class), size=1) +
  theme_classic(base_size = 7) +
  scale_color_manual(values=c("#CC79A7", "#0072B2"),
                     labels = c("FASI", "FCC")) +
  scale_linetype_manual(values=c("solid", "dashed")) +
  xlab('$\\pi_{2,F}$') +
  ylab("$FSR^{2,a}$") +
  labs(linetype="Race", colour="Method") +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), plot.subtitle = element_text(hjust = 0.5), legend.position = "none") +
  ylim(0.06,0.16) +
  xlim(0.15, 0.85) +
  labs(subtitle = "Data Driven (GAM)") +
  geom_hline(yintercept=0.1, linetype="dotted",
             color = "black", size=0.5)


#######################################
####### Proportion of Class 0 #########
#######################################

avg_matrix_uncomb <- as.data.frame(avg_matrix[,grep("Number of Decisions for Class 0", colnames(avg_matrix))])
avg_matrix_uncomb <- as.data.frame(avg_matrix_uncomb[,grep("nb_classification_ci|nb_classification_fasi", colnames(avg_matrix_uncomb))])
avg_matrix2 <- cbind.data.frame(num_indecisions_ci=avg_matrix_uncomb$`Number of Decisions for Class 0 Protected Group 0 nb_classification_ci`+avg_matrix_uncomb$`Number of Decisions for Class 0 Protected Group 1 nb_classification_ci`,
                                num_indecisions_fasi=avg_matrix_uncomb$`Number of Decisions for Class 0 Protected Group 0 nb_classification_fasi`+avg_matrix_uncomb$`Number of Decisions for Class 0 Protected Group 1 nb_classification_fasi`)
avg_matrix2 <- avg_matrix2 / 2000
avg_matrix2$p_iter <- p_iter
plot_matrix <- gather(avg_matrix2, method_type, num_class0, "num_indecisions_ci":"num_indecisions_fasi", factor_key=TRUE)

plt_dta_err <- plot_matrix
fasi_indicator <- vector(mode="character", length=nrow(plt_dta_err))
fasi_indicator[grep("_fasi", plt_dta_err$method_type)] <- "FASI"
fasi_indicator[grep("_ci", plt_dta_err$method_type)] <- "FCC"
plt_dta_err$fasi_indicator <- fasi_indicator
plt_dta_err$protected_class <- rep("Female and Male", nrow(plt_dta_err))


## CI error plot
indecision_prop_plt <- ggplot(plt_dta_err, aes(x=p_iter, y=num_class0)) +
  geom_line(aes(color = fasi_indicator, linetype=protected_class), size=1) +
  theme_classic(base_size = 7) +
  scale_color_manual(values=c("#CC79A7", "#0072B2"),
                     labels = c("FASI", "FCC")) +
  scale_linetype_manual(values=c("twodash")) +
  xlab('$\\pi_{2,F}$') +
  ylab("EPI") +
  labs(linetype="Race", colour="Method") +
  theme(legend.title = element_blank(), plot.subtitle = element_text(hjust = 0.5)) +
  ylim(0,0.12) +
  labs(subtitle = " ")


##################################
## Oracle Plots ####
##################################

#######################################
####### FSR Plot Class 1 ##############
#######################################

avg_matrix2 <- as.data.frame(avg_matrix[,grep("FSR", colnames(avg_matrix))])
avg_matrix2 <- as.data.frame(avg_matrix2[,grep("[0-1] Class 1 |Full Class 1 ", colnames(avg_matrix2))])
avg_matrix2 <- as.data.frame(avg_matrix2[,grep("aci_class_ci|aci_class_fasi", colnames(avg_matrix2))])
avg_matrix2$p_iter <- p_iter
plot_matrix <- gather(avg_matrix2, method_type, average_error, "FSR Protected Class Full Class 1 aci_class_ci":"FSR Protected Class 1 Class 1 aci_class_fasi", factor_key=TRUE)

protected_class <- c()
protected_class[grep("Protected Class 0", plot_matrix$method_type)] <- "Female"
protected_class[grep("Protected Class 1", plot_matrix$method_type)] <- "Male"
protected_class[grep("Protected Class Full", plot_matrix$method_type)] <- "Female and Male"
plot_matrix$protected_class <- protected_class


plt_dta_err <- plot_matrix

## CI error plot
plt_dta_ci <- plt_dta_err[grep("FSR Protected Class [0-9]", plt_dta_err$method_type),]
fasi_indicator <- vector(mode="character", length=nrow(plt_dta_ci))
fasi_indicator[grep("class_fasi", plt_dta_ci$method_type)] <- "FASI"
fasi_indicator[grep("class_ci", plt_dta_ci$method_type)] <- "FCC"
plt_dta_ci$fasi_indicator <- fasi_indicator

plt_dta_ci <- rbind.data.frame(plt_dta_ci,
                               cbind.data.frame(p_iter, method_type=rep("random", length(p_iter)), average_error=rep(0.5, length(p_iter)), protected_class=rep("Female and Male", length(p_iter)), fasi_indicator=rep("FASI", length(p_iter))))

plt_dta_ci$protected_class <-  factor(plt_dta_ci$protected_class, levels = c("Female and Male", "Female", "Male"))

err_plot_class1_oracle <- ggplot(plt_dta_ci, aes(x=p_iter, y=average_error)) +
  geom_line(aes(color = fasi_indicator, linetype = protected_class), size=1) +
  theme_classic(base_size = 7) +
  scale_color_manual(values=c("#CC79A7", "#0072B2"),
                     labels = c("FASI", "FCC")) +
  scale_linetype_manual(values=c("twodash", "solid", "dashed")) +
  xlab('$\\pi_{2,F}$') +
  ylab("$FSR^{1,a}$") +
  labs(linetype="Race", colour="Method") +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), plot.subtitle = element_text(hjust = 0.5), legend.position = "none") +
  ylim(0.06,0.16) +
  xlim(0.15, 0.85) +
  geom_hline(yintercept=0.1, linetype="dotted",
             color = "black", size=0.5) +
  labs(subtitle = " ")



#######################################
####### FSR Plot Class 2 ##############
#######################################

avg_matrix2 <- as.data.frame(avg_matrix[,grep("FSR", colnames(avg_matrix))])
avg_matrix2 <- as.data.frame(avg_matrix2[,grep("[0-1] Class 2 |Full Class 2 ", colnames(avg_matrix2))])
avg_matrix2 <- as.data.frame(avg_matrix2[,grep("aci_class_ci|aci_class_fasi", colnames(avg_matrix2))])
avg_matrix2$p_iter <- p_iter
plot_matrix <- gather(avg_matrix2, method_type, average_error, "FSR Protected Class Full Class 2 aci_class_ci":"FSR Protected Class 1 Class 2 aci_class_fasi", factor_key=TRUE)

protected_class <- c()
protected_class[grep("Protected Class 0", plot_matrix$method_type)] <- "Female"
protected_class[grep("Protected Class 1", plot_matrix$method_type)] <- "Male"
protected_class[grep("Protected Class Full", plot_matrix$method_type)] <- "Female and Male"
plot_matrix$protected_class <- protected_class

plt_dta_err <- plot_matrix

## CI error plot
plt_dta_ci <- plt_dta_err[grep("FSR Protected Class [0-9]", plt_dta_err$method_type),]
fasi_indicator <- vector(mode="character", length=nrow(plt_dta_ci))
fasi_indicator[grep("class_fasi", plt_dta_ci$method_type)] <- "FASI"
fasi_indicator[grep("class_ci", plt_dta_ci$method_type)] <- "FCC"
plt_dta_ci$fasi_indicator <- fasi_indicator
err_plot_class2_oracle <- ggplot(plt_dta_ci, aes(x=p_iter, y=average_error)) +
  geom_line(aes(color = fasi_indicator, linetype = protected_class), size=1) +
  theme_classic(base_size = 7) +
  scale_color_manual(values=c("#CC79A7", "#0072B2"),
                     labels = c("FASI", "FCC")) +
  scale_linetype_manual(values=c("solid", "dashed")) +
  xlab('$\\pi_{2,F}$') +
  ylab("$FSR^{2,a}$") +
  labs(linetype="Race", colour="Method") +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), plot.subtitle = element_text(hjust = 0.5), legend.position = "none") +
  ylim(0.06,0.16) +
  xlim(0.15, 0.85) +
  labs(subtitle = "Oracle") +
  geom_hline(yintercept=0.1, linetype="dotted",
             color = "black", size=0.5)


######


#######################################
####### Proportion of Class 0 #########
#######################################

avg_matrix_uncomb <- as.data.frame(avg_matrix[,grep("Number of Decisions for Class 0", colnames(avg_matrix))])
avg_matrix_uncomb <- as.data.frame(avg_matrix_uncomb[,grep("aci_class_ci|aci_class_fasi", colnames(avg_matrix_uncomb))])
avg_matrix2 <- cbind.data.frame(num_indecisions_ci=avg_matrix_uncomb$`Number of Decisions for Class 0 Protected Group 0 aci_class_ci`+avg_matrix_uncomb$`Number of Decisions for Class 0 Protected Group 1 aci_class_ci`,
                                num_indecisions_fasi=avg_matrix_uncomb$`Number of Decisions for Class 0 Protected Group 0 aci_class_fasi`+avg_matrix_uncomb$`Number of Decisions for Class 0 Protected Group 1 aci_class_fasi`)
avg_matrix2 <- avg_matrix2 / 2000
avg_matrix2$p_iter <- p_iter
plot_matrix <- gather(avg_matrix2, method_type, num_class0, "num_indecisions_ci":"num_indecisions_fasi", factor_key=TRUE)

plt_dta_err <- plot_matrix
fasi_indicator <- vector(mode="character", length=nrow(plt_dta_err))
fasi_indicator[grep("_fasi", plt_dta_err$method_type)] <- "FASI"
fasi_indicator[grep("_ci", plt_dta_err$method_type)] <- "FCC"
plt_dta_err$fasi_indicator <- fasi_indicator
plt_dta_err$protected_class <- rep("Female and Male", nrow(plt_dta_err))


## CI error plot
indecision_prop_plt_oracle <- ggplot(plt_dta_err, aes(x=p_iter, y=num_class0)) +
  geom_line(aes(color = fasi_indicator, linetype=protected_class), size=1) +
  theme_classic(base_size = 7) +
  scale_color_manual(values=c("#CC79A7", "#0072B2"),
                     labels = c("FASI", "FCC")) +
  scale_linetype_manual(values=c("twodash")) +
  xlab('$\\pi_{2,F}$') +
  ylab("EPI") +
  labs(linetype="Race", colour="Method") +
  theme(legend.title = element_blank(), plot.subtitle = element_text(hjust = 0.5)) +
  ylim(0,0.12) +
  labs(subtitle = " ")


##########################################
##########################################
#### Adding back in the power plots
##########################################
##########################################

#######################################
####### Proportion of Class 0 #########
#######################################

avg_matrix_power <- as.data.frame(avg_matrix[,grep("^Power Protected Class Full.+aci_class_ci|^Power Protected Class Full.+aci_class_fasi$", colnames(avg_matrix))])
avg_matrix2_class1 <- avg_matrix_power[,grep("Class 1", colnames(avg_matrix_power))]
avg_matrix2_class2 <- avg_matrix_power[,grep("Class 2", colnames(avg_matrix_power))]

avg_matrix2_class1$p_iter <- p_iter
avg_matrix2_class2$p_iter <- p_iter
plot_matrix_class1 <- gather(avg_matrix2_class1, method_type, power_1, "Power Protected Class Full Class 1 aci_class_ci":"Power Protected Class Full Class 1 aci_class_fasi", factor_key=TRUE)
plot_matrix_class2 <- gather(avg_matrix2_class2, method_type, power_2, "Power Protected Class Full Class 2 aci_class_ci":"Power Protected Class Full Class 2 aci_class_fasi", factor_key=TRUE)


plt_dta_err <- plot_matrix_class1
fasi_indicator <- vector(mode="character", length=nrow(plt_dta_err))
fasi_indicator[grep("_fasi", plt_dta_err$method_type)] <- "FASI"
fasi_indicator[grep("_ci", plt_dta_err$method_type)] <- "FCC"
plt_dta_err$fasi_indicator <- fasi_indicator
plt_dta_err$protected_class <- rep("Female and Male", nrow(plt_dta_err))


## CI error plot
power_class1 <- ggplot(plt_dta_err, aes(x=p_iter, y=power_1)) +
  geom_line(aes(color = fasi_indicator, linetype=protected_class), size=1) +
  theme_classic(base_size = 7) +
  scale_color_manual(values=c("#CC79A7", "#0072B2"),
                     labels = c("FASI", "FCC")) +
  scale_linetype_manual(values=c("twodash")) +
  xlab('$\\pi_{2,F}$') +
  ylab("Power for Class 1") +
  labs(linetype="Race", colour="Method") +
  theme(legend.title = element_blank(), plot.subtitle = element_text(hjust = 0.5)) +
  ylim(0.55,1) +
  labs(subtitle = "Simulation 2")

plt_dta_err <- plot_matrix_class2
fasi_indicator <- vector(mode="character", length=nrow(plt_dta_err))
fasi_indicator[grep("_fasi", plt_dta_err$method_type)] <- "FASI"
fasi_indicator[grep("_ci", plt_dta_err$method_type)] <- "FCC"
plt_dta_err$fasi_indicator <- fasi_indicator
plt_dta_err$protected_class <- rep("Female and Male", nrow(plt_dta_err))

## CI error plot
power_class2 <- ggplot(plt_dta_err, aes(x=p_iter, y=power_2)) +
  geom_line(aes(color = fasi_indicator, linetype=protected_class), size=1) +
  theme_classic(base_size = 7) +
  scale_color_manual(values=c("#CC79A7", "#0072B2"),
                     labels = c("FASI", "FCC")) +
  scale_linetype_manual(values=c("twodash")) +
  xlab('$\\pi_{2,F}$') +
  ylab("Power for Class 2") +
  labs(linetype="Race", colour="Method") +
  theme(legend.title = element_blank(), plot.subtitle = element_text(hjust = 0.5)) +
  ylim(0.55,1) +
  labs(subtitle = "Simulation 2")




## All plots

all_plots_oracle <- ggarrange(err_plot_class1_oracle, err_plot_class2_oracle, indecision_prop_plt_oracle,  ncol=3, nrow=1, common.legend = TRUE, legend="none")
all_plots_dd <- ggarrange(err_plot_class1, err_plot_class2, indecision_prop_plt,  ncol=3, nrow=1, common.legend = TRUE, legend="bottom")
all_plots_power <- ggarrange(power_class1, power_class2,  ncol=2, nrow=1, common.legend = TRUE, legend="right")

all_plots <- ggarrange(err_plot_class1_oracle, err_plot_class2_oracle, indecision_prop_plt_oracle,
                      err_plot_class1, err_plot_class2, indecision_prop_plt,
                      ncol=3, nrow=2, common.legend = TRUE, legend="bottom")

## save
all_plots_power1
all_plots_power2

power_plt <- ggarrange(all_plots_power1, all_plots_power2,
          ncol=1, nrow=2, common.legend = TRUE, legend="bottom")

