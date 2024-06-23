##########################################################
## Code for reproducing plots for census data analysis ###
##########################################################

## Load in data from the main file ##
load("adult_large_run.RData")

###############
## FASI Plot ##
###############

## Class 1 ##

err_fasi <- c(err_matrix_storage$`gam_classification_fasi Full Error Class 1`, err_matrix_storage$`gam_classification_fasi male Error Class 1`, err_matrix_storage$`gam_classification_fasi female Error Class 1`)

err_full <- err_fasi
err_full_adj <- err_full - rep(alpha_grid, 3)

x_axis_alpha <- rep(alpha_grid, 3)
race <- rep(rep(c("Female and Male", "Male", "Female"), each=length(alpha_grid)), 1)
method <- rep("FASI", length(err_fasi))

plt_cc_dta <- cbind.data.frame(alpha=x_axis_alpha, Race=race, err_full_adj, method)
plt_cc_dta$Race <- factor(plt_cc_dta$Race, levels = c("Female and Male", "Female", "Male"))

fasi_plt_ccapp_class1 <- ggplot(plt_cc_dta, aes(x=alpha, y=err_full_adj)) +
  geom_line(aes(color=Race, linetype=Race), size=1) +
  geom_point(aes(shape=Race, color=Race), size = 1.5) +
  scale_linetype_manual(values=c("solid", "solid", "solid")) +
  scale_color_manual(values=c("#009E73", "#0072B2", "#D55E00")) +
  theme_classic(base_size = 7) +
  xlab('$\\alpha$') +
  ylab('$\\widehat{FSR}^{2}_a - \\alpha$') +
  geom_hline(yintercept=0, linetype="dashed") +
  ylim(-0.05, 0.05) +
  coord_cartesian(xlim = c(0.005, 0.1))  +
  theme(legend.title = element_blank(), plot.subtitle = element_text(hjust = 0.5)) +
  labs(subtitle = "FASI - Class 1")

## Class 2 ##

err_fasi <- c(err_matrix_storage$`gam_classification_fasi Full Error Class 2`, err_matrix_storage$`gam_classification_fasi male Error Class 2`, err_matrix_storage$`gam_classification_fasi female Error Class 2`)

err_full <- err_fasi
err_full_adj <- err_full - rep(alpha_grid, 3)

x_axis_alpha <- rep(alpha_grid, 3)
race <- rep(rep(c("Female and Male", "Male", "Female"), each=length(alpha_grid)), 1)
method <- rep("FASI", length(err_fasi))

plt_cc_dta <- cbind.data.frame(alpha=x_axis_alpha, Race=race, err_full_adj, method)
plt_cc_dta$Race <- factor(plt_cc_dta$Race, levels = c("Female and Male", "Female", "Male"))

fasi_plt_ccapp_class2 <- ggplot(plt_cc_dta, aes(x=alpha, y=err_full_adj)) +
  geom_line(aes(color=Race, linetype=Race), size=1) +
  geom_point(aes(shape=Race, color=Race), size = 1.5) +
  scale_linetype_manual(values=c("solid", "solid", "solid")) +
  scale_color_manual(values=c("#009E73", "#0072B2", "#D55E00")) +
  theme_classic(base_size = 7) +
  xlab('$\\alpha$') +
  ylab('$\\widehat{FSR}^{2}_a - \\alpha$') +
  geom_hline(yintercept=0, linetype="dashed") +
  ylim(-0.05, 0.05) +
  coord_cartesian(xlim = c(0.005, 0.1))  +
  theme(legend.title = element_blank(), plot.subtitle = element_text(hjust = 0.5)) +
  labs(subtitle = "FASI - Class 2")

###############
## RCC ########
###############

## Class 1

err_nr <- c(err_matrix_storage$`gam_classification_nr Full Error Class 1`, err_matrix_storage$`gam_classification_nr male Error Class 1`, err_matrix_storage$`gam_classification_nr female Error Class 1`)

err_full <- err_nr
err_full_adj <- err_full - rep(alpha_grid, 3)

x_axis_alpha <- rep(alpha_grid, 3)
race <- rep(rep(c("Female and Male", "Male", "Female"), each=length(alpha_grid)), 1)
method <- rep("Race Adjusted", length(err_nr))

plt_cc_dta <- cbind.data.frame(alpha=x_axis_alpha, Race=race, err_full_adj, method)
plt_cc_dta$Race <- factor(plt_cc_dta$Race, levels = c("Female and Male", "Female", "Male"))

nr_plt_ccapp_class1 <- ggplot(plt_cc_dta, aes(x=alpha, y=err_full_adj)) +
  geom_line(aes(color=Race, linetype=Race), size=1) +
  geom_point(aes(shape=Race, color=Race), size = 1.5) +
  scale_color_manual(values=c("#009E73", "#0072B2", "#D55E00")) +
  theme_classic(base_size = 7) +
  xlab('$\\alpha$') +
  ylab('$\\widehat{FSR}^{2}_a - \\alpha$') +
  geom_hline(yintercept=0, linetype="dashed") +
  ylim(-0.05, 0.05) +
  coord_cartesian(xlim = c(0.005, 0.1)) +
  theme(legend.title = element_blank(), plot.subtitle = element_text(hjust = 0.5)) +
  scale_linetype_manual(values=c("dotdash", "dotdash", "dotdash")) +
  labs(subtitle = "FCC - Class 1")

## Class 2
err_nr <- c(err_matrix_storage$`gam_classification_nr Full Error Class 2`, err_matrix_storage$`gam_classification_nr male Error Class 2`, err_matrix_storage$`gam_classification_nr female Error Class 2`)

err_full <- err_nr
err_full_adj <- err_full - rep(alpha_grid, 3)

x_axis_alpha <- rep(alpha_grid, 3)
race <- rep(rep(c("Female and Male", "Male", "Female"), each=length(alpha_grid)), 1)
method <- rep("Race Adjusted", length(err_nr))

plt_cc_dta <- cbind.data.frame(alpha=x_axis_alpha, Race=race, err_full_adj, method)
plt_cc_dta$Race <- factor(plt_cc_dta$Race, levels = c("Female and Male", "Female", "Male"))

nr_plt_ccapp_class2 <- ggplot(plt_cc_dta, aes(x=alpha, y=err_full_adj)) +
  geom_line(aes(color=Race, linetype=Race), size=1) +
  geom_point(aes(shape=Race, color=Race), size = 1.5) +
  scale_color_manual(values=c("#009E73", "#0072B2", "#D55E00")) +
  theme_classic(base_size = 7) +
  xlab('$\\alpha$') +
  ylab('$\\widehat{FSR}^{2}_a - \\alpha$') +
  geom_hline(yintercept=0, linetype="dashed") +
  ylim(-0.05, 0.05) +
  coord_cartesian(xlim = c(0.005, 0.1)) +
  theme(legend.title = element_blank(), plot.subtitle = element_text(hjust = 0.5)) +
  scale_linetype_manual(values=c("dotdash", "dotdash", "dotdash")) +
  labs(subtitle = "FCC - Class 2")


###############
## FCC ########
###############

## Class 1

err_naive <- c(err_matrix_storage$`gam_classification_ci Full Error Class 1`, err_matrix_storage$`gam_classification_ci male Error Class 1`, err_matrix_storage$`gam_classification_ci female Error Class 1`)

err_full <- err_naive
err_full_adj <- err_full - rep(alpha_grid, 3)

x_axis_alpha <- rep(alpha_grid, 3)
race <- rep(rep(c("Female and Male", "Male", "Female"), each=length(alpha_grid)), 1)
method <- rep("Race Adjusted", length(err_naive))

plt_cc_dta <- cbind.data.frame(alpha=x_axis_alpha, Race=race, err_full_adj, method)
plt_cc_dta$Race <- factor(plt_cc_dta$Race, levels = c("Female and Male", "Female", "Male"))

naive_plt_ccapp_class1 <- ggplot(plt_cc_dta, aes(x=alpha, y=err_full_adj)) +
  geom_line(aes(color=Race, linetype=Race), size=1) +
  geom_point(aes(shape=Race, color=Race), size = 1.5) +
  scale_color_manual(values=c("#009E73", "#0072B2", "#D55E00")) +
  theme_classic(base_size = 7) +
  xlab('$\\alpha$') +
  ylab('$\\widehat{FSR}^{2}_a - \\alpha$') +
  geom_hline(yintercept=0, linetype="dashed") +
  ylim(-0.05, 0.05) +
  coord_cartesian(xlim = c(0.005, 0.1)) +
  theme(legend.title = element_blank(), plot.subtitle = element_text(hjust = 0.5)) +
  scale_linetype_manual(values=c("dotdash", "dotdash", "dotdash")) +
  labs(subtitle = "FCC - Class 1")

## Class 2

err_naive <- c(err_matrix_storage$`gam_classification_ci Full Error Class 2`, err_matrix_storage$`gam_classification_ci male Error Class 2`, err_matrix_storage$`gam_classification_ci female Error Class 2`)

err_full <- err_naive
err_full_adj <- err_full - rep(alpha_grid, 3)

x_axis_alpha <- rep(alpha_grid, 3)
race <- rep(rep(c("Female and Male", "Male", "Female"), each=length(alpha_grid)), 1)
method <- rep("Race Adjusted", length(err_naive))

plt_cc_dta <- cbind.data.frame(alpha=x_axis_alpha, Race=race, err_full_adj, method)
plt_cc_dta$Race <- factor(plt_cc_dta$Race, levels = c("Female and Male", "Female", "Male"))

naive_plt_ccapp_class2 <- ggplot(plt_cc_dta, aes(x=alpha, y=err_full_adj)) +
  geom_line(aes(color=Race, linetype=Race), size=1) +
  geom_point(aes(shape=Race, color=Race), size = 1.5) +
  scale_color_manual(values=c("#009E73", "#0072B2", "#D55E00")) +
  theme_classic(base_size = 7) +
  xlab('$\\alpha$') +
  ylab('$\\widehat{FSR}^{2}_a - \\alpha$') +
  geom_hline(yintercept=0, linetype="dashed") +
  ylim(-0.05, 0.05) +
  coord_cartesian(xlim = c(0.005, 0.1)) +
  theme(legend.title = element_blank(), plot.subtitle = element_text(hjust = 0.5)) +
  scale_linetype_manual(values=c("dotdash", "dotdash", "dotdash")) +
  labs(subtitle = "FCC - Class 2")

########################
########################
#### Indecision plot
########################
########################
err_fasi <- err_matrix_storage$numselect_gam_classification_fasi_Full_0 / (err_matrix_storage$numselect_gam_classification_fasi_Full_0 + err_matrix_storage$numselect_gam_classification_ci_Full_1 + err_matrix_storage$numselect_gam_classification_fasi_Full_2)


err_full <- err_fasi
err_full_adj <- err_full

x_axis_alpha <- rep(alpha_grid, 1)
race <- rep("Female and Male", length(alpha_grid))
method <- rep("FASI", length(err_fasi))

plt_cc_dta1 <- cbind.data.frame(alpha=x_axis_alpha, Race=race, err_full_adj, method)

err_naive <- err_matrix_storage$numselect_gam_classification_ci_Full_0 / (err_matrix_storage$numselect_gam_classification_ci_Full_0 + err_matrix_storage$numselect_gam_classification_ci_Full_1 + err_matrix_storage$numselect_gam_classification_ci_Full_2)

err_full <- err_naive
err_full_adj <- err_full

x_axis_alpha <- rep(alpha_grid, 1)
race <- rep("Female and Male", length(alpha_grid))
method <- rep("FCC", length(err_naive))

plt_cc_dta2 <- cbind.data.frame(alpha=x_axis_alpha, Race=race, err_full_adj, method)

plt_indec_compass_dta <- cbind.data.frame(rbind.data.frame(plt_cc_dta1, plt_cc_dta2) )
plt_indec_compass_dta <- rbind.data.frame(plt_indec_compass_dta,
                                          cbind.data.frame(alpha=x_axis_alpha, Race=c(rep("Female", length(x_axis_alpha)),rep("Male", length(x_axis_alpha))), err_full_adj=rep(2, 2*length(x_axis_alpha)), method=rep("FASI", 2*length(x_axis_alpha))))
plt_indec_compass_dta$Race <- factor(plt_indec_compass_dta$Race, levels = c("Female and Male", "Female", "Male"))

indecision_plts <- ggplot(plt_indec_compass_dta, aes(x=alpha, y=err_full_adj)) +
  geom_line(aes(color=Race, linetype=method), size=1) +
  geom_point(aes(shape=Race, color=Race), size = 1.5) +
  scale_color_manual(values=c("#009E73", "#0072B2", "#D55E00")) +
  scale_linetype_manual(values=c("solid", "dotdash")) +
  theme_classic(base_size = 7) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title = element_blank(), legend.position="bottom") +
  xlab('$\\alpha$') +
  ylab("$\\widehat{EPI}$") +
  coord_cartesian(xlim = c(0.005, 0.1)) +
  labs(subtitle = "Indecisions") +
  ylim(0.15, 0.82)

###############################################
## Both figures create the plot in the paper ##
###############################################

adult_plots_all_onerow <- ggarrange(naive_plt_ccapp_class1, fasi_plt_ccapp_class1,
                                    naive_plt_ccapp_class2, fasi_plt_ccapp_class2,
                                    ncol=2, nrow=2, common.legend = T,
                                    legend="bottom", legend.grob = get_legend(indecision_plts))

indecision_plt <- ggarrange(indecision_plts, legend="none")

