## Load data from main file run ##
load("mult_alpha_compass.rda")

err_fasi <- c(mult_alpha_err_mat$`nb_classification_fasi Other Error`,
              mult_alpha_err_mat$`nb_classification_fasi African-American Error`,
              mult_alpha_err_mat$`nb_classification_fasi Full Error`)
err_raceadj <- c(mult_alpha_err_mat$`nb_classification_ci Other Error`,
                 mult_alpha_err_mat$`nb_classification_ci African-American Error`,
                 mult_alpha_err_mat$`nb_classification_ci Full Error`)


err_full <- c(err_fasi, err_raceadj)

err_full_adj <- err_full - rep(alpha_grid, 6)

x_axis_alpha <- rep(alpha_grid, 6)
race <- rep(rep(c("Other", "Black", "All Races"), each=length(alpha_grid)), 4)

method <- c(rep("FASI", length(err_fasi)), rep("Race Adjusted", length(err_fasi)))

plt_compass_dta <- cbind.data.frame(alpha=x_axis_alpha, Race=race, err_full_adj, method)

## Plot Race Adjusted
dta_2 <- plt_compass_dta[plt_compass_dta$method=="Race Adjusted",]
raceadj_plt <- ggplot(dta_2, aes(x=alpha, y=err_full_adj)) +
  geom_line(aes(color=Race, linetype=Race), size=1) +
  geom_point(aes(shape=Race, color=Race), size = 1.5) +
  scale_color_manual(values=c("#009E73", "#0072B2", "#D55E00")) +
  scale_linetype_manual(values=c("dotdash", "dotdash", "dotdash")) +
  scale_x_continuous(breaks = c(0.1, 0.15, 0.2, 0.25, 0.3)) +
  theme_classic(base_size = 7) +
  xlab('$\\alpha$') +
  ylab('$\\widehat{FSR}^{2,a} - \\alpha$') +
  geom_hline(yintercept=0, linetype="dashed") +
  ylim(-0.06, 0.06) +
  coord_cartesian(xlim = c(0.15, 0.30)) +
  theme(legend.title = element_blank(), plot.subtitle = element_text(hjust = 0.5), legend.position = "none") +
  labs(subtitle = "FCC")


## Plot FASI
dta_3 <- plt_compass_dta[plt_compass_dta$method=="FASI",]
fasi_plt <- ggplot(dta_3, aes(x=alpha, y=err_full_adj)) +
  geom_line(aes(color=Race, linetype=Race), size=1) +
  scale_color_manual(values=c("#009E73", "#0072B2", "#D55E00")) +
  scale_linetype_manual(values=c("solid", "solid", "solid")) +
  geom_point(aes(shape=Race, color=Race), size = 1.5) +
  coord_cartesian(xlim = c(0.15, 0.30)) +
  scale_x_continuous(breaks = c(0.1, 0.15, 0.2, 0.25, 0.3)) +
  theme_classic(base_size = 7) +
  xlab('$\\alpha$') +
  ylab('$\\widehat{FSR}^{2,a} - \\alpha$') +
  geom_hline(yintercept=0, linetype="dashed") +
  ylim(-0.06, 0.06) +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), plot.subtitle = element_text(hjust = 0.5), legend.position = "none") +
  labs(subtitle = "FASI")

########################################
########################################
num_indecision <- c(mult_alpha_err_mat$numselect_nb_classification_ci_Full_0 /
                      (mult_alpha_err_mat$numselect_nb_classification_ci_Full_1 +
                         mult_alpha_err_mat$numselect_nb_classification_ci_Full_2 +
                         mult_alpha_err_mat$numselect_nb_classification_ci_Full_0),
                    mult_alpha_err_mat$numselect_nb_classification_fasi_Full_0 /
                      (mult_alpha_err_mat$numselect_nb_classification_fasi_Full_0 +
                         mult_alpha_err_mat$numselect_nb_classification_fasi_Full_1 +
                         mult_alpha_err_mat$numselect_nb_classification_fasi_Full_2))


x_axis_alpha <- rep(alpha_grid, 2)
race <- rep("All Races", length(x_axis_alpha))
plt_indec_compass_dta2 <- cbind.data.frame(alpha=x_axis_alpha, Race=race, num_indecision)
plt_indec_compass_dta2$method <- c(rep("FCC", length(alpha_grid)), rep("FASI", length(alpha_grid)))

## add random races (only for asthetic purposes)
plt_indec_compass_dta_ggplt <- rbind.data.frame(plt_indec_compass_dta2,
                                                cbind.data.frame(alpha=rep(alpha_grid,2),
                                                                 Race=c(rep("Black", length(alpha_grid)),
                                                                        rep("Other", length(alpha_grid))),
                                                                 num_indecision=rep(2, 2*length(alpha_grid)),
                                                                 method = rep("FASI", length(alpha_grid))))

indecision_plts <- ggplot(plt_indec_compass_dta_ggplt, aes(x=alpha, y=num_indecision)) +
  geom_line(aes(color=Race, linetype=method), size=1) +
  geom_point(aes(shape=Race, color=Race), size = 1.5) +
  scale_color_manual(values=c("#009E73", "#0072B2", "#D55E00")) +
  scale_linetype_manual(values=c("solid", "dotdash")) +
  scale_x_continuous(breaks = c(0.1, 0.15, 0.2, 0.25, 0.3)) +
  theme_classic(base_size = 7) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.title = element_blank(), legend.position="bottom") +
  xlab('$\\alpha$') +
  ylab("$\\widehat{EPI}$") +
  coord_cartesian(xlim = c(0.15, 0.3)) +
  ylim(0,1) +
  labs(subtitle = "Indecisions")


## Final plot in paper ##
compas_plots_all_onerow <- ggarrange(raceadj_plt, fasi_plt, indecision_plts,
                                     ncol=3, nrow=1, common.legend = T, legend="bottom",
                                     legend.grob = get_legend(indecision_plts))

