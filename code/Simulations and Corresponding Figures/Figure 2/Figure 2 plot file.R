#####
## Main plot file for figure 2
####

p_iter <- seq(0.05, 0.95, by=0.05)

### Naive Method
avg_matrix2 <- cbind.data.frame(avg_matrix, p_iter)
plt_wide <- as_tibble(avg_matrix2[,grep("^FSR .*aci_class_umi|p_iter", colnames(avg_matrix2))])
plt_data <- gather(plt_wide, method_type, average_error, "FSR Protected Class Full Class 1 aci_class_umi":"FSR Protected Class 1 Class 2 aci_class_umi", factor_key=TRUE)

## Overall Plot Class 2
plt_dta_overall <- plt_data[grep("Protected Class Full Class 2|Protected Class 0 Class 2|Protected Class 1 Class 2", plt_data$method_type),]
plt_dta_overall$Race <- rep("Female", nrow(plt_dta_overall))
plt_dta_overall$Race[grep("Protected Class 1", plt_dta_overall$method_type)] <- "Male"
plt_dta_overall$Race[grep("Full", plt_dta_overall$method_type)] <- "Female and Male"
plt_dta_overall$Race <- factor(plt_dta_overall$Race, levels = c("Female", "Male", "Female and Male"))

intro_plt_class2_naive <- ggplot(plt_dta_overall, aes(x=p_iter, y=average_error)) +
  geom_line(aes(color=Race, linetype=Race), size=1) +
  ylim(0, 0.22) +
  xlim(0.15,0.85) +
  theme_classic(base_size = 7) +
  scale_color_manual(values=c("#009E73", "#0072B2", "#CC79A7")) +
  xlab("$p_F$") +
  ylab("") +
  theme(legend.position = "none", legend.title = element_blank(), plot.subtitle = element_text(hjust = 0.5)) +
  scale_linetype_manual(values=c("longdash", "twodash", "solid")) +
  labs(subtitle = "RCC")



## Race-Adjusted Method
avg_matrix2 <- cbind.data.frame(avg_matrix, p_iter)
plt_wide <- as_tibble(avg_matrix2[,grep("^FSR .*aci_class_ci|p_iter", colnames(avg_matrix2))])
plt_data <- gather(plt_wide, method_type, average_error, "FSR Protected Class Full Class 1 aci_class_ci":"FSR Protected Class 1 Class 2 aci_class_ci", factor_key=TRUE)

## Overall Plot Class 2
plt_dta_overall <- plt_data[grep("Protected Class Full Class 2|Protected Class 0 Class 2|Protected Class 1 Class 2", plt_data$method_type),]
plt_dta_overall$Race <- rep("Female", nrow(plt_dta_overall))
plt_dta_overall$Race[grep("Protected Class 1", plt_dta_overall$method_type)] <- "Male"
plt_dta_overall$Race[grep("Full", plt_dta_overall$method_type)] <- "Female and Male"
plt_dta_overall$Race <- factor(plt_dta_overall$Race, levels = c("Female", "Male", "Female and Male"))


intro_plt_class2_raceadj <- ggplot(plt_dta_overall, aes(x=p_iter, y=average_error)) +
  geom_line(aes(color=Race, linetype=Race), size=1) +
  ylim(0, 0.22) +
  xlim(0.15,0.85) +
  theme_classic(base_size = 7) +
  scale_color_manual(values=c("#009E73", "#0072B2", "#CC79A7")) +
  xlab("$p_F$") +
  ylab("False Selection Rate") +
  theme(legend.position = "none", legend.title = element_blank(), plot.subtitle = element_text(hjust = 0.5)) +
  scale_linetype_manual(values=c("longdash", "twodash", "solid")) +
  labs(subtitle = "FCC")



## FASI Adjustment
avg_matrix2 <- cbind.data.frame(avg_matrix, p_iter)
plt_wide <- as_tibble(avg_matrix2[,grep("^FSR .*aci_class_fasi|p_iter", colnames(avg_matrix2))])
plt_data <- gather(plt_wide, method_type, average_error, "FSR Protected Class Full Class 1 aci_class_fasi":"FSR Protected Class 1 Class 2 aci_class_fasi", factor_key=TRUE)

plt_dta_overall <- plt_data[grep("Protected Class Full Class 1|Protected Class 0 Class 1|Protected Class 1 Class 1", plt_data$method_type),]
plt_dta_overall$Race <- rep("Female", nrow(plt_dta_overall))
plt_dta_overall$Race[grep("Protected Class 1", plt_dta_overall$method_type)] <- "Male"
plt_dta_overall$Race[grep("Full", plt_dta_overall$method_type)] <- "Female and Male"
plt_dta_overall$Race <- factor(plt_dta_overall$Race, levels = c("Female", "Male", "Female and Male"))


intro_plt_class2_fasi <- ggplot(plt_dta_overall, aes(x=p_iter, y=average_error)) +
  geom_line(aes(color=Race, linetype=Race), size=1) +
  ylim(0, 0.22) +
  xlim(0.15,0.85) +
  theme_classic(base_size = 7) +
  scale_color_manual(values=c("#009E73", "#0072B2", "#CC79A7")) +
  xlab("$p_F$") +
  ylab("") +
  theme(legend.title = element_blank(), legend.position = c(.7, .85), plot.subtitle = element_text(hjust = 0.5)) +
  scale_linetype_manual(values=c("longdash", "twodash", "solid")) +
  labs(subtitle = "FASI")

## Overall
all_plots <- ggarrange(intro_plt_class2_raceadj, intro_plt_class2_naive, intro_plt_class2_fasi,  ncol=3, nrow=1, common.legend = TRUE, legend="bottom")


