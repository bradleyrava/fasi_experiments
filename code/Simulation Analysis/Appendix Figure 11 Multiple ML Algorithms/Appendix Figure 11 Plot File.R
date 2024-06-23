## Multiple ML Models Plot File ##

## Load data from main file
load("appendix_mult_ml.rda")

### Logit Plot
avg_mat_logit <- avg_matrix[,grep("Logit", colnames(avg_matrix))]
avg_mat_logit_fsr <- avg_mat_logit[,grep("FSR", colnames(avg_mat_logit))]
avg_mat_logit_fsr_c2 <- cbind.data.frame(avg_mat_logit_fsr[,grep("Class 2", colnames(avg_mat_logit_fsr))], p_iter)
fsr_logit_plt_dta <- gather(avg_mat_logit_fsr_c2, protected_group, FSR, "Logit FSR Protected Class Full Class 2":"Logit FSR Protected Class 1 Class 2", factor_key=TRUE)
protected_group <- c()
protected_group[grep("Protected Class 1", fsr_logit_plt_dta$protected_group)] <- "Male"
protected_group[grep("Protected Class 0", fsr_logit_plt_dta$protected_group)] <- "Female"
protected_group[grep("Protected Class Full", fsr_logit_plt_dta$protected_group)] <- "Female and Male"
fsr_logit_plt_dta$protected_group <- protected_group

logit_fsr <- ggplot(fsr_logit_plt_dta, aes(x=p_iter, y=FSR)) +
  geom_line(aes(color = protected_group, linetype = protected_group)) +
  theme_minimal() +
  scale_color_manual(values=c('#56B4E9','#009E73', "#D55E00")) +
  xlab("$\\pi_{2|F}$") +
  ylab("$FSR^{2}_a$") +
  geom_hline(yintercept=0.1, linetype="dashed", color = "black") +
  ylim(0.08,0.12) +
  theme_classic(base_size = 7) +
  scale_linetype_manual(values=c("dashed", "solid", "dotdash")) +
  theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5)) +
  ggtitle("Logistic Regression") +
  ylim(0.08,0.12)



avg_matrix_uncomb <- as.data.frame(avg_matrix[,grep("Number of Decisions for Class 0", colnames(avg_matrix))])
avg_matrix_uncomb <- as.data.frame(avg_matrix_uncomb[,grep("Logit Number", colnames(avg_matrix_uncomb))])
avg_matrix2_logit <- cbind.data.frame(num_indecisions=avg_matrix_uncomb$`Logit Number of Decisions for Class 0 Protected Group 0`+
                                  avg_matrix_uncomb$`Logit Number of Decisions for Class 0 Protected Group 1`,
                                p_iter=p_iter)
avg_matrix2_logit$EPI <- avg_matrix2_logit$num_indecisions / 1000

epi_logit_plt_dta <- avg_matrix2_logit

logit_epi <- ggplot(epi_logit_plt_dta, aes(x=p_iter, y=EPI)) +
  geom_line(color='#009E73') +
  theme_minimal() +
  xlab("$\\pi_{2|F}$") +
  ylab("EPI") +
  theme_classic(base_size = 7) +
  theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5)) +
  ggtitle("Logistic Regression") +
  ylim(0,0.25)

ggarrange(logit_fsr, logit_epi, common.legend = T)

### GAM Plot
avg_mat_gam <- avg_matrix[,grep("GAM", colnames(avg_matrix))]
avg_mat_gam_fsr <- avg_mat_gam[,grep("FSR", colnames(avg_mat_gam))]
avg_mat_gam_fsr_c2 <- cbind.data.frame(avg_mat_gam_fsr[,grep("Class 2", colnames(avg_mat_gam_fsr))], p_iter)
fsr_gam_plt_dta <- gather(avg_mat_gam_fsr_c2, protected_group, FSR, "GAM FSR Protected Class Full Class 2":"GAM FSR Protected Class 1 Class 2", factor_key=TRUE)
protected_group <- c()
protected_group[grep("Protected Class 1", fsr_gam_plt_dta$protected_group)] <- "Male"
protected_group[grep("Protected Class 0", fsr_gam_plt_dta$protected_group)] <- "Female"
protected_group[grep("Protected Class Full", fsr_gam_plt_dta$protected_group)] <- "Female and Male"
fsr_gam_plt_dta$protected_group <- protected_group

gam_fsr <- ggplot(fsr_gam_plt_dta, aes(x=p_iter, y=FSR)) +
  geom_line(aes(color = protected_group, linetype = protected_group)) +
  theme_minimal() +
  scale_color_manual(values=c('#56B4E9','#009E73', "#D55E00")) +
  xlab("$\\pi_{2|F}$") +
  ylab("$FSR^{2}_a$") +
  geom_hline(yintercept=0.1, linetype="dashed", color = "black") +
  ylim(0.08,0.12) +
  theme_classic(base_size = 7) +
  scale_linetype_manual(values=c("dashed", "solid", "dotdash")) +
  theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5)) +
  ggtitle("GAM")+
  ylim(0.08,0.12)


avg_matrix_uncomb <- as.data.frame(avg_matrix[,grep("Number of Decisions for Class 0", colnames(avg_matrix))])
avg_matrix_uncomb <- as.data.frame(avg_matrix_uncomb[,grep("GAM Number", colnames(avg_matrix_uncomb))])
avg_matrix2_gam <- cbind.data.frame(num_indecisions=avg_matrix_uncomb$`GAM Number of Decisions for Class 0 Protected Group 0`+
                                        avg_matrix_uncomb$`GAM Number of Decisions for Class 0 Protected Group 1`,
                                      p_iter=p_iter)
avg_matrix2_gam$EPI <- avg_matrix2_gam$num_indecisions / 1000

epi_gam_plt_dta <- avg_matrix2_gam


gam_epi <- ggplot(epi_gam_plt_dta, aes(x=p_iter, y=EPI)) +
  geom_line(color='#009E73') +
  theme_minimal() +
  xlab("$\\pi_{2|F}$") +
  ylab("EPI") +
  theme_classic(base_size = 7) +
  theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5)) +
  ggtitle("GAM") +
  ylim(0,0.25)

ggarrange(gam_fsr, gam_epi, common.legend = T)

### XG Boost Plot
avg_mat_xg <- avg_matrix[,grep("XG", colnames(avg_matrix))]
avg_mat_xg_fsr <- avg_mat_xg[,grep("FSR", colnames(avg_mat_xg))]
avg_mat_xg_fsr_c2 <- cbind.data.frame(avg_mat_xg_fsr[,grep("Class 2", colnames(avg_mat_xg_fsr))], p_iter)
fsr_xg_plt_dta <- gather(avg_mat_xg_fsr_c2, protected_group, FSR, "XG Boost FSR Protected Class Full Class 2":"XG Boost FSR Protected Class 1 Class 2", factor_key=TRUE)
protected_group <- c()
protected_group[grep("Protected Class 1", fsr_xg_plt_dta$protected_group)] <- "Male"
protected_group[grep("Protected Class 0", fsr_xg_plt_dta$protected_group)] <- "Female"
protected_group[grep("Protected Class Full", fsr_xg_plt_dta$protected_group)] <- "Female and Male"
fsr_xg_plt_dta$protected_group <- protected_group

xg_fsr <- ggplot(fsr_xg_plt_dta, aes(x=p_iter, y=FSR)) +
  geom_line(aes(color = protected_group, linetype = protected_group)) +
  theme_minimal() +
  scale_color_manual(values=c('#56B4E9','#009E73', "#D55E00")) +
  xlab("$\\pi_{2|F}$") +
  ylab("$FSR^{2}_a$") +
  geom_hline(yintercept=0.1, linetype="dashed", color = "black") +
  ylim(0.08,0.12) +
  theme_classic(base_size = 7) +
  scale_linetype_manual(values=c("dashed", "solid", "dotdash")) +
  theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5)) +
  ggtitle("XG Boost")+
  ylim(0.08,0.12)


avg_matrix_uncomb <- as.data.frame(avg_matrix[,grep("Number of Decisions for Class 0", colnames(avg_matrix))])
avg_matrix_uncomb <- as.data.frame(avg_matrix_uncomb[,grep("XG Boost Number", colnames(avg_matrix_uncomb))])
avg_matrix2_xg <- cbind.data.frame(num_indecisions=avg_matrix_uncomb$`XG Boost Number of Decisions for Class 0 Protected Group 0`+
                                      avg_matrix_uncomb$`XG Boost Number of Decisions for Class 0 Protected Group 1`,
                                    p_iter=p_iter)
avg_matrix2_xg$EPI <- avg_matrix2_xg$num_indecisions / 1000

epi_xg_plt_dta <- avg_matrix2_xg

xg_epi <- ggplot(epi_xg_plt_dta, aes(x=p_iter, y=EPI)) +
  geom_line(color='#009E73') +
  theme_minimal() +
  xlab("$\\pi_{2|F}$") +
  ylab("EPI") +
  theme_classic(base_size = 7) +
  theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5)) +
  ggtitle("XG Boost") +
  ylim(0,0.25)

ggarrange(xg_fsr, xg_epi, common.legend = T)

### NB Plot
avg_mat_nb <- avg_matrix[,grep("NB", colnames(avg_matrix))]
avg_mat_nb_fsr <- avg_mat_nb[,grep("FSR", colnames(avg_mat_nb))]
avg_mat_nb_fsr_c2 <- cbind.data.frame(avg_mat_nb_fsr[,grep("Class 2", colnames(avg_mat_nb_fsr))], p_iter)
fsr_nb_plt_dta <- gather(avg_mat_nb_fsr_c2, protected_group, FSR, "NB FSR Protected Class Full Class 2":"NB FSR Protected Class 1 Class 2", factor_key=TRUE)
protected_group <- c()
protected_group[grep("Protected Class 1", fsr_nb_plt_dta$protected_group)] <- "Male"
protected_group[grep("Protected Class 0", fsr_nb_plt_dta$protected_group)] <- "Female"
protected_group[grep("Protected Class Full", fsr_nb_plt_dta$protected_group)] <- "Female and Male"
fsr_nb_plt_dta$protected_group <- protected_group

nb_fsr <- ggplot(fsr_nb_plt_dta, aes(x=p_iter, y=FSR)) +
  geom_line(aes(color = protected_group, linetype = protected_group)) +
  theme_minimal(base_size = 7) +
  scale_color_manual(values=c('#56B4E9','#009E73', "#D55E00")) +
  xlab("$\\pi_{2|F}$") +
  ylab("$FSR^{2}_a$") +
  geom_hline(yintercept=0.1, linetype="dashed", color = "black") +
  ylim(0.08,0.15) +
  theme_classic(base_size = 7) +
  scale_linetype_manual(values=c("dashed", "solid", "dotdash")) +
  theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5)) +
  ggtitle("Nonparametric Naive Bayes")+
  ylim(0.08,0.12)


avg_matrix_uncomb <- as.data.frame(avg_matrix[,grep("Number of Decisions for Class 0", colnames(avg_matrix))])
avg_matrix_uncomb <- as.data.frame(avg_matrix_uncomb[,grep("NB Number", colnames(avg_matrix_uncomb))])
avg_matrix2_nb <- cbind.data.frame(num_indecisions=avg_matrix_uncomb$`NB Number of Decisions for Class 0 Protected Group 0`+
                                     avg_matrix_uncomb$`NB Number of Decisions for Class 0 Protected Group 1`,
                                   p_iter=p_iter)
avg_matrix2_nb$EPI <- avg_matrix2_nb$num_indecisions / 1000

epi_nb_plt_dta <- avg_matrix2_nb



nb_epi <- ggplot(epi_nb_plt_dta, aes(x=p_iter, y=EPI)) +
  geom_line(color='#009E73') +
  theme_minimal(base_size = 7) +
  xlab("$\\pi_{2|F}$") +
  ylab("EPI") +
  theme_classic(base_size = 7) +
  theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5)) +
  ggtitle("Nonparametric Naive Bayes") +
  ylim(0,0.25)


## Final plot in the paper
multiple_models <- ggarrange(logit_fsr, logit_epi,
                             gam_fsr, gam_epi,
                             nb_fsr, nb_epi,
                             xg_fsr, xg_epi,
                             common.legend = T,
                             legend = "bottom",
                             ncol = 2, nrow=4)
