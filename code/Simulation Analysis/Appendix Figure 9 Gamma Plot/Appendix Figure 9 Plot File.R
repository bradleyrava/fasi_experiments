## Gamma Investigation, Figure 9 - Plot File ##

## Load data from main file, assuming same directory
load("gamma_sim.rda")

avg_matrix2 <- as.data.frame(avg_matrix[,grep("FSR", colnames(avg_matrix))])
avg_matrix2 <- as.data.frame(avg_matrix2[,grep("[0-1] Class 1 |Full Class 1 ", colnames(avg_matrix2))])
avg_matrix2$p_iter <- p_iter
avg_matrix2$gamma_c0 <- avg_matrix[,1]
avg_matrix2$gamma_c1 <- avg_matrix[,2]
colnames(avg_matrix2) <- c("p_iter", "gamma_pt0", "gamma_pt1")
plot_matrix <- gather(avg_matrix2, gamma_ptclass, gamma, "gamma_pt0":"gamma_pt1", factor_key=TRUE)

protected_class <- c()
protected_class[grep("pt0", plot_matrix$gamma_ptclass)] <- "Female"
protected_class[grep("pt1", plot_matrix$gamma_ptclass)] <- "Male"
plot_matrix$protected_class <- protected_class


gamma_plt <- ggplot(plot_matrix, aes(x=p_iter, y=gamma)) +
  geom_line(size=1, aes(color=protected_class, linetype=protected_class)) +
  xlab('$\\pi_{2,F}$') +
  ylab("$\\gamma_{2,a}$") +
  ggtitle("Simulation 1") +
  theme_classic(base_size = 7) +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), plot.subtitle = element_text(hjust = 0.5)) +
  ylim(0.9,1.1) +
  xlim(0.15, 0.85) +
  labs(subtitle = " ") +
  geom_hline(yintercept=1, linetype="dotted",
             color = "black", size=0.5) +
  scale_color_manual(values = c("#009E73", "#D55E00")) +
  scale_linetype_manual(values=c("solid", "twodash"))


## Final plot in the paper
plt_sim1 <- gamma_plt
gamma_both_sims_plt <- ggarrange(plt_sim1, plt_sim2,
                                 ncol=2, nrow=1, common.legend = T, legend="bottom")
