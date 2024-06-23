## Source Function File (assumed to be in the same directory)
source("Appendix Figure 8 Function File.R")

require(ggpubr)
require(ggplot2)

## Plot 1, n_cal=1000, m=10
n.cal<-1000 # the size of the calibration data
m<-10 # the size of the test data


n.rep <- 1000 # number of replications
bscore <- 0.9 # a given base score

mean_vec <- c(2,2,2,1,1,1,2,2,2,1,1,1)
means <- matrix(mean_vec, ncol=4, nrow=3, byrow = F)
colnames(means) <- c("f_white_y2", "f_white_y1", "f_black_y2", "f_black_y1")
rownames(means) <- c("x1", "x2", "x3")

sds <- diag(3)
diag(sds) <- c(2,2,2)
rownames(sds) <- rownames(means)
colnames(sds) <- rownames(means)

pi=0.5
p=c(0.8,0.8)

## Number of cores for parallel processing
num_cores <- 8

##############################
## Figure 1: n_cal=1000, m=10
##############################

data_gen1 <- mclapply(1:n.rep, function(jj) {
  cal_data <- gen_obs(n.cal, pi, p, means, sds, oracle_indicator = T)
  test_data <- gen_obs(m, pi, p, means, sds, oracle_indicator = T)

  Y_cal <- cal_data$true_label
  scores_cal <- cal_data$s_sideinfo_c2
  scores_test <- test_data$s_sideinfo_c2

  return_object <- list(Y_cal=Y_cal, scores_cal=scores_cal, scores_test=scores_test)
  print(return_object)
  return(return_object)
}, mc.cores = num_cores, mc.set.seed = TRUE)

y_iters_list <- lapply(data_gen1, function(l) l[[1]])
y_cal_iters <- do.call("rbind", y_iters_list)

scores.cal_list <- lapply(data_gen1, function(l) l[[2]])
scores.cal <- do.call("rbind", scores.cal_list)

scores.test_list <- lapply(data_gen1, function(l) l[[3]])
scores.test <- do.call("rbind", scores.test_list)

RVs1<-rep(0, n.rep)
RPVs1<-rep(0, n.rep)

## Extract r-values that are very close to the base score
for (i in 1:n.rep)
{
  Y.cal <- y_cal_iters[i,]
  num<-length(which(Y.cal[which(scores.cal[i, ]>=bscore)]==1))/(n.cal+1)
  den.r<-max(1, length(which(scores.test[i, ]>=bscore)))/m
  den.rp<-(1+length(which(scores.test[i, ]>=bscore))+length(which(scores.cal[i, ]>=bscore)))/(n.cal+m+1)
  RVs1[i]<-num/den.r
  RPVs1[i]<-num/den.rp
}

##############################
## Figure 2: n_cal=1000, m=50
##############################

m<-50 # the size of the test data

data_gen2 <- mclapply(1:n.rep, function(jj) {
  cal_data <- gen_obs(n.cal, pi, p, means, sds, oracle_indicator = T)
  test_data <- gen_obs(m, pi, p, means, sds, oracle_indicator = T)

  Y_cal <- cal_data$true_label
  scores_cal <- cal_data$s_sideinfo_c2
  scores_test <- test_data$s_sideinfo_c2

  return_object <- list(Y_cal=Y_cal, scores_cal=scores_cal, scores_test=scores_test)
  return(return_object)
}, mc.cores = num_cores, mc.set.seed = TRUE)


y_iters_list <- lapply(data_gen2, function(l) l[[1]])
y_cal_iters <- do.call("rbind", y_iters_list)

scores.cal_list <- lapply(data_gen2, function(l) l[[2]])
scores.cal <- do.call("rbind", scores.cal_list)

scores.test_list <- lapply(data_gen2, function(l) l[[3]])
scores.test <- do.call("rbind", scores.test_list)


RVs2<-rep(0, n.rep)
RPVs2<-rep(0, n.rep)

for (i in 1:n.rep)
{
  Y.cal <- y_cal_iters[i,]
  num<-length(which(Y.cal[which(scores.cal[i, ]>=bscore)]==1))/(n.cal+1)
  den.r<-max(1, length(which(scores.test[i, ]>=bscore)))/m
  den.rp<-(1+length(which(scores.test[i, ]>=bscore))+length(which(scores.cal[i, ]>=bscore)))/(n.cal+m+1)
  RVs2[i]<-num/den.r
  RPVs2[i]<-num/den.rp
}


##############################
## Figure 3: n_cal=1000, m=200
##############################
m<-200

data_gen3 <- mclapply(1:n.rep, function(jj) {
  cal_data <- gen_obs(n.cal, pi, p, means, sds, oracle_indicator = T)
  test_data <- gen_obs(m, pi, p, means, sds, oracle_indicator = T)

  Y_cal <- cal_data$true_label
  scores_cal <- cal_data$s_sideinfo_c2
  scores_test <- test_data$s_sideinfo_c2

  return_object <- list(Y_cal=Y_cal, scores_cal=scores_cal, scores_test=scores_test)
  return(return_object)
}, mc.cores = num_cores, mc.set.seed = TRUE)


y_iters_list <- lapply(data_gen3, function(l) l[[1]])
y_cal_iters <- do.call("rbind", y_iters_list)

scores.cal_list <- lapply(data_gen3, function(l) l[[2]])
scores.cal <- do.call("rbind", scores.cal_list)

scores.test_list <- lapply(data_gen3, function(l) l[[3]])
scores.test <- do.call("rbind", scores.test_list)



RVs3<-rep(0, n.rep)
RPVs3<-rep(0, n.rep)

for (i in 1:n.rep)
{
  Y.cal <- y_cal_iters[i,]
  num<-length(which(Y.cal[which(scores.cal[i, ]>=bscore)]==1))/(n.cal+1)
  den.r<-max(1, length(which(scores.test[i, ]>=bscore)))/m
  den.rp<-(1+length(which(scores.test[i, ]>=bscore))+length(which(scores.cal[i, ]>=bscore)))/(n.cal+m+1)
  RVs3[i]<-num/den.r
  RPVs3[i]<-num/den.rp
}

##############################
## Figure 4: n_cal=1000, m=5
##############################

m<-5

data_gen4 <- mclapply(1:n.rep, function(jj) {
  cal_data <- gen_obs(n.cal, pi, p, means, sds, oracle_indicator = T)
  test_data <- gen_obs(m, pi, p, means, sds, oracle_indicator = T)

  Y_cal <- cal_data$true_label
  scores_cal <- cal_data$s_sideinfo_c2
  scores_test <- test_data$s_sideinfo_c2

  return_object <- list(Y_cal=Y_cal, scores_cal=scores_cal, scores_test=scores_test)
  print(return_object)
  return(return_object)
}, mc.cores = num_cores, mc.set.seed = TRUE)


y_iters_list <- lapply(data_gen4, function(l) l[[1]])
y_cal_iters <- do.call("rbind", y_iters_list)

scores.cal_list <- lapply(data_gen4, function(l) l[[2]])
scores.cal <- do.call("rbind", scores.cal_list)

scores.test_list <- lapply(data_gen4, function(l) l[[3]])
scores.test <- do.call("rbind", scores.test_list)


RVs4<-rep(0, n.rep)
RPVs4<-rep(0, n.rep)

for (i in 1:n.rep)
{
  Y.cal <- y_cal_iters[i,]
  num<-length(which(Y.cal[which(scores.cal[i, ]>=bscore)]==1))/(n.cal+1)
  den.r<-max(1, length(which(scores.test[i, ]>=bscore)))/m
  den.rp<-(1+length(which(scores.test[i, ]>=bscore))+length(which(scores.cal[i, ]>=bscore)))/(n.cal+m+1)
  RVs4[i]<-num/den.r
  RPVs4[i]<-num/den.rp
}

###############
#### Plots ####
###############
ylim1 <- 0
ylim2 <- 45
xlim1 <- 0
xlim2 <- 0.15


RVstemp <- RVs1
RVstemp <- cbind.data.frame(rval=as.numeric(RVstemp), type=rep("$R^\\chi$-value", length(RVstemp)))
RVstemp <- rbind.data.frame(rval=RVstemp, cbind.data.frame(rval=0, type="$R$-value"))
RVstemp$type <- factor(RVstemp$type, levels = c("$R^\\chi$-value", "$R$-value"))

plt1 <- ggplot(RVstemp, aes(x=rval, fill=type)) +
  geom_histogram(aes(y=..density..), colour="black", bins=50) + geom_density(alpha=.2, fill="#D55E00")  +
  scale_fill_manual(values=c("#D55E00", "#009E73")) +
  xlab("$R^\\chi$-value with $s(x,a)=0.9$") + ggtitle("$|D^{test}|=10$") + theme_bw(base_size = 7) + theme(plot.title = element_text(hjust = 0.5)) +
  xlim(xlim1,xlim2) + ylim(ylim1,ylim2) +  theme(legend.title = element_blank())
RPVs1 <- as.data.frame(RPVs1)
plt2 <- ggplot(RPVs1, aes(x=RPVs1)) +
  geom_histogram(color="black", fill="#009E73", bins=50, aes(y=..density..)) + xlab("$R$-value with $s(x,a)=0.9$") + geom_density(alpha=.4, fill="#009E73")  +
  ggtitle("$|D^{test}|=10$") + xlim(xlim1,xlim2) + ylim(ylim1,ylim2) + theme_bw(base_size = 7) + theme(plot.title = element_text(hjust = 0.5))

RVs2 <- as.data.frame(RVs2)
plt3 <- ggplot(RVs2, aes(x=RVs2)) +
  geom_histogram(color="black", fill="#D55E00", bins=50, aes(y=..density..)) + geom_density(alpha=.4, fill="#D55E00")  +
  xlab("$R^\\chi$-value with $s(x,a)=0.9$") + ggtitle("$|D^{test}|=50$")  + xlim(xlim1,xlim2) + ylim(ylim1,ylim2) + theme_bw(base_size = 7) + theme(plot.title = element_text(hjust = 0.5))
RPVs2 <- as.data.frame(RPVs2)
plt4 <- ggplot(RPVs2, aes(x=RPVs2)) +
  geom_histogram(color="black", fill="#009E73", bins=50,  aes(y=..density..)) +  geom_density(alpha=.4, fill="#009E73") +
  xlab("$R$-value with $s(x,a)=0.9$") + ggtitle("$|D^{test}|=50$") + xlim(xlim1,xlim2) + ylim(ylim1,ylim2) + theme_bw(base_size = 7) + theme(plot.title = element_text(hjust = 0.5))

RVs3 <- as.data.frame(RVs3)
plt5 <- ggplot(RVs3, aes(x=RVs3)) +
  geom_histogram(color="black", fill="#D55E00", bins=50, aes(y=..density..)) +  geom_density(alpha=.4, fill="#D55E00")  +
  xlab("$R^\\chi$-value with $s(x,a)=0.9$") + ggtitle("$|D^{test}|=200$") + xlim(xlim1,xlim2) + ylim(ylim1,ylim2) + theme_bw(base_size = 7) + theme(plot.title = element_text(hjust = 0.5))
RPVs3 <- as.data.frame(RPVs3)
plt6 <- ggplot(RPVs3, aes(x=RPVs3)) +
  geom_histogram(color="black", fill="#009E73", bins=50, aes(y=..density..)) + geom_density(alpha=.4, fill="#009E73") +
  xlab("$R$-value with $s(x,a)=0.9$") + ggtitle("$|D^{test}|=200$") + xlim(xlim1,xlim2) + ylim(ylim1,ylim2) + theme_bw(base_size = 7) + theme(plot.title = element_text(hjust = 0.5))

RPVs4 <- as.data.frame(RPVs4)
plt8 <- ggplot(RPVs4, aes(x=RPVs4)) +
  geom_histogram(color="black", fill="#009E73", bins=50, aes(y=..density..)) + geom_density(alpha=.4, fill="#009E73") +
  xlab("$R$-value with $s(x,a)=0.9$") + ggtitle("$|D^{test}|=5$") + xlim(xlim1,xlim2) + ylim(ylim1,ylim2) + theme_bw(base_size = 7) + theme(plot.title = element_text(hjust = 0.5))

RVstemp <- RVs4
RVstemp <- cbind.data.frame(rval=as.numeric(RVstemp), type=rep("$R^\\chi$-value", length(RVstemp)))
RVstemp <- rbind.data.frame(rval=RVstemp, cbind.data.frame(rval=0, type="$R^+$-value"))
RVstemp$type <- factor(RVstemp$type, levels = c("$R^\\chi$-value", "$R$-value"))
plt7 <- ggplot(RVstemp, aes(x=rval, fill=type)) +
  geom_histogram(aes(y=..density..), colour="black", bins=50) + geom_density(alpha=.2, fill="#D55E00")  +
  scale_fill_manual(values=c("#D55E00", "#009E73")) +
  xlab("$R^\\chi$-value with $s(x,a)=0.9$") + ggtitle("$|D^{test}|=5$") + theme_bw(base_size = 7) + theme(plot.title = element_text(hjust = 0.5)) +
  xlim(xlim1,xlim2) + ylim(ylim1,ylim2) +  theme(legend.title = element_blank())


## Final plot in the paper
r_score_compare_plt <- ggarrange(plt7, plt8, plt3, plt4, plt5, plt6, ncol=2, nrow=3,
                                 common.legend = TRUE, legend="bottom", legend.grob =  get_legend(plt1))
