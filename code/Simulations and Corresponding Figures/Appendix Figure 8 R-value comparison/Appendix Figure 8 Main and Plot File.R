library(ggplot2)
library(dplyr)
library(tibble)
library(tidyr)
library(fastAdaboost)
library(xgboost)
library(mvtnorm)
library(MASS)
library(parallel)
library(gridExtra)

#### Testing out different R-score formulations ####
#############################################
#### Parameters used for all simulations ####
#############################################

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
p_iter <- seq(0.05, 0.95, by=0.05)


set.seed(1)

## Situation 1: n_train=n_test=1000

comparison1_testdenom <- sim_function(nits=nits, n_train=1000, n_test=1000, pi=pi, p=p, means=means, sds=sds, alpha_param=alpha_param, p_iter=p_iter, rscore_bothdenom=F, num_cores=8)
comparison1_bothdenom <- sim_function(nits=nits, n_train=1000, n_test=1000, pi=pi, p=p, means=means, sds=sds, alpha_param=alpha_param, p_iter=p_iter, rscore_bothdenom=T, num_cores=8)


## Situation 2: n_train=1000, n_test=500

comparison2_testdenom <- sim_function(nits=nits, n_train=1000, n_test=500, pi=pi, p=p, means=means, sds=sds, alpha_param=alpha_param, p_iter=p_iter, rscore_bothdenom=F, num_cores=8)
comparison2_bothdenom <- sim_function(nits=nits, n_train=1000, n_test=500, pi=pi, p=p, means=means, sds=sds, alpha_param=alpha_param, p_iter=p_iter, rscore_bothdenom=T, num_cores=8)

## Situation 3: n_train=1000, n_test=100

comparison3_testdenom <- sim_function(nits=nits, n_train=1000, n_test=100, pi=pi, p=p, means=means, sds=sds, alpha_param=alpha_param, p_iter=p_iter, rscore_bothdenom=F, num_cores=8)
comparison3_bothdenom <- sim_function(nits=nits, n_train=1000, n_test=100, pi=pi, p=p, means=means, sds=sds, alpha_param=alpha_param, p_iter=p_iter, rscore_bothdenom=T, num_cores=8)

## Situation 4: n_train=1000, n_test=50

comparison4_testdenom <- sim_function(nits=nits, n_train=1000, n_test=50, pi=pi, p=p, means=means, sds=sds, alpha_param=alpha_param, p_iter=p_iter, rscore_bothdenom=F, num_cores=8)
comparison4_bothdenom <- sim_function(nits=nits, n_train=1000, n_test=50, pi=pi, p=p, means=means, sds=sds, alpha_param=alpha_param, p_iter=p_iter, rscore_bothdenom=T, num_cores=8)


## Situation 5: n_train=1000, n_test=20

comparison5_testdenom <- sim_function(nits=nits, n_train=1000, n_test=25, pi=pi, p=p, means=means, sds=sds, alpha_param=alpha_param, p_iter=p_iter, rscore_bothdenom=F, num_cores=8)
comparison5_bothdenom <- sim_function(nits=nits, n_train=1000, n_test=25, pi=pi, p=p, means=means, sds=sds, alpha_param=alpha_param, p_iter=p_iter, rscore_bothdenom=T, num_cores=8)

## Situation 6: n_train=1000, n_test=10

comparison6_testdenom <- sim_function(nits=nits, n_train=1000, n_test=10, pi=pi, p=p, means=means, sds=sds, alpha_param=alpha_param, p_iter=p_iter, rscore_bothdenom=F, num_cores=8)
comparison6_bothdenom <- sim_function(nits=nits, n_train=1000, n_test=10, pi=pi, p=p, means=means, sds=sds, alpha_param=alpha_param, p_iter=p_iter, rscore_bothdenom=T, num_cores=8)



## Plots
comparison1_testdenom_plt <- rscore_plt_gen(comparison1_testdenom, p_iter, title="Test in Denom: n=1000, m=1000")
comparison1_bothdenom_plt <- rscore_plt_gen(comparison1_bothdenom, p_iter, title="Both in Denom: n=1000, m=1000")

comparison2_testdenom_plt <- rscore_plt_gen(comparison2_testdenom, p_iter, title="Test in Denom: n=1000, m=500")
comparison2_bothdenom_plt <- rscore_plt_gen(comparison2_bothdenom, p_iter, title="Both in Denom: n=1000, m=500")

comparison3_testdenom_plt <- rscore_plt_gen(comparison3_testdenom, p_iter, title="Test in Denom: n=1000, m=100")
comparison3_bothdenom_plt <- rscore_plt_gen(comparison3_bothdenom, p_iter, title="Both in Denom: n=1000, m=100")

comparison4_testdenom_plt <- rscore_plt_gen(comparison4_testdenom, p_iter, title="Test in Denom: n=1000, m=50")
comparison4_bothdenom_plt <- rscore_plt_gen(comparison4_bothdenom, p_iter, title="Both in Denom: n=1000, m=50")

comparison5_testdenom_plt <- rscore_plt_gen(comparison5_testdenom, p_iter, title="Test in Denom: n=1000, m=25")
comparison5_bothdenom_plt <- rscore_plt_gen(comparison5_bothdenom, p_iter, title="Both in Denom: n=1000, m=25")

comparison6_testdenom_plt <- rscore_plt_gen(comparison5_testdenom, p_iter, title="Test in Denom: n=1000, m=10")
comparison6_bothdenom_plt <- rscore_plt_gen(comparison5_bothdenom, p_iter, title="Both in Denom: n=1000, m=10")

## New plots due to corruption in save
comparison1_testdenom_plt <- plot_gen_new(comparison1_testdenom_plt$data, title="$R$-score: n=1000, m=1000")
comparison1_bothdenom_plt <- plot_gen_new(comparison1_bothdenom_plt$data, title="$R^{+}$-score: n=1000, m=1000")

comparison2_testdenom_plt <- plot_gen_new(comparison2_testdenom_plt$data, title="$R$-score: n=1000, m=500")
comparison2_bothdenom_plt <- plot_gen_new(comparison2_bothdenom_plt$data, title="$R^{+}$-score: n=1000, m=500")

comparison3_testdenom_plt <- plot_gen_new(comparison3_testdenom_plt$data, title="$R$-score: n=1000, m=100")
comparison3_bothdenom_plt <- plot_gen_new(comparison3_bothdenom_plt$data, title="$R^{+}$-score: n=1000, m=100")

comparison4_testdenom_plt <- plot_gen_new(comparison4_testdenom_plt$data, title="$R$-score: n=1000, m=50")
comparison4_bothdenom_plt <- plot_gen_new(comparison4_bothdenom_plt$data, title="$R^{+}$-score: n=1000, m=50")

comparison5_testdenom_plt <- plot_gen_new(comparison5_testdenom_plt$data, title="$R$-score: n=1000, m=25")
comparison5_bothdenom_plt <- plot_gen_new(comparison5_bothdenom_plt$data, title="$R^{+}$-score: n=1000, m=25")

comparison6_testdenom_plt <- plot_gen_new(comparison6_testdenom_plt$data, title="$R$-score: n=1000, m=10")
comparison6_bothdenom_plt <- plot_gen_new(comparison6_bothdenom_plt$data, title="$R^{+}$-score: n=1000, m=10")

plot(arrangeGrob(comparison1_testdenom_plt, comparison1_bothdenom_plt,
                 comparison3_testdenom_plt, comparison3_bothdenom_plt,
                 comparison4_testdenom_plt, comparison4_bothdenom_plt))

r_score_compare_plt <- plot(arrangeGrob(comparison1_testdenom_plt, comparison4_bothdenom_plt,
                 comparison6_testdenom_plt, comparison6_bothdenom_plt))

all_plots <- ggarrange(comparison1_testdenom_plt, comparison1_bothdenom_plt,
                       comparison3_testdenom_plt, comparison3_bothdenom_plt,
                       comparison4_testdenom_plt, comparison4_bothdenom_plt,
                       ncol=2, nrow=3, common.legend = TRUE, legend="bottom")


save(comparison1_testdenom_plt, comparison1_bothdenom_plt,
     comparison2_testdenom_plt, comparison2_bothdenom_plt,
     comparison3_testdenom_plt, comparison3_bothdenom_plt,
     comparison4_testdenom_plt, comparison4_bothdenom_plt,
     comparison5_testdenom_plt, comparison5_bothdenom_plt,
     comparison6_testdenom_plt, comparison6_bothdenom_plt,
     file = "r_score_formula_test_scenarios.RData")





