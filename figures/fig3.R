library(factorcopula)
library(parallel)
library(cheopsr)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(ggrepel)

detectOutlier <- function(y, k){
  yDiff <- abs(diff(y))
  which(yDiff > mean(yDiff) + k*sd(yDiff))
}

options(scipen = 9999)

#devtools::install_github("bonartm/cheopsr")
options(cheopsr.account = "AG-Wied")
options(cheopsr.username = "bonartm")
#cheops_install_github("bonartm/factorcopula", ref = "dev")

tSeq <- 300:1500
t <- max(tSeq)
brk <- floor(2/3*t)
N <- 21
k <- rep(1:3, each = 7)
beta <- config_beta(k)
M <- max(k)
P <- 2*M

theta0 <- c(c(0, 1, 1), c(0, 1, 1))
theta1 <- c(c(1.5, 1, 1), c(1.5, 1, 1))
lower <- c(rep(0, P))
upper <- c(rep(6, P))
names(theta0) <- paste0("beta", 1:P)
names(theta1) <- paste0("beta", 1:P)
names(lower) <- paste0("beta", 1:P)
names(upper) <- paste0("beta", 1:P)

Z <- config_factor(rst = list(nu = 1/0.25, lambda = -0.8), rt = list(df = 1/0.25), rt = list(df = 1/0.25), rt = list(df = 1/0.25))
eps <- config_error(rt = list(df = 1/0.25))
cop <- fc_create(Z, eps, beta)

Y <- readRDS("./Y.rds")
#Y <- qnorm(rbind(cop(theta0, brk), cop(theta1, t-brk)))
U <- apply(Y, 2, factorcopula:::empDist)
matrix(factorcopula:::moments(U[1:brk, ], k), ncol = 5, byrow = TRUE)
matrix(factorcopula:::moments(U[(brk+1):t, ], k), ncol = 5, byrow = TRUE)

#saveRDS(Y, "./Y.rds")

# Copula recursive estimation ---------------------------------------------
opt <- cheops_slurmcontrol(nodes = 80, tasks = 5, mem = "3gb", time = "08:00:00", mail = "bonartm@uni-koeln.de")
#opt <- cheops_slurmcontrol(nodes = 2, tasks = 2, mem = "1gb", time = "00:10:00", partition = "devel")

# job_bloc <- cheops_lapply(tSeq, function(t, Y, Z, eps, beta, lower, upper, k){
#   fc_fit(Y = Y[1:t, ], Z, eps, beta, lower, upper, recursive = FALSE, S = 25000, k = k, cl = NULL, trials = 10,
#          control = list(stopval = 0, xtol_rel = 1e-9, maxeval = 3000))
# }, options = opt, jobname = "bloc-equ", packages = "factorcopula", load.balancing = TRUE,
# args = list(Y = Y, Z = Z, eps = eps, beta = beta, lower = lower, upper = upper, k = k))

cheops_jobs()
cat(cheops_getlog("bloc-equ"), sep = "\n")
#cheops_cancel(job_bloc$name)

res <- cheops_readRDS("./bloc-equ/res.rds")
res <- data.frame(do.call(rbind, res))
res$p <- fc_pstat(res[,1:6], res$t)

out <- detectOutlier(res$p, 3)
res$p[out] <- NA

res$pSecond <- fc_pstat(res[,c("beta2", "beta5", "beta3", "beta6")], res$t)

# opt <- cheops_slurmcontrol(nodes = 40, tasks = 2, mem = "2gb", time = "00:20:00")
# job_crit <- cheops_run(fc_critval, options = opt, jobname = "cop-crit",
#                        args = list(type = "copula", Y = Y, B = 1000, tSeq = tSeq, k = k, 
#                                    config_factor = Z, config_error = eps, config_beta = beta, theta = res[nrow(res), 1:6]),
#                        packages = "factorcopula")
# cheops_jobs()
# cat(cheops_getlog("cop-crit"), sep = "\n")
pCrit <- cheops_readRDS("./cop-crit/res.rds")

res <- tidyr::gather(res[, c("t", "p", "pSecond")], key = "group", value = "P", p, pSecond)

ggplot(res, aes(x = t, y = P)) +
  geom_hline(yintercept = quantile(pCrit, 1-0.05), color = "black", linetype = 2) +
  #geom_label_repel(stat = "unique", aes(x = max(t)), y = quantile(pCrit, 1-0.05), label = "critical value", point.padding = 1, nudge_y = 20) +
  #geom_label_repel(stat = "unique", x = brk, y = 10, label = "theoretical breakpoint", point.padding = 1, nudge_x = - 2) +
  geom_line(color = "indianred") +
  geom_vline(xintercept = brk, color = "black", linetype = 2) +
  facet_grid(~ group, labeller = as_labeller(c(p = "all groups", pSecond = "second and third group"))) +
  theme_hc()




# Moments based test ------------------------------------------------------

cl <- makeCluster(4)
mStats <- fc_mstat(Y, tSeq, k, cl)
stopCluster(cl)

# opt <- cheops_slurmcontrol(nodes = 40, tasks = 2, mem = "2gb", time = "00:20:00")
# job_crit <- cheops_run(fc_critval, options = opt, jobname = "mom-crit",
#                        args = list(type = "moments", Y = Y, B = 1000, tSeq = tSeq, k = k),
#                        packages = "factorcopula")
# 
# cheops_jobs()
# #cheops_cancel(job_crit$name)
# cat(cheops_getlog("mom-crit"), sep = "\n")
mCrit <- cheops_readRDS("./mom-crit/res.rds")


result <- data.frame(t = tSeq, M = mStats)

result$labelM[result$t == brk] <- "theoretical breakpoint"

ggplot(result, aes(x = t, y = M, label = labelM)) +
  geom_hline(yintercept = quantile(mCrit, 1-0.05), color = "black", linetype = 2) +
  geom_label_repel(stat = "unique", aes(x = max(t)), y = quantile(mCrit, 1-0.05), label = "critival value", point.padding = 1, nudge_y = 1) +
  geom_line(color = "indianred3") +
  geom_label_repel(na.rm = TRUE, box.padding = 0.3, point.padding = 1, alpha = 0.8, nudge_x = 100) +
  geom_vline(xintercept = brk, color = "black", linetype = 2) +
  theme_hc()



