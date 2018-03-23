library(factorcopula)
library(parallel)
library(cheopsr)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(ggrepel)


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
matrix(factorcopula:::moments(U[1:brk, ], k), ncol = 5)
matrix(factorcopula:::moments(U[(brk+1):t, ], k), ncol = 5)
matrix(factorcopula:::moments(U, k), ncol = 5)

#saveRDS(Y, "./Y.rds")

# Copula recursive estimation ---------------------------------------------
opt <- cheops_slurmcontrol(nodes = 60, tasks = 4, mem = "2gb", time = "03:00:00", mail = "bonartm@uni-koeln.de")
job_bloc <- cheops_run(fc_fit, opt, "bloc-equ",
           args = list(Y = Y, copFun = cop, lower = lower, upper = upper, k = k, recursive = TRUE,
                       control = list(stopval = 0, xtol_rel = 1e-13, maxeval = 3000), S = 25000), packages = "factorcopula")

cheops_jobs()
cat(cheops_getlog("bloc-equ"), sep = "\n")
#cheops_cancel(job_bloc$name)

res <- cheops_readRDS("./bloc-equ/res.rds")

pStats <- fc_pstat(res[-1], res$t)
#pCrit <- fc_critval("copula", Y, 1000, tSeq, cop, res[nrow(res), ], k = k)
pCrit <- 17

result <- data.frame(t = tSeq, P = pStats)
result$labelP[result$t == brk] <- "theoretical breakpoint"
result$labelP[which.max(result$P)] <- "observed breakpoint"
ggplot(result, aes(x = t, y = P, label = labelP)) +
  geom_hline(yintercept = quantile(pCrit, 1-0.05), color = "black", linetype = 2) +
  geom_label_repel(stat = "unique", aes(x = max(t)), y = quantile(pCrit, 1-0.05), label = "critival value", point.padding = 1, nudge_y = 1) +
  geom_line(color = "indianred3") +
  geom_label_repel(na.rm = TRUE, box.padding = 0.3, point.padding = 1, alpha = 0.8) +
  geom_vline(xintercept = brk, color = "gray", linetype = 2) +
  geom_smooth() +
  theme_hc()



# Moments based test ------------------------------------------------------

cl <- makeCluster(4)
mStats <- fc_mstat(Y, tSeq, k, cl)
stopCluster(cl)

opt <- cheops_slurmcontrol(nodes = 40, tasks = 4, mem = "2gb", time = "00:20:00")
job_crit <- cheops_run(fc_critval, options = opt, jobname = "mom-crit",
                       args = list(type = "moments", Y = Y, B = 1000, tSeq = tSeq, k = k),
                       packages = "factorcopula")

cheops_jobs()
#cheops_cancel(job_crit$name)
cat(cheops_getlog(job_crit$name), sep = "\n")
mCrit <- cheops_readRDS(job_crit$results)


result <- data.frame(t = tSeq, M = mStats, P = pStats)

result$labelM[result$t == brk] <- "theoretical breakpoint"
result$labelM[which.max(result$M)] <- "observed breakpoint"

#result$labelP[result$t == brk] <- "Real breakpoint"
#result$labelP[which.max(result$P)] <- "Estimated breakpoint"

ggplot(result, aes(x = t, y = M, label = labelM)) +
  geom_hline(yintercept = quantile(mCrit, 1-0.05), color = "black", linetype = 2) +
  geom_label_repel(stat = "unique", aes(x = max(t)), y = quantile(mCrit, 1-0.05), label = "critival value", point.padding = 1, nudge_y = 1) +
  geom_line(color = "indianred3") +
  geom_label_repel(na.rm = TRUE, box.padding = 0.3, point.padding = 1, alpha = 0.8) +
  geom_vline(xintercept = brk, color = "gray", linetype = 2) +
  theme_hc()




