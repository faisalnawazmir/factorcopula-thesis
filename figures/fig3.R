library(factorcopula)
library(parallel)
library(cheopsr)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(ggrepel)

detectOutlier <- function(y, k){
  ySmooth <- smooth(y)
  yDiff <- abs(y - ySmooth)
  which(yDiff > mean(yDiff) + k*sd(yDiff))
}

options(scipen = 9999)

#devtools::install_github("bonartm/cheopsr")
#devtools::install_github("bonartm/factorcopula")
options(cheopsr.account = "AG-Wied")
options(cheopsr.username = "bonartm")
#cheops_install_github("bonartm/factorcopula", ref = "master")

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

Y <- readRDS("./figures/Y.rds")
#Y <- qnorm(rbind(cop(theta0, brk), cop(theta1, t-brk)))
U <- apply(Y, 2, factorcopula:::empDist)
matrix(factorcopula:::moments(U[1:brk, ], k), ncol = 5, byrow = TRUE)
matrix(factorcopula:::moments(U[(brk+1):t, ], k), ncol = 5, byrow = TRUE)

#saveRDS(Y, "./Y.rds")

# Full Copula recursive estimation ---------------------------------------------
opt <- cheops_slurmcontrol(nodes = 60, tasks = 4, mem = "8gb", time = "06:00:00")
#opt <- cheops_slurmcontrol(nodes = 2, tasks = 2, mem = "1gb", time = "00:10:00", partition = "devel")

job_bloc <- cheops_lapply(tSeq, function(t, Y, Z, eps, beta, lower, upper, k){
  fc_fit(Y = Y[1:t, ], Z, eps, beta, lower, upper, S = 37500, k = k, 
         control.first.stage = list(algorithm = "NLOPT_GN_MLSL_LDS", stopval = 0, xtol_rel = 1e-10, maxeval = 2000,
                                    local_opts = list(algorithm = "NLOPT_LN_SBPLX", xtol_rel = 1e-10, maxeval = 2000)), 
         control.second.stage = list(algorithm = "NLOPT_LN_SBPLX", xtol_rel = 1e-16, maxeval = 2000))
}, options = opt, jobname = "bloc-equ", packages = "factorcopula", load.balancing = TRUE,
args = list(Y = Y, Z = Z, eps = eps, beta = beta, lower = lower, upper = upper, k = k))

cheops_jobs()
cat(cheops_getlog("bloc-equ"), sep = "\n")
#cheops_cancel("bloc-equ")

res <- cheops_readRDS("./bloc-equ/res.rds")
res <- data.frame(t(vapply(res, function(x) x$theta.second.stage[1:6], numeric(6))))
names(res) <- names(lower)
res$t <- tSeq
res$p <- fc_pstat(res[,1:6], res$t)

out <- detectOutlier(res$p, 1)
res$p[out] <- NA

# opt <- cheops_slurmcontrol(nodes = 40, tasks = 2, mem = "2gb", time = "00:20:00")
# job_crit <- cheops_run(fc_critval, options = opt, jobname = "bloc-crt",
#                        args = list(type = "copula", Y = Y, B = 1000, tSeq = tSeq, k = k, 
#                                    config_factor = Z, config_error = eps, config_beta = beta, theta = res[nrow(res), 1:6]),
#                        packages = "factorcopula")
# cheops_jobs()
#cat(cheops_getlog("cop-crit"), sep = "\n")
pCrit <- cheops_readRDS("./cop-crit/res.rds")
crit <- quantile(pCrit, 1-0.05)
estBrk <- res$t[which.max(res$p)]
res$group <- ifelse(res$p < crit & res$t < estBrk, 1, 
                    ifelse(res$p < crit & res$t >= estBrk, 3, 2))
res$group <- as.factor(res$group)


ggplot(res, aes(x = t, y = p)) +
  geom_hline(yintercept = quantile(pCrit, 1-0.05), color = "black", linetype = 2) +
  geom_label_repel(stat = "unique", aes(x = max(t)), y = quantile(pCrit, 1-0.05), label = "0.05-CV", point.padding = 1, nudge_y = 20) +
  geom_label_repel(stat = "unique", x = brk, y = 10, label = "breakpoint", point.padding = 1, nudge_x = - 2) +
  geom_line(col = "indianred3") + 
  geom_vline(xintercept = brk, color = "black", linetype = 2) +
  theme_hc()


# Only second and third group copula recursive estimation ---------------------------------------------
beta[beta == "beta1"] <- 0
beta[beta == "beta4"] <- 0.88
beta
lower <- rep(0, 4)
upper <- rep(5, 4)
names(lower) <- paste0("beta", c(2, 3, 5, 6))
names(upper) <- names(lower)

opt <- cheops_slurmcontrol(nodes = 60, tasks = 4, mem = "4gb", time = "04:00:00")
#opt <- cheops_slurmcontrol(nodes = 2, tasks = 2, mem = "1gb", time = "00:10:00", partition = "devel")

cheops_lapply(tSeq, function(t, Y, Z, eps, beta, lower, upper, k){
  fc_fit(Y = Y[1:t, ], Z, eps, beta, lower, upper, S = 37500, k = k, 
         control.first.stage = list(algorithm = "NLOPT_GN_MLSL_LDS", stopval = 0, xtol_rel = 1e-10, maxeval = 2000,
                                    local_opts = list(algorithm = "NLOPT_LN_SBPLX", xtol_rel = 1e-10, maxeval = 2000)), 
         control.second.stage = list(algorithm = "NLOPT_LN_SBPLX", xtol_rel = 1e-16, maxeval = 2000))
}, options = opt, jobname = "group-23", packages = "factorcopula", load.balancing = TRUE,
args = list(Y = Y, Z = Z, eps = eps, beta = beta, lower = lower, upper = upper, k = k))

cheops_jobs()
cat(cheops_getlog("group-23"), sep = "\n")
#cheops_cancel("group-23")

res <- cheops_readRDS("./group-23/res.rds")
res <- data.frame(t(vapply(res, function(x) x$theta.second.stage[1:4], numeric(4))))
res$t <- tSeq
res$p <- fc_pstat(res[,1:4], res$t)

out <- detectOutlier(res$p, 1)
res$p[out] <- NA


opt <- cheops_slurmcontrol(nodes = 40, tasks = 2, mem = "2gb", time = "00:20:00")
cheops_run(fc_critval, options = opt, jobname = "gp23-crt",
                       args = list(type = "copula", Y = Y, B = 1000, tSeq = tSeq, k = k,
                                   factor = Z, error = eps, beta = beta, theta = res[nrow(res), 1:4]),
                       packages = "factorcopula")
cheops_jobs()
cat(cheops_getlog("gp23-crt"), sep = "\n")



# Copula estimation full, before and after the breakpoint detected by the test ---
beta <- config_beta(k)
lower <- c(rep(0, P))
upper <- c(rep(6, P))
names(lower) <- paste0("beta", 1:P)
names(upper) <- paste0("beta", 1:P)

cl <- makeCluster(3)
clusterExport(cl, ls())
cluster_library(cl, "factorcopula")
models <- parLapply(cl, X = list(full = 1:nrow(Y), before = 1:1000, after = 1001:nrow(Y)), function(tSeq){
  fc_fit(Y = Y[tSeq, ], Z, eps, beta, lower, upper, se = TRUE, S = 37500, B = 1000, k = k,
         control.first.stage = list(algorithm = "NLOPT_GN_MLSL_LDS", xtol_rel = 1e-6, maxeval = 400,
                                    local_opts = list(algorithm = "NLOPT_LN_SBPLX", xtol_rel = 1e-4, maxeval = 200)), 
         control.second.stage =  list(algorithm = "NLOPT_LN_SBPLX", xtol_rel = 1e-12, maxeval = 3000))
})

stopCluster(cl)

lapply(models, function(x) round(x$objective, 4))
lapply(models, function(x) round(x$solution, 2))
lapply(models, function(x) x$message)

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



