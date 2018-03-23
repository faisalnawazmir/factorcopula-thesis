library(factorcopula)
library(parallel)
library(cheopsr)
library(ggplot2)
library(dplyr)
library(ggthemes)

#devtools::install_github("bonartm/cheopsr")
options(cheopsr.account = "AG-Wied")
options(cheopsr.username = "bonartm")
#cheops_install_github("bonartm/factorcopula", ref = "dev")

tSeq <- rep(c(100, 1000, 10000), each = 500)
N <- c(2, 3, 10)
values <- expand.grid(t = tSeq, n = N)

theta0 <- c(beta1 = 1.5)
lower <- c(beta1 = 0)
upper <- c(beta1 = 5)

Z <- config_factor(rst = list(nu = 1/0.25, lambda = -0.8))
eps <- config_error(rt = list(df = 1/0.25))

opt <- cheops_slurmcontrol(nodes = 60, tasks = 1, mem = "4gb", time = "02:00:00")
#opt <- cheops_slurmcontrol(nodes = 2, tasks = 2, mem = "4gb", time = "00:05:00", partition = "devel")

job <- cheops_lapply(1:nrow(values), function(i, values, theta0, lower, upper, Z, eps){
  T <- values[i, 1]
  N <- values[i, 2]
  k <- rep(1, N)
  beta <- config_beta(k, Z = 1)
  cop <- fc_create(Z, eps, beta)
  Y <- qnorm(cop(theta0, T))
  fc_fit(Y, Z, eps, beta, lower = lower, upper = upper, recursive = FALSE, trials = 4,
                   control = list(stopval = 0, xtol_rel = 1e-12, maxeval = 1000), S = 25000, cl = NULL, k = k)

  },
  options = opt, args = list(values = values, theta0 = theta0, lower = lower, upper = upper, Z = Z, eps = eps),
  packages = c("factorcopula"), jobname = "mcstudy", load.balancing = FALSE)

cheops_jobs()
cat(cheops_getlog("mcstudy"), sep = "\n")
#cheops_cancel("mcstudy")

res <- cheops_readRDS("./mcstudy/res.rds")
res <- data.frame(do.call(rbind, res))
theta <- cbind(values, beta1 = res$beta1)
theta$tLabel <- factor(paste0("t = ", theta$t), levels = c("t = 100", "t = 1000", "t = 10000"), ordered = TRUE)
theta$nLabel <- factor(paste0("n = ", theta$n), levels = c("n = 2", "n = 3", "n = 10"), ordered = TRUE)

res <- theta %>%
  group_by(t, n) %>%
  summarise(bias = mean(beta1)-1.5, sd = sd(beta1),  mse = mean((beta1-1.5)^2))

theta <- theta %>%
  left_join(res, by = c("t", "n"))


options(scipen = 9999)

theta$mseLabel <- paste0("mse = ", round(theta$mse, 2), "\nbias = ", round(theta$bias, 2))

ggplot(theta, aes(x = beta1, label = mseLabel)) +
  geom_density(fill = "indianred3", kernel = "gaussian") +
  facet_grid(tLabel ~ nLabel) +
  geom_vline(aes(xintercept = mean(beta1)), linetype = 3, color = "black") +
  geom_text(x = 1, y = 11, stat = "unique", hjust = 0) +
  scale_x_continuous(limits = c(1, 2)) +
  theme_hc() +
  theme(strip.text = element_text(size = 14))






