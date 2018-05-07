library(factorcopula)
library(ggplot2)
library(ggthemes)

k <- c(1, 1)
theta1 <- c(beta1 = 1.5)
beta <- config_beta(k, Z = 1)


f1 <- fc_create(config_factor(rst = list(nu = 1/0.25, lambda = -0.8)),
                config_error(rt = list(df = 1/0.25)), beta)
f2 <- fc_create(config_factor(rgamma = list(shape = 1)),
                config_error(rnorm = list()), beta)
f3 <- fc_create(config_factor(rt = list(df = 1/0.25)),
                config_error(rt = list(df = 1/0.25)), beta)
f4 <- fc_create(config_factor(rnorm = list()),
                config_error(rnorm = list()), beta)

T <- 1500

Y1 <- qnorm(f1(theta1, S = T, seed = NULL))
Y2 <- qnorm(f2(theta1, S = T, seed = NULL))
Y3 <- qnorm(f3(theta1, S = T, seed = NULL))
Y4 <- qnorm(f4(theta1, S = T, seed = NULL))

dat <- data.frame(cbind(rep(1:4, each = T), rbind(Y1, Y2, Y3, Y4)))
colnames(dat) <- c("group", "X1", "X2")
dat$group <- factor(dat$group, levels = 1:4, labels = c("skew t - t", "exp - normal", "t - t", "norm - norm"))


ggplot(data.frame(dat), aes(x = X1, y = X2)) +
  geom_point(alpha = 0.7, col = "indianred3") +
  facet_wrap(~ group, ncol = 2) +
  labs(x = "", y = "") +
  theme_hc()

