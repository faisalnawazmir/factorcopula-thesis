library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(tidytext)
library(topicmodels)
library(SnowballC)
library(tm)
library(snow)
library(forecast)
library(factorcopula)
library(cheopsr)
library(lubridate)
library(ggthemes)
library(GGally)
library(xtable)
library(scales)
# Options and Constants ---------------------------------------------------

options(cheopsr.account = "AG-Wied")
options(cheopsr.username = "bonartm")
options(stringsAsFactors = FALSE, scipen = 999)
PARTIES <- c("#000000", "#EB001F", "#BE3075", "#64A12D", "#FFED00", "#009EE0")
names(PARTIES) <- c("CDU/CSU", "SPD", "Linke", "Grüne", "FDP", "AfD")
fromto <- as.Date(c("2014-01-01", "2017-12-31"))
tSeq <- seq(fromto[1], fromto[2], "days")

stopWords <- readLines("./data/stopwords.txt")

isStopword <- function(words, stopWords){
  words %in% stopWords
}

standardize <- function(x){
  require(rugarch)
  arima <- auto.arima(x)
  cat("p:", arima$arma[1], "q:", arima$arma[2], "\n")
  spec <- ugarchspec(mean.model = list(armaOrder= c(arima$arma[1], arima$arma[2]), arfima = TRUE), fixed.pars = list(arfima = 1), distribution.model = "norm")
  m <- ugarchfit(spec = spec, x)
  res <- as.vector(residuals(m, standardize = TRUE))
  
  return(res)
}

topics <- list(
  flucht = "flucht|fluecht"
)

posts <- readRDS("./data/posts.rds") %>%
  filter(createdDate %in% tSeq) %>%
  filter(!(cleanText == "" | is.na(cleanText))) %>%
  mutate(weight = 1, party = ifelse(party %in% c("CDU", "CSU"), "CDU/CSU", party), match = str_detect(cleanText, topics$flucht))

topicsTrend <- posts %>%
  group_by(createdDate, party) %>%
  summarise(topic = weighted.mean(match, weight)) %>%
  spread(party, topic) %>%
  ungroup

topicsTrendRes <- data.frame(apply(topicsTrend[,-1], 2, standardize))
names(topicsTrendRes) <- names(topicsTrend)[-1]

T <- nrow(topicsTrendRes)
N <- ncol(topicsTrendRes)
tSeq <- 300:T
k <- rep(1, N)

# [x]posts and active accounts per month  ----------------------------------------------------------


posts %>%
  group_by(createdDate = round_date(createdDate, "month", week_start = 1), party) %>%
  summarise(nPosts = n(), nAccounts = n_distinct(fromId), nMatch = sum(match)) %>%
  tidyr::gather("type", "n", nPosts, nAccounts, nMatch) %>%
  mutate(type = factor(type, levels = c("nAccounts", "nPosts", "nMatch"), ordered = TRUE)) %>%
  ggplot(aes(x = createdDate, y = n, fill = party)) + 
  geom_area(color = "black", alpha = 0.8, size = 0.3) + 
  facet_grid(type ~ ., scales = "free_y", labeller = as_labeller(c(nAccounts = "Active accounts", nPosts = "Posts", nMatch = "\"refugee\" posts"))) + 
  guides(fill = guide_legend(nrow = 1)) + 
  scale_x_date(date_breaks = "1 year", date_minor_breaks = "3 month", date_labels = "%Y", expand = c(0.0, 365/4)) + 
  scale_fill_manual(values = PARTIES) + 
  scale_y_continuous(labels = scales::comma) + 
  geom_vline(xintercept = as.Date("2015-09-18"), color = "black", linetype = 2) +
  labs(x = "Date", y = "N") + 
  theme_hc() + 
  theme(legend.title = element_blank(), 
        panel.grid.minor.x = element_line(color = "lightgray"), 
        panel.grid.major.x = element_line(color = "lightgray"))


tab <- posts %>%
  group_by(party) %>%
  summarise(nPosts = n(), nAccounts = n_distinct(fromId), likesInMillion = sum(likes)/1000000, sharesInMilion = sum(shares)/1000000, 
            mean = round(mean(match)*100, 2))

print(xtable(tab), include.rownames=FALSE)
 
nrow(posts)
length(unique(posts$fromId))
sum(posts$likes)/1000000
sum(posts$shares)/1000000
round(mean(posts$match)*100, 2)

# [x]Pairwise analysis -----------------------------------------------------------


tab <- data.frame(t(combn(names(topicsTrendRes), 2)))
U <- apply(topicsTrendRes, 2, factorcopula:::empDist)
tab <- cbind(tab, t(vapply(1:nrow(tab), function(i) {
  factorcopula:::dependence(U[,tab$X1[i]], U[,tab$X2[i]])
}, numeric(5))))
tab$label <- paste(tab$X1, tab$X2, sep = "-")
tab <- cbind(tab$label, tab[,1:5])
tab <- tab[, -c(1, 2)]
tab <- rbind(tab, c("Average", round(colMeans(tab[, 2:6]),2)))
print(xtable(tab), include.rownames = FALSE)

topicsTrend %>%
  gather("party", "share", -createdDate) %>%
  ggplot(aes(x = createdDate, y = share)) +
  geom_line(color = "indianred3") + 
  facet_wrap(~party, ncol = 2) + 
  labs(x = "Date", y = "Rel. frequency of posts")+ 
  theme_hc()

topicsTrendRes %>%
  mutate(createdDate = topicsTrend$createdDate) %>%
  gather("party", "share", -createdDate) %>%
  ggplot(aes(x = createdDate, y = share)) +
  geom_line(color = "indianred3") + 
  facet_wrap(~party, ncol = 2) + 
  labs(x = "Date", y = "Residual") + 
  theme_hc()

ggpairs(topicsTrendRes[, -1], 
        lower = "blank", 
        diag = list(continuous = wrap("densityDiag", fill = "indianred3", alpha = 0.5)), 
        upper = list(continuous = wrap("points", color = "indianred3", alpha = 0.5))) +  theme_hc()


eigen <- data.frame(Index = 1:N, Eigenvalue = eigen(cor(topicsTrendRes, method = "spearman"))$values)
ggplot(eigen, aes(x = Index, y = Eigenvalue))+
  #geom_line(color = "indianred3") + 
  #geom_point(color = "indianred3") + 
  geom_col(fill = "indianred3", color = "black", alpha = 0.7) + 
  geom_hline(yintercept = 1, linetype = 2, color = "black") + 
  scale_y_continuous(limits = c(0, NA)) + 
  scale_x_continuous(breaks = 1:N) + 
  theme_hc()



# [x]Moments based restrictive and unrestriuctive test -----------------------------------------------------

mStat <- fc_mstat(topicsTrendRes, tSeq, k = k)

# opt <- cheops_slurmcontrol(nodes = 2, tasks = 8, mem = "2gb", time = "00:20:00", partition = "devel")
# cheops_run(fc_critval, options = opt, jobname = "fac-mom1",
#                        args = list(type = "moments", Y = topicsTrendRes, B = 2000, tSeq = tSeq, k = k),
#                        packages = "factorcopula")
# cheops_jobs()
#cheops_cancel("fac-mom1")
cat(cheops_getlog("fac-mom1"), sep = "\n")
mCrit <- cheops_readRDS("./fac-mom1/res.rds")

brk <- tSeq[which.max(mStat)]
topicsTrend$createdDate[brk] 
max(mStat) 
quantile(mCrit, 1-0.05)


k <- 1:N
mStat <- fc_mstat(topicsTrendRes, tSeq, k = k)
# opt <- cheops_slurmcontrol(nodes = 2, tasks = 8, mem = "2gb", time = "00:30:00", partition = "devel")
# cheops_run(fc_critval, options = opt, jobname = "fac-mom2",
#            args = list(type = "moments", Y = topicsTrendRes, B = 2000, tSeq = tSeq, k = k),
#            packages = "factorcopula")
# cheops_jobs()
#cheops_cancel("fac-mom2")
cat(cheops_getlog("fac-mom2"), sep = "\n")
mCrit <- quantile(cheops_readRDS("./fac-mom2/res.rds"), 1-0.05)

brk <- tSeq[which.max(mStat)]
topicsTrend$createdDate[brk] 
max(mStat) 
mCrit # 




# [x]Fit various Factor Copula to all Residuals ----------------------------------------

opt <- cheops_slurmcontrol(nodes = 2, tasks = 4, mem = "4gb", time = "00:30:00", partition = "devel")

cheops_lapply(paste0("fac-cop", 1:6), function(name, topicsTrendRes){
  N <- ncol(topicsTrendRes)
  # unrestrictive 
  if (name == "fac-cop1"){
    k <- 1:N
    eps <- config_error(rnorm = list())
    Z <- config_factor(rnorm = list())
    beta <- config_beta(k, 1)
    
    lower <- c(beta1 = 0, beta2 = 0, beta3 = 0, beta4 = 0, beta5 = 0, beta6 = 0)
    upper <- c(rep(5, 6))
    names(upper) <- names(lower)
  }
  if (name == "fac-cop2"){
    k <- 1:N
    eps <- config_error(rt = list(df = df), par = "df")
    Z <- config_factor(rt = list(df = df), par = c("df"))
    beta <- config_beta(k, 1)
    
    lower <- c(beta1 = 0, beta2 = 0, beta3 = 0, beta4 = 0, beta5 = 0, beta6 = 0, df = 2.01)
    upper <- c(rep(5, 6), 100)
    names(upper) <- names(lower)
  }
  if (name == "fac-cop3"){
    k <- 1:N 
    eps <- config_error(rt = list(df = df), par = "df")
    Z <- config_factor(rst = list(nu = df, lambda = lambda), par = c("df", "lambda"))
    beta <- config_beta(k, 1)
    
    lower <- c(beta1 = 0, beta2 = 0, beta3 = 0, beta4 = 0, beta5 = 0, beta6 = 0, df = 2.01, lambda = -0.99)
    upper <- c(rep(5, 6), 100, 0.99)
    names(upper) <- names(lower)
  }
  # restrictive
  if (name == "fac-cop4"){
    k <- rep(1, N)
    eps <- config_error(rnorm = list())
    Z <- config_factor(rnorm = list())
    beta <- config_beta(k, 1)
    
    lower <- c(beta1 = 0)
    upper <- c(5)
    names(upper) <- names(lower)
  }
  if (name == "fac-cop5"){
    k <- rep(1, N)
    eps <- config_error(rt = list(df = df), par = "df")
    Z <- config_factor(rt = list(df = df), par = c("df"))
    beta <- config_beta(k, 1)
    
    lower <- c(beta1 = 0, df = 2.01)
    upper <- c(rep(5, 1), 100)
    names(upper) <- names(lower)
  }
  if (name == "fac-cop6"){
    k <- rep(1, N)
    eps <- config_error(rt = list(df = df), par = "df")
    Z <- config_factor(rst = list(nu = df, lambda = lambda), par = c("df", "lambda"))
    beta <- config_beta(k, 1)
    
    lower <- c(beta1 = 0, df = 2.01, lambda = -0.99)
    upper <- c(rep(5, 1), 100, 0.99)
    names(upper) <- names(lower)
  }
  
  fc_fit(Y = topicsTrendRes, Z, eps, beta, lower, upper, S = 36525, k = k, se = FALSE,
         control.first.stage = list(algorithm = "NLOPT_GN_MLSL_LDS", stopval = 0, xtol_rel = 1e-13, maxeval = 3000,
                                    local_opts = list(algorithm = "NLOPT_LN_SBPLX", xtol_rel = 1e-13, maxeval = 3000)), 
         control.second.stage = list(algorithm = "NLOPT_LN_SBPLX", xtol_rel = 1e-19, maxeval = 3000))
}, options = opt, jobname = "emp-cop", packages = "factorcopula", args = list(topicsTrendRes = topicsTrendRes))


cheops_jobs()
#cheops_cancel("emp-cop")
cat(cheops_getlog("emp-cop"), sep  = "\n")

models <- cheops_readRDS("./emp-cop/res.rds")

lapply(models, function(x) round(x$Q, 4))
lapply(models, function(x) round(x$theta.second.stage, 2))

models[[6]]


# [x]Fit a recursive skew t-t equidependence copula model --------------------

k <- rep(1, N)
eps <- config_error(rt = list(df = 96))
Z <- config_factor(rst = list(nu = 96, lambda = -0.36))
beta <- config_beta(k, 1)

lower <- c(beta1 = 0)
upper <- c(beta1 = 5)

opt <- cheops_slurmcontrol(nodes = 50, tasks = 5, mem = "4gb", time = "02:00:00")

cheops_lapply(tSeq, function(t, topicsTrendRes, Z, eps, beta, lower, upper, k){
  fc_fit(Y = topicsTrendRes[1:t, ], Z, eps, beta, lower, upper, S = 36525, k = k,
         control.first.stage = list(algorithm = "NLOPT_GN_MLSL_LDS", stopval = 0, xtol_rel = 1e-14, maxeval = 4000,
                                    local_opts = list(algorithm = "NLOPT_LN_SBPLX", xtol_rel = 1e-14, maxeval = 4000)),
         control.second.stage = list(algorithm = "NLOPT_LN_SBPLX", xtol_rel = 1e-20, maxeval = 4000))
}, options = opt, jobname = "fac-rec", packages = "factorcopula", load.balancing = FALSE,
args = list(topicsTrendRes = topicsTrendRes, Z = Z, eps = eps, beta = beta, lower = lower, upper = upper, k = k))

cheops_jobs()
cat(cheops_getlog("fac-rec"), sep = "\n")
#cheops_cancel("fac-rec")

res <- cheops_readRDS("./fac-rec/res.rds")

theta <- data.frame(do.call(rbind, lapply(res, function(x) x$theta.second.stage)))

theta$p <- fc_pstat(theta[, 1, drop = FALSE], tSeq)

# opt <- cheops_slurmcontrol(nodes = 20, tasks = 3, mem = "2gb", time = "00:20:00")
# cheops_run(fc_critval, options = opt, jobname = "rec-crt",
#            args = list(type = "copula", Y = topicsTrendRes, B = 2000, tSeq = tSeq, k = k, 
#                        factor = Z, error = eps, beta = beta, theta = theta[nrow(theta), 1, drop = FALSE]),
#            packages = "factorcopula")
# cheops_jobs()
# cat(cheops_getlog("rec-crt"), sep = "\n")

crit <- cheops_readRDS("./rec-crt/res.rds")

brk <- tSeq[which.max(theta$p)]
topicsTrend$createdDate[brk]
max(theta$p)
quantile(crit, 1-0.05)
1-ecdf(crit)(max(theta$p))


#  [x] Before and after breakpoint copula estimation --------------------------
k <- rep(1, N) 
eps <- config_error(rt = list(df = 96))
Z <- config_factor(rst = list(nu = 96, lambda = lambda), par = c("lambda"))
beta <- config_beta(k, 1)

lower <- c(beta1 = 0, lambda = -0.8)
upper <- c(5, 0.8)
names(upper) <- names(lower)


cl <- makeCluster(3)
clusterExport(cl, ls()[!grepl("posts", ls())])
cluster_library(cl, "factorcopula")
models <- parLapply(cl, X = list(full = 1:nrow(topicsTrendRes), before = 1:626, after = 627:nrow(topicsTrendRes)), function(tSeq){
  fc_fit(Y = topicsTrendRes[tSeq, ], Z, eps, beta, lower, upper, se = TRUE, S = 36525, B = 2000, k = k,
         control.first.stage = list(algorithm = "NLOPT_GN_MLSL_LDS", xtol_rel = 1e-14, maxeval = 4000,
                                    local_opts = list(algorithm = "NLOPT_LN_SBPLX", xtol_rel = 1e-14, maxeval = 2000)), 
         control.second.stage =  list(algorithm = "NLOPT_LN_SBPLX", xtol_rel = 1e-20, maxeval = 3000))
})

stopCluster(cl)

lapply(models, function(x) round(x$Q, 4))
lapply(models, function(x) round(x$theta.second.stage, 2))
lapply(models, function(x) x$message)
