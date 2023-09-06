## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(pmcalibration)

# simulate some data for vignette
set.seed(2345)
dat <- sim_dat(1000, a1 = -3, a3 = .3)

# show the first 3 columns (col 4 is the true linear predictor/LP)
head(dat[-4])

## -----------------------------------------------------------------------------
p <- plogis(with(dat, -3 + x1 + x2))
y <- dat$y

## -----------------------------------------------------------------------------
logistic_cal(y = y, p = p)

## -----------------------------------------------------------------------------
(cc <- pmcalibration(y = y, p = p, 
                     smooth = "gam", bs = "tp", 
                     k = 10, transf="logit",
                     ci = "sim", method="REML"))


## ---- fig.height=5, fig.width=6-----------------------------------------------
plot(cc)

## ---- fig.height=5, fig.width=6-----------------------------------------------
library(ggplot2)

pcc <- get_cc(cc)

ggplot(pcc, aes(x = p, y = p_c, ymin=lower, ymax=upper)) +
  geom_abline(intercept = 0, slope = 1, lty=2) +
  geom_line() +
  geom_ribbon(alpha = 1/2, fill="lightblue") +
  coord_cartesian(xlim=c(0,1), ylim=c(0,1)) +
  labs(x = "Predicted", y = "Estimated") +
  theme_bw(base_size = 14) +
  geom_histogram(data = data.frame(p = p), aes(x=p, y=after_stat(density)*.01),
                 binwidth = .001, inherit.aes = F, alpha=1/2)


## ---- fig.height=5, fig.width=6-----------------------------------------------
library(rms)
val.prob(p = p, y = y)

## -----------------------------------------------------------------------------
library(simsurv)
library(survival)

# simulate some data
n <- 2000
X <- data.frame(id = seq(n), x1 = rnorm(n), x2 = rnorm(n))
X$x3 <- X$x1*X$x2 # interaction

b <- c("x1" = -.2, "x2" = -.2, "x3" = .1)

d <- simsurv(dist = "weibull", lambdas = .01, gammas = 1.5, x = X, betas = b, seed = 246)

mean(d$eventtime)
median(d$eventtime)
mean(d$status) # no censoring

d <- cbind(d, X[,-1])

head(d)

# split into development and validation
ddev <- d[1:1000, ]
dval <- d[1001:2000, ]

# fit a cox model
cph <- coxph(Surv(eventtime, status) ~ x1 + x2, data = ddev)

# predicted probability of event at time = 15
p = 1 - exp(-predict(cph, type="expected", newdata = data.frame(eventtime=15, status=1, x1=dval$x1, x2=dval$x2)))

y <- with(dval, Surv(eventtime, status))

# calibration curve at time = 15
(cc <- pmcalibration(y = y, p = p, smooth = "rcs", nk = 5, ci = "pw", time = 15))
# pointwise standard errors for plot but no CI for metrics
# 'boot' CIs are also available for time to event outcomes


## ---- fig.height=5, fig.width=6-----------------------------------------------
plot(cc)
mtext("time = 15")

## ---- fig.height=5, fig.width=6-----------------------------------------------
plot(val.surv(S = y, est.surv = 1-p, u=15, fun = function(x) log(-log(x))))

