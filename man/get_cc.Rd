% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{get_cc}
\alias{get_cc}
\title{Extract plot data from \code{pmcalibration} object}
\usage{
get_cc(x, conf_level = 0.95)
}
\arguments{
\item{x}{\code{pmcalibration} object}

\item{conf_level}{width of the confidence interval (0.95 gives 95\% CI). Ignored if call to \code{pmcalibration} didn't request confidence intervals}
}
\value{
data frame for plotting with 4 columns
\itemize{
\item{\code{p} - values for the x-axis (predicted probabilities - note these are *not* from your data and are only used for plotting)}
\item{\code{p_c} - probability implied by the calibration curve given \code{p}}
\item{\code{lower} and \code{upper} - bounds of the confidence interval}
}
}
\description{
Extract plot data from \code{pmcalibration} object
}
\examples{
library(pmcalibration)
# simulate some data with a binary outcome
n <- 500
dat <- sim_dat(N = n, a1 = .5, a3 = .2)
head(dat)
# predictions
p <- with(dat, invlogit(.5 + x1 + x2 + x1*x2*.1))

# fit calibration curve
cal <- pmcalibration(y = dat$y, p = p, smooth = "gam", k = 20, ci = "pw")

cplot <- get_cc(cal, conf_level = .95)
head(cplot)

if (requireNamespace("ggplot2", quietly = TRUE)){
library(ggplot2)
ggplot(cplot, aes(x = p, y = p_c, ymin=lower, ymax=upper)) +
  geom_abline(intercept = 0, slope = 1, lty=2) +
  geom_line() +
  geom_ribbon(alpha = 1/4) +
  lims(x=c(0,1), y=c(0,1))
}
}
