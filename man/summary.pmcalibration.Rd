% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{summary.pmcalibration}
\alias{summary.pmcalibration}
\title{Summarize a pmcalibration object}
\usage{
\method{summary}{pmcalibration}(object, conf_level = 0.95, ...)
}
\arguments{
\item{object}{object created with \code{pmcalibration}}

\item{conf_level}{width of the confidence interval (0.95 gives 95\% CI). Ignored if call to \code{pmcalibration} didn't request confidence intervals}

\item{...}{ignored}
}
\value{
prints a summary of calibration metrics. Returns a list of two tables: \code{metrics} and \code{plot}
}
\description{
Summarize a pmcalibration object
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

summary(cal)
}
