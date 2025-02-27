% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{riskdist}
\alias{riskdist}
\title{Make a plot of predicted risks by outcome}
\usage{
riskdist(
  y,
  p,
  ypos = 0,
  labels = c(0, 1),
  nbins = 101,
  add = TRUE,
  maxh = 0.15
)
}
\arguments{
\item{y}{vector of binary outcome}

\item{p}{vector of predicted risks}

\item{ypos}{where to center the y axis}

\item{labels}{labels for outcomes 0 and 1, respectively. Default to "0" and "1"}

\item{nbins}{Default 101}

\item{add}{if TRUE (default) added to an existing plot. If FALSE a new plot is made}

\item{maxh}{maximum height of a bar (the bin with largest number of observations). Default = .15}
}
\value{
No return value, called for side effects
}
\description{
Make a plot of predicted risks by outcome
}
\keyword{internal}
