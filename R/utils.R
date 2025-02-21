#' Summarize a pmcalibration object
#'
#' @param object object created with \code{pmcalibration}
#' @param conf_level width of the confidence interval (0.95 gives 95\% CI). Ignored if call to \code{pmcalibration} didn't request confidence intervals
#' @param ... ignored
#'
#' @return prints a summary of calibration metrics. Returns a list of two tables: \code{metrics} and \code{plot}
#' @export
#'
#' @examples
#' library(pmcalibration)
#' # simulate some data with a binary outcome
#' n <- 500
#' dat <- sim_dat(N = n, a1 = .5, a3 = .2)
#' head(dat)
#' # predictions
#' p <- with(dat, invlogit(.5 + x1 + x2 + x1*x2*.1))
#'
#' # fit calibration curve
#' cal <- pmcalibration(y = dat$y, p = p, smooth = "gam", k = 20, ci = "pw")
#'
#' summary(cal)
summary.pmcalibration <- function(object, conf_level = .95, ...){

  x <- object

  probs <- c((1 - conf_level)/2, 1 - (1 - conf_level)/2)

  m_tab <- data.frame(Estimate = x$metrics)

  if (!is.null(x$metrics.samples)){
    m_ci <- t(apply(x$metrics.samples, MARGIN = 2, FUN = quantile, probs = probs, na.rm = TRUE))
    colnames(m_ci) <- c("lower", "upper")
    m_tab <- cbind(m_tab, m_ci)
  }

  plot_tab <- data.frame(p = x$plot$p, p_c = x$plot$p_c_plot)

  if (!is.null(x$plot$plot.samples)){
    p_ci <- t(apply(x$plot$plot.samples, MARGIN = 2, FUN = quantile, probs = probs, na.rm = TRUE))
    colnames(p_ci) <- c("lower", "upper")
    plot_tab <- cbind(plot_tab, p_ci)
  }

  if (x$ci == "pw" & !is.null(x$plot$p_c_plot_se)){
    zcrit <- qnorm((1-conf_level)/2, lower.tail = FALSE)
    p_ci <- x$plot$p_c_plot + t(outer(c(-1, 1)*zcrit, x$plot$p_c_plot_se))
    colnames(p_ci) <- c("lower", "upper")
    plot_tab <- cbind(plot_tab, p_ci)
    if (x$outcome == "tte" & x$smooth != "gam"){
      plot_tab[, 2:4] <- 1 - exp(-plot_tab[, 2:4])
    }
  }

  out <- list(
    metrics = m_tab,
    plot = plot_tab,
    smooth = x$smooth,
    ci = x$ci,
    conf_level = conf_level,
    n = x$n,
    smooth_args = x$smooth_args,
    transf = x$transf,
    time = x$time,
    outcome=x$outcome
  )

  class(out) <- "pmcalibrationsummary"

  return(out)
}

#' Print summary of pmcalibration object
#'
#' @param x a \code{pmcalibrationsummary} object
#' @param digits number of digits to print
#' @param ... ignored
#'
#' @returns invisible(x) - prints a summary
#' @rdname print.pmcalibrationsummary
#' @export
print.pmcalibrationsummary <- function(x, digits = 2, ...){
  smoothtext = list("none" = "no smooth",
                    "rcs" = "a restricted cubic spline (see ?rms::rcs)",
                    "ns" = "a natural cubic spline (see ?splines::ns)",
                    "bs" = "a B-spline (see ?splines::bs)",
                    "gam" = "a generalized additive model (see ?mgcv::s)"
  )

  citext <- list("boot" = "bootstrap resampling",
                 "sim" = "simulation based inference"
  )

  transp <- if (is.function(x$transf)) "logit transformed" else "untransformed"
  if (is.function(x$transf)){
    transp <- "transformed (user defined)"
  } else{
    if (x$transf == "none"){
      transp <- "untransformed"
    } else if (x$transf == "logit"){
      transp <- "logit transformed"
    } else if (x$transf == "cloglog"){
      transp <- "complementary log-log transformed"
    }
  }

  if (x$outcome == "binary"){
    otext <- "binary outcome"
  } else if (x$outcome == "tte"){
    otext <- paste0("time-to-event outcome (time = ", x$time, ")")
  }

  cat("Calibration metrics based on a calibration curve estimated for a", otext,"via",
      smoothtext[[x$smooth]], "using", transp, "predicted probabilities.\n\n")

  print(x$metrics, digits = digits)

  if (x$ci %in% names(citext)){
    cat("\n")
    cat(sprintf("%.0f%%", x$conf_level*100),
        "confidence intervals calculated via",
        citext[[x$ci]], "with", x$n, "replicates.\n")
  } else if (x$ci == "pw" & !is.null(x$plot$p_c_plot_se)){
    cat("\n")
    cat("Pointwise confidence intervals calculated for plotting only.")
  }

  invisible(x)
}

#' print a pmcalibration object
#'
#' @param x a \code{pmcalibration} object
#' @param digits number of digits to print
#' @param conf_level width of the confidence interval (0.95 gives 95\% CI)
#' @param ... optional arguments passed to print
#'
#' @returns prints a summary
#'
#' @rdname print.pmcalibration
#' @export
print.pmcalibration <- function(x, digits = 2, conf_level = .95, ...) {
  print(summary(x, conf_level = conf_level), digits = digits, ...)
}


#' Extract plot data from \code{pmcalibration} object
#'
#' @param x \code{pmcalibration} object
#' @param conf_level width of the confidence interval (0.95 gives 95\% CI). Ignored if call to \code{pmcalibration} didn't request confidence intervals
#'
#' @return data frame for plotting with 4 columns
#' \itemize{
#' \item{\code{p} - values for the x-axis (predicted probabilities - note these are *not* from your data and are only used for plotting)}
#' \item{\code{p_c} - probability implied by the calibration curve given \code{p}}
#' \item{\code{lower} and \code{upper} - bounds of the confidence interval}
#' }
#' @export
#'
#' @examples
#' library(pmcalibration)
#' # simulate some data with a binary outcome
#' n <- 500
#' dat <- sim_dat(N = n, a1 = .5, a3 = .2)
#' head(dat)
#' # predictions
#' p <- with(dat, invlogit(.5 + x1 + x2 + x1*x2*.1))
#'
#' # fit calibration curve
#' cal <- pmcalibration(y = dat$y, p = p, smooth = "gam", k = 20, ci = "pw")
#'
#' cplot <- get_curve(cal, conf_level = .95)
#' head(cplot)
#'
#' if (requireNamespace("ggplot2", quietly = TRUE)){
#' library(ggplot2)
#' ggplot(cplot, aes(x = p, y = p_c, ymin=lower, ymax=upper)) +
#'   geom_abline(intercept = 0, slope = 1, lty=2) +
#'   geom_line() +
#'   geom_ribbon(alpha = 1/4) +
#'   lims(x=c(0,1), y=c(0,1))
#' }
get_curve <- function(x, conf_level = .95){
  cc <- summary(x, conf_level = conf_level)$plot
  return(cc)
}

#' Make a plot of predicted risks by outcome
#'
#' @param y vector of binary outcome
#' @param p vector of predicted risks
#' @param ypos where to center the y axis
#' @param labels labels for outcomes 0 and 1, respectively. Default to "0" and "1"
#' @param nbins Default 101
#' @param add if TRUE (default) added to an existing plot. If FALSE a new plot is made
#' @param maxh maximum height of a bar (the bin with largest number of observations). Default = .15
#'
#' @returns No return value, called for side effects
#' @keywords internal
#' @export
riskdist <- function(y, p, ypos=0, labels=c(0,1), nbins=101, add=TRUE, maxh=.15){
  if (!add){
    plot(x=NA, y=NA, xlim=range(p)*c(-1.08, 1.08),
         ylim=ypos + c(-maxh, maxh),
         axes=FALSE, xlab="", ylab="")
    axis(1)
  }
  bins <- seq(min(p), max(p), length.out=nbins)
  pbin <- cut(p, bins, include.lowest = TRUE)
  n0 <- table(pbin[y==0])
  n1 <- table(pbin[y==1])
  maxn <- max(n0, n1)
  h0 <- maxh*(n0/maxn)
  h1 <- maxh*(n1/maxn)
  bins <- bins + c(diff(bins)/2, 0) # plot tick in the middle of range
  bins <- bins[-nbins]
  segments(x0 = bins[h1>0], y0 = ypos,
           x1 = bins[h1>0], y1 = ypos + h1[h1>0])
  segments(x0 = bins[h0>0], y0 = ypos,
           x1 = bins[h0>0], y1 = ypos - h0[h0>0])

  segments(x0 = min(p), x1 = max(p), y0 = ypos, y1 = ypos)

  text(x = max(p)*1.02, y = ypos + maxh/2, labels = labels[2])
  text(x = max(p)*1.02, y = ypos - maxh/2, labels = labels[1])
}

#' Plot a calibration curve
#'
#' @description
#' Plot a \code{pmcalibration} object. For binary outcomes, also plot the distribution of predicted risks by outcome.
#' Alternatively you can use \code{get_curve()} to get the data required to plot the calibration curve.
#'
#' @param x a \code{pmcalibration} calibration curve
#' @param conf_level width of the confidence interval (0.95 gives 95\% CI). Ignored if call to \code{pmcalibration} didn't request confidence intervals
#' @param riskdist add risk distribution plot under calibration curve (TRUE) or not (FALSE)
#' @param linecol color of the calibration curve line
#' @param fillcol color of the confidence interval
#' @param ideallty line type of the ideal unit slope line
#' @param idealcol color of the ideal unit slope line
#' @param ... other args for \code{plot()} (currently only \code{lim}s and \code{lab}s can be specified)
#'
#' @return No return value, called for side effects
#' @export
#'
#' @examples
#' library(pmcalibration)
#' # simulate some data with a binary outcome
#' n <- 500
#' dat <- sim_dat(N = n, a1 = .5, a3 = .2)
#' head(dat)
#' # predictions
#' p <- with(dat, invlogit(.5 + x1 + x2 + x1*x2*.1))
#'
#' # fit calibration curve
#' cal <- pmcalibration(y = dat$y, p = p, smooth = "gam", k = 20, ci = "pw", plot = FALSE)
#'
#' plot(cal, xlab = "Predicted Risk of Outcome") # customize plot
plot.pmcalibration <- function(x, conf_level = .95, riskdist = TRUE,
                               linecol="black", fillcol="grey",
                               ideallty=2, idealcol="red", ...){

  dots <- list(...)

  if (x$outcome != "binary") riskdist <- FALSE

  # pdat <- summary.pmcalibration(x, conf_level = conf_level)
  # pdat <- pdat$plot
  pdat <- get_curve(x, conf_level = conf_level)

  if ("xlim" %in% names(dots)) xlim <- dots[['xlim']] else xlim <- c(0,1)
  if ("ylim" %in% names(dots)) ylim <- dots[['ylim']] else ylim <- c(0,1)
  if ("xlab" %in% names(dots)) xlab <- dots[['xlab']] else xlab <- "Predicted Probability"
  if ("ylab" %in% names(dots)) ylab <- dots[['ylab']] else ylab <- "Estimated Probability"

  # check lims
  if (length(xlim) != 2 | length(ylim) != 2 | !is.numeric(xlim) | !is.numeric(ylim)) stop("Problem with xlim and/or ylim. Should be vectors of length 2")
  if (xlim[1] > xlim[2]) xlim <- c(xlim[2], xlim[1])
  if (ylim[1] > ylim[2]) ylim <- c(ylim[2], ylim[1])
  if (any(xlim < 0) | any(xlim > 1)){
    message("xlim extends beyond c(0,1). Reverting to this")
    xlim <- c(0,1)
  }
  if (any(ylim < 0) | any(ylim > 1)){
    message("ylim extends beyond c(0,1). Reverting to this")
    ylim <- c(0,1)
  }

  yrange <- diff(ylim)

  if (riskdist){
    # extend y axis
    ylim2 <- ylim - c(yrange*.3, 0)
  } else{
    ylim2 <- ylim
  }

  plot(x = NA, y = NA, type="l",
       xlim = xlim, ylim = ylim2, xlab = xlab, ylab = ylab, axes=FALSE)
  axis(side = 1, at = axTicks(side = 1, usr = xlim))
  ytix <- axTicks(side = 2, usr = ylim)
  axis(side = 2, at = ytix[ytix >= ylim[1] & ytix <= ylim[2]], las=1)
  box()

  clip(xlim[1], xlim[2], ylim[1], ylim[2])
  abline(0, 1, lty=ideallty, col=idealcol)

  if ("lower" %in% colnames(pdat)){
    # lines(x = pdat$p, y = pdat$lower, lty=2)
    # lines(x = pdat$p, y = pdat$upper, lty=2)
    polygon(x = c(pdat$p, rev(pdat$p)),
            y = c(pdat$upper, rev(pdat$lower)),
            col = adjustcolor(fillcol, alpha.f = .5), border = NA)
  }

  lines(x = pdat$p, y = pdat$p_c, col=linecol)
  do.call("clip", as.list(par()$usr))

  if (riskdist){
    p <- x$data$p; y <- x$data$y
    i <- p >= xlim[1] & p <= xlim[2]
    if (any(!i)) message("xlim leads to observations being omitted from curve and riskdist")
    y <- y[i]; p <- p[i]
    riskdist(y = y, p = p, ypos = ylim[1] - c(yrange*.15),
             maxh = yrange*.15)
  }

}

#' Logit transformation
#' @param mu vector of numeric values between 0 and 1
#' @return logit transformed mu (log(mu/(1-mu)))
#' @keywords internal
#' @export
logit <- binomial()$linkfun

#' Inverse logit transformation
#' @param eta vector of numeric values
#' @return inverse logit transformed eta (exp(eta)/(1 + exp(eta)))
#' @keywords internal
#' @export
invlogit <- binomial()$linkinv

#' Simulate a binary outcome with either a quadratic relationship or interaction
#'
#' @description
#' Function for simulating data either with a single 'predictor' variable with a quadratic relationship with logit(p)
#' or two predictors that interact (see references for examples).
#'
#' @param N number of observations to simulate
#' @param a1 value of the intercept term (in logits). This must be provided along with either \code{a2} or \code{a3}.
#' @param a2 value of the quadratic coefficient. If specified the linear predictor is simulated as follows: \code{LP <- a1 + x1 + a2*x1^2} where \code{x1} is sampled from a standard normal distribution.
#' @param a3 value of the interaction coefficient. If specified the linear predictor is simulated as follows: \code{LP <- a1 + x1 + x2 + x1*x2*a3} where \code{x1} and \code{x2} are sampled from independent standard normal distributions.
#'
#' @references Austin, P. C., & Steyerberg, E. W. (2019). The Integrated Calibration Index (ICI) and related metrics for quantifying the calibration of logistic regression models. Statistics in medicine, 38(21), 4051-4065.
#' @references Rhodes, S. (2022, November 4). Using restricted cubic splines to assess the calibration of clinical prediction models: Logit transform predicted probabilities first. https://doi.org/10.31219/osf.io/4n86q
#'
#' @return a simulated data set with \code{N} rows. Can be split into 'development' and 'validation' sets.
#' @export
#'
#' @examples
#' library(pmcalibration)
#' # simulate some data with a binary outcome
#' n <- 500
#' dat <- sim_dat(N = n, a1 = .5, a3 = .2)
#'
#' head(dat) # LP = linear predictor
#'
sim_dat = function(N, a1, a2=NULL, a3=NULL){

  x1 <- rnorm(N)
  if (is.null(a3)){
    # m1
    LP <- a1 + x1 + a2*x1^2
    y <- rbinom(n = N, size = 1, prob = invlogit(LP))
    d <- data.frame(x1, y, LP)
  } else{
    # m2
    if (!is.null(a2)) warning("value of a3 is given so a2 will be ignored...")
    x2 <- rnorm(N)
    LP <- a1 + x1 + x2 + x1*x2*a3
    y <- rbinom(n = N, size = 1, prob = invlogit(LP))
    d <- data.frame(x1, x2, y, LP)
  }
  return(d)
}
