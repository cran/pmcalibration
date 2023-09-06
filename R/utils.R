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

  checkmate::assert_double(conf_level, len = 1)

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


#' Summarize a logistic_cal object
#'
#' @param object a \code{logistic_cal} object
#' @param conf_level width of the confidence interval (0.95 gives 95\% CI)
#' @param ... ignored
#'
#' @return estimates and conf_level*100 confidence intervals for calibration intercept and calibration slope.
#' The former is estimated from a \code{glm} (family = binomial("logit")) where the linear predictor (logit(p)) is included as an offset.
#'
#' @export
summary.logistic_cal <- function(object, conf_level = .95, ...){
  x <- object

  ci_ci <- suppressMessages(confint(x$calibration_intercept, level = conf_level)) # profile cis
  cs_ci <- suppressMessages(confint(x$calibration_slope, level = conf_level))
  cs_ci <- cs_ci["LP", ]

  names(ci_ci) <- names(cs_ci) <- c('lower', "upper")

  ci_s <- summary.glm(x$calibration_intercept)
  cs_s <- summary.glm(x$calibration_slope)

  ci_s <- ci_s$coefficients
  cs_s <- cs_s$coefficients[2, ]

  cs_s[[3]] <- (cs_s[[1]] - 1)/cs_s[[2]]

  cs_s[[4]] <- 2*pnorm(q = abs(cs_s[[3]]), lower.tail = FALSE)

  c_tab <- rbind(
    cbind(ci_s, t(ci_ci)),
    cbind(t(cs_s), t(cs_ci))
  )

  rownames(c_tab) <- c("Calibration Intercept", "Calibration Slope")

  c_tab <- as.data.frame(c_tab)

  out <- list(stats = c_tab, conf_level = conf_level)

  class(out) <- c("logistic_calsummary")

  return(out)
}

#' Print a logistic_cal summary
#'
#' @param x a \code{logistic_calsummary} object
#' @param digits number of digits to print
#' @param ... ignored
#'
#' @returns prints a summary
#'
#' @rdname print.logistic_calsummary
#' @export
print.logistic_calsummary <- function(x, digits=2, ...){
  stats <- x$stats
  stats$`Pr(>|z|)` <- format.pval(stats$`Pr(>|z|)`, digits = digits, eps = 0.001)

  cat("Logistic calibration intercept and slope:\n\n")

  #print.data.frame(out, digits=digits)
  print(format.data.frame(stats, digits=digits, nsmall=digits))
  cat("\n")
  cat("z-value for calibration slope is relative to slope = 1.\n")
  cat("lower and upper are the bounds of", sprintf("%.0f%%", x$conf_level*100), "profile confidence intervals.")

  invisible(x)
}

#' Print a \code{logistic_cal} object
#'
#' @param x a \code{logistic_cal} object
#' @param digits number of digits to print
#' @param conf_level width of the confidence interval (0.95 gives 95\% CI)
#' @param ... optional arguments passed to print
#'
#' @returns prints a summary
#'
#' @rdname print.logistic_cal
#' @export
print.logistic_cal <- function(x, digits = 2, conf_level = .95, ...) {
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
#' cplot <- get_cc(cal, conf_level = .95)
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
get_cc <- function(x, conf_level = .95){
  cc <- summary(x, conf_level = conf_level)$plot
  return(cc)
}

#' Plot a calibration curve (\code{pmcalibration} object)
#'
#' @description
#' This is for a quick and dirty calibration curve plot.
#' Alternatively you can use \code{get_cc()} to get the data required to plot the calibration curve.
#'
#' @param x a \code{pmcalibration} calibration curve
#' @param conf_level width of the confidence interval (0.95 gives 95\% CI). Ignored if call to \code{pmcalibration} didn't request confidence intervals
#' @param ... other args for \code{plot()} (\code{lim} and \code{lab} can be specified)
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
#' cal <- pmcalibration(y = dat$y, p = p, smooth = "gam", k = 20, ci = "pw")
#'
#' plot(cal)
plot.pmcalibration <- function(x, conf_level = .95, ...){

  dots <- list(...)

  # pdat <- summary.pmcalibration(x, conf_level = conf_level)
  # pdat <- pdat$plot
  pdat <- get_cc(x, conf_level = conf_level)

  if ("xlim" %in% names(dots)) xlim <- dots[['xlim']] else xlim <- c(0,1)
  if ("ylim" %in% names(dots)) ylim <- dots[['ylim']] else ylim <- c(0,1)
  if ("xlab" %in% names(dots)) xlab <- dots[['xlab']] else xlab <- "Predicted Probability"
  if ("ylab" %in% names(dots)) ylab <- dots[['ylab']] else ylab <- "Estimated Probability"

  plot(x = pdat$p, y = pdat$p_c, type="l",
       xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab)
  abline(0, 1, lty=2, col="grey")

  if ("lower" %in% colnames(pdat)){
    lines(x = pdat$p, y = pdat$lower, lty=2)
    lines(x = pdat$p, y = pdat$upper, lty=2)
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
