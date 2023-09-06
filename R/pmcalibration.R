#' Create a calibration curve
#'
#' @description
#' Assess calibration of clinical prediction models (agreement between predicted and observed probabilities) via different smooths. Binary and time-to-event outcomes are supported.
#'
#' @param y a binary or a right-censored time-to-event outcome. Latter must be an object created via \code{survival::Surv}.
#' @param p predicted probabilities from a clinical prediction model. For a time-to-event object \code{time} must be specified and \code{p} are predicted probabilities of the outcome happening by \code{time} units of time follow-up.
#' @param smooth what smooth to use. Available options:
#' \itemize{
#' \item{'rcs' = restricted cubic spline using \code{rms::rcs}. Optional arguments for this smooth are \code{nk} (number of knots; defaults to 5) and \code{knots} (knot positions; set by \code{Hmisc::rcs.eval} if not specified) }
#' \item{'ns' = natural spline using \code{splines::ns}. Optional arguments are \code{df} (default = 6), \code{knots}, \code{Boundary.knots} (see \code{?splines::ns}) }
#' \item{'bs' = B-spline using \code{splines::bs}. Optional arguments are \code{df} (default = 6), \code{knots}, \code{Boundary.knots} (see \code{?splines::bs}) }
#' \item{'gam' = generalized additive model via \code{mgcv::gam} and \code{mgcv::s}. Optional arguments are \code{bs}, \code{k}, \code{fx}, \code{method} (see \code{?mgcv::gam} and  \code{?mgcv::s}) }
#' \item{'lowess' = uses \code{lowess(x, y, iter = 0)} based on \code{rms::calibrate}. Only for binary outcomes.}
#' \item{'loess' = uses \code{loess} with all defaults. Only for binary outcomes. }
#' \item{'none' = logistic or Cox regression with single predictor variable (for binary outcome performs logistic calibration when \code{transf = "logit"}). See \code{\link{logistic_cal}} }
#' }
#' 'rcs', 'ns', 'bs', and 'none' are fit via \code{glm} or \code{survival::coxph} and 'gam' is fit via \code{mgcv::gam} with \code{family = Binomial(link="logit")} for a binary outcome or \code{mgcv::cox.ph} when \code{y} is time-to-event.
#' @param time what follow up time do the predicted probabilities correspond to? Only used if \code{y} is a \code{Surv} object
#' @param ci what kind of confidence intervals to compute?
#' \itemize{
#' \item{'sim' = simulation based inference. Note this is currently only available for binary outcomes. \code{n} samples are taken from a multivariate normal distribution with mean vector = coef(mod) and variance covariance = vcov(model).}
#' \item{'boot' = bootstrap resampling with \code{n} replicates. \code{y} and \code{p} are sampled with replacement and the calibration curve is reestimated. If \code{knots} are specified the same knots are used for each resample (otherwise they are calculated using resampled \code{p} or transformation thereof)}
#' \item{'pw' = pointwise confidence intervals calculated via the standard errors produced by relevant \code{predict} methods. Only for plotting curves; if selected, CIs are not produced for metrics (not available for smooth = 'lowess')}
#' }
#' Calibration metrics are calculated using each simulation or boot sample. For both options percentile confidence intervals are returned.
#' @param n number of simulations or bootstrap resamples
#' @param transf transformation to be applied to \code{p} prior to fitting calibration curve. Valid options are 'logit', 'cloglog', 'none', or a function (must retain order of \code{p}). If unspecified defaults to 'logit' for binary outcomes and 'cloglog' (complementary log-log) for time-to-event outcomes.
#' @param eval number of points (equally spaced between \code{min(p)} and \code{max(p)}) to evaluate for plotting (0 or NULL = no plotting). Can be a vector of probabilities.
#' @param ... additional arguments for particular smooths. For ci = 'boot' the user is able to run samples in parallel (using the \code{parallel} package) by specifying a \code{cores} argument
#'
#' @references Austin P. C., Steyerberg E. W. (2019) The Integrated Calibration Index (ICI) and related metrics for quantifying the calibration of logistic regression models. \emph{Statistics in Medicine}. 38, pp. 1–15. https://doi.org/10.1002/sim.8281
#' @references Van Calster, B., Nieboer, D., Vergouwe, Y., De Cock, B., Pencina M., Steyerberg E.W. (2016). A calibration hierarchy for risk models was defined: from utopia to empirical data. \emph{Journal of Clinical Epidemiology}, 74, pp. 167-176. https://doi.org/10.1016/j.jclinepi.2015.12.005
#' @references Austin, P. C., Harrell Jr, F. E., & van Klaveren, D. (2020). Graphical calibration curves and the integrated calibration index (ICI) for survival models. \emph{Statistics in Medicine}, 39(21), 2714-2742. https://doi.org/10.1002/sim.8570
#'
#' @returns a \code{pmcalibration} object containing calibration metrics and values for plotting
#' @export
#'
#' @examples
#' # binary outcome -------------------------------------
#' library(pmcalibration)
#' # simulate some data
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
#'
#' plot(cal)
#'
#' # time to event outcome -------------------------------------
#' library(pmcalibration)
#' if (requireNamespace("survival", quietly = TRUE)){
#' library(survival)
#'
#' data('transplant', package="survival")
#' transplant <- na.omit(transplant)
#' transplant = subset(transplant, futime > 0)
#' transplant$ltx <- as.numeric(transplant$event == "ltx")
#'
#' # get predictions from coxph model at time = 100
#' # note that as we are fitting and evaluating the model on the same data
#' # this is internal calibration (see vignette("internal-validation", package = "pmcalibration"))
#' cph <- coxph(Surv(futime, ltx) ~ age + sex + abo + year, data = transplant)
#'
#' time <- 100
#' newd <- transplant; newd$futime <- time; newd$ltx <- 1
#' p <- 1 - exp(-predict(cph, type = "expected", newdata=newd))
#' y <- with(transplant, Surv(futime, ltx))
#'
#' cal <- pmcalibration(y = y, p = p, smooth = "rcs", nk=5, ci = "pw", time = time)
#'
#' summary(cal)
#'
#' plot(cal)
#'
#' }
pmcalibration <- function(y, p,
                          smooth=c("none", "ns", "bs", "rcs", "gam", "lowess", "loess"),
                          time = NULL,
                          ci = c("sim", "boot", "pw", "none"),
                          n=1000,
                          transf = NULL,
                          eval=100, ...){

  # TODO
  # - survival methods (DONE glm, gam; todo - hare, flexsurv?)
  # - competing risks? Austin, P. C., Putter, H., Giardiello, D., & van Klaveren, D. (2022). Graphical calibration curves and the integrated calibration index (ICI) for competing risk models. Diagnostic and prognostic research, 6(1), 2.
  call <- match.call()
  dots <- list(...)

  chk::vld_compatible_lengths(y, p)

  if (any(is.na(p)) | any(is.na(y))){
    i <- which(is.na(p) | is.na(y))
    y <- y[-i]; p <- p[-i]
    message(sprintf("%i records with missing values were removed", length(i)))
  }

  smooth <- match.arg(smooth)
  ci <- match.arg(ci)

  surv <- is(y, "Surv")

  if (surv & ci == "sim") stop("Simulation based inference not available for time-to-event outcomes")
  if (surv & smooth %in% c("lowess", "loess")) stop("smooth = 'lowess' or 'loess' not available for time-to-event outcomes")

  if (ci == "boot"){
    if ("cores" %in% names(dots)){
      cores <- dots[["cores"]]
    } else{
      cores <- 1
    }
  }

  if (length(eval) > 1){
    if (any(eval < 0) | any(eval > 1)){
      stop("When providing p for plotting directly via eval, all elements should be between 0 and 1")
    }
    pplot <- eval
  } else{
    if (!is.null(eval) && eval > 0){
      pplot <- seq(min(p), max(p), length.out = eval)
    } else{
      pplot <- NULL
    }
  }

  pw <- ci == "pw"
  if (is.null(pplot) & pw){
    warning("ci = 'pw' but eval = 0 or is.null. Will not calculate pointwise standard errors")
    pw <- FALSE
  }

  if (is.null(transf)){
    if (surv){
      transf <- "cloglog"
    } else{
      transf <- "logit"
    }
  }

  if (is.function(transf)){
    if (length(formals) > 1) stop("transf should just take one argument")
    tfun <- transf
  } else{
    if (!transf %in% c("none", "logit", "cloglog")) {
      stop("invalid transf. Should be 'none', 'logit', 'cloglog' or a function")
    }
    if (transf == "none"){
      tfun <- function(x) x
    } else if (transf == "logit"){
      tfun <- logit
    } else if (transf == "cloglog"){
      tfun <- function(x) log(-log(1 - x))
    }
  }
  xp <- if (!is.null(pplot)) tfun(pplot) else NULL
  x <- tfun(p)

  # fit cc
  if (smooth %in% c("none", "ns", "bs", "rcs")){
    cal <- glm_cal(y = y, p = p, x = x, xp = xp, smooth = smooth, time=time,
                   save_data = TRUE, save_mod = TRUE, pw = pw, ...)
  } else if (smooth == "lowess"){
    if (pw) warning("ci = 'pw' is ignored for smooth = 'lowess'")
    cal <- lowess_cal(y = y, p = p, x = x, xp = xp, save_data = TRUE)
  } else if (smooth == "loess"){
    cal <- loess_cal(y = y, p = p, x = x, xp = xp, save_data = TRUE, save_mod = TRUE, pw = pw)
  } else if (smooth == "gam"){
    cal <- gam_cal(y = y, p = p, x = x, xp = xp, time=time,
                   save_data = TRUE, save_mod = TRUE, pw = pw, ...)
  }

  if (ci == "boot"){
    b.cal <- run_boots(cal, R = n, cores = cores)
  } else if (ci == "sim"){
    b.cal <- simb(cal, R = n)
  } else{
    b.cal <- NULL
  }

  if (!is.null(b.cal)){
    #extract samples
    metrics.samples <- do.call(rbind, lapply(b.cal, function(samp) samp$metrics))
    if (any(is.na(metrics.samples))){
      warning("Metrics samples contain NAs. This is probably due to a loess not extrapolating for to-be-plotted values...")
    }

    if (!is.null(b.cal[[1]]$p_c_plot)){
      plot.samples <- do.call(rbind, lapply(b.cal, function(samp) samp$p_c_plot))
      if (any(is.na(plot.samples))){
        warning("Plot samples contain NAs. This is probably due to a loess not extrapolating for to-be-plotted values...")
      }
    } else{
      plot.samples <- NULL
    }
  } else{
    metrics.samples <- NULL
    plot.samples <- NULL
  }

  out <- list(
    call = call,
    metrics = cal$metrics,
    metrics.samples = metrics.samples,
    plot = list(
      p = pplot,
      p_c_plot = cal$p_c_plot,
      p_c_plot_se = cal$p_c_plot_se,
      plot.samples = plot.samples
    ),
    smooth = smooth,
    ci = ci,
    n = n,
    smooth_args = cal$smooth_args,
    transf = transf,
    time = time,
    outcome = ifelse(surv, "tte", "binary")
  )

  class(out) <- "pmcalibration"

  return(out)

}
