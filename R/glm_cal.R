#' fits a calibration curve via glm or Cox proportional hazards model
#'
#' @param y binary or a time-to-event (\code{Surv}) outcome. Former is fit via \code{glm} and latter is fit via \code{survival::coxph}.
#' @param p predicted probabilities
#' @param x predictor (could be transformation of \code{p})
#' @param xp values for plotting (same scale as \code{x})
#' @param smooth 'rcs', 'ns', 'bs', or 'none'
#' @param time time to calculate survival probabilities at (only relevant if \code{y} is a \code{Surv} object)
#' @param save_data whether to save the data elements in the returned object
#' @param save_mod whether to save the model in the returned object
#' @param pw save pointwise standard errors for plotting
#'
#' @returns list of class \code{glm_cal}
#' @keywords internal
#' @export
#' @examples
#' library(pmcalibration)
#' # simulate some data
#' n <- 500
#' dat <- sim_dat(N = n, a1 = .5, a3 = .2)
#'
#' # predictions
#' p <- with(dat, invlogit(.5 + x1 + x2 + x1*x2*.1))
#'
#' glm_cal(y = dat$y, p = p, x = p, xp = NULL, smooth="ns", df=5)
glm_cal <- function(y, p, x, xp, smooth, time=NULL, save_data = TRUE, save_mod = TRUE, pw = FALSE, ...){

  surv <- is(y, "Surv")

  if ( surv & (is.null(time) || length(time) > 1) ) {
    stop("if y is a Surv object a single time must be specified")
  }

  XX <- reg_spline_X(x = x, xp = xp, smooth = smooth, ...)
  X <- XX$X
  Xp <- XX$Xp

  # fit the calibration curve model
  if (surv){
    times <- y[, 1]
    events <- y[, 2]
    d <- data.frame(times, events, X)
    mod <- survival::coxph(survival::Surv(times, events) ~ ., data = d, x = TRUE)
    p_c <- 1 - exp(-predict(mod, type = "expected",
                                             newdata = data.frame(times=time, events=1, X)))
  } else{
    d <- data.frame(y, X)
    mod <- glm(y ~ ., data = d, family = binomial(link="logit"))
    p_c <- predict(mod, type = "response")
  }

  if (!is.null(Xp)){
    if (pw){
      if (surv){
        p_c_p <- predict(mod, type = "expected",
                         newdata = data.frame(times=time, events=1, Xp), se.fit = TRUE)
        # don't do 1 - exp(-S) until summary
      } else{
        p_c_p <- predict(mod, newdata = Xp, type = "response", se.fit = TRUE)
      }
      p_c_plot <- p_c_p$fit
      p_c_plot_se <- p_c_p$se
    } else{
      if (surv){
        p_c_plot <- 1 - exp(-predict(mod, type = "expected",
                            newdata = data.frame(times=time, events=1, Xp)))
      } else{
        p_c_plot <- predict(mod, newdata = Xp, type = "response")
      }
      p_c_plot_se <- NULL
    }
  } else{
    p_c_plot <- NULL
    p_c_plot_se <- NULL
  }

  out <- list(
    y = if (save_data) y else NULL,
    p = if (save_data) p else NULL,
    x = if (save_data) x else NULL,
    xp = if (save_data) xp else NULL,
    X = if (save_data) X else NULL,
    Xp = if (save_data) Xp else NULL,
    p_c = p_c,
    metrics = cal_metrics(p, p_c),
    p_c_plot = p_c_plot,
    p_c_plot_se = p_c_plot_se,
    model = if (save_mod) mod else NULL,
    smooth_args = XX$smooth_args,
    time = time,
    outcome = ifelse(surv, "tte", "binary")
  )

  class(out) <- "glm_cal"
  return(out)
}

#' Run logistic calibration
#'
#' @description
#' Assess 'weak' calibration (see, e.g., Van Calster et al. 2019) via calibration intercept
#' and calibration slope.
#'
#' @param y binary outcome
#' @param p predicted probabilities (these will be logit transformed)
#'
#' @references Van Calster, B., McLernon, D. J., Van Smeden, M., Wynants, L., & Steyerberg, E. W. (2019). Calibration: the Achilles heel of predictive analytics. BMC medicine, 17(1), 1-7.
#'
#' @return an object of class \code{logistic_cal} containing \code{glm} results for calculating calibration intercept and calibration slope
#'
#' @export
#' @examples
#' library(pmcalibration)
#' # simulate some data
#' n <- 500
#' dat <- sim_dat(N = n, a1 = .5, a3 = .2)
#'
#' # predictions
#' p <- with(dat, invlogit(.5 + x1 + x2 + x1*x2*.1))
#'
#' logistic_cal(y = dat$y, p = p)
logistic_cal <- function(y, p){
  LP <- logit(p)

  lcal_int <- glm(y ~ 1 + offset(LP), family = binomial(link = "logit"))

  lcal_slo <- glm(y ~ 1 + LP, family = binomial(link = "logit"))

  out <- list(
    calibration_intercept = lcal_int,
    calibration_slope = lcal_slo
  )
  class(out) <- "logistic_cal"
  return(out)
}
