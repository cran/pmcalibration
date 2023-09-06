#' fits a calibration curve via gam
#'
#' @param y binary or a time-to-event (\code{Surv}) outcome. For the former \code{family = binomial(link="logit")} and for the latter \code{family = mgcv::cox.ph()}.
#' @param p predicted probabilities
#' @param x predictor (could be transformation of \code{p})
#' @param xp values for plotting (same scale as \code{x})
#' @param time time to calculate survival probabilities at (only relevant if \code{y} is a \code{Surv} object)
#' @param save_data whether to save the data elements in the returned object
#' @param save_mod whether to save the model in the returned object
#' @param pw save pointwise standard errors for plotting
#' @param ... additional arguments for \code{mgcv::gam} and \code{mgcv::s}
#'
#' @returns list of class \code{gam_cal}
#' @keywords internal
#' @export
#' @examples
#' library(pmcalibration)
#' # simulate some data
#' n <- 500
#' dat <- sim_dat(N = n, a1 = .5, a3 = .2)
#' head(dat)
#' # predictions
#' p <- with(dat, invlogit(.5 + x1 + x2 + x1*x2*.1))
#'
#' gam_cal(y = dat$y, p = p, x = p, xp = NULL, k = 20, method="REML")
gam_cal <- function(y, p, x, xp, time=NULL, save_data = TRUE, save_mod = TRUE, pw = FALSE, ...){

  dots <- list(...)
  if ("bs" %in% names(dots)) bs <- dots[['bs']] else bs <- "tp"
  if ("k" %in% names(dots)) k <- dots[['k']] else k <- -1
  if ("fx" %in% names(dots)) fx <- dots[['fx']] else fx <- FALSE
  if ("method" %in% names(dots)) method <- dots[['method']] else method <- "GCV.Cp"

  surv <- is(y, "Surv")

  # fit the calibration curve model
  if (surv){
    times <- y[, 1]
    events <- y[, 2]
    d <- data.frame(times, events, x)

    mod <- mgcv::gam(times ~ s(x, k = k, fx = fx, bs = bs), data = d,
                     family = mgcv::cox.ph(),
                     weights = events,
                     method = method)

    p_c = 1 - as.vector(predict(mod, type = "response", newdata = data.frame(times = time, x=x)))
  } else{
    d <- data.frame(y, x)

    mod <- mgcv::gam(y ~ s(x, k = k, fx = fx, bs = bs), data = d,
                     family = binomial(link="logit"),
                     method = method)

    p_c <- as.vector(predict(mod, type = "response"))
  }

  if (!is.null(xp)){
    if (pw){
      if (surv){
        p_c_p <- predict(mod, type = "response", newdata = data.frame(times = time, x = xp), se.fit=TRUE)
        p_c_plot <- 1 - as.vector(p_c_p$fit)
      } else{
        p_c_p <- predict(mod, newdata = data.frame(x = xp), type = "response", se.fit = TRUE)
        p_c_plot <- as.vector(p_c_p$fit)
      }
      p_c_plot_se <- as.vector(p_c_p$se)

    } else{
      if (surv){
        p_c_plot <- 1 - as.vector(predict(mod, type = "response",
                                       newdata = data.frame(times = time, x = xp)))
      } else{
        p_c_plot <- as.vector(predict(mod, newdata = data.frame(x = xp), type = "response"))
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
    p_c = p_c,
    metrics = cal_metrics(p, p_c),
    p_c_plot = p_c_plot,
    p_c_plot_se = p_c_plot_se,
    model = if (save_mod) mod else NULL,
    smooth_args = list(
      smooth = "gam",
      bs = bs,
      k = k,
      fx = fx,
      method = method
    ),
    time = time,
    outcome = ifelse(surv, "tte", "binary")
  )

  class(out) <- "gam_cal"
  return(out)
}

