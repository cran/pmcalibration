#' calibration curve via \code{loess}
#'
#' @param y binary outcome
#' @param p predicted probabilities
#' @param x predictor (could be transformation of \code{p})
#' @param xp values for plotting (same scale as \code{x})
#' @param save_data whether to save y, p, x, xp in the returned object
#' @param save_mod whether to save the model in the returned object
#' @param pw save pointwise standard errors for plotting
#'
#' @returns list of class \code{loess_cal}
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
#' loess_cal(y = dat$y, p = p, x = p, xp = NULL)
loess_cal <- function(p, y, x, xp, save_data = TRUE, save_mod = TRUE, pw = FALSE){

  mod <- loess(y ~ x)
  # TODO
  # explore loess options more fully...
  # control = loess.control(surface="direct")
  # would allow extrapolation on bs resamples but seems to take longer...

  p_c <- predict(mod, newdata = x)

  if (!is.null(xp)){
    if (pw){
      p_c_p <- predict(mod, newdata = data.frame(x = xp), se = TRUE)
      p_c_plot <- as.vector(p_c_p$fit)
      p_c_plot_se <- as.vector(p_c_p$se)
    } else{
      p_c_plot <- predict(mod, newdata = xp)
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
    smooth_args = list(smooth = "loess")
  )

  class(out) <- "loess_cal"
  return(out)
}
