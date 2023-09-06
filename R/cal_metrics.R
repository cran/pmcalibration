#' Calculate calibration metrics from calibration curve
#'
#' @description
#' Calculates metrics used for summarizing calibration curves. See Austin and Steyerberg (2019)
#'
#' @param p predicted probabilities
#' @param p_c probabilities from the calibration curve
#'
#' @return a named vector of metrics based on absolute difference between predicted and calibration curve implied probabilities \code{d = abs(p - p_c)}
#' \itemize{
#'  \item{Eavg - average absolute difference (aka integrated calibration index or ICI)}
#'  \item{E50 - median absolute difference}
#'  \item{E90 - 90th percentile absolute difference}
#'  \item{Emax - maximum absolute difference}
#'  \item{ECI - average squared difference. Estimated calibration index (Van Hoorde et al. 2015)}
#' }
#'
#' @references Austin PC, Steyerberg EW. (2019) The Integrated Calibration Index (ICI) and related metrics for quantifying the calibration of logistic regression models. \emph{Statistics in Medicine}. 38, pp. 1–15. https://doi.org/10.1002/sim.8281
#' @references Van Hoorde, K., Van Huffel, S., Timmerman, D., Bourne, T., Van Calster, B. (2015). A spline-based tool to assess and visualize the calibration of multiclass risk predictions. \emph{Journal of Biomedical Informatics}, 54, pp. 283-93
#' @references Van Calster, B., Nieboer, D., Vergouwe, Y., De Cock, B., Pencina M., Steyerberg E.W. (2016). A calibration hierarchy for risk models was defined: from utopia to empirical data. \emph{Journal of Clinical Epidemiology}, 74, pp. 167-176
#' @export
#' @examples
#' library(pmcalibration)
#'
#' LP <- rnorm(100) # linear predictor
#' p_c <- invlogit(LP) # actual probabilities
#' p <- invlogit(LP*1.3) # predicted probabilities that are miscalibrated
#'
#' cal_metrics(p = p, p_c = p_c)
cal_metrics <- function(p, p_c){
  # code modified from the Austin and Steyerberg (2019, SiM) paper
  stopifnot(length(p) == length(p_c))

  if (any(is.na(p_c) | is.infinite(p_c))){
    d <- NA
    message("some p_c's are missing or Inf")
  } else{
    d <- abs(p - p_c)
  }

  m <- c('Eavg' = mean(d),
        'E50' = median(d),
        'E90' = if (any(is.na(d))) NA else unname(quantile(d, .9)),
        'Emax' = max(d),
        'ECI' = mean(d^2)*100
        )

  return(m)
}
