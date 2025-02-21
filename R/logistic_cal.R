
#' Run logistic calibration
#'
#' @description
#' Fit the models required to assess calibration in the large (calibration intercept), calibration slope, and overall
#' 'weak' calibration (see, e.g., Van Calster et al. 2019). Fits the models required to do the
#' three likelihood ratio tests described by Miller et al. (1993) (see \code{summary.logistic_cal}).
#'
#' @param y binary outcome
#' @param p predicted probabilities (these will be logit transformed)
#'
#' @references Van Calster, B., McLernon, D. J., Van Smeden, M., Wynants, L., & Steyerberg, E. W. (2019). Calibration: the Achilles heel of predictive analytics. BMC medicine, 17(1), 1-7.
#' @references Miller, M. E., Langefeld, C. D., Tierney, W. M., Hui, S. L., & McDonald, C. J. (1993). Validation of probabilistic predictions. Medical Decision Making, 13(1), 49-57.
#'
#' @return an object of class \code{logistic_cal} containing \code{glm} results for calculating calibration intercept, calibration slope, and LRTs.
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

  if (length(y) != length(p)) stop("y and p are not the same length")

  if (any(is.na(p)) | any(is.na(y))){
    i <- which(is.na(p) | is.na(y))
    y <- y[-i]; p <- p[-i]
    message(sprintf("%i records with missing values were removed", length(i)))
  }

  if (any(p <= 0 | p >= 1)){
    i <- which(p <= 0 | p >= 1)
    y <- y[-i]; p <- p[-i]
    warning(sprintf("%i records with p <= 0 or p >= 1 were removed. ", length(i)),
            "This suggests a potential problem with the model fit. ",
            "Excluding these observations (or replacing them with values close to 0 or 1) will affect results so interpret with caution!")
  }

  LP <- logit(p)

  # fit the three models to get the LRTs described in Miller et al 93
  m_fixed <- glm(y ~ 0 + offset(LP), family = binomial(link = "logit"))
  m_int <- glm(y ~ 1 + offset(LP), family = binomial(link = "logit"))
  m_intslo <- glm(y ~ 1 + LP, family = binomial(link = "logit"))

  out <- list(
    calibration_intercept = m_int,
    calibration_slope = m_intslo,
    m_fixed = m_fixed
  )
  class(out) <- "logistic_cal"
  return(out)
}

#' Summarize a logistic_cal object
#'
#' @param object a \code{logistic_cal} object
#' @param conf_level width of the confidence interval (0.95 gives 95\% CI)
#' @param ... ignored
#'
#' @return estimates and conf_level*100 confidence intervals for calibration intercept and calibration slope.
#' The former is estimated from a \code{glm} (family = binomial("logit")) where the linear predictor (logit(p)) is included as an offset.
#' Results of the three likelihood ratio tests described by Miller et al. (2013) (see details).
#'
#' @details
#' The likelihood ratio tests proposed by Miller et al. test the following: The first assesses weak calibration
#' overall by testing the null hypothesis that the intercept (a) and slope (b) are equal to 0 and 1, respectively.
#' The second assesses calibration in the large and tests the intercept against 0 with the slope fixed to 1. The third test
#' assesses the calibration slope after correcting for calibration in the large (by estimating a new intercept term). Note the
#' p-values from the calibration intercept and calibration slope estimates will typically agree with the p-values from
#' the second and third likelihood ratio tests but will not always match perfectly as the former are based on z-statistics and
#' the latter are based on log likelihood differences (chi-squared statistics).
#'
#' @references Miller, M. E., Langefeld, C. D., Tierney, W. M., Hui, S. L., & McDonald, C. J. (1993). Validation of probabilistic predictions. Medical Decision Making, 13(1), 49-57.
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

  # miller LRT
  # 1. H_0: a = 0 & b = 1 - overall test
  # 2. H_0: a = 0 | b = 1 - test of calibration in large after refinement (recalibration using slope)
  # 3. H_0: b = 1 | a - test of slope given correction of calibration in large
  LL <- function(x) as.numeric(logLik(x))

  stat1 <- -2*(LL(x$m_fixed) - LL(x$calibration_slope))
  p1 <- pchisq(stat1, df = 2, lower.tail = FALSE)

  stat2 <- -2*(LL(x$m_fixed) - LL(x$calibration_intercept))
  p2 <- pchisq(stat2, df = 1, lower.tail = FALSE)

  stat3 <- -2*(LL(x$calibration_intercept) - LL(x$calibration_slope))
  p3 <- pchisq(stat3, df = 1, lower.tail = FALSE)

  miller <- data.frame(statistic = c(stat1, stat2, stat3),
                       df = c(2, 1, 1),
                       p.value = c(p1, p2, p3),
                       row.names = c("Weak calibration - H0: a = 0, b = 1",
                                     "Calibration in the large - H0: a = 0 | b = 1",
                                     "Calibration slope - H0: b = 1 | a"))

  out <- list(stats = c_tab, conf_level = conf_level, miller = miller)

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

  cat("\n\nLikelihood ratio tests (a = intercept, b = slope):\n\n")
  miller <- x$miller
  miller$p.value <- format.pval(miller$p.value, digits = digits, eps = 0.001)
  colnames(miller)[3] <- "Pr(>Chi)"
  miller$df <- as.character(miller$df)
  print(format.data.frame(miller, digits=digits, nsmall=digits))

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
