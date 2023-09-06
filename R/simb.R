#' Simulation based inference with a calibration curve object
#'
#' @param cal an object created using one of the \code{cal} functions
#' @param R number of simulated replicates
#'
#' @return simulated calibration metrics and values for plotting
#' @keywords internal
#' @export
simb <- function(cal, R){
  UseMethod("simb", cal)
}

#' @rdname simb
#' @export
simb.glm_cal <- function(cal, R = 1000){

  mod <- cal$model; y <- cal$y; p <- cal$p; X <- cal$X; Xp <- cal$Xp

  b <- coef(mod); Vb <- vcov(mod)

  B <- MASS::mvrnorm(n = R, mu = b, Sigma = Vb)

  out <- lapply(1:R, function(i){
    p_c <- as.numeric(invlogit( as.matrix(cbind(1, X)) %*% B[i, ]))

    if (!is.null(Xp)){
      p_c_plot <- as.numeric(invlogit( as.matrix(cbind(1, Xp)) %*% B[i, ]))
    } else{
      p_c_plot <- NULL
    }

    s <- list(
      p_c = p_c,
      metrics = cal_metrics(p, p_c),
      p_c_plot = p_c_plot
    )

    return(s)
  }
  )

  return(out)
}

#' @rdname simb
#' @export
simb.gam_cal <- function(cal, R = 1000){

  mod <- cal$model; y <- cal$y; p <- cal$p; x <- cal$x; xp <- cal$xp

  b <- coef(mod); Vb <- vcov(mod)

  B <- MASS::mvrnorm(n = R, mu = b, Sigma = Vb)

  X <- mgcv::predict.gam(mod, type = "lpmatrix")

  if (!is.null(xp)) {
    Xp <- mgcv::predict.gam(mod, newdata = data.frame(x = xp), type = "lpmatrix")
  }

  out <- lapply(1:R, function(i){
    p_c <- as.numeric(invlogit( X %*% B[i, ]))

    if (!is.null(Xp)){
      p_c_plot <- as.numeric(invlogit( Xp %*% B[i, ]))
    } else{
      p_c_plot <- NULL
    }

    s <- list(
      p_c = p_c,
      metrics = cal_metrics(p, p_c),
      p_c_plot = p_c_plot
    )

    return(s)
  }
  )

  return(out)
}

#' @rdname simb
#' @export
simb.lowess_cal <- function(cal, R){
  stop("ci = 'sim' is not available for lowess or loess calibration curves (try ci = 'boot')")
}

#' @rdname simb
#' @export
simb.loess_cal <- function(cal, R){
  stop("ci = 'sim' is not available for lowess or loess calibration curves (try ci = 'boot')")
}

