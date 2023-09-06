#' Bootstrap resample a calibration curve object
#'
#' @param cal an object created using one of the \code{cal} functions
#'
#' @return bootstrap resamples of calibration metrics and values for plotting
#' @keywords internal
#' @export
boot <- function(cal){
  UseMethod("boot", cal)
}

#' @rdname boot
#' @export
boot.glm_cal <- function(cal){
  # y <- cal$y; p <- cal$p; X <- cal$X; Xp <- cal$Xp
  #
  # i <- sample(1:length(y), replace = TRUE)
  #
  # b <- glm_cal(y = y[i], p = p[i], X = X[i, ], Xp = Xp, save_data=FALSE, save_mod = FALSE)

  y <- cal$y; p <- cal$p; x <- cal$x; xp <- cal$xp; time <- cal$time

  i <- sample(1:length(y), replace = TRUE)

  args <- list(
    y = y[i], p = p[i], x = x[i], xp = xp, time=time, save_data=FALSE, save_mod=FALSE
  )

  args <- c(args, cal$smooth_args)

  b <- do.call(glm_cal, args)

  return(b)
}

#' @rdname boot
#' @export
boot.gam_cal <- function(cal){
  y <- cal$y; p <- cal$p; x <- cal$x; xp <- cal$xp; time <- cal$time

  i <- sample(1:length(y), replace = TRUE)

  args <- list(
    y = y[i], p = p[i], x = x[i], xp = xp, time=time, save_data=FALSE, save_mod=FALSE
  )

  args <- c(args, cal$smooth_args)

  b <- do.call(gam_cal, args)

  return(b)
}

#' @rdname boot
#' @export
boot.lowess_cal <- function(cal){
  y <- cal$y; p <- cal$p; x <- cal$x; xp <- cal$xp

  i <- sample(1:length(y), replace = TRUE)

  b <- lowess_cal(y = y[i], p = p[i], x = x[i], xp = xp, save_data=FALSE)

  return(b)
}

#' @rdname boot
#' @export
boot.loess_cal <- function(cal){
  y <- cal$y; p <- cal$p; x <- cal$x; xp <- cal$xp

  i <- sample(1:length(y), replace = TRUE)

  b <- loess_cal(y = y[i], p = p[i], x = x[i], xp = xp, save_data = FALSE, save_mod = FALSE)

  return(b)
}


#' Wrapper to run bootstrap resamples using \code{parallel}
#'
#' @param cal an object created with one of the \code{_cal} functions
#' @param R number of resamples (default = 1000)
#' @param cores number of cores (for \code{parallel})
#'
#' @return a list created by one of the \code{boot.} functions
#' @keywords internal
#' @export
run_boots <- function(cal, R = 1000, cores=1){

  cl <- parallel::makeCluster(cores)
  # replace with pbapply?
  parallel::clusterExport(cl, varlist = c("cal", "R"),
                          envir = environment())
  # out <- parallel::parLapply(cl, 1:R, function(i) boot(cal = cal))
  out <- pbapply::pblapply(seq(R), function(i) boot(cal = cal), cl = cl)
  parallel::stopCluster(cl)
  closeAllConnections()

  return(out)
}
