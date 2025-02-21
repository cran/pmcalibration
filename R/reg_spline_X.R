#' Make a design matrix for regression spline
#'
#' @param x values of the predictor
#' @param xp values of the predictor for plotting the calibration curve
#' @param smooth spline to use (\code{rms::rcs}, \code{splines::ns}, \code{splines::bs} currently supported via 'rcs', 'ns', 'bs'). \code{smooth} = 'none' results in \code{x} as only predictor (i.e., no spline)
#' @param ... additional arguments for specific splines ('nk' or 'knots' for 'rcs', 'df' or 'knots' for 'ns' or 'bs')
#'
#' @returns a list containing
#' \itemize{
#' \item{\code{X} the design matrix for the data}
#' \item{\code{Xp} the design matrix for plotting}
#' }
#' @keywords internal
#' @export
#' @examples
#' x <- rnorm(100)
#' xp <- seq(min(x), max(x), length.out=50)
#' reg_spline_X(x = x, xp = xp, smooth="rcs", nk=6)
reg_spline_X <- function(x, xp, smooth, ...){
  dots <- list(...)

  # make matrix of predictors (i.e., spline basis functions, if any)
  if (smooth == "none"){
    X <- x #model.matrix(~x)
    Xp <- xp

    smooth_args <- list(smooth = "none")

  } else if (smooth == "rcs"){
    if ("nk" %in% names(dots)) nk <- dots[['nk']] else nk <- NULL
    if ("knots" %in% names(dots)) knots <- dots[['knots']] else knots <- NULL

    if (is.null(nk) & is.null(knots)){
      message("for smooth = 'rcs' either nk (number of knots) or knots must be provided. Defaulting to nk = 5. See ?Hmisc::rcspline.eval")
      nk <- 5
    }

    X <- Hmisc::rcspline.eval(x, nk = nk, knots = knots, inclx = TRUE)
    if (!is.null(xp)){
      Xp <- Hmisc::rcspline.eval(xp, knots = attr(X, "knots"), inclx = TRUE)
    } else{
      Xp <- NULL
    }

    smooth_args <- list(smooth = smooth,
                        nk = nk,
                        knots = knots, # knots specified by user
                        knots_used = attr(X, "knots"))

  } else if (smooth %in% c("ns", "bs")){
    if ("df" %in% names(dots)) df <- dots[['df']] else df <- NULL
    if ("knots" %in% names(dots)) knots <- dots[['knots']] else knots <- NULL
    if ("Boundary.knots" %in% names(dots)) Boundary.knots <- dots[['Boundary.knots']] else Boundary.knots <- NULL

    if (is.null(df) & is.null(knots)){
      message("for smooth = ns or bs either df or knots must be provided. Defaulting to df = 6")
      df <- 6
    }

    # could use getFromNamespace(x = smooth, ns = "splines")
    if (smooth == "ns"){
      X <- splines::ns(x, df = df,
                       knots = knots,
                       Boundary.knots = if (!is.null(Boundary.knots)) Boundary.knots else range(x),
                       intercept = FALSE)
    } else if (smooth == "bs"){
      X <- splines::bs(x, df = df,
                       knots = knots,
                       Boundary.knots = if (!is.null(Boundary.knots)) Boundary.knots else range(x),
                       intercept = FALSE)
    }
    if (!is.null(xp)){
      #Xp <- splines:::predict(X, newx = xp)
      Xp <- predict(X, newx = xp)
    } else{
      Xp <- NULL
    }

    smooth_args <- list(smooth = smooth,
                        df = df,
                        knots = knots, # knots specified by user
                        Boundary.knots = Boundary.knots, # set by user
                        knots_used = attr(X, "knots"),
                        Boundary.knots_used = attr(X, "Boundary.knots"))
  }

  X <- data.frame(X)
  colnames(X) <- paste0("V", 1:ncol(X))

  if (!is.null(Xp)){
    Xp <- data.frame(Xp)
    stopifnot(ncol(X) == ncol(Xp))
    colnames(Xp) <- colnames(X)
  }

  return(list(X = X, Xp = Xp, smooth_args = smooth_args))
}

