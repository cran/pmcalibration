## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(pmcalibration)
library(data.table)

n <- 1000
dat <- sim_dat(n, a1 = -3, a3 = .3)
# add some noise variables
dat$x3 <- rnorm(n)
dat$x4 <- rnorm(n)

mean(dat$y)

m1 <- glm(y ~ (x1 + x2 + x3 + x4)^2, data = dat, 
          family = binomial(link = "logit"))

p1 <- predict(m1, type="response")


## ---- fig.width=6, fig.height=5-----------------------------------------------
(cc_app <- pmcalibration(y = dat$y, p = p1, smooth = "gam", bs="tp", ci="none"))

plot(cc_app)

## -----------------------------------------------------------------------------
mod <- function(data){
  # function that implements the model development procedure
  glm(y ~ (x1 + x2 + x3 + x4)^2, data = data, 
          family = binomial(link = "logit"))
}

pred <- function(model, data){
  # function to get predictions
  predict.glm(object = model, newdata = data, type="response")
}

## -----------------------------------------------------------------------------
internal_cal <- function(model_fun, data, pred_fun, y="y", B=100){
  # assess apparent performance
  appmodel <- model_fun(data)
  p <- pred_fun(appmodel, data)
  pp <- seq(min(p), max(p), length.out=100) # for plotting
  apparent <- pmcalibration(y = data[[y]], p = p, smooth = "gam", ci = "none", neval = pp)
  
  # one bootstrap resample
  one_boot <- function(model, data, pred_fun){
    d <- data[sample(nrow(data), replace = T), ]
    #modelcall[["data"]] <- d
    
    # bootmodel <- eval(modelcall) # fit model on resampled data
    bootmodel <- model_fun(d)
    
    p_boot <- pred_fun(bootmodel, d) # evaluate on 'training' data (bootstrap resample)
    p_orig <- pred_fun(bootmodel, data) # evaluate on 'test' (original data)
    
    cc_boot <- pmcalibration(y = d[[y]], p = p_boot, smooth = "gam", ci = "none", neval = pp)
    cc_orig <- pmcalibration(y = data[[y]], p = p_orig, smooth = "gam", ci = "none", neval = pp)
    
    # calculate optimism and return
    metrics_optimism <- cc_boot$metrics - cc_orig$metrics
    plot_optimism <- cc_boot$plot$p_c_plot - cc_orig$plot$p_c_plot
    
    return(list(metrics_optimism, plot_optimism))
  }
  # run B bootstrap replicates (could be sped up via, e.g., pbapply)
  opt <- lapply(seq(B), function(i){
    one_boot(model = model, data = data, pred_fun = pred_fun)
  })
  # calculate average optimism for metrics and plot (to make bias corrected curve)
  metrics_opt <- rbindlist(lapply(opt, function(x) data.frame(t(x[[1]]))))
  metrics_opt <- apply(metrics_opt, 2, mean)
  plot_opt <- rbindlist(lapply(opt, function(x) data.frame(t(x[[2]]))))
  plot_opt <- apply(plot_opt, 2, mean)
  # return
  out <- list(
    metrics = data.frame(apparent = apparent$metrics, 
                         optimism = metrics_opt, 
                         bias_corrected = apparent$metrics - metrics_opt
    ),
    plot = data.frame(p = pp, 
                      apparent = apparent$plot$p_c_plot, 
                      optimism = plot_opt, 
                      bias_corrected = apparent$plot$p_c_plot - plot_opt
    )
  )
  
  return(out)
}


## ---- fig.width=6, fig.height=5-----------------------------------------------
iv <- internal_cal(model_fun = mod, data = dat, pred_fun = pred)

iv$metrics

plot(iv$plot$p, iv$plot$apparent, type="l", xlim=c(0,1), ylim=c(0,1), 
     xlab="Predicted", ylab="Observed")
lines(iv$plot$p, iv$plot$bias_corrected, lty=2)
legend("topleft", lty=c(1,2), legend = c("Apparent", "Bias Corrected"))


## ---- fig.width=6, fig.height=5-----------------------------------------------
mod2 <- function(data){
  # function that implements the model development procedure
  m <- glm(y ~ (x1 + x2 + x3 + x4)^2, data = data, 
          family = binomial(link = "logit"))
  
  step(m, direction = "backward", trace = 0)
}


iv2 <- internal_cal(model_fun = mod2, data = dat, pred_fun = pred)

iv2$metrics

plot(iv2$plot$p, iv2$plot$apparent, type="l", xlim=c(0,1), ylim=c(0,1), 
     xlab="Predicted", ylab="Observed")
lines(iv2$plot$p, iv2$plot$bias_corrected, lty=2)
legend("topleft", lty=c(1,2), legend = c("Apparent", "Bias Corrected"))


