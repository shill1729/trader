# Continuous time financial market models

#' Fit parameters of geometric Brownian motion to data
#'
#' @param log_returns the daily log returns of a stock
#' @param time_step the time-step between observations, defaults to 1/252.
#'
#' @description {Fit a geometric Brownian motion model to daily log-returns data.}
#'
#' @return vector
#' @export fit_gbm
fit_gbm <- function(log_returns, time_step = 1/252)
{
  volat <- stats::sd(log_returns)/sqrt(time_step)
  mu <- mean(log_returns)/(time_step)+0.5 * volat^2
  param <- data.frame(drift = mu, volat = volat)
  return(param)
}


