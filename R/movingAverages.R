#' Compute a simple moving average
#'
#' @param x the time-series of observations
#' @param n the length of the moving window to average over (positive integer)
#'
#' @description {An implementation of the basic simple moving average. A sample mean is computed over a moving window
#' of a given time series.}
#' @details {An error is returned if the length of the moving window is larger than the sample size of the data.}
#' @return vector
#' @export sma
sma <- function(x, n = 30)
{
  K <- length(x)
  if(n > K)
  {
    stop(paste("Length of moving window", n, "must be less than sample size of time series", K))
  }
  s <- matrix(NA, nrow = K)
  s[n+1] <- mean(x[1:n])
  for(i in 1:(K-1-n))
  {
    s[n+i+1] <- mean(x[1:n+i])
  }
  return(s)
}

#' Compute an exponential moving average of a time-series
#'
#' @param x the time-series of the signal
#' @param alpha smoothing parameter, a value in the unit-interval
#'
#' @description {The standard exponential moving average}
#' @details {The exponential moving average is defined by \eqn{s_n = \alpha x_n+(1-\alpha)s_{n-1}} for \eqn{n>1}, with some initial value.}
#' @return vector
#' @export ema
ema <- function(x, alpha = 0.5)
{
  if(alpha <= 0 || alpha >1)
  {
    stop("Smoothing parameter alpha must be in the unit-interval")
  }
  K <- length(x)
  # if(K<n)
  # {
  #   stop(paste("Data size", K, "is less than moving-window", n))
  # }
  s <- matrix(data = NA, nrow = K)
  s[1] <- x[1]
  for(i in 2:K)
  {
    s[i] <- alpha*x[i]+(1-alpha)*s[i-1]
  }
  return(s)
}

#' Compute a so-called triple exponential moving average of a time-series
#'
#' @param x the time-series of the signal
#' @param alpha smoothing parameter, a value in the unit-interval
#'
#' @description {The triple exponential moving average}
#' @details {See wikipedia for triple exponential moving average (not triple exponential smoothing)}
#' @return vector
#' @export tema
tema <- function(x, alpha)
{
  tma <- 3*ema(x, alpha)-3*ema(ema(x, alpha), alpha)+ema(ema(ema(x, alpha), alpha), alpha)
  return(tma)
}
