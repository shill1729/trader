#' Generate a risk-profile under GBM of a single stock
#'
#' @param log_returns daily log returns
#' @param rate risk-neutral rate proxy
#'
#' @description {Computes drift, volatility, Sharpe ratio, Kelly criterion, and growth-rate.}
#' @return data.frame
gbm_profile1 <- function(log_returns, rate = 0.02076367)
{
  ms <- qfin::fitGBM(log_returns)
  m <- data.frame(drift = ms[1], volat = ms[2])
  m$sharpe <- (m$drift-rate)/m$volat
  m$kelly <- (m$drift-rate)/m$volat^2
  m$entropy <- rate+0.5*m$sharpe^2
  return(m)
}

#' Generate a risk-profile under GBM of a single stock
#'
#' @param log_returns daily log returns
#' @param rate risk-neutral rate proxy
#'
#' @description {Computes drift, volatility, Sharpe ratio, Kelly criterion, and growth-rate.}
#' @return data.frame
#' @export gbm_profile
gbm_profile <- function(log_returns, rate = 0.02076367)
{
  if(ncol(log_returns) > 1)
  {
    stat_set <- do.call(rbind, lapply(log_returns, function(x) gbm_profile1(x, rate)))
    stat_set <- stat_set[order(stat_set$entropy, decreasing = TRUE), ]
    return(stat_set)
  } else {
    m <- gbm_profile1(log_returns, rate)
    return(m)
  }
}


