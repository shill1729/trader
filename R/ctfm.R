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

#' Compute optimal log-growth portfolio for multivariate GBM stocks
#'
#' @param symbols vector of stock symbols
#' @param rate riskless rate of return
#' @param long_frac fraction of bankroll to use
#'
#' @description {Computes both long-only and short-only log-growth optimal
#' portfolio allocations up to a percentage of total bankroll and the respective
#' growth rates. }
#' @return list
#' @export gbm_portfolio
gbm_portfolio <- function(symbols, rate = 0, long_frac = 1)
{
  stocks <- load_stocks(symbols = symbols)
  log_ret <- stock_returns(stocks)
  long <- KellyCriterion::kelly_portfolio(log_ret, rate, long_frac)
  short <- KellyCriterion::kelly_portfolio(log_ret, rate, 1-long_frac, "short")
  port <- cbind(long$bet, short$bet)
  colnames(port) <- c("long", "short")
  growths <- data.frame(long$growth, short$growth)
  return(list(weights = port, growth = growths))
}

