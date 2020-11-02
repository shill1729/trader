# Continuous time financial market strategies:


#' Compute optimal log-growth portfolio for multivariate GBM stocks
#'
#' @param symbols vector of stock symbols
#' @param rate riskless rate of return
#' @param long_frac fraction of bankroll to use
#' @param rolling the rolling period of prior data to use for estimation, if \code{NULL}
#' will use the full sample (default)
#'
#' @description {Computes both long-only and short-only log-growth optimal
#' portfolio allocations up to a percentage of total bankroll and the respective
#' growth rates. }
#' @return list
#' @export gbm_portfolio
gbm_portfolio <- function(symbols, rate = 0, long_frac = 1, rolling = NULL)
{
  stocks <- load_stocks(symbols = symbols)

  log_ret <- stock_returns(stocks)
  if(!is.null(rolling))
  {
    log_ret <- tail(log_ret, rolling)
  }

  long <- qfin::portfolioGBM(log_ret, rate, long_frac)
  short <- qfin::portfolioGBM(log_ret, rate, 1-long_frac, "short")
  port <- cbind(long$bet, short$bet)
  colnames(port) <- c("long", "short")
  growths <- data.frame(long$growth, short$growth)
  return(list(weights = port, growth = growths))
}

