#' Optimal long-short portfolios under geometric Brownian motion
#'
#' @param symbols portfolio of stocks
#' @param rate risk-free rate on cash
#' @param restraint percentage of wealth to go up to
#' @param rollingPeriod rolling period size, can be \code{NULL} for full
#' @param stocks default NULL, data returned from \code{getStocks()}.
#'
#' @description {Kelly-criterion for long and short portfolios under GBM with rolling
#' estimations. Price data will either be looked up or can be passed.}
#' @return data.frame
#' @export optimalPortfolio
optimalPortfolio <- function(symbols, rate = 0, restraint = 1, rollingPeriod = 60, stocks = NULL)
{
  # Get stocks and compute daily log-returns
  if(is.null(stocks))
  {
    stocks <- getStocks(symbols)
  } else
  {
    if(!all.equal(names(stocks), symbols))
    {
      stop("stock names are not equal to given symbols")
    }
  }

  # Check input
  if(nrow(stocks) < rollingPeriod && !is.null(rollingPeriod))
  {
    stop("Not enough samples of prices")
  }
  log_returns <- stockReturns(stocks)
  # Estimate either rolling drift/covariance or full-sample
  if(!is.null(rollingPeriod))
  {
    par <- findistr::fitGBMs(tail(log_returns, rollingPeriod))
  } else
  {
    par <- findistr::fitGBMs(log_returns)
  }
  # Compute optimal long-short allocations
  drift <- par$drift
  Sigma <- par$Sigma
  long <- kellyfractions::kellyPortfolioGBM(drift, Sigma, rate, restraint, "long")
  short <- kellyfractions::kellyPortfolioGBM(drift, Sigma, rate, 1-restraint, "short")
  w <- data.frame(long = long, short = short)
  return(w)
}

