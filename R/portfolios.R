#' Optimal long-short positions
#'
#' @param symbols portfolio of stocks
#' @param rate risk-free rate on cash
#' @param restraint percentage of wealth to go up to
#' @param rollingPeriod rolling period size, can be \code{NULL} for full
#'
#' @description {Kelly-criterion for long and short portfolios under GBM with rolling
#' estimations.}
#' @return data.frame
#' @export logOptimalWeightsGBM
logOptimalWeightsGBM <- function(symbols, rate = 0, restraint = 1, rollingPeriod = 60)
{
  if(nrow(stocks) < rollingPeriod)
  {
    stop("Not enough samples of prices")
  }
  stocks <- getStocks(symbols)
  log_returns <- stockReturns(stocks)
  if(!is.null(rollingPeriod))
  {
    par <- findistr::fitGBMs(tail(log_returns, rollingPeriod))
  } else
  {
    par <- findistr::fitGBMs(log_returns)
  }
  drift <- par$drift
  Sigma <- par$Sigma
  long <- kellyfractions::kellyPortfolioGBM(drift, Sigma, rate, restraint, "long")
  short <- kellyfractions::kellyPortfolioGBM(drift, Sigma, rate, 1-restraint, "short")
  w <- data.frame(long = long, short = short)
  return(w)
}
