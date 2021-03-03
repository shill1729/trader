

#' Compute univariate Kelly-criterion for a given distribution of asset returns.
#'
#' @param model the distribution used for the returns under which the Kelly criterion is to be computed
#' @param rate the risk-free rate earned on a money-market account
#' @param symbol the stock ticker to look up
#' @param prices optinally pass the time-series of prices
#' @param period for looking up by symbol, the period between prices (daily, intraday, weekly, monthly)
#' @param key premium or free API key for Alpha-Vantage
#'
#' @description {Compute the Kelly-criterion log-optimal fraction under a variety of models for the distribution
#' of period asset returns. Models include continuous time GBM, jump-GBM with normally, kou, and uniformly distributed
#' jumps, as well as discrete-time models such as uniform, normal, gmm, and stable. Pass either
#' the stock symbol ticker to look up data or pass the price-series itself.}
#'
#' @return numeric
#' @export kellyCriterion
kellyCriterion <- function(model = "gbm", rate = 0, symbol = NULL, prices = NULL, period = "daily", key = "premium")
{
  k <- 0
  if(model == "gbm")
  {
    v <- getGBM(symbol, prices, period, key)
    k <- kellyfractions::kellyGBM(v[1], v[2], rate, restraint = NULL)
  } else
  {
    stop("Only model = 'gbm' has been implemented; bug the author!")
  }
  return(k)
}


#' Estimate parameters of a GBM from historical stock prices
#'
#' @param symbol the stock ticker to look up
#' @param prices optinally pass the time-series of prices
#' @param period for looking up by symbol, the period between prices (daily, intraday, weekly, monthly)
#' @param key premium or free API key for Alpha-Vantage
#'
#' @description {Pass either the stock ticker symbol or the time-series data itself to fit
#' a Geometric Brownian motion to the data via the basic MLE routine. Returns a vector containing
#' the mean drift and the volatility; for daily price-series these are annualized, all other periods are not.}
#' @return vector
#' @export getGBM
getGBM <- function(symbol = NULL, prices = NULL, period = "daily", key = "premium")
{
  if(is.null(symbol) && is.null(prices))
  {
    stop("One of 'symbol' or 'prices' must be passed")
  } else
  {
    x <- 0
    if(!is.null(symbol) && is.null(prices))
    {
      prices <- getPriceTimeSeries(symbol, period = period, key = key)
      if(period == "daily")
      {
        prices <- prices$adj_close
      } else
      {
        prices <- prices$close
      }
      x <- dailyReturns(prices)$log
    } else if(is.null(symbol) && !is.null(prices))
    {
      x <- dailyReturns(prices)$log
    }
    timeScale <- ifelse(period == "daily", 1/252, 1)
    param <- findistr::fitGBM(x, timeScale = timeScale)
    return(param)
  }
}


