#' Compute daily returns of a stock
#'
#' @param s the time-series of daily prices, returned from \code{getPriceTimeSeries}
#'
#' @description {Computes both the daily log return and daily arithmetic return
#' of a given price time-series.}
#' @return xts of logarithmic and arithmetic returns
#' @importFrom stats time
#' @export dailyReturns
dailyReturns <- function(s)
{
  x <- diff(log(s), na.pad = FALSE)
  r <- exp(x)-1
  returns <- xts::xts(x = data.frame(x, r), order.by = stats::time(x))
  names(returns) <- c("log", "arithmetic")
  return(returns)
}

#' Daily returns of a set of stocks
#'
#' @param stocks xts of stocks
#' @param type "log" or "arithmetic"
#'
#' @description {Daily returns of multiple stocks.}
#' @return xts of daily-returns of multiple stocks
#' @export stockReturns
stockReturns <- function(stocks, type = "log")
{
  if(type != "log" && type != "arithmetic")
  {
    stop("argument 'type' must be 'log' or 'arithmetic'")
  }
  returns <- do.call(cbind, lapply(stocks, function(x) dailyReturns(x)[, type]))
  names(returns) <- names(stocks)
  return(returns)
}
