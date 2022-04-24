#' Rank a set of stocks via monthly momentum
#'
#' @param monthsBack months back to estimate averages of arithmetic returns
#' @param stocks data-set of stocks, can be NULL to look up via tickers
#' @param symbols tickers of stocks
#'
#' @description {Computes monthly returns, averages them over the last few periods, and
#' sorts them.}
#' @return data.frame
#' @importFrom quantmod monthlyReturn
#' @export monthly_momentum
monthly_momentum <- function(monthsBack = 6, stocks = NULL, symbols = NULL)
{
  if(is.null(stocks))
  {
    stocks <- ravapi::getAssets(symbols)
  }
  monthlyReturns <- list()
  for(i in 1:ncol(stocks))
  {
    monthlyReturns[[i]] <- quantmod::monthlyReturn(x = stocks[, i])
  }
  monthlyReturns <- do.call(cbind, monthlyReturns)
  colnames(monthlyReturns) <- colnames(stocks)

  if(monthsBack > nrow(monthlyReturns))
  {
    stop(paste("monthsBack cannot be greater than", nrow(monthlyReturns)))
  }
  momentum <- apply(tail(monthlyReturns, monthsBack), 2, mean)
  momentum <- sort(momentum, TRUE)
  print(tail(monthlyReturns))
  return(t(t(momentum)))
}

