#' Load a single stock
#'
#' @param ticker the stock ticker to load
#'
#' @return see quantmod \code{getSymbols}
#' @export load_stock
load_stock <- function(ticker)
{
  s <- quantmod::getSymbols(Symbols = ticker,src = "av", auto.assign = FALSE, output.size = "full",  api.key = Sys.getenv("api.key"))
  return(s)
}

#' Load multiple stocks
#'
#' @param tickers the stock tickers to load
#' @param call_rate how long to wait between groups of API calls to Alpha Vantage
#'
#' @return see quantmod \code{getSymbols}
#' @export load_stocks
load_stocks <- function(tickers, call_rate = 60)
{
  # Function
  stock_list <- list()
  N <- length(tickers)
  for(i in 1:N)
  {
    if(i%%5 == 0)
    {
      print("Free AV calls are limited, waiting...")
      Sys.sleep(call_rate)
    }
    print(tickers[i])
    stock_list[[i]] <- trader::load_stock(ticker = tickers[i])
  }
  stocks <- do.call(cbind, args = lapply(stock_list, quantmod::Cl))
  stocks <- stocks[stats::complete.cases(stocks),]
  return(stocks)
}
