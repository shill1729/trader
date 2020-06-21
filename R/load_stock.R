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
