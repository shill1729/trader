#' Download option-chain from Yahoo-Finance
#'
#' @param symbol stock-ticker symbol to look up
#' @param option_type "puts" or "calls"
#' @param plotSurface boolean for plotting the 3D raw price-surface
#'
#' @description {Download option chains for all available expiries of a given stock ticker,
#' then merge them by common strike prices into one matrix/data.frame. This is essentially
#' a wrapper to \code{quantmod::getOptionChain} with some processing/formatting
#' added afterwards.}
#' @return data.frame
#' @export getYahooOptionChain
getYahooOptionChain <- function(symbol, option_type = "calls", plotSurface = FALSE)
{
  #=========================================================
  spot <- trader::getQuoteAV(symbol)$price
  z <- quantmod::getOptionChain(symbol, Exp = NULL)
  #=========================================================
  # Error check, sometimes Yahoo has no data intermittently
  if(is.null(names(z)))
  {
    stop(paste("No expiries returned for", symbol))
  }
  # Convert date-format from Month.dd.yyyy to yyyy-mm-dd
  expiries <- gsub("\\.", "-", names(z))
  expiries <- as.Date(expiries, format = "%b-%d-%Y")
  maturity <- trader::date_yte(expiries) # convert to YTE
  # Loop through all expiries and merge by strike, for given payoff-type
  marketData <- lapply(z, function(x) data.frame(Strike = x[[option_type]]$Strike, Last = x[[option_type]]$Last))
  # Suppres duplicate column warning
  suppressWarnings(marketData <- Reduce(function(...) merge(..., all = TRUE, by="Strike"), marketData))
  marketData <- marketData[complete.cases(marketData), ]
  colnames(marketData) <- c("Strike", as.character(expiries))
  if(plotSurface)
  {
    plot3D::persp3D(z = as.matrix(marketData)[,-1], phi = 10, theta = 60)
  }
  return(marketData)
}
