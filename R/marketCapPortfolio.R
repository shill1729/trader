#' Get market capitalization from AV
#'
#' @param symbol symbol to look up
#'
#' @description {The share-price multiplied by shares outstanding}
#' @return numeric
#' @export getMarketCap
getMarketCap <- function(symbol)
{
  return(as.numeric(getCompanyOverviewAV(symbol)$MarketCapitalization))
}

#' Create market portfolio via market-cap weights
#'
#' @param symbols list of symbols
#'
#' @description {The market portfolio i.e. weights equal to market caps}
#' @return vector
#' @export getMarketPortfolio
getMarketPortfolio <- function(symbols)
{
  caps <- matrix(0, length(symbols))
  for(i in 1:length(symbols))
  {
    print(symbols[i])
    caps[i] <- getMarketCap(symbols[i])
    if(i%%30==0 && length(symbols) > 30)
    {
      Sys.sleep(60)
    }
  }
  weights <- caps/sum(caps)
  rownames(weights) <- symbols
  return(weights)
}

#' Diversity weighted market portfolio
#'
#' @param market_port object returned from \code{getMarketPortfolio}
#' @param p diversity index (a number in the unit interval)
#'
#' @description {See SPT (2002) by E. F.}
#' @return vector
#' @export diverseMarketPortfolio
diverseMarketPortfolio <- function(market_port, p = 1)
{
  (market_port^p)/sum(market_port^p)
}
