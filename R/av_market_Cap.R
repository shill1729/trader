#' Get market capitalization from AV
#'
#' @param symbol symbol to look up
#'
#' @description {The share-price multiplied by shares outstanding}
#' @return numeric
#' @export market_cap
market_cap <- function(symbol)
{
  return(as.numeric(company_overview(symbol)$MarketCapitalization))
}

#' Create market portfolio via market-cap weights
#'
#' @param symbols list of symbols
#'
#' @description {The market portfolio i.e. weights equal to market caps}
#' @return vector
#' @export market_portfolio
market_portfolio <- function(symbols)
{
  caps <- matrix(0, length(symbols))
  for(i in 1:length(symbols))
  {
    print(symbols[i])
    caps[i] <- market_cap(symbols[i])
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
#' @param market_port object returned from \code{market_portfolio}
#' @param p diversity index (a number in the unit interval)
#'
#' @description {See SPT (2002) by E. F.}
#' @return vector
#' @export diversity_weighted_port
diversity_weighted_port <- function(market_port, p = 1)
{
  (market_port^p)/sum(market_port^p)
}
