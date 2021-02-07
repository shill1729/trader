
#' Backtest fixed fractional reallocation with TEMA exits
#'
#' @param N number of days for the moving window in the TEMA filter
#' @param prices the time series of prices
#' @param kelly the fraction to invest
#' @param rate the rate earned on cash account
#' @param bankroll the initial bankroll to invest
#' @param plotPaths boolean to plot sample paths
#'
#' @description {Backtest fixed fractiona reallocation with TEMA exit signals
#' on a given time series of prices. Assumes perfect trading at close prices with no
#' transaction costs, etc.}
#' @return list
#' @export temaCrossoverBacktest
temaCrossoverBacktest <- function(N, prices, kelly, rate, bankroll, plotPaths = FALSE)
{
  weight <- kelly
  temaSignal <- tema(prices, alpha = 2/(N+1))
  if(plotPaths)
  {
    par(mfrow = c(1, 1))
    plot(time(prices), prices, type = "l")
    lines(time(prices), temaSignal, col = "blue")
  }
  # Iterate over each day and check for a crossover, buy/sell accordingly.
  portfolio <- matrix(0, nrow = length(prices), ncol = 5)
  colnames(portfolio) <- c("equity", "cash", "shares", "weights", "total")
  portfolio[1, c(2, 5)] <- bankroll

  for(i in 2:length(prices))
  {


    # Update cash
    portfolio[i, "cash"] <- portfolio[i-1, "cash"]*exp(rate/252)
    # Update equity: current price times previous shares
    portfolio[i, "equity"] <- as.numeric(prices[i])*portfolio[i-1, "shares"]
    # Update total value
    portfolio[i, "total"] <- portfolio[i, "cash"] + portfolio[i, "equity"]
    # Update weight
    portfolio[i, "weights"] <- portfolio[i, "equity"]/portfolio[i, "total"]
    # Rebalance:
    portfolio[i, "equity"] <- portfolio[i, "total"]*weight
    portfolio[i, "shares"] <- portfolio[i, "equity"]/(as.numeric(prices[i]))
    portfolio[i, "cash"] <- portfolio[i, "total"]*(1-weight)
    # Update weight
    portfolio[i, "weights"] <- portfolio[i, "equity"]/portfolio[i, "total"]

    if(temaSignal[i] < prices[i])
    {
      # print("Buy")
      if(plotPaths)
      {
        graphics::points(time(prices[i]), prices[i], col = "green")
      }
      weight <- kelly
    } else if(temaSignal[i] >= prices[i])
    {
      # print("Sell")
      if(plotPaths)
      {
        graphics::points(time(prices[i]), prices[i], col = "red")
      }
      # Update cash
      portfolio[i, "cash"] <- portfolio[i-1, "cash"]*exp(rate/252)+portfolio[i-1, "equity"]
      # Update equity: 0
      portfolio[i, "equity"] <- 0
      # Update shares
      portfolio[i, "shares"] <- 0
      # Update total value
      portfolio[i, "total"] <- portfolio[i, "cash"] + portfolio[i, "equity"]
      # Update weight
      portfolio[i, "weights"] <- portfolio[i, "equity"]/portfolio[i, "total"]
      weight <- 0
    }
  }
  portx <- time(prices)
  portfolio <- as.data.frame(portfolio)
  total_growth <- log(portfolio$total[nrow(portfolio)]/bankroll)
  if(plotPaths)
  {
    plot(portx, portfolio[, "total"], type = "l")
  }
  return(list(portfolio = portfolio, growth = total_growth))
}

#' Compute the optimal TEMA-window period
#'
#' @param prices time-series of prices
#' @param kelly the fixed fraction for reallocation
#' @param rate the rate earned on the cash account
#' @param bankroll the initial bankroll to invest
#'
#' @description {Numerically maximize log-growth of the backtest
#' over the range of TEMA-window sizes for the given fraction to invest.}
#' @return list or data.frame
#' @export optimalTEMA
optimalTEMA <- function(prices, kelly, rate, bankroll)
{
  # TODO add to trader: optimizer
  g <- function(N)
  {
    z <- temaCrossoverBacktest(N, prices, kelly, rate, bankroll, FALSE)
    return(z$growth)
  }
  w <- stats::optimize(f = g, interval = c(1, 600), maximum = TRUE)
  return(w)
}
