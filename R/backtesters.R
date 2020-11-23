# Continuous time GBM backtester for portfolios


#' Backtester for GBM portfolio strategy
#'
#' @param stocks the data set of stocks
#' @param rolling boolean for rolling estimates
#' @param bankroll initial bankroll
#' @param rate risk-free rate
#' @param restraint max percentage of wealth to use
#' @param numDays number of days to trade for
#' @param sampleSize number of days for initial sample size in training data or rolling window size
#'
#' @description {Continuous time idealized backtester on close prices using
#' optimal allocations recomputed each day under GBM model dynamics.}
#' @return list
#' @export backtestPortfolioGBM
backtestPortfolioGBM <- function(stocks, rolling = TRUE, bankroll = 1500, rate = 0.0, restraint = 0.9, numDays = 30, sampleSize = 30)
{

  returns <- stockReturns(stocks, "log")
  # Number of days, assets
  n <- nrow(stocks)
  m <- ncol(stocks)
  if(sampleSize > n)
  {
    stop(paste("sampleSize must be less than ", n))
  }
  if(numDays > n-sampleSize)
  {
    stop(paste("numDays must be less than", n-sampleSize))
  }
  # Create the portfolio array
  portfolio <- matrix(0, nrow = numDays)
  portfolio[1] <- bankroll
  par <- findistr::fitGBMs(returns[1:(sampleSize), ])
  drift <- par$drift
  Sigma <- par$Sigma
  ww <- kellyfractions::kellyPortfolioGBM(drift, Sigma, rate, restraint)
  w <- (ww[-c(m+1)]) # remove cash component
  shares <- bankroll*w/stocks[sampleSize+1, ]
  cash <- ww[m+1]*bankroll
  # Populate it

  for(i in 2:numDays)
  {

    # Continuous time rebalancing:
    # New value
    portfolio[i] <- as.numeric((shares)%*%as.numeric(stocks[sampleSize+i, ])+cash*exp(rate/252))
    # Optimal fraction recomputed each step
    # use i:(...) for rolling window of data used in the estimation
    if(rolling)
    {
      par <- findistr::fitGBMs(returns[i:(sampleSize+i-1),])
      drift <- par$drift
      Sigma <- par$Sigma
      ww <- kellyfractions::kellyPortfolioGBM(drift, Sigma, rate, restraint)

    } else
    {
      par <- findistr::fitGBMs(returns[1:(sampleSize+i-1),])
      drift <- par$drift
      Sigma <- par$Sigma
      ww <- kellyfractions::kellyPortfolioGBM(drift, Sigma, rate, restraint)
    }

    w <- (ww[-c(m+1)]) # remove cash component
    shares <- portfolio[i]*w/stocks[sampleSize+i, ]
    cash <- ww[m+1]*portfolio[i]
    # print(shares[, shares>0])


  }


  # Time-stamps
  tts <- time(stocks[(sampleSize+1):(sampleSize+numDays),])
  portfolio <- xts::xts(x = portfolio, order.by = tts)
  profit <- as.numeric(portfolio[nrow(portfolio)])-as.numeric(portfolio[1])
  rr <- as.numeric(profit/portfolio[1])
  lg <- log(as.numeric(portfolio[nrow(portfolio)]))-log(as.numeric(portfolio[1]))
  results <- data.frame(pnl = profit, return = rr, log_growth = lg)
  print(plot(portfolio, type = "l"))
  output <- list(input = data.frame(bankroll, sampleSize, numDays, rate, restraint),
                 results = results)
  return(output)
}




