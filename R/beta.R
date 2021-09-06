#' Compute beta-neutral portfolio for two assets
#'
#' @param symbol the stock symbol ticker of the market-correlated asset
#' @param period the time-series resolution "daily", "intraday", "weekly", or "monthly"
#' @param key the tier of Alpha Vantage API key: "premium" or "free"
#'
#' @description {For two assets with one positive beta and one negative beta, the beta neutral portfolio is a simple
#' linear system that can be solved explicitly. The weights add up to one and the aggregate beta is zero. This function returns
#' these weights based off historical estimates of the betas.}
#' @return vector
#' @export beta2Hedge
beta2Hedge <- function(symbol, period = "daily", key = "premium")
{
  symbols <- c("SPY", symbol, "VXX")
  # TODO: can we figure out a better way than commenting out this?
  stocks <- ravapi::getStocks(symbols)
  arithmeticReturns <- stockReturns(stocks, "arithmetic")
  return(betaHedge2(arithmeticReturns))
}

#' Estimate beta from OLS estimate of a linear regression against benchmark returns
#'
#' @param arithmeticReturns the multivariate time-series of returns. Assumes the first column is the benchmark, e.g. S&P 500
#'
#' @description {Compute historical estimates for the betas of a sample of assets. These are
#' based on a linear regression of the arithmetic returns of each asset against that of the benchmark index.}
#' @return vector
#' @export computeBetas
computeBetas <- function(arithmeticReturns)
{
  # Assuming SPY is first
  reg <- stats::lm(arithmeticReturns[, -1] ~ arithmeticReturns[, 1])
  betas <- reg$coefficients[2, ]
  return(betas)
}

#' Compute the beta-neutral portfolio for one market-correlated asset hedged with a inversely correlated asset.
#'
#' @param arithmeticReturns the multivariate time-series of returns. Assumes the first column is the benchmark, e.g. S&P 500
#'
#' @description {For two assets with one positive beta and one negative beta, the beta neutral portfolio is a simple
#' linear system that can be solved explicitly. The weights add up to one and the aggregate beta is zero. This function returns
#' these weights based off historical estimates of the betas.}
#' @return vector of betas
#' @export betaHedge2
betaHedge2 <- function(arithmeticReturns)
{
  betas <- computeBetas(arithmeticReturns)
  # assuming  VXX is last
  marketNeutralWeights <- c(-1,1)*rev(betas)/(betas[1]-betas[2])
  names(marketNeutralWeights) <- names(arithmeticReturns)[-1]
  return(marketNeutralWeights)
}

#' Backtest a two-asset beta neutral strategy
#'
#' @param asset the high beta asset to long
#' @param rolling whether to use a rolling window of size \code{sampleSize}
#' @param bankroll initial bankroll
#' @param rate the risk-free rate earned on cash
#' @param numDays number of days to trade
#' @param sampleSize initial training data size/rolling window size
#' @param fraction fraction to trade in stocks versus cash
#'
#' @description {Idealized backtest on closing prices under a 2-asset beta neutral strategy.
#' The estimating of the betas can be done on a rolling basis, optionally. Returns various
#' measures of performance and plots the total log-growth of the indidivual assets compared to
#' the benchmark and the beta-neutral portfolio.}
#' @return NULL
#' @export backtestBeta2Neutral
backtestBeta2Neutral <- function(asset, rolling, bankroll, rate, numDays, sampleSize, fraction)
{
  greenDays <- function(x) sum(fkpde::indicator(x > 0))/length(x)
  performanceMeasures <- list("mean", "sd", "min", "max", "greenDays")
  symbols <- c("SPY", asset, "VXX")
  # TODO: can we figure out a better way than commenting out this?
  stocks <- getStocks(symbols)
  if(Sys.time() < paste(Sys.Date(), "16:00:00 EST"))
  {
    ww <- quantmod::getQuote(Symbols = symbols)[, "Last"]
    ww <- as.xts(t(ww), order.by = Sys.Date())
    stocks <- rbind(stocks, ww)
  }
  arithmeticReturns <- stockReturns(stocks, "arithmetic")
  # Number of days, asset
  n <- nrow(stocks[,-1])
  m <- ncol(stocks[, -1])
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
  w <- betaHedge2(arithmeticReturns[1:(sampleSize), ])*fraction
  shares <- bankroll*w/stocks[sampleSize+1, -1] # Remove SPY
  cash <- bankroll*(1-sum(w))
  # Populate it

  for(i in 2:numDays)
  {

    # Continuous time rebalancing:
    # New value
    portfolio[i] <- as.numeric((shares)%*%as.numeric(stocks[sampleSize+i, -1])+cash*exp(rate/252))
    # Optimal fraction recomputed each step
    # use i:(...) for rolling window of data used in the estimation
    if(rolling)
    {

      w <- betaHedge2(arithmeticReturns[i:(sampleSize+i-2), ])*fraction

    } else
    {

      w <- betaHedge2(arithmeticReturns[1:(sampleSize+i-2), ])*fraction
    }
    shares <- portfolio[i]*w/stocks[sampleSize+i, -1]
    cash <- portfolio[i]*(1-sum(w))
    # print(shares[, shares>0])
  }
  # Time-stamps
  tts <- time(stocks[(sampleSize+1):(sampleSize+numDays), -1])
  portfolio <- xts::xts(x = portfolio, order.by = tts)
  profit <- as.numeric(portfolio[nrow(portfolio)])-as.numeric(portfolio[1])
  rr <- as.numeric(profit/portfolio[1])
  lg <- log(as.numeric(portfolio[nrow(portfolio)]))-log(as.numeric(portfolio[1]))
  results <- data.frame(pnl = profit, return = rr, log_growth = lg)
  output <- list(input = data.frame(bankroll, sampleSize, numDays, rate, rolling), results = results)
  # Compute log-returns for each path
  paths <- stocks
  paths$portfolio <- portfolio
  paths <- paths[complete.cases(paths), ]
  paths <- apply(paths, 2, function(x) log(x)-log(x[1]))

  # Plotting total-log return log(S_t/S_0) for SPY, Asset, VXX, Portfolio
  # print(plot(portfolio, type = "l"))
  xdates <- time(stocks[(sampleSize+1):(sampleSize+numDays), ])
  plot(xdates, paths[, 1], type = "l", ylim = c(min(paths), max(paths)))
  for(i in 2:ncol(paths))
  {
    lines(xdates, paths[, i], col = i)
  }
  legend(x = "topleft", legend = colnames(paths), lty = 1, col = 1:4, cex = 0.5)

  # Compute performance statistics
  dailyPaths <- stocks
  dailyPaths$portfolio <- portfolio
  dailyPaths <- dailyPaths[complete.cases(dailyPaths), ]
  dailyPaths <- apply(dailyPaths, 2, function(x) diff(log(x)))
  dailyLogStats <- lapply(performanceMeasures, function(t) apply(dailyPaths, 2, t))
  dailyLogStats <- do.call(rbind, dailyLogStats)
  rownames(dailyLogStats) <- performanceMeasures


  # Finally compute betas
  ariPaths <- stocks
  ariPaths$portoflio <- portfolio
  ariPaths <- stockReturns(ariPaths, type = "arithmetic")
  computeBetas(ariPaths)
  dailyLogStats
  endBetas <- c(1, computeBetas(ariPaths))
  dailyLogStats <- rbind(dailyLogStats, endBetas)
  qqq <- rbind(betaHedge2(arithmeticReturns), betaHedge2(arithmeticReturns)*bankroll)
  print(utils::head(output))
  print(dailyLogStats)
  print(qqq)
}
