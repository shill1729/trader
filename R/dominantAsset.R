#' Dominant asset via optimal log growth
#'
#' @param returns arithmetic returns of the stocks
#' @param windowSize how many days to use in the rolling window
#' @param limit the upper limit in percentage of wealth to invest
#'
#' @description {Compute the dominant asset via Kelly-criterion.}
#'
#' @return list
#' @export rollingDominantAsset
rollingDominantAsset <- function(returns, windowSize = 30, limit = 0.5)
{
  k <- nrow(returns)
  if(k-windowSize+1 <= 0)
  {
    stop("Index must be greater than windowSize plus one")
  }
  n <- ncol(returns)
  R <- matrix(0, nrow = n, ncol = n)
  for(i in 1:n)
  {
    for(j in 1:n)
    {
      R[i, j] <- mean((1+returns[(k-windowSize+1):k, i])/(1+returns[(k-windowSize+1):k, j]))
    }
  }
  colnames(R) <- colnames(returns)
  rownames(R) <- colnames(returns)
  weights <- matrix(0, nrow = n)
  rownames(weights) <- colnames(R)
  weights[which.min(apply(R-1, 2, sum))] <- limit
  return(list(R = R, weights = weights))
}


#' Backtest dominant asset strategy
#'
#' @param returns arithmetic returns of stocks
#' @param bankroll size of initial investment
#' @param windowSize rolling window size
#' @param limit upper limit of allocation percentage
#'
#' @description {Backtest the dominant asset strategy on close prices.}
#' @return xts
#' @export backtestDominantAsset
backtestDominantAsset <- function(returns, bankroll = 1, windowSize = 30, limit = 0.5)
{
  N <- nrow(returns)
  if(windowSize > N-1)
  {
    stop(paste("windowSize must be less than ", N-1))
  }
  n <- ncol(returns)
  portfolioReturn <- matrix(0, nrow = N)
  for(i in (windowSize+1):N)
  {

    dat <- rollingDominantAsset(returns[1:(i-1), ], windowSize, limit)
    w <- dat$weights
    portfolioReturn[i] <- 1+t(w)%*%as.numeric(returns[i, ])
  }
  results <- bankroll*c(1, cumprod(portfolioReturn[(windowSize+1):N]))
  results <- xts::xts(x = results, order.by = time(returns[-c(1:(windowSize-1)), ]))
  return(results)
}
