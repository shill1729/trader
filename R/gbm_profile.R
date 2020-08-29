#' Analyze a list of stocks under GBM dynamics
#'
#' @param ticker stock ticker to analyze
#' @param rate risk-neutral rate to benchmark against
#'
#' @description {Estimates the parameters for a geometric Brownian motion model
#' of the stock prices, the Sharpe-ratio, the Kelly-fraction, and growth-rate
#' with said parameter-estimates. The estimation is done via MLE.}
#' @return data.frame
gbm_profile1 <- function(ticker, rate  = 0)
{
  hist_dat <- load_stock(ticker)
  prices <- quantmod::Ad(hist_dat)
  gbm_param <- pside::mle_gbm(prices = as.numeric(prices))
  sharpe_ratio <- (gbm_param$mu-rate)/gbm_param$volat
  kelly_frac <- KellyCriterion::kelly_gbm(gbm_param$mu, rate, gbm_param$volat)
  growth_rate <- KellyCriterion::entropy_gbm(kelly_frac, gbm_param$mu, rate, gbm_param$volat)
  risk_profile <- data.frame(drift = gbm_param$mu,
                             volat = gbm_param$volat,
                             sharpe = sharpe_ratio,
                             kelly = kelly_frac,
                             entropy = growth_rate
  )
  rownames(risk_profile) <- ticker
  return(risk_profile)
}

#' Analyze a list of stocks under GBM dynamics
#'
#' @param tickers vector of stock tickers to analyze, at least of length 1
#' @param rate risk-neutral rate to benchmark against
#'
#' @description {Estimates the parameters for a geometric Brownian motion model
#' of the stock prices, the Sharpe-ratio, the Kelly-fraction, and growth-rate
#' with said parameter-estimates. The estimation is done via MLE.}
#' @return data.frame
gbm_profile2 <- function(tickers, rate = 0)
{
  z <- do.call(rbind, lapply(tickers, function(x) gbm_profile1(x, rate)))
  return(z)
}

#' Analyze a list of stocks under GBM dynamics
#'
#' @param tickers vector of stock tickers to analyze, at least of length 1
#' @param rate risk-neutral rate to benchmark against
#'
#' @description {Estimates the parameters for a geometric Brownian motion model
#' of the stock prices, the Sharpe-ratio, the Kelly-fraction, and growth-rate
#' with said parameter-estimates. The estimation is done via MLE.}
#' @details {Currently, each stock ticker's historical data is pulled in full,
#' so direct comparisons of estimations and statistics is not actually sound, due
#' to the different time horizons and resolution of the sample.}
#' @return data.frame containing \code{drift}, the annual mean drift rate \eqn{\mu},
#' \code{volat} the annual volatility \eqn{\sigma}, \code{sharpe} the Sharpe-ratio \eqn{\lambda := (\mu-r)/\sigma},
#' \code{kelly} the Kelly-fraction \eqn{(\mu-r)/\sigma^2}, and \code{entropy} the annual growth rate under
#' the Kelly-fraction, \eqn{0.5(r+0.5\lambda^2)}.
#' @export gbm_profile
gbm_profile <- function(tickers, rate = 0)
{
  if(length(tickers) == 1)
  {
    return(gbm_profile1(ticker = tickers, rate))
  } else if(length(tickers) > 1)
  {
    return(gbm_profile2(tickers, rate))
  } else
  {
    stop("Empty vector of vectors")
  }
}
