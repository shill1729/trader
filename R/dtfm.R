#' Fit returns distribution to daily-arithmetic returns
#'
#' @param x time series of daily arithmetic returns
#' @param distr distribution name: "norm" or "unif"
#'
#' @return vector
#' @export fit_dtfm
fit_dtfm <- function(x, distr = "norm")
{
  if(distr == "unif")
  {
    return(c(min = min(x), max = max(x)))
  } else if(distr == "norm")
  {
    return(c(mean = mean(x), sd = stats::sd(x)))
  }
}

#' Probability density function of fitted model
#'
#' @param r region of returns
#' @param distr distribution name: "norm" or "unif"
#' @param param parameters of distribution as a vector
#'
#' @return numeric
#' @export ddtfm
ddtfm <- function(r, distr = "norm", param)
{
  ddistr <- paste("d", distr, sep = "")
  ddistr <- get(ddistr)
  return(do.call(ddistr, args = c(list(r), as.list(param))))
}

#' Fit a discrete time daily returns distribution and get log-optimal allocation
#'
#' @param symbol stock ticker to look up
#' @param distr distribution to fit: "unif", "norm", more to come...
#' @param rate risk-neutral rate
#'
#' @description {Looks up a stock's history on AV, estimates a return distribution
#' for daily-arithmetic returns, and then computes the optimal log utility criterion.
#' }
#' @return list of fitted parameters, optimal allocation, and optimal growth (annualized)
#' @export dtfm
dtfm <- function(symbol, distr = "unif", rate = 0)
{
  rrate <- (1+rate)^(1/252)-1
  s <- time_series(symbol, "daily")
  x <- daily_returns(s$adj_close)$arithmetic
  param <- fit_dtfm(x, distr)
  kelly <- KellyCriterion::kelly_dtfm(distr, param, rrate)
  entropy <- KellyCriterion::entropy_dtfm(distr, param, rrate)*252
  # print(KellyCriterion::sim_dtfm(n, as.numeric(s$adj_close[1]), 1800, distr, param, rrate))
  return(list(param = param, kelly = kelly, entropy = entropy))
}
