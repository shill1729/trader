#' Fit returns distribution to daily-arithmetic returns
#'
#' @param x time series of daily arithmetic returns
#' @param distr distribution name: "norm" or "unif"
#'
#' @return vector
#' @import mclust
#' @export fit_dtfm
fit_dtfm <- function(x, distr = "norm")
{
  if(distr == "unif")
  {
    return(c(min = min(x), max = max(x)))
  } else if(distr == "norm")
  {
    return(c(mean = mean(x), sd = stats::sd(x)))
  } else if(distr == "gmm")
  {
    mcfit <- mclust::Mclust(data = x)
    mix_param <- numerics::extract_mixture(mcfit, 1)
    return(mix_param)
  } else if(distr == "stable")
  {
    # Fit stable distribution
    stable_param <- libstableR::stable_fit_mle(rnd = x)
    return(stable_param)
  }
}

#' Probability density function of fitted model
#'
#' @param r region of returns
#' @param distr distribution name: "norm" or "unif"
#' @param param parameters of distribution as a vector
#'
#' @return numeric
#' @importFrom numerics dgmm dstable
#' @export ddtfm
ddtfm <- function(r, distr = "norm", param)
{
  ddistr <- paste("d", distr, sep = "")
  ddistr <- get(ddistr)
  if(distr == "norm" || distr == "unif")
  {
    args <- c(list(r), as.list(param))
  } else if(distr == "gmm")
  {
    args <- list(c(r), param[1, ], param[2, ], param[3, ])
  } else if(distr == "stable")
  {
    args <- list(c(r), param)
  }

  return(do.call(ddistr, args))
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
