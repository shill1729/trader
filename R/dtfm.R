

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
  s <- getPriceTimeSeries(symbol, "daily")
  x <- dailyReturns(s$adj_close)$arithmetic
  param <- findistr::fitDTFM(x, distr)

  kelly <- kellyfractions::kellyDTFM(distr, param, rrate)
  entropy <- kellyfractions::entropyDTFM(distr, param, rrate)*252
  # print(KellyCriterion::sim_dtfm(n, as.numeric(s$adj_close[1]), 1800, distr, param, rrate))
  return(list(param = param, kelly = kelly, entropy = entropy))
}


#' Optimal log-growth allocation for single symbol
#'
#' @param symbol stock to invest
#' @param rollingWindow use NULL for entire price history or integer for most recent \code{n} days
#' @param rate risk-free bond yield rate
#' @param kf reduction for leveraged positions
#'
#' @description {Downloads prices, likelihood ratio tests mixture against stable
#' distribution then computes Kelly-criterion under results.}
#' @return vector
#' @importFrom graphics legend lines
#' @importFrom stats density
#' @export dtfm_strategy
dtfm_strategy <- function(symbol, rollingWindow = NULL, rate = 0, kf = 1)
{
  models <- c("gmm", "stable")
  r <- exp(rate/252)-1
  print(paste("1. Downloading", symbol, "prices from Alpha-Vantage"))
  stock <- getPriceTimeSeries(symbol, "daily")
  s <- stock$adj_close
  x <- dailyReturns(s)$arithmetic
  if(!is.null(rollingWindow) && nrow(x) >= rollingWindow)
  {
    x <- utils::tail(x, rollingWindow)
  } else
  {
    stop(paste("'rollingWindow' must be less than data-length", nrow(x)))
  }
  print("2. Fitting mixture and stable distributions to daily arithmetic returns")
  params <- lapply(models, function(X) findistr::fitDTFM(x, X))
  names(params) <- models
  # Empirical density and model densities
  epdf <- density(x)
  # Model densities
  pdfs <- mapply(function(X, Y) {
    findistr::ddtfm(epdf$x, X, Y)
  }, X = models, Y = params)

  # Plot comparisons of densities
  par(mfrow = c(1, 1))
  plot(epdf, type = "l", ylim = c(0, max(epdf$y, pdfs)), main = paste(symbol, "daily returns PDF"))
  lines(epdf$x, pdfs[, 1], col = "blue", lty = "dashed")
  lines(epdf$x, pdfs[, 2], col = "green", lty = "dashed")
  legend(x = "topright", legend = c("Empirical", "Mixture", "Stable"), col = c("black", "blue", "green"), lty = 1, cex = 0.6)

  print("3. Computing likelihood ratio test for mixture vs stable fit")
  LR_test <- findistr::likelihood_ratio(pdfs[, "stable"], pdfs[, "gmm"])
  if(length(LR_test) > 1)
  {
    if(LR_test[[3]] == "Choose H1: X~f")
    {
      print("Stable distribution is not rejected")
      model_fit <- "stable"
    } else if(LR_test[[3]] == "Choose H2: X~g")
    {
      print("Mixture distribution is not rejected")
      model_fit <- "gmm"
    }
  } else if(length(LR_test) == 1)
  {
    print("Likelihood ratio test inconclusive; choosing stable anyway")
    model_fit <- "stable"
  }

  print(paste("4. Computing log-optimal allocation under", model_fit))
  kelly <- kellyfractions::kellyDTFM(model_fit, params[[model_fit]], r)*kf
  growth <- kellyfractions::entropyDTFM(model_fit, params[[model_fit]], r)
  return(data.frame(kelly, growth = 252*growth, fractional = kf))
}


