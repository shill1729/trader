#' Risk profile under four discrete time models
#'
#' @param symbol stock ticker
#' @param singleAriRet single-asset daily arithmetic returns
#' @param rate risk-free date earned on cash
#' @param rollingWindow rolling window size to use, or NULL for full sample size
#'
#' @description {Estimates the parameters for four distributions for discrete time
#' financial returns models, provides the Kelly fractions under such distributions,
#' together with a comparison of densities and value at risk under the stable distribution.
#' }
#' @return list
#' @export discreteTimeRiskProfile
discreteTimeRiskProfile <- function(symbol, singleAriRet, rate = 0, rollingWindow = NULL)
{
  # Function call starts here:
  if(is.null(rollingWindow))
  {
    dataSet <- singleAriRet
  } else
  {
    dataSet <- tail(singleAriRet, rollingWindow)
  }
  # Fit four distributions to data
  models <- c("unif", "norm", "gmm", "stable")
  # Estimation routine
  parameters <- lapply(models, function(y) findistr::fitDTFM(x = dataSet, distr = y))
  names(parameters) <- models

  # Compute empirical densitiy
  epdf <- density(dataSet)
  x <- epdf$x
  y <- epdf$y
  # Compute all model probability density functions
  model_pdfs <- mapply(function(X, Y) {
    findistr::ddtfm(x, X, Y)
  }, X = models, Y = parameters)
  # Plot all densities together for visual comparison
  plot(x, y, type = "l", main = paste(symbol, "returns PDF"))
  for(i in 1:4)
  {
    lines(x, model_pdfs[, i], col = i+1)
    print(colnames(model_pdfs)[i])
  }
  legend(x = "topright", legend = c("empirical", "uniform", "gaussian", "mixture", "stable"), col = c(1:5), lty = 1, cex = 0.5)

  # Compute all model probability density functions
  model_kellys <- mapply(function(X, Y) {
    kellyfractions::kellyDTFM(X, Y, rate = (exp(rate)^(1/252)-1))
  }, X = models, Y = parameters)
  model_kellys

  # Likelihood ratio test for stable vs gmm
  lrt <- findistr::likelihood_ratio(f = model_pdfs[, "stable"], g = model_pdfs[, "gmm"])
  vrisk <- findistr::stableVAR(p = 0.99, pars = parameters$stable)
  # Gather together in one output
  output <- list(parameters = parameters, lrt = lrt[[3]], valueAtRisk = vrisk, fractions = model_kellys)
  return(output)
}
