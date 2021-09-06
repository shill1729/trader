#' Merton vs Mixture likelihood ratio test
#'
#' @param logReturns time-series of daily log returns
#' @param fits named list of \code{merton} parameters (vector) and \code{mixture} parameters (matrix)
#' @param spot the current price
#' @param rate the discounting rate
#' @param symbol relevant stock symbol name
#' @param plotDensities boolean for plotting
#'
#' @description {Run a likelihood ratio test to reject one of the Merton or mixture model against
#' the empirical density.}
#' @return list
#' @export mertonMixtureDecision
mertonMixtureDecision <- function(logReturns, fits, spot, rate = 0, symbol = "", plotDensities = TRUE)
{
  mertonParam <- fits$merton
  mixtureFit <- fits$mixture
  epdf <- density(logReturns)
  gmmDen <- findistr::dgmm(epdf$x, mixtureFit[1, ], (mixtureFit[2, ]-0.5*mixtureFit[3, ]^2)/252, mixtureFit[3, ]/sqrt(252))
  mertonDen <- findistr::dmerton(epdf$x, 1/252, mertonParam)
  # Plotting finally
  if(plotDensities)
  {
    graphics::par(mfrow = c(1, 1))
    plot(epdf$x, epdf$y, type = "l", main = symbol, ylim = c(0, max(epdf$y, gmmDen, mertonDen)))
    lines(epdf$x, gmmDen, col = "blue")
    lines(epdf$x, mertonDen, col = "purple")
    graphics::legend("topright",
           legend = c("empirical", "mixture", "merton"),
           col = c("black", "blue", "purple"),
           lty = 1, cex = 0.6)

  }
  print("Merton = f,  Mixture = g")
  lrt <- findistr::likelihood_ratio(f = mertonDen, g = gmmDen[gmmDen>0])
  print(lrt)
  if(lrt$result == "Choose H1: X~f")
  {
    param <- mertonParam
    k <- kellyfractions::kellyMerton(param, rate)
    g <- kellyfractions::entropyMerton(param, rate)
    output <- data.frame(model = "merton", leverage = k$root, growth = g)
  } else if(lrt$result == "Choose H2: X~g")
  {
    param <- mixtureFit
    print(param)
    # TODO consider optionality for log(s/spot) past data
    k <- kellyfractions::kellyMixtureDiffusion(0, spot, spot, rate, param)
    g <- kellyfractions::entropyMixtureDiffusion(1, spot, spot, rate, param, resolution = c(100, 100))
    output <- data.frame(model = "mixture", leverage = k, growth = g)
  } else
  {
    output <- NULL
    warning(lrt$result)
  }
  return(output)

}
