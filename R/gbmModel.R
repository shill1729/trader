#' A comprehensive analysis under a GBM model
#'
#' @param symbol stock symbol to analyze
#' @param days_back number of data points back to look at
#' @param period resolution: daily or months
#' @param adj whether to use adjusted close prices or not
#' @param envir environment to load stock data into
#'
#' @description {A in-depth look at a stock under a GBM model and all its associated properties,
#' like hitting times. The target is to double wealth within a year, and both
#' the Buy and Hold strategy and the Kelly criterion are compared to this. An update
#' will come eventually to allow users to choose target wealth goals.}
#' @return data.frame
#' @export gbm_model
gbm_model <- function(symbol, days_back = "full", period = "daily", adj = TRUE, envir = parent.frame())
{
  symbol_data <- paste(symbol, "_", period,"_data", sep = "")
  training_input <- ifelse(adj, "adj_close", "close")
  if(exists(symbol_data))
  {
    print("Data already loaded")
    dat <- eval(as.symbol(symbol_data))
  } else
  {
    if(symbol %in% c("DOGE", "BTC", "ETH", "LTC"))
    {
      dat <- ravapi::getAssets(symbol)
    } else
    {
      dat <- ravapi::getAssets(symbol, period)
    }
  }
  assign(x = symbol_data, value = dat, envir = envir)
  if(is.numeric(days_back))
  {
    if(days_back <= nrow(dat))
    {
      dat <- tail(dat, days_back)
    }
  } else if(!is.character(days_back))
  {
    stop("days_back must be set to 'full' if not using partial data")
  }

  x <- dailyReturns(dat[, training_input])$log
  # Fit GBM model
  gbm <- findistr::fitGBM(x)
  mu <- gbm[1]
  volat <- gbm[2]
  # Sharpe-Ratio
  lambda <- mu/volat
  # Expected log-growth from buy-and-hold, targets, and Kelly criterion
  g1 <- mu-0.5*volat^2
  g2 <- kellyfractions::wealthGoalEntropy(0, 1, 2, 1, mu, volat, 0)
  g3 <- kellyfractions::entropyGBM(mu, volat)
  mlg <- c(g1, g2, g3)
  names(mlg) <- c("b&h", "target", "kelly")
  a1 <- kellyfractions::wealthGoalControl(0, 1, 2, 1, mu, volat, 0)
  a2 <- kellyfractions::kellyGBM(mu, volat)
  a <- c(1, a1, a2)
  names(a) <- c("b&h", "target", "kelly")
  # Hitting modes, in days
  t1 <- findistr::hitmode_gbm(gbm, 2, 1)
  t2 <- findistr::hitmode_gbm(c(mu*a1, volat*a1), 2, 1)
  t3 <- findistr::hitmode_gbm(c(lambda^2, lambda), 2, 1)
  hitModes <- c(t1, t2, t3)*252
  names(hitModes) <- c("b&h", "target", "kelly")
  p1 <- 1-findistr::pgbm(2, 1, 1, mu, volat)
  p2 <- kellyfractions::wealthGoalChance(0, 1, 2, 1, mu, volat, 0)
  p3 <- 1-findistr::pgbm(2, 1, 1, lambda^2, lambda)
  p <- c(p1, p2, p3)
  names(p) <- c("b&h", "target", "kelly")
  qq1 <- findistr::phit_gbm(1, gbm, 2, 1)
  qq2 <- findistr::phit_gbm(1, c(mu*a1, volat*a1), 2, 1)
  qq3 <- findistr::phit_gbm(1, c(lambda^2, lambda), 2, 1)
  qq <- c(qq1, qq2, qq3)
  names(qq) <- c("b&h", "target", "kelly")
  q1 <- findistr::pgbm(1, 1, 1, mu, volat)
  q2 <- findistr::pgbm(1, 1, 1, mu*a1, volat*a1)
  q3 <- findistr::pgbm(1, 1, 1, lambda^2, lambda)
  q <- c(q1, q2, q3)
  names(q) <- c("b&h", "target", "kelly")
  output <- rbind(a, mlg, hitModes, qq, p, q)
  rownames(output) <- c("control", "Elog-growth", "hitmodes", "chanceHit", "targetChance", "lossChance")
  return(output)
}



