#' Log optimal allocations
#'
#' @param ticker stock ticker to download
#' @param jt jump threshold
#' @param rate bond rate
#' @param data_back data to go back for TEMA plot
#' @param n_tema TEMA window
#' @param N number of divisions in entropy grid
#'
#' @description {Implementation of optimal log utility for Merton jump diffusion}
#' @return list
logopt_stock <- function(ticker, jt = 0.01, rate = 0, data_back = 180, n_tema = 63, N = 100)
{
  # Load data, close prices, arithmetic and log returns
  sdata <- getPriceTimeSeries(ticker, period = "daily")
  s <- sdata$adj_close
  lx <- diff(log(as.numeric(s)))
  epdf <- stats::density(lx)
  # Empirical estimates
  B <- (lx) <= -jt
  Bb <- lx > -jt
  diffp <- qfin::fitGBM(as.numeric(s)[Bb])
  mu <- diffp[1]
  volat <- diffp[2]
  lambda <- (sum(B)/length(lx))*252
  jm <- mean(lx[B])
  jv <- stats::sd(lx[B])
  jump_param <- list(mean = jm, sd = jv)
  model_param <- data.frame(diffp, lambda = lambda, jm = jm, jv = jv)
  model_pdf <- qfin::dmerton(epdf$x, 1/252, drift = mu, volat, lambda, jm, jv)
  # Compute Kelly fraction via fixed point
  graphics::par(mfrow = c(2, 2))

  bet <- qfin::kelly_jdf_fixp(mu, rate, volat, lambda, distr = "norm", jump_param)
  max_growth <- qfin::entropy_jdf(bet, 1, 1, mu, rate, volat, lambda, distr = "norm", jump_param)
  # Check entropy/growth
  a <- seq(-2, 2, length.out = N)
  growth_rate <- matrix(N)
  for(i in 1:N)
  {
    growth_rate[i] <- qfin::entropy_jdf(a[i], 1, 1, mu, rate, volat, lambda, distr = "norm", jump_param)
  }

  # TEMA plotting for end
  plot(stats::time(utils::tail(s, data_back)), as.numeric(utils::tail(s, data_back)), type = "l", main = ticker)
  graphics::lines(stats::time(utils::tail(s, data_back)), qfin::tema(utils::tail(s, data_back), alpha = 2/(n_tema+1)), col = "purple")

  # Plotting of entropy/growth curve
  plot(a, growth_rate, type = "l", main = "Entropy")
  graphics::abline(v = bet)
  graphics::abline(h = max_growth)
  # Density function comparison of empirical vs estimated
  plot(epdf$x, epdf$y, type = "l", main = "Density")
  graphics::lines(epdf$x, model_pdf, col = "blue", lty = "dashed")
  # Print out estimated parameters and Kelly bet
  print(model_param)
  print("Optimal control")
  print(data.frame(optimal_control = bet, max_log_growth = max_growth))
  return(list(s = s, x = lx, parameters = model_param, output = data.frame(optimal_control = bet, max_log_growth = max_growth)))

}
