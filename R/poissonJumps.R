#' Count the number of jumps per period above a threshold
#'
#' @param prices the time-series of close prices
#' @param jump_thresh the jump-threshold (positive)
#' @param return_type the return type "log" or "arithmetic
#' @param side the side of the jump: "up", "down" or "both"
#' @param period the period, "daily", "weekly", "monthly", "quarterly", "yearly"
#'
#' @description {Counts the number of jumps \eqn{X_n >= a} (and the other respective events) for some jump-threshold \eqn{a} for different periods}
#' @return time-series of counts per period
#' @import xts
#' @export countJumps
countJumps <- function(prices, jump_thresh = 0.01, return_type = "arithmetic", side = "down", period = "weekly")
{
  x <- dailyReturns(prices)[, return_type]
  indicator_up <- ifelse(x >= jump_thresh, 1, 0)
  indicator_down <- ifelse(x <= -jump_thresh, 1, 0)
  indicator_both <- ifelse(abs(x) >= jump_thresh, 1, 0)
  indicator <- eval(as.name(paste("indicator_", side, sep = "")))
  apply_period <- paste("apply.", period, sep = "")
  apply_period <- get(apply_period, envir = environment(xts))
  jump_counts <- apply_period(indicator, sum)
  return(jump_counts)
}


#' Ad-hoc Poisson jump model
#'
#' @param symbol ticker symbol; can be stock ticker or a cryptocurrency
#' @param jump_thresh threshold of jump size
#' @param side direction of jump: up or down
#'
#' @description {An ad-hoc model fitting a Poisson distribution to the number of
#' jumps excess a threshold per week.}
#' @return numeric
#' @importFrom graphics par
#' @importFrom stats Box.test acf dpois pexp
#' @importFrom utils tail
#' @export poissonJumps
poissonJumps <- function(symbol, jump_thresh = 0.03, side = "up")
{
  adj_prices <- 0
  if(!symbol %in% c("BTC", "DOGE", "ETH", "LTC"))
  {
    s <- getPriceTimeSeries(symbol, "daily")
    adj_prices <- s$adj_close
  } else
  {
    s <- getCryptoCurrency(symbol)
    adj_prices <- s$close
  }

  jc <- countJumps(adj_prices, jump_thresh = jump_thresh, side = side, period = "weekly")

  # Testing for independence
  # print(tseries::adf.test(x = jc))
  # print(tseries::pp.test(x = jc))
  print(Box.test(x = jc))
  par(mfrow = c(1, 1))
  acf(as.numeric(jc), main = paste(symbol, "ACF"))

  # Poisson fit + evaluating goodness of fit
  z <- poissonFits::poissonFit(countsData = jc)
  print("Date range:")
  print(time(adj_prices)[c(1, length(adj_prices))])
  print("Number of samples in time-series")
  print(length(adj_prices))
  print("Time between jumps")
  print(c(weeks = 1/z$poisFit$estimate, days = 5/z$poisFit$estimate))
  print(paste("At least one jump", 1-dpois(x = 0, lambda = z$poisFit$estimate)))

  xtable::xtable(x = z$chiTest$table)
  xtable::xtable(x = z$chiTest$chisq_test)

  # Days since last jump
  jc_daily <- countJumps(adj_prices, jump_thresh = jump_thresh, side = side, period = "daily")
  last_jump_date <- as.Date(time(jc_daily[tail(which(jc_daily == 1), 1)]))
  date_yte(last_jump_date) # To initialize trading calendar
  days_since_last <- bizdays::bizdays(from = last_jump_date, to = Sys.Date(), cal = "trading")

  print(paste("Days since last jump:", days_since_last, "date:", last_jump_date))
  print(paste("Chance = ", 1-pexp(days_since_last/5, rate = z$poisFit$estimate)))
  return(z)
}
