#' Ad-hoc Poisson jump model
#'
#' @param symbol ticker symbol
#' @param jump_thresh threshold of jump size
#' @param side direction of jump: up or down
#'
#' @return numeric
#' @importFrom graphics par
#' @importFrom stats Box.test acf dpois pexp
#' @importFrom utils tail
#' @export pois_jumps
pois_jumps <- function(symbol, jump_thresh = 0.03, side = "up")
{
  ticker <- symbol
  s <- time_series(ticker, "daily")
  adj_prices <- s$adj_close
  jc <- count_jumps(adj_prices, jump_thresh = jump_thresh, side = side, period = "weekly")

  # Testing for independence
  # print(tseries::adf.test(x = jc))
  # print(tseries::pp.test(x = jc))
  print(Box.test(x = jc))
  par(mfrow = c(1, 1))
  acf(as.numeric(jc), main = paste(ticker, "ACF"))

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
  jc_daily <- count_jumps(adj_prices, jump_thresh = jump_thresh, side = side, period = "daily")
  last_jump_date <- as.Date(time(jc_daily[tail(which(jc_daily == 1), 1)]))
  date_yte(last_jump_date) # To initialize trading calendar
  days_since_last <- bizdays::bizdays(from = last_jump_date, to = Sys.Date(), cal = "trading")

  print(paste("Days since last jump:", days_since_last, "date:", last_jump_date))
  print(paste("Chance = ", 1-pexp(days_since_last/5, rate = z$poisFit$estimate)))
  return(z)
}
