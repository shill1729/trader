#' Chart a candlestick series of a stock with a TEMA filter and volume
#'
#' @param symbol the stock symbol
#' @param days_back number of days back to view
#' @param tema_window the TEMA moving window
#' @param period "daily", "weekly", "monthly", "yearly" or "intraday", only for stocks. Coins return daily series only and this argument is ignored.
#' @param adj whether to use adjusted close prices or not (boolean)
#' @param envir environment to load stock data into
#'
#' @description {Chart a candlestick series of a stock price or cryptocurrency history.
#' In blue/red is the long/short-term TEMA filter on either the close or adjusted close prices,
#' and below is the volume.}
#' @return NULL
#' @import ggplot2
#' @importFrom rlang .data
#' @export chart
chart <- function(symbol, days_back = 120, tema_window = 50, period = "daily", adj = FALSE, envir = parent.frame())
{
  symbol_data <- paste(symbol, "_", period,"_data", sep = "")
  tema_input <- ifelse(adj, "adj_close", "close")
  alpha <- 2/(tema_window+1)
  alpha2 <- 2/(tema_window/2+1)

  if(exists(symbol_data))
  {
    print("Data already loaded")
    dat <- eval(as.symbol(symbol_data))
  } else
  {
    if(symbol %in% c("DOGE", "BTC", "ETH", "LTC"))
    {
      dat <- ravapi::getAssets(symbol, period, interval = "1min")
    } else
    {
      dat <- ravapi::getAssets(symbol, period, interval = "1min")
    }
  }
  # Don't store intraday data because we want to keep it real-time for crypto
  if(period != "intraday")
  {
    assign(x = symbol_data, value = dat, envir = envir)
  }

  if(days_back <= nrow(dat))
  {
    dat <- tail(dat, days_back)
  }
  dat <- data.frame(Date = as.POSIXct(time(dat)), dat)
  # Custom candlesticks.
  dat$chg <- ifelse(dat$close > dat$open, "up", "down")
  dat$colour <- ifelse(dat$close > dat$open, "green", "red")
  dat$width <- as.numeric(xts::periodicity(dat)[1])
  dat$flat_bar <- dat$high == dat$low
  dat$tema <- trader::tema(dat[, tema_input], alpha = alpha)
  dat$tema2 <- trader::tema(dat[, tema_input], alpha = alpha2)

  # Plotting with ggplot2
  plt <- ggplot(dat, aes(x = .data$Date))+
    geom_linerange(aes(ymin=.data$low, ymax=.data$high)) +
    theme_bw() +
    labs(title=symbol) +
    geom_rect(aes(xmin = .data$Date-.data$width*0.9*0.5,
                  xmax = .data$Date+.data$width*0.9*0.5,
                  ymin = pmin(open, close),
                  ymax = pmax(open, close), fill = .data$chg)) +
    guides(fill = FALSE, colour = FALSE) +
    scale_fill_manual(values = c("down" = "darkred", "up" = "darkgreen"))
  # Handle special case of drawing a flat bar where OHLC = Open:
  if (any(dat$flat_bar)) plt <- plt + geom_segment(data = dat[dat$flat_bar,], aes(x = .data$Date - .data$width / 2 * 0.9, y = close, yend = close, xend = .data$Date + .data$width / 2 * 0.9))
  # Adding tema filter
  plt <- plt +
    geom_line(mapping = aes(x = .data$Date, y = .data$tema), data = dat, colour = "red")+
    geom_line(mapping = aes(x = .data$Date, y = .data$tema2), data = dat, colour = "blue")+ylab("Price")
  # Adding volume
  vo_plt <- ggplot(dat, aes(x = .data$Date))+
    geom_bar(stat = "identity", mapping = aes(x = .data$Date, y = .data$volume), fill = dat$colour)+
    theme(axis.text.y = element_text(size = 5))
  gridExtra::grid.arrange(plt, vo_plt, nrow = 2, heights = c(2, 1))
}
