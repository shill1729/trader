#' One-period arithmetic returns of a given time-series
#'
#' @param s the time-series of prices
#'
#' @description {The daily arithmetic returns between two periods is the difference of the prices over the
#' two periods divided by the first period's price.}
#' @return vector
#' @export arithmetic_return
arithmetic_return <- function(s)
{
  # x <- diff(s)[-1]
  # r <- x
  # for(i in 1:length(x))
  # {
  #   if(s[i] == 0)
  #   {
  #     msg <- paste("Zero price in historical data @ index: ", i)
  #     stop(msg)
  #   }
  #   r[i] <- x[i]/s[i]
  # }

  # Just wrap to quantmod
  r <- quantmod::periodReturn(x = s, period = "daily", type = "arithmetic")
  r <- r[-1]
  names(r) <- names(s)
  return(r)
}


#' Daily log-returns
#'
#' @param prices the time-series of (adjusted close) prices
#'
#' @description {Returns the daily log returns of a price series}
#' @return numeric
#' @export log_return
log_return <- function(prices)
{
  return(diff(log(prices))[-1])
}
