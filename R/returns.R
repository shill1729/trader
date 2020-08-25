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
  x <- diff(s)/(s[1:(length(s)-1)])
  return(x[-1])
}
