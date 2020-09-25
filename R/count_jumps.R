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
#' @export count_jumps
count_jumps <- function(prices, jump_thresh = 0.01, return_type = "arithmetic", side = "down", period = "weekly")
{
  x <- daily_returns(prices)[, return_type]
  indicator_up <- ifelse(x >= jump_thresh, 1, 0)
  indicator_down <- ifelse(x <= -jump_thresh, 1, 0)
  indicator_both <- ifelse(abs(x) >= jump_thresh, 1, 0)
  indicator <- eval(as.name(paste("indicator_", side, sep = "")))
  apply_period <- paste("apply.", period, sep = "")
  apply_period <- get(apply_period, envir = environment(xts))
  jump_counts <- apply_period(indicator, sum)
  return(jump_counts)
}
