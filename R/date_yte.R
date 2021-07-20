#' Convert dates to trading years
#'
#' @param expiry the date to convert, as a string/character, in the \code{yyyy-mm-dd} format.
#'
#' @description {Convert time until expiry date into trading years until expiration}
#' @return numeric, the time until the expiry date in trading years
#' @export date_yte
date_yte <- function(expiry)
{
  # If a financial calendar is not available, create one
  if(!bizdays::has_calendars("trading"))
  {
    bizdays::create.calendar(name = "trading",
                             weekdays = c("saturday", "sunday"),
                             financial = TRUE
                             )
    bizdays::bizdays.options$set(default.calendar = "trading")
  }
  time_diff <- bizdays::bizdays(from = Sys.Date(), to = as.Date(expiry), cal = "trading")/252
  return(time_diff)
}


#' Convert dates to trading years
#'
#' @param expiry the date to convert, as a string/character, in the \code{yyyy-mm-dd} format.
#'
#' @description {Convert time until expiry date into trading years until expiration. NOTE: this function includes
#' the exact time-difference but does not account for trading hours.}
#' @return numeric, the time until the expiry date in trading years
time_yte <- function(expiry)
{
  close_time <- as.POSIXct(paste(expiry, "16:00:00 EDT"), tz = Sys.timezone())
  dte <- as.numeric(difftime(close_time, Sys.time(), tz = Sys.timezone(), units = "days"))
  yte <- dte/252
  return(yte)
}

