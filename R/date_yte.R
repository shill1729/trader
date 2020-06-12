#' Convert dates to trading years
#'
#' @param expiry the date to convert, as a string/character, in the \code{yyyy-mm-dd} format.
#'
#' @description {Convert time until expiry date into trading years until expiration}
#' @return numeric, the time until the expiry date in trading years
#' @export date_yte
date_yte <- function(expiry)
{
  # as.Date has good enough error messages.
  return(as.numeric(as.Date(expiry)-Sys.Date())/252)
}


#' Convert dates to trading years
#'
#' @param expiry the date to convert, as a string/character, in the \code{yyyy-mm-dd} format.
#'
#' @description {Convert time until expiry date into trading years until expiration. NOTE: this function includes
#' the exact time-difference but does not account for trading hours.}
#' @return numeric, the time until the expiry date in trading years
#' @export time_yte
time_yte <- function(expiry)
{
  close_time <- as.POSIXct(paste(expiry, "16:00:00 EDT"), tz = Sys.timezone())
  dte <- as.numeric(difftime(close_time, Sys.time(), tz = Sys.timezone(), units = "days"))
  yte <- dte/252
  return(yte)
}

