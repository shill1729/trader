#' Download historical stock prices as a time-series from AV
#'
#' @param symbol stock ticker to look up
#' @param period period: "intraday", "daily", "weekly", "monthly"
#' @param datatype "json" or "csv"
#'
#' @description {Wrapper to AV API. Requires free or premium key.}
#' @details {Default 1 min for intraday, full output size, and adjusted.}
#'
#' @return parsed json or csv file
#' @export time_series
time_series <- function(symbol, period = "intraday", datatype = "json")
{
  if(period == "daily" || period == "weekly" || period == "monthly")
  {
    per <- paste(period, "_adjusted", sep = "")
  }
  # period = intraday requires separate payload
  if(period == "intraday")
  {
    payload <- list("function" = paste("TIME_SERIES_", toupper(period), sep = ""),
                    symbol = symbol,
                    interval = "1min",
                    adjusted = TRUE,
                    outputsize = "full",
                    datatype = datatype,
                    apikey = Sys.getenv("premium_api_key")
    )
  } else # period = daily, weekly, monthly have identical payloads
  {
    payload <- list("function" = paste("TIME_SERIES_", toupper(per), sep = ""),
                    symbol = symbol,
                    outputsize = "full",
                    datatype = datatype,
                    apikey = Sys.getenv("premium_api_key")
    )
  }
  # GET request
  request <- httr::GET(url = av_endpoint(), query = payload)
  # Print the status for the user
  print(httr::http_status(request))
  # Depending on requested data type we have different parsing and formatting
  if(datatype == "json")
  {
    # Parse the json response and then format
    response <- jsonlite::fromJSON(txt = httr::content(x = request, type = "text", encoding = "UTF-8"))
    # Rbind open high low close volume and unlist columns
    dat <- do.call(what = rbind, response[[2]])
    dat <- apply(dat, 2, unlist)
    # Extract time points, convert columns to numeric, matrix to data.frame
    time_points <- rownames(dat)
    dat <- apply(dat, 2, as.numeric)
    dat <- data.frame(time = time_points, dat)
    # Rename, add time row labels: column names change depending on period
    if(period == "intraday")
    {
      names(dat) <- c("time", "open", "high", "low", "close", "volume")
    } else if(period == "daily")
    {
      names(dat) <- c("time", "open", "high", "low", "close", "adj_close", "volume", "div_amt", "split_coef")
    } else if(period == "weekly" || period == "monthly")
    {
      names(dat) <- c("time", "open", "high", "low", "close", "adj_close", "volume", "div_amt")
    }

    # Finally convert to xts
    dat <- xts::xts(dat[, -1], order.by = as.POSIXct(x = dat[, 1]))
    return(dat)
  } else if(datatype == "csv")
  {
    # read_csv will do the heavy lifting for us in terms of formatting.
    dat <- readr::read_csv(httr::content(x = request, type = "text", encoding = "UTF-8"))
    return(dat)
  }

}


#' Load multiple stocks
#'
#' @param symbols list of stock symbols to look up
#'
#' @description {Returns common sample of historical prices of given portfolio.}
#' @return xts
#' @importFrom stats complete.cases
#' @export load_stocks
load_stocks <- function(symbols)
{
  tickers <- symbols
  stocks <- list()
  for(i in 1:length(tickers))
  {
    print(tickers[i])
    stocks[[i]] <- time_series(tickers[i], "daily")$adj_close
    if(i%%30==0)
    {
      Sys.sleep(60)
    }
  }
  # stocks <- lapply(tickers, function(x) time_series(x, "daily")$adj_close)
  stocks <- do.call(cbind, stocks)
  stocks <- stocks[complete.cases(stocks), ]
  colnames(stocks) <- tickers
  return(stocks)
}
