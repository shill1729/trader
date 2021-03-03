#' Backend function for Alpha-vantage endpoints
#'
#' @description {Returns the endpoint url for Alpha-Vantage's API.}
#' @return string of the url "https://www.alphavantage.co/query"
avEndpoint <- function()
{
  av_url <- "https://www.alphavantage.co/query"
  return(av_url)
}

#' Global instrument quote via Alpha-Vantage
#'
#' @param symbol symbol to look up
#' @param datatype "json" or "csv" return type from AV
#' @param key "premium" or "free"; the type of API key obtained from AV
#'
#' @description {Look up global quote via AV.}
#'
#' @return data.frame containing columns
#' \code{symbol}, \code{open}, \code{high}, \code{low}, \code{price}, \code{volume},
#' \code{latest_trading_day}, \code{previous_close}, \code{change}, \code{change_percent}.
#' @export getQuoteAV
getQuoteAV <- function(symbol, datatype = "json", key = "premium")
{
  if(key != "premium" && key != "free")
  {
    stop("argument 'key' must be either 'premium' or 'free'")
  }
  apikey <- paste(key, "_api_key", sep = "")
  payload <- list("function" = "GLOBAL_QUOTE",
                  symbol = symbol,
                  apikey = Sys.getenv(apikey),
                  datatype = datatype
  )
  request <- httr::GET(url = avEndpoint(), query = payload)
  print(unlist(httr::http_status(request)))
  if(datatype == "json")
  {
    dat <- jsonlite::fromJSON(txt = httr::content(x = request, type = "text", encoding = "UTF-8"))
    # Clean up the response
    gquote <- data.frame(dat$`Global Quote`)
    names(gquote) <- substring(names(dat$`Global Quote`), 5)
    names(gquote) <- gsub(" ", "_", names(gquote))
    # Convert change_percent separately to divide by 100
    gquote$change_percent <- as.numeric(gsub("%", "", gquote$change_percent))/100
    columns <- c("open", "high", "low", "price", "volume", "previous_close", "change")
    # Convert the rest of the revelant columns to numeric
    gquote[, columns] <- lapply(columns, function(x) as.numeric(gquote[[x]]))
    return(gquote)
  } else if(datatype == "csv")
  {
    dat <- readr::read_csv(httr::content(x = request, type = "text", encoding = "UTF-8"))
    return(dat)
  }

}

#' Get company overview
#'
#' @param symbol stock ticker to look up
#' @param key "premium" or "free"; the type of API key obtained from AV
#'
#' @description {Return company financial overview plus basic metrics.}
#'
#' @return list
#' @export getCompanyOverviewAV
getCompanyOverviewAV <- function(symbol, key = "premium")
{
  if(key != "premium" && key != "free")
  {
    stop("argument 'key' must be either 'premium' or 'free'")
  }
  apikey <- paste(key, "_api_key", sep = "")
  payload <- list("function" = "OVERVIEW",
                  symbol = symbol,
                  apikey = Sys.getenv(apikey)
  )

  z <- httr::GET(url = avEndpoint(), query = payload)
  print(unlist(httr::http_status(z)))
  dat <- jsonlite::fromJSON(txt = httr::content(x = z, type = "text", encoding = "UTF-8"))
  return(dat)
}

#' Download historical stock prices as a time-series from AV
#'
#' @param symbol stock ticker to look up
#' @param period period: "intraday", "daily", "weekly", "monthly"
#' @param datatype "json" or "csv"
#' @param key "premium" or "free"; the type of API key obtained from AV
#'
#' @description {Wrapper to AV API. Requires free or premium key.}
#' @details {Default 1 min for intraday, full output size, and adjusted.}
#'
#' @return parsed json or csv file
#' @export getPriceTimeSeries
getPriceTimeSeries <- function(symbol, period = "daily", datatype = "json", key = "premium")
{
  # Input-error handling
  if(key != "premium" && key != "free")
  {
    stop("argument 'key' must be either 'premium' or 'free'")
  }
  apikey <- paste(key, "_api_key", sep = "")


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
                    apikey = Sys.getenv(apikey)
    )
  } else # period = daily, weekly, monthly have identical payloads
  {
    payload <- list("function" = paste("TIME_SERIES_", toupper(per), sep = ""),
                    symbol = symbol,
                    outputsize = "full",
                    datatype = datatype,
                    apikey = Sys.getenv(apikey)
    )
  }
  # GET request
  request <- httr::GET(url = avEndpoint(), query = payload)
  # Print the status for the user
  print(unlist(httr::http_status(request)))
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
#' @param key "premium" or "free"; the type of API key obtained from AV
#'
#' @description {Returns common sample of historical prices of given portfolio.}
#' @return xts
#' @importFrom stats complete.cases
#' @export getStocks
getStocks <- function(symbols, key = "premium")
{
  if(key == "premium")
  {
    callLimit <- 30
  } else if(key == "free")
  {
    callLimit <- 5
  }
  tickers <- symbols
  stocks <- list()
  for(i in 1:length(tickers))
  {
    print(tickers[i])
    stocks[[i]] <- getPriceTimeSeries(symbol = tickers[i], period = "daily", key = key)$adj_close
    if(i%%callLimit==0)
    {
      Sys.sleep(60)
    }
  }
  stocks <- do.call(cbind, stocks)
  stocks <- stocks[complete.cases(stocks), ]
  colnames(stocks) <- tickers
  return(stocks)
}


#' Download daily time series of cryptocurrency from Alpha-Vantage
#'
#' @param symbol cryptocurrency symbol, i.e. "BTC" (string)
#' @param market physical currency symbol for quoting i.e. "USD", "CNY", etc.
#' @param datatype "json" or "csv" for return type to be parsed
#' @param key string for the API key to use Alpha Vantage
#'
#' @description {Download cryptocurrency daily time series from Alpha vantage using
#' either a free or premium API key. Includes open high low close, volume and market cap.}
#' @return xts object
#' @export getCryptoCurrency
getCryptoCurrency <- function(symbol = "BTC", market = "USD", datatype = "json", key = "premium")
{
  # TODO add symbol check in digital_currency_list.csv from AV
  # TODO add market check in physical_currency_list.csv from AV
  if(key != "premium" && key != "free")
  {
    stop("argument 'key' must be either 'premium' or 'free'")
  }
  if(datatype != "json" && datatype != "csv")
  {
    stop("argument 'datatype' must be either 'json' or 'csv'")
  }

  apikey <- paste(key, "_api_key", sep = "")
  payload <- list("function" = "DIGITAL_CURRENCY_DAILY",
                  symbol = symbol,
                  market = market,
                  datatype = datatype,
                  apikey = Sys.getenv(apikey)
  )
  request <- httr::GET(url = avEndpoint(), query = payload)
  if(datatype == "json")
  {
    # Parse the json response and then format
    response <- httr::content(x = request, type = "application/json")
    # Rbind open high low close volume and unlist columns
    dat <- do.call(what = rbind, response[[2]])
    dat <- apply(dat, 2, unlist)

    # Extract time points, convert columns to numeric, matrix to data.frame
    time_points <- rownames(dat)
    dat <- apply(dat, 2, as.numeric)
    dat <- data.frame(time = time_points, dat)
    dat <- dat[, c(1, (1:5)*2, 11)]
    colnames(dat) <- c("time", "open", "high", "low", "close", "volume", "market_cap")
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

#' Load multiple cryptocurrencies
#'
#' @param symbols list of cryptocurrency symbols to look up
#' @param key "premium" or "free"; the type of API key obtained from AV
#'
#' @description {Returns common sample of historical prices of given portfolio of cryptocurrencies.}
#' @return xts
#' @importFrom stats complete.cases
#' @export getCoins
getCoins <- function(symbols, key = "premium")
{
  if(key == "premium")
  {
    callLimit <- 30
  } else if(key == "free")
  {
    callLimit <- 5
  }
  tickers <- symbols
  coins <- list()
  for(i in 1:length(tickers))
  {
    print(tickers[i])
    coins[[i]] <- getCryptoCurrency(symbol = tickers[i], key = key)$close
    if(i%%callLimit==0)
    {
      Sys.sleep(60)
    }
  }
  coins <- do.call(cbind, coins)
  coins <- coins[complete.cases(coins), ]
  colnames(coins) <- tickers
  return(coins)
}



