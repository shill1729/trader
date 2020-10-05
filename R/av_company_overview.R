#' Get company overview
#'
#' @param symbol stock ticker to look up
#'
#' @description {Return company financial overview plus basic metrics.}
#'
#' @return list
#' @export company_overview
company_overview <- function(symbol)
{
  payload <- list("function" = "OVERVIEW",
                  symbol = symbol,
                  apikey = Sys.getenv("premium_api_key")
  )
  z <- httr::GET(url = av_endpoint(), query = payload)
  print(httr::http_status(z))
  dat <- jsonlite::fromJSON(txt = httr::content(x = z, type = "text", encoding = "UTF-8"))
  return(dat)
}


