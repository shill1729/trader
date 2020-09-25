#' Backend function for Alpha-vantage endpoints
#'
#' @description {Returns the endpoint url for Alpha-Vantage's API.}
#' @return string
av_endpoint <- function()
{
  av_url <- "https://www.alphavantage.co/query"
  return(av_url)
}
