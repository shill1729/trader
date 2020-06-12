#' Setup defaults for quantmod to pull data from Alpha-Vantage
#'
#' @description {Set defaults for quantmod functions for pulling data from Alpha Vantage.}
#' @details {Make sure to register with Alpha Vantage and obtain a free API key to use their data
#' services. See documentation for \code{getSymbols} or \code{getSymbols.av} for more info, from the
#' \code{quantmod} package. Then be sure to store this API key as an environment variable, with the name
#' \code{api.key}.}
#' @return null
#' @export setup_av
setup_av <- function()
{
  quantmod::setDefaults(quantmod::getSymbols.av, api.key = Sys.getenv("api.key"))
  quantmod::setDefaults(quantmod::getSymbols, src = "av")
  return(NULL)
}
