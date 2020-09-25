#' Fit returns distribution to daily-arithmetic returns
#'
#' @param x time series of daily arithmetic returns
#' @param distr distribution name: "norm" or "unif"
#'
#' @return vector
#' @export fitDTFM
fitDTFM <- function(x, distr = "norm")
{
  if(distr == "unif")
  {
    return(c(min = min(x), max = max(x)))
  } else if(distr == "norm")
  {
    return(c(mean = mean(x), sd = stats::sd(x)))
  }
}

#' Probability density function of fitted model
#'
#' @param r region of returns
#' @param distr distribution name: "norm" or "unif"
#' @param param parameters of distribution as a vector
#'
#' @return numeric
#' @export dDTFM
dDTFM <- function(r, distr = "norm", param)
{
  ddistr <- paste("d", distr, sep = "")
  ddistr <- get(ddistr)
  return(do.call(ddistr, args = c(list(r), as.list(param))))
}
