
# trader

<!-- badges: start -->
<!-- badges: end -->

The package trader provides a library/wrapper to the Alpha Vantage API provided a free or premium API key (see below). Additionally, various trading models based on historical data are provided, none of which (disclaimer) is investment advice.

## Installation

You can install the package via devtools from github

``` r
devtools::install_github("shill1729/trader")
```
Note that all the trading-model functions depend on the package qfin.

## Setting up your API key in .Renviron
Register with **[Alpha Vantage](https://www.alphavantage.co/support/#api-key)** to obtain a free API key or subscribe for a premium API key. Save these in your .Renviron file with
```r
usethis::edit_r_environ()
```
and simply set in the .Renviron file the following variables:
```r
free_api_key = "YOUR_KEY_CODE1"
premium_api_key = "YOUR_KEY_CODE2"
```
where the RHS is the actual key obtained from AV. The second line is not required obviously if you only plan on registering a free API key.

By default a premium key is assumed in every function that calls the AV API. In the future we will add an option to set defaults.

## Examples
Here are various examples on how to use the package:
### Downloading a single stock's price history
```r
library(trader)
symbol <- "NTDOY"
s <- getPriceTimeSeries(symbol) # Default key = "premium"

# Plot the price chart
print(plot(s$adj_close, main = symbol))

# Print latest adjust close prices
print(tail(s$adj_close))
```

