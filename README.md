
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
s <- getPriceTimeSeries(symbol, key = "free")

# Plot the price chart
print(plot(s$adj_close, main = symbol))

# Print latest adjust close prices
print(tail(s$adj_close))
```
### Download multiple stocks with common history
When downloading multiple stocks the data-set will go back as far
as the latest common date between all assets. As of now, only adjusted
close prices are available. In the future options to return any of the OHLC prices will be available.

```r
library(trader)
symbols <- c("NTDOY", "ROKU", "TSLA")
s <- getStocks(symbols, key = "free")
print(plot(s))
print(tail(s))
```

### Optimal log-utility investment under discrete-time stable vs Gaussian mixture returns
This function performs four tasks:
1. Downloads a stock's prices from AV
2. Fits a Gaussian mixture distribution and stable distribution to
daily **arithmetic** returns.
3. Computes a likelihood ratio test to reject one of the two fits, if possible.
4. Computes the optimal log-utility allocation in the stock.

Returned is the optimal fraction, the long-term optimal growth rate, and the reduction fraction together with a plot of the empirical density function compared to the model densities (Gaussian mixture and stable).
```r
library(trader)
symbol <- "NTDOY"
z <- dtfm_strategy(symbol)
print(z)
```

