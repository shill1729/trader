
# trader

<!-- badges: start -->
<!-- badges: end -->

The goal of trader is to provide a wrapper to AV api if you have a free or premium api key, along with various mathematical finance functions.

## Installation

You can install the package via devtools from github

``` r
devtools::install_github("shill1729/trader")
```

## Setting up your API key in .Renviron
Register with Alpha-Vantage to obtain a free API key or subscribe for a premium API key. Save these in your .Renviron file with
```r
usethis::edit_r_environ()
```
and simply set in the .Renviron file the following variables:
```r
free_api_key = "YOUR_KEY_CODE1"
premium_api_key = "YOUR_KEY_CODE2"
```
where the RHS is the actual key obtained from AV.

By default a premium key is assumed in every function that calls the AV API. In the future we will add an option to set defaults.




