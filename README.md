
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pmtables

<!-- badges: start -->

<!-- badges: end -->

Tables for pharmacometrics

## Installation

Installation information to be updated when ready.

## Examples

Continuous covariate summary\!

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(pmtables)
library(pmplots)
```

    ## Loading required package: ggplot2

``` r
data <- pmplots_data_id()
```

``` r
pt_cont_study(data, cols = "BMI,ALB,AAG", study_col = vars(Study = STUDYc)) 
```
