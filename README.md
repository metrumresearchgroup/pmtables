
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pmtables <img align="right" src = "man/figures/metrum_pmtables_git_logo.png" width="135px">

<!-- badges: start -->
<!-- badges: end -->

The goal of pmtables is to create summary tables commonly used in
pharmacometrics.

## Installation

You can install the released version of pmtables from
[MPN](https://mpn.metworx.com/docs/) with:

``` r
mpn <- "https://mpn.metworx.com/snapshots/stable/2021-06-20"
install.packages("pmtables", repos = mpn)
```

This installs from a specific, dated snapshot. Please check
<https://mpn.metworx.com/docs/snapshots/> for the most recent snapshot
date.

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("metrumresearchgroup/pmtables")
```

# Documentation

We maintain a pmtables book
[here](https://github.com/metrumresearchgroup/pmt-book)

## Examples

### Data Disposition

![](man/figures/table-1.png)

### Continuous Covariate Summary

<img src="man/figures/table-2.png" style="width:80.0%" />

### Discrete Covariate Summary

<img src="man/figures/table-3.png" style="width:80.0%" />

# Examples in working docs

-   General table examples:
    [inst/demo-table.pdf](https://github.com/metrumresearchgroup/pmtables/blob/main/inst/demo-table.pdf)

-   General tables - pipe interface:
    [inst/demo-pipe.pdf](https://github.com/metrumresearchgroup/pmtables/blob/main/inst/demo-pipe.pdf)

-   Standard table examples:
    [inst/demo-pmtable.pdf](https://github.com/metrumresearchgroup/pmtables/blob/main/inst/demo-pmtable.pdf)

-   Long table examples:
    [inst/demo-longtable.pdf](https://github.com/metrumresearchgroup/pmtables/blob/main/inst/demo-longtable.pdf)

-   What is sanitized:
    [inst/demo-sanitize.pdf](https://github.com/metrumresearchgroup/pmtables/blob/main/inst/demo-sanitize.pdf)
