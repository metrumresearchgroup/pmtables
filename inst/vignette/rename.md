rename
================

``` r
library(pmtables)
vars <- dplyr::vars
```

# Motivation

I wanted a way to capture column names as a character vector along with
a second set of names for renaming those data items. The primary need
was a vector of names to use rather columns to select from a data set.

# Solution

`new_names` class with methods for character and quosure. This is two
methods and the generic.

``` r
pmtables:::new_names("a,b,c")
```

    .   a   b   c 
    . "a" "b" "c"

``` r
pmtables:::new_names(c("a", "b", "c"))
```

    .   a   b   c 
    . "a" "b" "c"

``` r
pmtables:::new_names(vars(a,b,c))
```

    .   a   b   c 
    . "a" "b" "c"

``` r
pmtables:::new_names(vars(A = a, B = b, C = c))
```

    .   A   B   C 
    . "a" "b" "c"

# Also accepts rename as named list

  - new names cal also be added through a named list
  - this is the third function

<!-- end list -->

``` r
re <- list(A = "a", B = "b")

pmtables:::new_names("A,B", table = re)
```

    .   a   b 
    . "A" "B"

This is a little backward from the `rename(new_name = old_name)` syntax;
but it is made to handle output from yspec, where things are already
keyed by the existing column name. This could easily be modified to
invert names / values during this lookup process.

## Works with select

  - My main movitation was to get a character vector along with re-names
    in a convenient and flexible way. The output can also be used to
    drive tidyverse select and rename functionality:

<!-- end list -->

``` r
library(dplyr)
x <- pmtables:::new_names(vars(b = B, a = A))

x
```

    .   b   a 
    . "B" "A"

``` r
tab <- tibble(A = 1, B = 2)


tab
```

    . # A tibble: 1 x 2
    .       A     B
    .   <dbl> <dbl>
    . 1     1     2

``` r
select(tab,all_of(x))
```

    . # A tibble: 1 x 2
    .       b     a
    .   <dbl> <dbl>
    . 1     2     1

## Or rename

``` r
rename(tab, all_of(x))
```

    . # A tibble: 1 x 2
    .       a     b
    .   <dbl> <dbl>
    . 1     1     2
