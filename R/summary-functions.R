#' Function for continuous wide summaries
#'
#' @inheritParams pt_cont_wide
#' @param value the data to summarize
#' @param digit_fun a function to format digits in the summaries
#' @param id a vector of subject IDs; same length as `value`
#' @param digits the number of digits in the summary; the current implementation
#' passes `digits` to `digit_fun()`
#' @param ... not used
#'
#' @return A tibble with one row and one column named `summary`; the summary
#' has this format: `mean (sd) [count]` for all non-missing data in `value`.
#'
#' @examples
#' pmtables:::cont_wide_fun(rnorm(100))
#'
#' @keywords internal
cont_wide_fun <- function(value,digit_fun=sig,id=NULL,digits=3,
                          na_fill = "--",...) {
  if(is.null(id)) {
    n <- sum(!is.na(value))
  } else {
    n <- length(unique(id))
  }
  value <- na.omit(value)
  if(length(value)==0) {
    if(is.null(na_fill)) {
      return(tibble())
    }
    if(is.character(na_fill)) {
      return(tibble(summary  = na_fill))
    }
    stop("no non-missing values in the summary",call.=FALSE)
  }
  mn <- digit_fun(mean(value),digits=digits)
  sd <- digit_fun(sd(value),digits=digits)
  ans <- tibble(summary = paste0(mn, " (",sd,")", " [",n,"]"))
  ans
}
#' Function for continuous long summaries
#'
#' @inheritParams cont_wide_fun
#'
#' @examples
#' pmtables:::cont_long_fun(rnorm(100))
#'
#' @return
#' The function returns a tibble with one row and five columns:
#' 1. `n` the number of non-missing observations in `value`
#' 1. `Mean` the mean of `value`
#' 1. `Median` the median of `value`
#' 1. `SD` the standard deviation of `value`
#' 1. `Min / Max` the range of value
#'
#' All columns are returned as `character`.
#'
#' @keywords internal
cont_long_fun <- function(value,digit_fun=sig,id=NULL,digits=3,
                          na_fill = "--",...) {
  if(is.null(id)) {
    n <- sum(!is.na(value))
  } else {
    n <- length(unique(id))
  }
  value <- na.omit(value)
  if(length(value)==0) {
    if(is.null(na_fill)) {
      return(tibble()[0,])
    }
    if(is.character(na_fill)) {
      ans <-  tibble(
        n = na_fill,
        Mean = na_fill,
        Median = na_fill,
        SD = na_fill,
        `Min / Max`  = na_fill
      )
      return(ans)
    }
    stop("no non-missing values in the summary",call.=FALSE)
  }
  rng <- digit_fun(range(value),digits=digits)
  rng <- paste0(rng[1]," / ", rng[2])
  ans <- tibble(
    n = as.character(n),
    Mean = digit_fun(mean(value),digits = digits),
    Median = digit_fun(median(value),digits = digits),
    SD = digit_fun(sd(value),digits = digits),
    `Min / Max` = rng
  )
  ans
}

n_missing <- function(x,bql) {
  sum(is.na(x) & bql==0)
}

# ncov start
n_non_missing <- function(x) {
  sum(!is.na(x))
}

n_total <- function(x) {
  length(x)
}

n_unique <- function(x) {
  length(unique(x))
}

# nocov end
