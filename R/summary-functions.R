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
#' @examples
#' pmtables:::cont_wide_fun(rnorm(100))
#'
#' @keywords internal
cont_wide_fun <- function(value,digit_fun=sig,id=NULL,digits=3,
                          na_action = "omit", na_fill = "--",...) {
  if(is.null(id)) {
    n <- sum(!is.na(value))
  } else {
    n <- length(unique(id))
  }
  value <- na.omit(value)
  if(length(value)==0) {
    if(identical(na_action, "omit")) {
      return(tibble())
    }
    if(identical(na_action, "pass")) {
      return(tibble(summary  = na_fill))
    }
    stop("no non-missing values",call.=FALSE)
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
#' @keywords internal
cont_long_fun <- function(value,digit_fun=sig,id=NULL,digits=3,
                          na_action = "omit", na_fill = "--",...) {
  if(is.null(id)) {
    n <- sum(!is.na(value))
  } else {
    n <- length(unique(id))
  }
  value <- na.omit(value)
  if(length(value)==0) {
    if(identical(na_action, "omit")) {
      return(tibble()[0,])
    }
    if(identical(na_action, "pass")) {
      ans <-  tibble(
        n = na_fill,
        Mean = na_fill,
        Median = na_fill,
        SD = na_fill,
        `Min / Max`  = na_fill
      )
      return(ans)
    }
    stop("no non-missing values",call.=FALSE)
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
