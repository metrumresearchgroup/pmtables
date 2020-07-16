str_sum_2 <- function(value,digit_fun=sig,id=NULL,digits=3,name=NULL,footnote = FALSE,...) {
  if(footnote) {
    return("summary is: mean (standard deviation) [number non-missng]")
  }
  if(is.null(id)) {
    n <- sum(!is.na(value))
  } else {
    n <- length(unique(id))
  }
  value <- na.omit(value)
  if(length(value)==0) stop("no non-missing values",call.=FALSE)
  mn <- digit_fun(mean(value),digits=digits)
  sd <- digit_fun(sd(value),digits=digits)
  ans <- tibble(summary = paste0(mn, " (",sd,")", " [",n,"]"))
  ans
}

df_sum_2 <- function(value,digit_fun=sig,id=NULL,digits=3,name=NULL,footnote = FALSE,...) {
  if(footnote) {
    footn <- list()
    footn[[1]] <- list(
      footnote = "standard deviation",
      locations = cells_column_labels(
        columns = "SD"
      )
    )
    footn[[2]] <- list(
      footnote = "subjects with non-missing values",
      locations = cells_column_labels(
        columns = "n"
      )
    )
    return(footn)
  }
  if(is.null(id)) {
    n <- sum(!is.na(value))
  } else {
    n <- length(unique(id))
  }
  value <- na.omit(value)
  if(length(value)==0) stop("no non-missing values",call.=FALSE)
  rng <- digit_fun(range(value),digits=digits)
  rng <- paste0(rng[1]," / ", rng[2])
  ans <- tibble(
    n = n,
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

n_non_missing <- function(x) {
  sum(!is.na(x))
}

n_total <- function(x) {
  length(x)
}

n_unique <- function(x) {
  length(unique(x))
}
