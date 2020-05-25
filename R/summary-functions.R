str_sum_2 <- function(value,digit_fun = sig, name=NULL, footnote = FALSE,...) {
  if(footnote) {
    return("summary is: mean (standard deviation)")
  }
  mn <- digit_fun(mean(value))
  sd <- digit_fun(sd(value))
  tibble(summary = paste0(mn, " (",sd,")"))
}

df_sum_2 <- function(value,digit_fun = sig,name=NULL,footnote = FALSE) {
  if(footnote) {
    footn <- list(
      footnote = "standard deviation",
      locations = cells_column_labels(
        columns = vars(SD)
      )
    )
    return(footn)
  }
  rng <- digit_fun(range(value))
  rng <- paste0(rng[1]," / ", rng[2])
  tibble(
    Mean = digit_fun(mean(value)),
    Median = digit_fun(median(value)),
    SD = digit_fun(sd(value)),
    `Min / Max` = rng
  )
}

n_missing <- function(x,bql) {
  sum(is.na(x) & bql ==0)
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
