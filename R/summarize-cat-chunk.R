
#' Summarize a chunk of categorical data
#'
#' @param data the data frame to summarize.
#' @param cols the column names to summarize.
#' @param N the `total` number in the original data.
#' @param denom says what to use as the denominator for calculating percent.
#' @keywords internal
#' @noRd
summarize_cat_chunk <- function(data, cols, N, denom = "group") {
  Nchunk <- nrow(data)
  if(denom=="group") {
    D <- Nchunk
  } else {
    D <- N
  }
  ans <- map_dfr(cols, summarize_cat_col, data = data, D = D, Nchunk = Nchunk)
  ans <- mutate(ans, name = fct_inorder(.data[["name"]]))
  ans <- mutate(ans, summary = paste0(n," (",.data[["Percent"]],")"))
  mutate(ans, n = NULL, Percent = NULL)
}

#' Summarize a single categorical data column
#'
#' @param name the column name to summarize.
#' @param data the data frame to summarize from.
#' @param D the denominator to use; this was set in [summarize_cat_chunk()].
#' @param Nchunk the number of records in the chunk being summarized.
#' @return
#' - `N` is the number of records in the chunk
#' - `level` is the current level of the categorical data item
#' - `name` is the column name
#' - `n` is the number summarized
#' @keywords internal
#' @noRd
summarize_cat_col <- function(name, data, D, Nchunk) {
  data <- ungroup(data)
  pick <- select(data, .data[["ID"]], level = all_of(unname(name)))
  pick <- mutate(pick, name = .env[["name"]])
  pick <- group_by(pick, .data[["name"]], .data[["level"]], .drop = FALSE)
  summ <- summarise(pick, n = n())
  summ <- ungroup(summ)
  summ <- mutate(summ, N = Nchunk)
  summ <- mutate(summ, Percent = digit1(100*.data[["n"]]/D))
  summ <- mutate(summ, level = as.character(.data[["level"]]))
  summ <- mutate(summ, name = as.character(.data[["name"]]))
  return(summ)
}
