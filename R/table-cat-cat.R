
#' Creates a factor from col_rename data
#' @param x a vector to be converted to factor
#' @param col col_rename data
#' @return
#' A factor with levels from `col` and labels from `names_col`
#' @noRd
nn_factor <- function(x, col) {
  assert_that(is_named(col))
  factor(x, levels = col, labels = names(col))
}

n_pct <- function(count, total) {
  #' Calculate and format count (percent)
  #' This uses `digit1` for percent
  pct <- digit1(100*count/total)
  as.character(glue("{count} ({pct})"))
}

is.cat_cat_data <- function(x) inherits(x, "cat_cat_data")

cat_cat_notes <- function() {
  c(
    "The summaries in each cell are count (percent).",
    "The marginal summary on the right  is over all levels in column variables.",
    "The marginal summary on the bottom is over all levels in panel variables."
  )
}

#' Categorical by categorical table
#'
#' @param data a `data.frame` to summarize.
#' @param cols_down columns to summarize by and to be listed along the
#' left hand side of the table; may be a named character vector or quosure.
#' @param cols_across columns to summarize by and to be listed across the top
#' of the table; may be a named character vector or quosure.
#'
#' @export
pt_cat_cat <- function(data, cols_down, cols_across, summarize_down = TRUE,
                       summarize_across = TRUE) {
  tab <- pt_cat_cat_data(
    data,
    cols_down,
    cols_across,
    summarize_down,
    summarize_across
  )
  pt_cat_cat_style(tab)
}
#' @rdname pt_cat_cat
#' @export
pt_cat_cat_style <- function(tab) {
  assert_that(is.cat_cat_data(tab))
  object <- list(
    data = data.frame(),
    panel = as.panel("name"),
    span_split = colsplit(sep = "__"),
    cols_blank = c("value")
  )
  if(isTRUE(attr(tab, "sum_across"))) {
    sr <- sumrow(nrow(tab), depanel = TRUE, bold = TRUE, hline2 = TRUE)
    object$sumrows <- sr
  }
  if(isTRUE(attr(tab, "sum_down"))) {
    nc <- ncol(tab)
    scol <- names(tab)[nc]
    scol <- tex_bold(scol)
    names(tab)[nc] <- scol
  }
  object$data <- tab
  object$notes <- cat_cat_notes()
  class(object) <- c("pmtable", class(object))
  object
}

#' @rdname pt_cat_cat
#' @export
pt_cat_cat_data <- function(data, cols_down, cols_across, summarize_down = TRUE,
                            summarize_across = TRUE) {
  na_value <- "0 (0.00)"
  all_cols <- unname(c(across, down))
  check_discrete(data, cols_across)
  check_discrete(data, cols_down)
  across <- rename_cols(cols_across)
  down <- rename_cols(cols_down)
  NNN <- nrow(data) # The number of records

  # count by down and across variables
  long <- pivot_longer(data, cols = all_of(unname(down)))
  long2 <- pivot_longer(
    long,
    cols = all_of(unname(across)),
    names_to = "name2", values_to = "value2"
  )
  long2 <- mutate(
    long2,
    name = fct_inorder(name),
    name2 = fct_inorder(name2)
  )
  summ <- group_by(long2, name, name2, value, value2)
  summ <- count(summ)
  summ <- ungroup(summ)
  summ <- mutate(summ, name = nn_factor(name, down))
  summ <- mutate(summ, name2 = nn_factor(name2, across))
  summ <- mutate(summ, name2 = paste(name2, value2, sep = '__'))
  summ$value2 <- NULL

  # primary summary --------
  summ1 <- mutate(summ, summary = n_pct(n, NNN))
  summ1 <- pivot_wider(
    select(summ1, -n),
    names_from = "name2",
    values_from = "summary"
  )
  ncol <- ncol(summ1)
  summ1 <- mutate(summ1, across(seq(3, ncol), as.character))
  summ1 <- mutate(summ1, across(seq(3, ncol), replace_na, na_value))

  tab <- select(summ1, name, value, everything())

  if(isTRUE(summarize_down)) {
    # marginal summary of of down columns -------
    summ2 <- pivot_longer(data, cols = all_of(unname(down)))
    summ2 <- group_by(summ2, name, value)
    summ2 <- count(summ2)
    summ2 <- group_by(summ2, name)
    summ2 <- mutate(summ2, Summary = n_pct(n, sum(n)))
    summ2 <- ungroup(summ2)
    summ2$n <- NULL
    summ2 <- mutate(summ2, name = nn_factor(name, down))
    summ2 <- mutate(summ2, Summary = replace_na(Summary, na_value))
    tab <- left_join(tab, summ2, by = c("name", "value"))
  }

  if(isTRUE(summarize_across)) {
    # marginal summary of across columns -------
    # Count the across covariates
    summ3 <- pivot_longer(data, cols = all_of(unname(across)))
    summ3 <- group_by(summ3, name, value)
    summ3 <- count(summ3)
    summ3 <- ungroup(summ3)
    # Substitutes rename
    summ3 <- mutate(summ3, name = nn_factor(name, across))
    summ3 <- mutate(summ3, name = paste0(name, "__", value))
    summ3$value <- NULL
    summ3 <- mutate(summ3, Summary = n_pct(n, NNN))
    summ3 <- mutate(summ3, Summary = replace_na(Summary, na_value))
    summ3 <- pivot_wider(select(summ3, -n), values_from = "Summary")
    summ3 <- mutate(summ3, Summary = paste0(NNN, " (100.0)"))
    summ3$name <- "All data"
    summ3$value <- "Summary"
    # end column summary ------
    summ3 <- select(summ3, names(tab))
    tab <- bind_rows(tab, summ3)
  }
  structure(
    tab,
    class = c("cat_cat_data", class(tab)),
    sum_across = summarize_across,
    sum_down = summarize_down
  )
}
