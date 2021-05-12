

smr <- function(value = seq(1,5)) {
  tibble(
    `mean (sd)` = paste0(sig(mean(value, na.rm = TRUE)), " (", sig(sd(value,na.rm=TRUE)), ")"),
    `min-max` = paste0(sig(range(value,na.rm=TRUE)), collapse = " - ")
  )
}

#' This is the title
#'
#' This function makes a single table from both continuous and categorical data.
#'
#' @param data
#' @param cols_cont
#' @param cols_cat
#'
#' @return
#' An object of class `pmtable`
#'
#' @examples
#'
#' @seealso [pt_cont_long()], [pt_cat_long()]
#'
#' @export
pt_demographics <- function(data, cols_cont, cols_cat, span,
                            stat_name = "Statistic", stat_width = 2) {

  assert_that(is.data.frame(data))

  cols_cont <- new_names(cols_cont)
  cols_cat <- new_names(cols_cat)

  span <- new_names(span)
  span_lvl <- levels(fct_inorder(data[[unname(span)]]))

  cont_table <- pivot_longer(data, cols = all_of(cols_cont))
  cont_table <- group_by(cont_table, name, !!sym(span))
  cont_table <- summarise(cont_table, smr(value), .groups = "drop")
  cont_table <- pivot_wider(
    cont_table,
    names_from  = "name",
    values_from = names(smr()),
    names_glue  = "{name}_{.value}"
  )
  cont_table <- pivot_longer(cont_table, -!!sym(span))
  cont_table <- pivot_wider(cont_table, names_from = all_of(unname(span)))
  cont_table <- separate(cont_table, .data[["name"]], c("name", "level"), sep="_")
  cont_table <- arrange(cont_table, .data[["name"]])
  # rename the `name` column`
  # add units

  cat_table <- pt_cat_long(
    data,
    cols = all_of(cols_cat),
    span = span,
    summarize = "top"
  )

  table_data <- bind_rows(cont_table, cat_table[["data"]])
  table_data <- rename(table_data, !!sym(stat_name) := level)

  align <- cols_left()
  ragged <- list(col_ragged(stat_width))
  names(ragged) <- stat_name
  align[["update"]] <- ragged

  ans <- list(
    data = table_data,
    span = as.span(title = names(span), vars = all_of(span_lvl)),
    panel = as.panel("name"),
    align = align,
    cols_extra = cat_table$cols_extra
  )

  structure(ans, class = c("pmtable", class(ans)))
}
