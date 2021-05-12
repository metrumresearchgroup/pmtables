

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
pt_demographics <- function(data, cols_cont, cols_cat, span, units = NULL,
                            stat_name = "Statistic", stat_width = 2) {

  assert_that(is.data.frame(data))

  cols_cont <- new_names(cols_cont)
  cols_cat <- new_names(cols_cat)

  span <- new_names(span)
  span_lvl <- levels(fct_inorder(data[[unname(span)]]))

  cont_table <- pt_cont_long(
    data,
    cols = cols_cont,
    by = span
  )
  # Change formatting
  cont_table <- unite(separate(cont_table$data,`Min / Max`,c("min","max"),sep=" / "), "min-max", min:max,sep=" - ")
  cont_table <- unite(cont_table,'mean (sd)',c("Mean","SD"), sep = " (")
  cont_table <- mutate(cont_table,'mean (sd)'= paste0(`mean (sd)`,")"))
  cont_table <- dplyr::select(cont_table,-c(Median,n))

  # rearrange
  cont_table <- pivot_longer(cont_table, cols = c("mean (sd)", "min-max"))
  cont_table <- pivot_wider(cont_table, names_from = names(span), values_from = "value")
  cont_table <- dplyr::select(cont_table,-c(`All data`))
  cont_table <- dplyr::rename(cont_table, name=Variable, level=name)

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
