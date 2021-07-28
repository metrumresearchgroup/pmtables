#' Check if summary function evaluates to the correct format
#' @param sum_func The summary function to use for summarizing the continuous data in [pt_demographics()]
#' @keywords internal
check_sum_func <- function(sum_func){
  if(!identical(sum_func,dem_cont_fun)){
    tmp <- sum_func(seq(1,5))
    empty <- is_empty(tmp)
    struc <- !(is_tibble(tmp) | is.data.frame(tmp))
    row_n <- ifelse(!struc, nrow(tmp)!=1,FALSE)
    if(empty | struc| row_n){
      vals <- c(empty=empty, struc=struc, row_n=row_n)
      msgs = c("- Your function returned an empty object.",
               "- Your function does not return values in the correct format. Output must be a tibble or data frame.",
               "- Your function does not summarize data to a single row.")
      msg <- paste0(glue::glue("{paste(msgs[vals],collapse = '\n')} \nPlease see pmtables::dem_cont_fun() for more information."))
      stop(msg)
    }
  }
}


#' Default summary function in pt_demographics for continuous variables
#' @param value a vector or column to summarize
#' @param digits the number of digits in the summary; the current implementation
#' passes digits to [sig()]
#'
#' @return
#' A tibble with one row and two columns:
#'  1. `mean (sd)` the mean and standard deviation of `value`
#'  2. `min-max` the range of the `value`
#'
#' @examples
#' pmtables:::dem_cont_fun(value = seq(1,7), digits = 2)
#'
#' @keywords internal
dem_cont_fun <- function(value = seq(1,5), digits = 3) {
  Mean  <- sig(mean(value, na.rm = TRUE), digits = digits)
  Sd    <- sig(sd(value, na.rm = TRUE), digits = digits)
  Range <- sig(range(value, na.rm = TRUE), digits = digits)
  tibble(
    `mean (sd)` = paste0(Mean, " (", Sd, ")"),
    `min-max` = paste0(Range, collapse = " - "),
    `non-missing` = as.character(sum(!is.na(value)))
  )
}

#' Summarize continuous and categorical data in long format
#'
#' This function makes a single table from both continuous and categorical data.
#'
#' @param data 	the data frame to summarize; the user should filter or subset
#' so that data contains exactly the records to be summarized; pmtables will not
#' add or remove rows prior to summarizing data
#' @param cols_cont the continuous columns to summarize; may be character
#' vector or quosure
#' @param cols_cat the categorical columns to summarize; may be character vector
#' or quosure
#' @param span variable name for column spanner

#' @param units optional units for each summarized column; must be a named list
#' @param stat_name name of statistic column
#' @param stat_width distance (in cm) between the statistic and summarized
#' columns
#' @param summarize_all logical; if `TRUE`, summaries across all `span`
#' levels will be appended to the right hand side of the table
#' @param all_name a character name for the all data summary invoked by
#' `summarize_all`
#' @param fun The summary function to use for summarizing the continuous
#' data; the default is [dem_cont_fun()]
#'
#' @return
#' An object of class `pmtable`
#'
#' @examples
#'
#' # Example summary function
#' new_fun <- function(value = seq(1,5)) {
#'  tibble::tibble(
#'   `mean (sd)` = paste0(sig(mean(value, na.rm = TRUE)), " (", sig(sd(value,na.rm=TRUE)), ")"),
#'   `median` = paste0(sig(median(value, na.rm = TRUE))),
#'   `min-max` = paste0(sig(range(value,na.rm=TRUE)), collapse = " - ")
#'  )
#' }
#'
#' out <- pt_demographics(
#'   data = pmt_first,
#'   cols_cont = c('Age' = 'AGE', 'Weight' = 'WT'),
#'   cols_cat = c(Sex = 'SEXf',Race = 'ASIANf'),
#'   units = list(WT = "kg"),
#'   fun = new_fun,
#'   span = c("Study" = "STUDYf"),
#'   stat_name = "Statistic"
#')
#'
#' @details
#'
#' The categorical data is summarized using [pt_cat_long()].
#' The default summary function for continuous variables is [dem_cont_fun()].
#' Please review that documentation for details on the default summary for this
#' table.
#'

#'
#' If you wish to define your own function, please ensure the output is in the
#' same format. Any number of columns is acceptable.
#'
#' @seealso [pt_cont_long()], [pt_cat_long()]
#'
#' @return An object with class pmtable; see class-pmtable.
#'
#' @export
pt_demographics <- function(data, cols_cont, cols_cat, span = NULL, units = NULL,
                            stat_name = "Statistic", stat_width = 2,
                            summarize_all = TRUE, all_name = "All data",
                            fun = dem_cont_fun) {

  summarize_all <- isTRUE(summarize_all)
  summarize_span <- !is.null(span)
  summarize_all <- isTRUE(summarize_all) || !summarize_span

  assert_that(is.data.frame(data))
  data <- as.data.frame(data)
  check_continuous(data, cols_cont)
  check_discrete(data, cols_cat)
  check_sum_func(fun)

  cols_cont <- new_names(cols_cont)
  cols_cat <- new_names(cols_cat)

  if(summarize_span) {
    span <- new_names(span)
    span_lvl <- levels(fct_inorder(data[[unname(span)]]))
    new_span <- as.span(title = names(span), vars = span_lvl)
  } else {
    new_span <- NULL
  }

  # Continuous Variables
  cont_table0 <- pivot_longer(data, cols = all_of(unname(cols_cont)))
  cont_table0 <- mutate(cont_table0, name = fct_inorder(.data[["name"]]))

  if(summarize_all) {
    cont_table0[[".span"]] <- "value"
    cont_table_all <- demo_summarize_cont(
      data = cont_table0,
      span = ".span",
      cols = cols_cont,
      fun = fun
    )
  }

  if(summarize_span) {
    cont_table <- demo_summarize_cont(
      data = cont_table0,
      span = span,
      cols = cols_cont,
      fun = fun
    )
  }

  # Categorical Variables ###
  if(summarize_span) {
    cat_table <- pt_cat_long(
      data,
      cols = all_of(cols_cat),
      span = span,
      summarize = "top"
    )
  }

  if(summarize_all) {
    cat_table_all0 <- pt_cat_long(
      data,
      cols = all_of(cols_cat),
      summarize = "top"
    )
    cat_table_all <- rename(cat_table_all0[["data"]], value = "Summary")
  }

  if(summarize_all && summarize_span) {
    cont_df <- left_join(cont_table, cont_table_all, by = c("name", "level"))
    cat_df <- left_join(cat_table[["data"]], cat_table_all, by = c("name", "level"))
    cols_extra <- left_join(
      cat_table$cols_extra,
      cat_table_all0$cols_extra,
      by = "level"
    )
  }
  if(summarize_all && !summarize_span) {
    cont_df <- cont_table_all
    cat_df <- cat_table_all
    cols_extra <- cat_table_all[["cols_extra"]]
  }
  if(!summarize_all && summarize_span) {
    cont_df <- cont_table
    cat_df <- cat_table[["data"]]
    cols_extra <- cat_table[["cols_extra"]]
  }


  # Combined Table ###
  table_data <- bind_rows(cont_df, cat_df)
  table_data <- rename(table_data, !!sym(stat_name) := .data[["level"]])

  if(summarize_all) {
    table_data <- rename(table_data, !!sym(all_name) := .data[["value"]])
  }

  # add units
  units <- validate_units(units, data)
  if(!is.null(units)) {
    all_cols <- c(cols_cont, cols_cat)
    has_unit <- match(names(units), all_cols)
    nw <- names(all_cols)
    nw[has_unit] <- paste(nw[has_unit],  unlist(units))
    names(nw) <- names(all_cols)
    table_data[["name"]] <- nw[table_data[["name"]]]
  }

  align <- cols_left()
  ragged <- list(col_ragged(stat_width))
  names(ragged) <- stat_name
  align[["update"]] <- ragged

  ans <- list(
    data = table_data,
    span = new_span,
    panel = as.panel("name"),
    align = align,
    cols_extra = cols_extra
  )

  structure(ans, class = c("pmtable", class(ans)))
}

demo_summarize_cont <- function(data, span, cols, fun) {

  cont_table <- group_by(data, .data[["name"]], !!sym(span))
  cont_table <- summarise(
    cont_table,
    fun(.data[["value"]]),
    .groups = "drop"
  )

  cont_table <- pivot_wider(
    cont_table,
    names_from  = "name",
    values_from = names(fun(1:10)),
    names_glue  = "{name}_{.value}"
  )
  cont_table <- pivot_longer(cont_table, -!!sym(span))
  cont_table <- pivot_wider(cont_table, names_from = all_of(unname(span)))
  cont_table <- separate(cont_table, .data[["name"]], c("name", "level"), sep="_")

  cont_table <- mutate(cont_table, name = factor(name, levels = unname(cols)))
  cont_table <- arrange(cont_table, .data[["name"]])
  cont_table <- mutate(cont_table, name = names(cols)[.data[["name"]]])
  cont_table
}
