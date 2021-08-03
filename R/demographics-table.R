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
      msgs = c("`fun` returned an empty object.",
               "`fun` must return a data frame.",
               "`fun` must return a data frame with a single row.")
      msg <- paste0(glue::glue("{paste(msgs[vals],collapse = '\n')} See ?pmtables:::dem_cont_fun."))
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
#'  3. `non-missing` the number of non-missing `values`
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
#' @param cols_cont the continuous data columns to summarize; this argument
#' may be specified as a character vector, comma-separated string or
#' quosure
#' @param cols_cat the categorical columns to summarize; this argument
#' may be specified as a character vector, comma-separated string or
#' quosure
#' @param span variable name for column spanner
#' @param units optional units for each summarized column; must be a named list
#' where the names correspond with continuous data columns in `data`
#' @param stat_name name of statistic column
#' @param stat_width width (in cm) of the statistic column
#' @param summarize_all logical; if `TRUE`, summaries across all `span`
#' levels will be appended to the right hand side of the table
#' @param all_name a character name for the all data summary invoked by
#' `summarize_all`
#' @param fun The summary function to use for summarizing the continuous
#' data; the default is [dem_cont_fun()]
#' @param notes notes a character vector of notes to place under the table
#' @param paneled logical; if `TRUE`, the table will be paneled with the
#' covariate names; otherwise, the covariate names will appear as the left-most
#' column with non-repeating names cleared and separated with `hline` (see
#' examples)
#'
#' @return
#' An object of class `pmtable`.
#'
#' @examples
#'
#' out <- pt_demographics(
#'   data = pmt_first,
#'   cols_cont = c("Age" = "AGE", Weight = "WT"),
#'   cols_cat = c(Sex = "SEXf", Race = "ASIANf"),
#'   units = list(WT = "kg"),
#'   span = c(Study = "STUDYf")
#' )
#'
#' out <- pt_demographics(
#'   data = pmt_first,
#'   cols_cont = "AGE,WT",
#'   cols_cat = "SEXf,ASIANf",
#'   paneled = FALSE,
#'   span = "FORMf"
#' )
#' tab <- stable(out)
#'
#' pmtables:::pt_demographics_notes()
#'
#' new_fun <- function(value = seq(1,5)) {
#' value <- value[!is.na(value)]
#'  tibble::tibble(
#'   `mean` = sig(mean(value)),
#'   `median` = sig(median(value)),
#'   `min-max` = paste0(sig(range(value)), collapse = " - ")
#'  )
#' }
#'
#' out <- pt_demographics(
#'   data = pmt_first,
#'   cols_cont = "AGE,WT",
#'   cols_cat = "SEXf,ASIANf",
#'   fun = new_fun
#' )
#'
#' pmtables:::dem_cont_fun(rnorm(20))
#' new_fun(rnorm(20))
#'
#'
#' @details
#'
#' The categorical data is summarized using [pt_cat_long()].
#' The default summary function for continuous variables is [dem_cont_fun()].
#' Please review that documentation for details on the default summary for this
#' table.
#'
#' If you wish to define your own function, please ensure the output is in the
#' same format. Any number of columns is acceptable.
#'
#' @seealso [pt_cont_long()], [pt_cat_long()]
#'
#' @return An object with class pmtable; see class-pmtable.
#'
#' @export
pt_demographics <- function(data, cols_cont, cols_cat,
                            span = NULL, units = NULL,
                            stat_name = "Statistic", stat_width = 2,
                            summarize_all = TRUE, all_name = "All data",
                            fun = dem_cont_fun,
                            notes = pt_demographics_notes(),
                            paneled = TRUE) {

  summarize_all <- isTRUE(summarize_all)
  summarize_span <- !is.null(span)
  summarize_all <- isTRUE(summarize_all) || !summarize_span
  paneled <- isTRUE(paneled)

  assert_that(is.data.frame(data))
  assert_that(is.character(notes))
  data <- as.data.frame(data)
  cols_cont <- new_names(cols_cont)
  cols_cat <- new_names(cols_cat)
  check_continuous(data, cols_cont)
  check_discrete(data, cols_cat)
  check_sum_func(fun)

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
    span = new_span,
    panel = as.panel("name"),
    align = align,
    cols_extra = cols_extra,
    notes = notes
  )

  if(!paneled) {
    ans[["panel"]] <- as.panel(NULL)
    ans[["clear_reps"]] <- "Covariate"
    if(!is.null(new_span)) {
      ans[["cols_extra"]] <- bind_cols(
        tibble(Covariate = ""),
        ans[["cols_extra"]]
      )
    }
    ans[["hline_from"]] <- "Covariate"
    table_data <- select(table_data, Covariate = .data[["name"]], everything())
  }

  ans[["data"]] <- table_data
  structure(ans, class = c("pmtable", class(ans)))
}

demo_summarize_cont <- function(data, span, cols, fun) {

  summary_names <- names(fun(1:5))

  cont_table <- group_by(data, .data[["name"]], !!sym(span))
  cont_table <- summarise(
    cont_table,
    fun(.data[["value"]]),
    .groups = "drop"
  )

  cont_table <- mutate_at(cont_table, .vars = summary_names, as.character)

  cont_table <- pivot_wider(
    cont_table,
    names_from  = "name",
    values_from = summary_names,
    names_glue  = "{name}_{.value}"
  )
  cont_table <- pivot_longer(cont_table, -!!sym(span))
  cont_table <- pivot_wider(cont_table, names_from = all_of(unname(span)))
  cont_table <- separate(
    cont_table,
    .data[["name"]],
    c("name", "level"),
    sep = "_"
  )

  cont_table <- mutate(
    cont_table,
    name = factor(.data[["name"]], levels = unname(cols))
  )
  cont_table <- arrange(cont_table, .data[["name"]])
  cont_table <- mutate(cont_table, name = names(cols)[.data[["name"]]])
  cont_table
}

pt_demographics_notes <- function() {
  cat_notes <- "Categorical summary is count (percent)"
  cont_notes <- c(
    "sd: standard deviation",
    "min: minimum",
    "max: maximum"
  )
  c(cat_notes, cont_notes)
}
