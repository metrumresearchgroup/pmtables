
#' Default summary function in pt_demographics for continuous variables
#' @param value a vector or column to summarize
#'
#' @return A tibble with one row and two columns named `mean (sd)` and `min-max`
#'
#' @examples
#' pmtables:::sum_cont(value = seq(1,5))
#'
#' @keywords internal
sum_cont <- function(value = seq(1,5)) {
  tibble(
    `mean (sd)` = paste0(sig(mean(value, na.rm = TRUE)), " (", sig(sd(value,na.rm=TRUE)), ")"),
    `min-max` = paste0(sig(range(value,na.rm=TRUE)), collapse = " - ")
  )
}

#' Summarize continuous and categorical data in long format
#'
#' This function makes a single table from both continuous and categorical data.
#'
#' @param data 	the data frame to summarize; the user should filter or subset so that data contains exactly the records to be summarized; pmtables will not add or remove rows prior to summarizing data
#' @param cols_cont the continuous columns to summarize; may be character vector or quosure
#' @param cols_cat the categorical columns to summarize; may be character vector or quosure
#' @param sum_func The summary function to use for summarizing the continuous data. Default is [sum_cont()].
#' @param span variable name for column spanner
#' @param stat_name name of statistic column
#' @param units optional units for each summarized column; must be a named list
#' @param stat_width distance (in cm) between the statistic and summarized columns
#'
#' @return
#' An object of class `pmtable`
#'
#' @examples
#'
#' # Example summary function
#' sum_cont <- function(value = seq(1,5)) {
#'  tibble(
#'   `mean (sd)` = paste0(sig(mean(value, na.rm = TRUE)), " (", sig(sd(value,na.rm=TRUE)), ")"),
#'   `min-max` = paste0(sig(range(value,na.rm=TRUE)), collapse = " - ")
#' )
#' }
#'
#' out <- pt_demographics(
#'   data = pmt_first,
#'   cols_cont = c('Age'='AGE', 'Weight'='WT'),
#'   cols_cat = c(Sex='SEXf',Race='ASIANf'),
#'   units = list(WT="kg"),
#'   span = c("Study"="STUDYf"),
#'   stat_name = "Statistic"
#')
#'
#' @details
#' The data is summarized using [pt_cont_long()] and [pt_cat_long()] for the continuous and categorical data respectively.
#' The default summary function for summarizing continuous variables ([sum_cont()]) returns a tibble with
#' one row and two columns named `mean (sd)` and `min-max`:
#'

#'
#' If you wish to define your own function, please ensure the output is in the same format. Any number of columns is acceptable.
#'
#' @seealso [pt_cont_long()], [pt_cat_long()]
#'
#' @return An object with class pmtable; see class-pmtable.
#'
#' @export
pt_demographics <- function(data, cols_cont, cols_cat, sum_func = sum_cont, span, units = NULL,
                            stat_name = "Statistic", stat_width = 2) {

  assert_that(is.data.frame(data))
  pmtables:::check_continuous(data, cols_cont)
  pmtables:::check_discrete(data, cols_cat)

  cols_cont <- new_names(cols_cont)
  cols_cat <- new_names(cols_cat)

  span <- new_names(span)
  span_lvl <- levels(fct_inorder(data[[unname(span)]]))

  ### Continuous Variables ###
  cont_table <- pivot_longer(data, cols = all_of(unname(cols_cont)))
  cont_table <- mutate(cont_table, name = fct_inorder(name))
  cont_table <- group_by(cont_table, name, !!sym(span))
  cont_table <- summarise(cont_table, sum_func(value), .groups = "drop")
  cont_table <- pivot_wider(
    cont_table,
    names_from  = "name",
    values_from = names(sum_func()),
    names_glue  = "{name}_{.value}"
  )
  cont_table <- pivot_longer(cont_table, -!!sym(span))
  cont_table <- pivot_wider(cont_table, names_from = all_of(unname(span)))
  cont_table <- separate(cont_table, .data[["name"]], c("name", "level"), sep="_")
  cont_table <- arrange(cont_table, .data[["name"]])
  cont_table <- mutate(cont_table, name = fct_inorder(name))
  cont_table <- mutate(cont_table, name = names(cols_cont)[.data[["name"]]])

  ### Categorical Variables ###
  cat_table <- pt_cat_long(
    data,
    cols = all_of(cols_cat),
    span = span,
    summarize = "top"
  )

  ### Combined Table ###
  table_data <- bind_rows(cont_table, cat_table[["data"]])
  table_data <- rename(table_data, !!sym(stat_name) := level)

  # add units
  units <- validate_units(units,data)
  if(!is.null(units)){
    all_cols <- as.list(c(cols_cont,cols_cat))
    names(units) <- names(all_cols[match(names(units),all_cols)])

    unit_names <- lapply(seq(units), function(i){
      ifelse(units[[i]]!="",paste0(names(units[i]), " (",units[[i]],")"),names(units[i]))
    })
    names(unit_names) <- names(units)

    for(i in 1:length(unit_names)){
      table_data <- mutate(table_data, name = ifelse(name==names(unit_names[i]), unit_names[[i]], name))
    }
  }



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

#' @rdname pt_demographics
#'
