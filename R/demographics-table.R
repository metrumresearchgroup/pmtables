

smr <- function(value = seq(1,5)) {
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
#' @param span variable name for column spanner
#' @param stat_name name of statistic column
#' @param units optional units for each summarized column; must be a named list
#' @param stat_width distance (in cm) between the statistic and summarized columns
#'
#' @return
#' An object of class `pmtable`
#'
#' @examples
#'out <- pt_demographics(
#' data = pmt_first,
#' cols_cont = c('Age (yrs)'='AGE', 'Weight (kg)'='WT'),
#' cols_cat = c(Sex='SEXf',Race='ASIANf'),
#' span = c("Study"="STUDYf"),
#' stat_name = "Statistic"
#')
#'
#' @details
#' The data is summarized using [pt_cont_long()] and [pt_cat_long()] for the continuous and categorical data respectively.
#'
#' @seealso [pt_cont_long()], [pt_cat_long()]
#'
#' @return An object with class pmtable; see class-pmtable.
#'
#' @export
pt_demographics <- function(data, cols_cont, cols_cat, span, units = NULL,
                            stat_name = "Statistic", stat_width = 2) {

  assert_that(is.data.frame(data))

  cols_cont <- new_names(cols_cont)
  cols_cat <- new_names(cols_cat)

  span <- new_names(span)
  span_lvl <- levels(fct_inorder(data[[unname(span)]]))

  ### Continuous Variables ###
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
    units <- units[match(names(units),all_cols)]
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
