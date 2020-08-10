#' Create continuous summary data frame
#'
#' @inheritParams pt_cont_long
#' @param by grouping variable name
#' @param panel paneling variable name
#' @param all_name label for full data summary
#' @param digits named list specifying `digits` argument for `digit_fun`
#' @param wide `logical`; if `TRUE`, output will be wide; output will be long
#' otherwise
#' @param fun continuous data summary function
#' @param id_col the ID column name
#'
#' @export
cont_table_data <- function(data, cols, by = ".total", panel = by, wide = FALSE,
                            all_name = "all", digits = new_digits(), id_col = "ID",
                            fun = df_sum_2) {

  cols <- unname(new_names(cols))
  by <- unname(new_names(by))
  panel <- unname(new_names(panel))

  data <- data_total_col(data, all_name = all_name)
  check_continuous(data,cols)
  check_discrete(data,by)
  check_exists(data,id_col)

  assert_that(inherits(digits, "digits"))

  digits <- update_digits(digits,cols)
  digit_fun <- get_digits_fun(digits)
  digit_data <- get_digits_list(digits)

  groups <- c("name")
  if(!is.null(by)) groups <- c(by,groups)
  if(!is.null(panel)) groups <- c(panel,by,groups)
  groups <- unique(groups)

  d0 <- select(data, all_of(unname(c(panel,by,cols))))

  d1 <- pivot_longer(d0,all_of(cols))
  d1 <- mutate(d1, digitn = unlist(digit_data[.data[["name"]]]))
  d1 <- mutate(d1,name = fct_inorder(.data[["name"]]))

  if(!is.null(by)) {
    d1 <- group_by(d1,!!!syms(groups))
    join_cols <- unique(c(panel,by,"name"))
  } else {
    d1 <- group_by(d1,!!!syms(groups))
    join_cols <- "name"
  }

  if(packageVersion("dplyr") < '0.8.99') { # nocov start
    d2 <- group_modify(
      d1,
      ~fun(
        value = .$value,
        digit_fun = digit_fun,
        digits = .$digitn[1],
        name = .$name[1],
        id = .[[id_col]]
      ),
      keep = TRUE
    ) # nocov end
  } else {
    d2 <- group_modify(
      d1,
      ~fun(
        value = .$value,
        digit_fun = digit_fun,
        digits = .$digitn[1],
        name = .$name[1],
        id = .[[id_col]]
      ),
      .keep = TRUE
    )
  }

  d4 <- rename(d2, outer = !!sym(by))
  if(wide) {
    d4 <- pivot_wider(d4, names_from  = "name", values_from = "summary")
  }
  d4 <- ungroup(d4)
  return(d4)
}

#' Create a continuous data summary table in wide format
#'
#' @param data the data frame to summarize
#' @param cols the columns to summarize; may be character vector or quosure
#' @param by a grouping variable; may be character vector or quosure
#' @param panel data set column name to stratify the summary
#' @param table a named list to use for renaming columns (see details and
#' examples)
#' @param units a named list to use for unit lookup (see details and examples)
#' @param digits a `digits` object (see [new_digits()])
#' @param all_name a name to use for the complete data summary
#' @param fun the data summary function (see details)
#' @param id_col the ID column name
#'
#' @export
pt_cont_wide <- function(data, cols,
                         by = ".total",
                         panel = by,
                         table = NULL,
                         units = NULL,
                         digits = new_digits(),
                         all_name = "All data",
                         fun = str_sum_2,
                         id_col = "ID") {

  has_panel <- !missing(panel)
  panel_data <- as.panel(panel)
  panel <- panel_data$col

  has_by <- !missing(by)

  tst <- fun(rnorm(10))
  assert_that(identical(names(tst),"summary"))

  cols <- new_names(cols,table)
  by <- new_names(by,table)
  panel <- new_names(panel,table)

  data <- data_total_col(data, all_name = all_name)

  ans <- cont_table_data(
    data = data,
    cols = cols,
    by = by,
    id_col = id_col,
    panel = panel,
    fun = fun,
    digits = digits,
    wide = TRUE
  )

  all_summary <- FALSE
  if(has_panel || has_by) {
    all_summary <- TRUE
    ans2 <- cont_table_data(
      data = data,
      cols = cols,
      by = ".total",
      panel = ".total",
      fun = fun,
      digits = digits,
      wide = TRUE
    )

    if(has_panel) {
      ans2 <- mutate(ans2, !!sym(panel) := all_name)
    } else {
      if(has_by) {
        ans2[["outer"]] <- paste0("\\hline \\hline {\\bf ", all_name, "}")
      }
    }
    ans <- bind_rows(ans,ans2)
  }

  if(has_by & !has_panel) {
    ans <- rename(ans, !!sym(by) := outer)
  }

  if(exists(by, ans)) {
    where <- names(ans)==by
    names(ans)[where] <- names(by)
  }

  ans[["outer"]] <- NULL
  ans[[".total"]] <- NULL

  .panel <- rowpanel(NULL)
  if(has_panel) {
    .panel <- panel_data
    .panel$prefix_skip <- all_name
  }

  out <- list(
    data = ans,
    align = cols_left(),
    panel = .panel,
    units = units,
    bold_cols = !has_panel,
    notes = "Summary is mean (sd) [count]"
  )

  out <- structure(out, class = "pmtable")

  out
}

#' Continuous data summary in long format
#'
#' @inheritParams pt_cont_wide
#' @param summarize_all if `TRUE` then a complete data summary will be appended
#' to the bottom of the table
#'
#' @examples
#'
#' ans <- pt_cont_long(pmt_first, cols = dplyr::vars(WT,ALB,CRCL))
#'
#' ans <- pt_cont_long(pmt_first, cols = "WT,CRCL", panel = "SEXf")
#'
#' @export
pt_cont_long <- function(data,
                         cols,
                         panel = ".total",
                         table = NULL,
                         units = NULL,
                         digits = new_digits(),
                         summarize_all = TRUE,
                         all_name = "All data",
                         fun = df_sum_2,
                         id_col = "ID") {

  has_panel <- !missing(panel)
  panel_data <- as.panel(panel)
  panel <- panel_data$col

  by <- panel
  summarize_all <- summarize_all & by != ".total"
  data <- data_total_col(data)

  cols <- new_names(cols,table)
  by <- new_names(by,table)

  ans <- cont_table_data(
    data = data,
    cols = unname(cols),
    by = unname(by),
    id_col = id_col,
    fun = fun,
    digits = digits,
    wide = FALSE
  )

  if(by==".total") ans <- mutate(ans, outer = all_name)

  if(summarize_all) {
    ans2 <- cont_table_data(
      data = data,
      cols = unname(cols),
      by = ".total",
      fun = fun,
      digits = digits,
      wide = FALSE
    )
    ans2 <- mutate(ans2, outer = all_name)
    ans <- bind_rows(ans,ans2)
  }

  .name <- as.character(ans$name)
  ans <- mutate(ans, name = as.character(names(cols)[.data[["name"]]]))

  if(is.list(units) & rlang::is_named(units)) {
    has_unit <- .name %in% names(units)
    ans <- mutate(
      ans,
      name   = case_when(
        has_unit ~ paste0(.data[["name"]], " ", units[.name]),
        TRUE ~ .data[["name"]]
      )
    )
  }

  if(names(ans)[1]=="outer") {
    names(ans)[1] <- unname(by)
  }

  for(i in c(1,2)) {
    if(names(ans)[i] == "name") {
      names(ans)[i] <- "Variable"
    }
  }

  ans[[".total"]] <- NULL

  .panel <- rowpanel(NULL)
  if(has_panel) {
    .panel <- panel_data
    .panel$prefix_skip <- all_name
  }

  out <- list(
    data = ans,
    align = cols_center(.outer = "lr"),
    panel = .panel,
    bold_cols = !has_panel
  )
  out <- structure(out, class = "pmtable")
  out
}

