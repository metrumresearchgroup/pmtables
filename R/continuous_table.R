#' Create continuous summary data frame
#'
#' @inheritParams pt_cont_study
#' @param by grouping variable name
#' @param panel paneling variable name
#' @param all_name label for full data summary
#' @param digits named list specifing `digits` argument for `digit_fun`
#' @param fun continuous data summary function
#'
#' @export
cont_table_data <- function(data, cols, by = ".total", panel = by, wide = FALSE,
                            all_name = "all", digits = NULL,
                            fun = pt_opts$cont.long.fun) {

  cols <- unname(new_names(cols))
  by <- unname(new_names(by))
  panel <- unname(new_names(panel))

  if(is.null(digits)) {
    digits <- new_digits(sig,3)
  }

  check_exists(data, pt_opts$id_col)

  digits <- update_digits(digits,cols)
  digit_fun <- get_digits_fun(digits)
  digit_data <- get_digits_list(digits)

  data <- data_total_col(data, all_name = all_name)

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

  if(packageVersion("dplyr") < '0.8.99') {
    d2 <- group_modify(
      d1,
      ~fun(
        value = .$value,
        digit_fun = digit_fun,
        digits = .$digitn[1],
        name = .$name[1],
        id = .[[pt_opts$id_col]]
      ),
      keep = TRUE
    )
  } else {
    d2 <- group_modify(
      d1,
      ~fun(
        value = .$value,
        digit_fun = digit_fun,
        digits = .$digitn[1],
        name = .$name[1],
        id = .[[pt_opts$id_col]]
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
#' @inheritParams pt_cont_long
#' @inheritParams pt_opts
#'
#' @param by a grouping variable name
#' @param panel a variable for paneling the summary
#'
#' @export
pt_cont_wide <- function(data, cols,
                         by = ".total",
                         panel = by,
                         table = NULL,
                         units = NULL,
                         digits = NULL,
                         all_name = "All data",
                         fun = str_sum_2,
                         panel.label.add = pt_opts$panel.label.add) {

  tst <- fun(rnorm(10))
  assert_that(identical(names(tst),"summary"))

  cols <- new_names(cols,table)
  by <- new_names(by,table)
  panel <- new_names(panel,table)

  data <- data_total_col(data, all_name = all_name)

  check_continuous(data,cols)
  check_discrete(data,by)

  ans <- cont_table_data(
    data = data,
    cols = cols,
    by = by,
    panel = panel,
    fun = fun,
    digits = digits,
    wide = TRUE
  )

  all_summary <- FALSE
  if(by != ".total") {
    all_summary <- TRUE
    ans2 <- cont_table_data(
      data=data,
      cols=cols,
      by=".total",
      panel = ".total",
      fun = fun,
      digits = digits,
      wide = TRUE
    )
    ans2 <- mutate(ans2, outer := all_name)
    if(panel != by) {
      ans2 <- mutate(ans2, !!sym(panel) := "Total")
      if(isTRUE(panel.label.add)) {
        ans <- mutate(
          ans,
          !!sym(panel) := paste0(names(panel),": ", !!sym(panel))
        )
      }
    }
    ans <- bind_rows(ans,ans2)
  }

  if(by != ".total") {
    ans <- rename(ans,!!sym(names(by)) := outer)
  }

  ans[[".total"]] <- NULL

  if(by==panel) ans[["outer"]] <- NULL

  if(panel==by) {
    out <- gt(ans,row_group.sep=" ")
  } else {
    out <- gt(ans,row_group.sep=" ",groupname_col=panel)
  }

  if(exists(by,ans)) {
    out <- cols_label(out, !!sym(by) := names(by)[1])
  }

  if(is.list(units)) {
    for(col in cols) {
      if(exists(col,units)) {
        lab <- paste0(col," ", units[[col]])
        out <- cols_label(out, !!col := lab)
      }
    }
  }

  out <- tab_stubhead(out,"Variable")
  out <- cols_align(out,"right")

  if(is.list(table)) {
    out <-
      tab_source_note(
        out,
        source_note = foot(table,unname(cols))
      )
  }

  if(is.logical(formals(fun)[["footnote"]])) {
    out <- tab_source_note(out, fun(footnote = TRUE))
  }

  out
}

#' Continuous data summary in long format
#'
#'
#' @inheritParams pt_cont_study
#' @inheritParams pt_opts
#' @param panel data set column name to stratify the summary
#' @param table a named list to use for renaming columns (see details and
#' examples)
#' @param units a named list to use for unit lookup (see details and examples)
#' @param digits a `digits` object (see [new_digits])
#' @param summarize_all if `TRUE` then a complete data summary will be appended
#' to the bottom of the table
#' @param all_name a name to use for the complete data summary
#' @param fun the data summary function (see details)
#'
#' @examples
#' data <- pmtables:::data("id")
#'
#' ans <- pt_cont_long(data, cols = .cols(WT,ALB,CRCL))
#'
#' ans <- pt_cont_long(data, cols = "WT,CRCL", panel = "SEXf")
#'
#' @export
pt_cont_long <- function(data,
                         cols,
                         panel = ".total",
                         table = NULL,
                         units = NULL,
                         digits = pt_opts$digits,
                         summarize_all = TRUE,
                         all_name="All data",
                         fun = df_sum_2,
                         panel.label.add = pt_opts$panel.label.add) {

  by <- panel
  summarize_all <- summarize_all & by != ".total"
  data <- data_total_col(data)

  cols <- new_names(cols,table)
  by <- new_names(by,table)

  check_continuous(data,cols)
  check_discrete(data,by)

  ans <- cont_table_data(
    data = data,
    cols = unname(cols),
    by = unname(by),
    fun = fun,
    digits = digits,
    wide = FALSE
  )

  if(isTRUE(panel.label.add)) {
    ans <- mutate(ans,outer=paste(names(by)[1], outer))
  }

  if(by==".total") ans <- mutate(ans, outer=all_name)

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

  out <- gt(
    ans,
    row_group.sep=" ",
    rowname_col = "name",
    groupname_col = c("outer")
  )

  out <- cols_label(out, outer = names(by)[1])
  out <- tab_stubhead(out,"Variable")
  out <- cols_align(out,"right")

  if(exists("Min..Max", ans)) {
    out <- cols_label(out, Min..Max = "Min / Max")
  }

  if(is.list(table)) {
    out <-
      tab_footnote(
        out,
        footnote = foot(table,unname(cols)),
        locations = cells_stubhead()
      )
  }

  if(is.logical(formals(fun)[["footnote"]])) {
    footn <- fun(footnote = TRUE)
    if(is.list(footn)) {
      for(this_foot in footn) {
        out <- tab_footnote(
          out,
          footnote = this_foot$footnote,
          locations = this_foot$locations
        )
      }
    }
  }

  out
}

#' Continuous covariate table by study
#'
#' @param data the data frame to summarize
#' @param cols the columns to summarize; may be character vector or quosure
#' @param study_col character name of the study ID column; passed to
#' [pt_cont_long] as `panel` or [pt_cont_wide] as `by`
#' @param wide if `TRUE` covariates are rendered west to east; if `FALSE` then
#' they are rendered north to south
#' @param ... other arguments passed to [pt_cont_long] or [pt_cont_wide]
#'
#' @examples
#'
#' data <- pmtables:::data("id")
#'
#' pt_cont_study(data, cols = "WT,ALB,SCR", study_col="STUDYf")
#'
#' pt_cont_study(data, cols = "WT,ALB,SCR", study_col="STUDYf", wide = TRUE)
#'
#' @export
pt_cont_study <- function(data,cols,study_col = "STUDY", wide = FALSE,...) {
  if(wide) {
    pt_cont_wide(
      data = data,
      cols = cols,
      by = study_col,
      ...
    )
  } else {
    pt_cont_long(
      data = data,
      cols = cols,
      panel = study_col,
      ...
    )
  }
}
