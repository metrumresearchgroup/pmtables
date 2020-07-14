#' Summarize categorical data
#'
#' @inheritParams pt_cont_study
#' @inheritParams pt_cont_long
#' @inheritParams pt_cat_long
#'
#' @param cols character vector of column names for summary
#' @param by grouping variable name
#' @param summarize_all logical indicating whether or not to include a summary
#' of the full data in the output
#' @param nby number of unique levels for the `by` variable
#' @param preshape if `TRUE`, returns summarized data prior to reshaping;
#' this is intended for internal use
#'
#' @examples
#' data <- pmtables:::data("id")
#'
#' cat_data(data, cols = c(SEX = "SEXf", RF = "RFf"), by = "STUDYf")
#'
#' @export
cat_data <- function(data, cols, by = ".total", panel = by,
                     summarize_all = TRUE, all_name = "All",
                     wide = FALSE, nby = NULL, preshape = FALSE) {

  cols <- new_names(cols)

  data <- ungroup(data)

  data <- data_total_col(data, all_name)

  if(is.null(nby)) nby <- length(unique(data[[by]]))

  for(col in cols) {
    if(!is.factor(data[[col]])) {
      data[[col]] <- fct_inorder(data[[col]])
    }
  }

  .groups <- unique(c(panel,by))

  data <- group_by(data, !!!syms(unname(.groups)))

  ans <- group_modify(data, ~ summarize_cat_chunk(.,cols))

  ans <- ungroup(ans)

  ans <- mutate(ans,name=names(cols)[.data[["name"]]])

  if(preshape) return(ans)

  if(wide) {
    ans <- pivot_wider(
      ans,
      names_from = c("name", "level"),
      values_from = "summary",
      names_sep = '.'
    )
  } else {
    ans <- pivot_wider(
      ans,
      names_from = by,
      values_from = "summary",
      names_sep = "."
    )
  }
  ans
}

#' Create categorical data summary tables
#'
#' @inheritParams pt_cont_long
#' @inheritParams pt_opts
#'
#' @param by variable name for grouping
#' @param span variable name for column spanner
#'
#'
#' @export
pt_cat_long <- function(data, cols, span  = by, by = ".total",
                        all_name = "All Groups", summarize_all = TRUE,
                        table = NULL) {

  if(missing(by) & !missing(span)) {
    by <- span
  }

  if(by == ".total" & missing(all_name)) {
    all_name <- "Summary"
  }

  cols <- new_names(cols, table = table)

  data <- data_total_col(data, all_name)

  assert_that(length(by)==1)
  by <- new_names(by, table = table)

  bys <- levels(factor(data[[by]]))
  nby <- length(bys)

  check_discrete(data = data, cols = cols, others = by)

  ans <- cat_data(
    data = data,
    cols = cols,
    by = by,
    nby = nby
  )

  summarize_all <- summarize_all & nby > 1

  if(summarize_all) {
    all <- cat_data(
      data,
      cols = cols,
      by = ".total",
      nby = nby,
      all_name = all_name
    )
    ans <- left_join(ans, all, by=c("name", "level"))
  }

  tab <- gt(ans, rowname_col = "level", groupname_col = "name")

  if(nby > 1) {
    span_cols <- intersect(bys,names(ans))
    span_label <- names(by)[1]
    tab <- tab_spanner(
      tab,
      label = span_label,
      columns = span_cols,
      gather = FALSE
    )
  }

  tab <- tab_source_note(tab, "Summaries are count (percent)")

  gt_opts_(tab)
}

#' @rdname pt_cat_long
#' @export
pt_cat_wide <- function(data,cols, by = ".total", panel = by,
                        table = NULL, all_name="All data",
                        summarize_all = TRUE,
                        panel.label.add = pt_opts$panel.label_add) {

  cols <- new_names(cols, table)
  by <- new_names(by, table)
  panel <- new_names(panel,table)

  assert_that(length(by) == 1)

  data <- data_total_col(data, all_name)

  nby <- length(unique(data[[by]]))

  check_discrete(data = data, cols = cols, others = by)

  summarize_all <- summarize_all & nby > 1

  ans <- cat_data(data, cols, by = by, panel = panel, wide = TRUE)
  ans <- mutate(ans, !!sym(by) := as.character(!!sym(by)))

  if(summarize_all) {

    all <- cat_data(data, cols, by = ".total", panel = ".total", wide = TRUE)

    if(panel != by) {

      all <- mutate(all, !!sym(panel) := "Total")
      if(isTRUE(panel.label.add)) {
        ans <- mutate(
          ans,
          !!sym(panel) := paste0(names(panel),": ", !!sym(panel))
        )
      }
    }
    all <- mutate(all, !!sym(by) := all_name)
    ans <- bind_rows(ans, all)
  }

  ans <- ungroup(ans)

  ans[[".total"]] <- NULL

  if(panel==by) {
    out <- gt(ans, row_group.sep = " ")
  } else {
    out <- gt(ans, row_group.sep=" ", groupname_col = panel, rowname_col = by)
    out <- tab_stubhead(out, names(by))
  }

  if(exists(by,ans)) {
    out <- cols_label(
      out,
      {{by}} := names(by)[1]
    )
  }

  out <- tab_sp_delim(out, delim = '.')

  out <- tab_source_note(out, "Summaries are count (percent)")

  gt_opts_(out)
}

#' Discrete covariate table by study
#'
#' @inheritParams pt_cont_long
#'
#' @param study_col character name of the data set column containing the study
#' identifier
#' @param all_name label for full data summary
#' @param table a named list to use for renaming columns (see details and
#' examples)
#' @param wide if `TRUE` the table will be rendered in a wide format
#'
#' @examples
#'
#' data <- pmtables:::data("id")
#'
#' ans <- pt_cat_study(data, cols = .cols(SEXf,FORMf), study = "STUDYf")
#'
#' ans <- pt_cat_study(data, cols = .cols(FORMf), study = "STUDYf", wide = TRUE)
#'
#' @export
pt_cat_study<- function(data,
                        cols,
                        study_col = vars("Study ID" = all_of("STUDY")),
                        summarize_all = TRUE,
                        all_name = "All studies",
                        table = NULL,
                        wide = FALSE) {
  if(wide) {
    tab <- pt_cat_wide(
      data = data,
      cols = cols,
      by = study_col,
      all_name = all_name,
      summarize_all = summarize_all,
      table = table
    )
  } else {
    tab <- pt_cat_long(
      data = data,
      cols = cols,
      by = study_col,
      all_name = all_name,
      summarize_all = summarize_all,
      table = table
    )
  }
  tab
}
