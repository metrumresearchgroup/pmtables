#' @export
cat_data <- function(data,cols,by=".total",wide=FALSE, summarize_all=TRUE,
                     all_name = "All", table=NULL, nby = NULL) {

  data <- ungroup(data)

  data <- data_total_col(data, all_name)

  if(is.null(nby)) nby <- length(unique(data[[by]]))

  for(col in cols) {
    if(!is.factor(data[[col]])) {
      data[[col]] <- fct_inorder(data[[col]])
    }
  }

  data <- group_by(data, !!sym(by))

  ans <- group_modify(data, ~ summarize_cat_chunk(.,cols))

  ans <- ungroup(ans)

  ans <- mutate(ans,name=names(cols)[name])

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
      id_cols = c("name", "level"),
      names_from = !!by,
      values_from = "summary",
      names_sep = "."
    )
  }
  ans
}

#' @export
pt_cat_long <- function(data,
                        cols,
                        by = ".total",
                        all_name = "All Groups",
                        summarize_all = TRUE,
                        table = NULL,
                        notes = NULL) {

  if(by==".total" & missing(all_name)) {
    all_name <- "Summary"
  }

  cols <- new_names(cols,table=table)

  data <- data_total_col(data,all_name)

  assert_that(length(by)==1)
  by <- new_names(by, table = table)

  bys <- levels(factor(data[[by]]))
  nby <- length(bys)

  check_discrete(data, cols, by)

  ans <- cat_data(
    data = data,
    cols = cols,
    table = table,
    by = by,
    nby = nby
  )

  summarize_all <- summarize_all & nby > 1

  if(summarize_all) {
    all <- cat_data(
      data,
      cols = cols,
      by = ".total",
      table = table,
      nby = nby,
      all_name = all_name
    )
    ans <- left_join(ans, all, by=c("name","level"))
  }

  tab <- gt(ans, rowname_col="level", groupname_col="name")

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

  tab <- tab_source_note(
    tab,
    "Summaries are count (percent)"
  )

  if(is.character(notes)) {
    tab <- tab_source_note(tab, notes)
  }

  tab
}

#' @export
pt_cat_wide <- function(data,cols, by = ".total", table = NULL, all_name="All",
                        summarize_all = TRUE, notes = NULL) {

  cols <- new_names(cols,table)
  by <- new_names(by,table)

  assert_that(length(by)==1)

  data <- data_total_col(data, all_name)

  nby <- length(unique(data[[by]]))

  check_discrete(data,cols,by)

  summarize_all <- summarize_all & nby > 1

  ans <- cat_data(data, cols, by = by, table = table, wide = TRUE)
  ans <- mutate(ans, !!sym(by) := as.character(!!sym(by)))

  if(summarize_all) {
    all <- cat_data(data, cols, by = ".total", table = table, wide = TRUE)
    all <- rename(all, !!sym(by) := .total)
    ans <- bind_rows(ans,all)
  }

  out <- gt(ungroup(ans))

  out <- cols_label(
    out,
    {{by}} := names(by)[1]
  )

  if(nby > 1) {
    out <- tab_row_group(
      out,
      group = "Total",
      rows = ans[[1]]==all_name
    )
    out <- row_group_order(
      out,
      groups = c(NA, "Total")
    )
  }

  out <- tab_sp_delim(out,delim='.')

  if(nby==1 & exists(".total", ans)) {
    out <- cols_label(
      out,
      .total = ""
    )
  }

  if(is.character(notes)) {
    out <- tab_source_note(out,notes)
  }

  out <- tab_source_note(out, "Summaries are count (percent)")

  out
}

#' Discrete covariate table by study
#'
#' @inheritParams pt_cont_study
#'
#' @param ... other arguments passed to [pt_cat_long] or [pt_cat_wide]
#'
#' @examples
#'
#' data <- pmtables:::data("id")
#'
#' pt_cat_study(data,cols = "SEXf,FORMf", study="STUDYf")
#'
#' pt_cat_study(data,cols = "SEXf,FORMf", study="STUDYf", wide = TRUE)
#'
#' @export
pt_cat_study<- function(data,
                        cols,
                        study_col = vars("Study ID" = STUDY),
                        all_name = "All studies",
                        summarize_all = TRUE,
                        table = NULL,
                        notes = NULL,
                        wide = FALSE) {
  if(wide) {
    tab <- pt_cat_wide(
      data = data,
      cols = cols,
      by = study_col,
      all_name = all_name,
      summarize_all = summarize_all,
      table = table,
      notes = notes
    )
  } else {
    tab <- pt_cat_long(
      data = data,
      cols = cols,
      by = study_col,
      all_name = all_name,
      summarize_all = summarize_all,
      table = table,
      notes = notes
    )
  }
  tab
}

