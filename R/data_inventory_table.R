
#' Create a data inventory summary for a data chunk
#'
#' @inheritParams pt_data_inventory
#' @param tot logical indicating if a summary row should be included
#' @param dv_col character name of `DV` column
#' @param bq_col character name of `BQL` column
#' @param id_col character name of `ID` column
#' @param ... used to absorb other arguments; not used
data_inventory_chunk <- function(data, by, panel = by, stacked = FALSE,
                                 tot = FALSE, all_name = "all",
                                 dv_col = pt_opts$dv_col,
                                 bq_col = pt_opts$bq_col,
                                 id_col = pt_opts$id_col,
                                 ...) {

  if(by==".total" | panel == ".total") {
    data <- data_total_col(data,all_name = all_name)
  }

  miss <- FALSE
  miss_required <- function(needed,pass) {
    x <- c(
      "couldn't find the {needed} column; ",
      "set the column name to '{needed}' or ",
      "pass the name as '{pass}'"
    )
    glue::glue(paste0(x,collapse=""))
  }

  if(!exists(dv_col,data)) {
    emessage(miss_required("DV", "dv_col"))
    miss <- TRUE
  }

  if(!exists(bq_col,data)) {
    emessage(miss_required("BQL", "bq_col"))
    miss <- TRUE
  }

  if(!exists(id_col,data)) {
    emessage(miss_required("ID", "id_col"))
    miss <- TRUE
  }

  if(miss) {
    stop(
      "there was a problem finding required columns ",
      "for the data inventory summary; ",
      "please see error messages as well as '?data_inventory_chunk' ",
      "help topic",
      call.=FALSE
    )
  }

  .groups <- unique(c(panel,by))

  data <- ungroup(data)

  if(stacked) {
    data <- group_by(data, !!sym(panel))
    data <- mutate(data,.N = n_non_missing(!!sym(dv_col)))
    data <- ungroup(data)
  } else {
    data <- mutate(data,.N = n_non_missing(!!sym(dv_col)))
  }
  data <- group_by(data, !!sym(by))
  data <- mutate(data,..n = n_non_missing(!!sym(dv_col)))
  data <- ungroup(data)
  data <- group_by(data, !!!syms(.groups))
  body <- summarise(
    data,
    SUBJ = n_unique(!!sym(id_col)),
    NOBS = n_non_missing(!!sym(dv_col)),
    NMISS = n_missing(!!sym(dv_col), !!sym(bq_col)),
    POBS = digit1(100*.data[["NOBS"]]/first(.data[["..n"]])),
    OOBS = digit1(100*.data[["NOBS"]]/first(.data[[".N"]]))
  )
  bq <- summarise(
    data,
    NBQL = sum(!!sym(bq_col) !=0),
    PBQL = digit1(100*.data[["NBQL"]]/first(.data[["..n"]])),
    OBQL = digit1(100*.data[["NBQL"]]/first(.data[[".N"]]))
  )
  summ <- left_join(body,bq,by = unique(c(by,panel)))
  summ <- select(
    summ,
    !!sym(by),
    !!sym(panel),
    .data[["SUBJ"]],
    .data[["NMISS"]],
    .data[["NOBS"]],
    .data[["NBQL"]],
    .data[["POBS"]],
    .data[["PBQL"]],
    .data[["OOBS"]],
    .data[["OBQL"]]
  )
  summ <- ungroup(summ)
  summ <- mutate(summ, !!sym(by) := as.character(!!sym(by)))
  summ <- mutate(summ, !!sym(panel) := as.character(!!sym(panel)))
  summ
}

data_inventory_data_split <- function(data,by,panel=by,stacked=FALSE,...) {
  data <- ungroup(data)
  data <- split(data,data[[panel]],drop=TRUE)
  data <- map_dfr(data,data_inventory_data,by=by,panel=panel,stacked=FALSE,...)
  data <- mutate(data, !!sym(by) := replace_na(!!sym(by),"all"))
  data <- fill(data,!!sym(panel),.direction = "down")
  data[[".total"]] <- NULL
  data

}

#' Create a summary of endpoint data
#'
#' @inheritParams pt_data_inventory
#'
#' @param ... passed to subsequent summary functions
#'
#' @export
data_inventory_data <- function(data, by, panel = by, all_name = "all",
                                stacked = FALSE, ...) {
  by <- unname(by)
  panel <- unname(panel)

  if(stacked) {
    ans <- data_inventory_data_split(data, by, panel, stacked = FALSE, ...)
    return(ans)
  }

  data <- data_total_col(data, all_name = all_name)

  check_discrete(data,cols = unique(c(panel,by)))

  ans <- data_inventory_chunk(
    data = data,
    by = by,
    panel = panel,
    all_name = all_name,
    stacked = stacked,
    ...
  )

  if(by != ".total") {
    tot <- data_inventory_chunk(
      data, by = ".total", panel=".total", stacked = FALSE,
      all_name = all_name, ...
    )
    tot <- mutate(tot, .total = all_name)
    ans <- bind_rows(ans,tot)
  }

  if(panel != by) {
    ans <- mutate(ans, !!sym(by) := replace_na(!!sym(by),".total"))
    ans <- fill(ans, !!sym(panel), .direction = "down")
  }

  if(panel == by) {
    ans <- mutate(ans, !!sym(by) := replace_na(!!sym(by),".total"))
  }

  ans
}

#' Data inventory by study
#'
#' This is a convenience wrapper around [pt_data_inventory]
#'
#' @param data the data frame to summarize
#' @param study_col the name of the column containing the study identifier;
#' may be characer or quosure (see [dplyr::vars])
#' @param panel another categorical data set column name to stratify the
#' data summary
#' @param ... other arguments passed to [pt_data_inventory] and
#' [data_inventory_chunk]
#'
#' @examples
#' data <- pmtables:::data("obs")
#'
#' ans <- pt_data_study(data, study_col = "STUDYf")
#'
#' @export
pt_data_study <- function(data, study_col = "STUDY", panel = study_col, ...) {
  pt_data_inventory(data  = data, by = study_col, panel = panel, ...)
}

#' Create a data inventory table
#'
#' Output columns include counts for subjects, observations, BQL observations,
#' and missing observations and percentage of observations that are BQL.
#'
#' @inheritParams pt_cont_long
#'
#' @param by the outer grouping variable; may be character or quosure
#' @param panel the panel grouping variable; may be character or quosure
#' @param inner_summary if `TRUE`, then a summary of the inner variable will
#' be provided
#' @param drop_miss if `TRUE`, then `MISS` will be dropped, but only when all
#' `MISS` values are equal to zero
#' @param stacked if `TRUE`, then independent summaries are created by `outer`
#' and included in a single table (see examples)
#' @param ... other arguments passed to [data_inventory_chunk]
#'
#' @details
#'
#' The summary function is expecting certain columns to be named in a certain
#' way. This can be modified to suit your need by passing the following
#' arguments: `dv_col` (for observations), `bq_col` (for BQL observations),
#' and `id_col` (for ID). For example, if BQL indicator is in a column called
#' `BELOW` you would pass `bq_col = BELOW`.
#'
#' See the [data_inventory_chunk] help topic for a description of these columns.
#'
#' @examples
#' data <- pmtables:::data("obs")
#'
#' ans <- pt_data_inventory(data, by = .cols("Renal function" = RFf))
#'
#' ans <- pt_data_inventory(
#'    data,
#'    by = "STUDYf",
#'    panel = "RFf"
#' )
#'
#' ans <- pt_data_inventory(
#'    data,
#'    by = "STUDYf",
#'    panel = "SEQf",
#'    stacked = TRUE
#' )
#'
#' @export
pt_data_inventory <- function(data, by = ".total", panel = by,
                              inner_summary = TRUE, drop_miss = FALSE,
                              stacked = FALSE, table = NULL,
                              all_name = "all", ...) {

  by <- new_names(by,table)
  panel <- new_names(panel,table)

  if(panel==by | stacked) {
    inner_summary <- FALSE
  }

  total_name <- ifelse(stacked, "Group Total", "Grand Total")

  ans <- data_inventory_data(
    data,
    by = by,
    panel = panel,
    stacked = stacked,
    all_name = all_name,
    ...
  )

  if(exists(by,ans)) {
    ans <- mutate(
      ans,
      !!sym(by) := ifelse(!!sym(by)==".total", total_name, !!sym(by))
    )
  }

  if(inner_summary) {
    ans <- rename(
      ans,
      `Group percent.OBS` = .data[["POBS"]],
      `Group percent.BQL` = .data[["PBQL"]],
      `Overall percent.OBS` = .data[["OOBS"]],
      `Overall percent.BQL` = .data[["OBQL"]]
    )
  } else {
    ans <- rename(
      ans,
      `Percent.OBS` = .data[["OOBS"]],
      `Percent.BQL` = .data[["OBQL"]]
    )
    ans <- mutate(ans,POBS=NULL,PBQL=NULL)
  }

  ans <- rename(
    ans,
    Number.SUBJ = .data[["SUBJ"]],
    Number.MISS = .data[["NMISS"]],
    Number.OBS = .data[["NOBS"]],
    Number.BQL = .data[["NBQL"]]
  )

  if(drop_miss) {
    if(all(ans$MISS==0)) {
      ans <- mutate(ans, MISS = NULL)
    }
  }

  if(panel != by) {
    ans <- mutate(
      ans,
      .total = NULL,
      !!sym(panel) := paste0(names(panel), ": ", !!sym(panel))
    )
    ans <- rename(ans,panel)
    ans <- rename(ans,by)
    out <- gt(ans,groupname_col=names(panel))
  }
  if(panel==by & by != ".total") {
    ans <- rename(ans, by)
    ans <- mutate(ans,.total = NULL)
    out <- gt(ans)
  }
  if(by==".total" & panel == by ) {
    ans <- mutate(ans,.total = NULL)
    out <- gt(ans)
  }

  out <- tab_sp_delim(out,delim = '.')

  out <- tab_source_note(
    out,
    "SUBJ: subjects; OBS: observations; MISS: missing;
     BQL: below quantitation limit"
  )

  gt_opts_(out)
}

# data_inventory_stacked <- function(data,inner,stack_by,...) {
#   pt_data_inventory(
#     data,
#     by = outer,
#     inner = inner,
#     inner_summary = FALSE,
#     stacked = TRUE,
#     ...
#   )
# }

to_html <- function(x) {
  if(interactive()) return(x)
  as_raw_html(x)
}
