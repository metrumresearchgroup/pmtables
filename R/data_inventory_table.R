
#' Create a data inventory summary for a data chunk
#'
#' @inheritParams pt_data_inventory
#' @param tot logical indicating if a summary row should be included
#' @param dv_col character name of `DV` column
#' @param bq_col character name of `BQL` column
#' @param id_col character name of `ID` column
#' @param ... used to absorb other arguments; not used
data_inventory_chunk <- function(data, outer, inner = outer, stacked = FALSE,
                                 tot = FALSE, all_name = "all",
                                 dv_col = "DV", bq_col = "BQL", id_col = "ID",
                                 ...) {

  if(outer==".total" | inner==".total") {
    data <- data_total_col(data,all_name = all_name)
  }

  bq_col <- match.arg(bq_col)

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

  by <- unique(c(outer,inner))

  data <- ungroup(data)

  if(stacked) {
    data <- group_by(data, !!sym(outer))
    data <- mutate(data,.N = n_non_missing(!!sym(dv_col)))
    data <- ungroup(data)
  } else {
    data <- mutate(data,.N = n_non_missing(!!sym(dv_col)))
  }
  data <- group_by(data, !!sym(outer))
  data <- mutate(data,..n = n_non_missing(!!sym(dv_col)))
  data <- ungroup(data)
  data <- group_by(data, !!!syms(by))
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
  summ <- left_join(body,bq,by = unique(c(outer,inner)))
  summ <- select(
    summ,
    !!sym(outer),
    !!sym(inner),
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
  summ <- mutate(summ, !!sym(outer) := as.character(!!sym(outer)))
  summ <- mutate(summ, !!sym(inner) := as.character(!!sym(inner)))
  summ
}

data_inventory_data_split <- function(data,outer,inner=outer,stacked=FALSE,...) {
  data <- ungroup(data)
  data <- split(data,data[[outer]],drop=TRUE)
  data <- map_dfr(data,data_inventory_data,outer=outer,inner=inner,stacked=FALSE,...)
  data <- mutate(data, !!sym(inner) := replace_na(!!sym(inner),"all"))
  data <- fill(data,!!sym(outer),.direction = "down")
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
data_inventory_data <- function(data, outer,inner=outer,all_name = "all",
                                stacked=FALSE, ...) {
  outer <- unname(outer)
  inner <- unname(inner)

  if(stacked) {
    ans <- data_inventory_data_split(data,outer,inner, stacked = FALSE, ...)
    return(ans)
  }

  data <- data_total_col(data, all_name = all_name)
  check_discrete(data,cols = unique(c(outer,inner)))

  ans <- data_inventory_chunk(
    data = data,
    outer = outer,
    inner = inner,
    all_name = all_name,
    stacked = stacked,
    ...
  )

  if(outer != ".total") {
    tot <- data_inventory_chunk(
      data, outer = ".total", inner=".total", stacked = FALSE,
      all_name = all_name, ...
    )
    tot <- mutate(tot, .total = all_name)
    ans <- bind_rows(ans,tot)
  }

  if(inner!=outer) {
    ans <- mutate(ans, !!sym(inner) := replace_na(!!sym(inner),".total"))
    ans <- fill(ans, !!sym(outer), .direction = "down")
  }

  if(inner==outer) {
    ans <- mutate(ans, !!sym(outer) := replace_na(!!sym(outer),".total"))
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
#' @param inner another categorical data set column name to stratify the
#' data summary
#' @param ... other arguments passed to [pt_data_inventory] and
#' [data_inventory_chunk]
#'
#' @examples
#' data <- pmtables:::data("id")
#'
#' ans <- pt_data_study(data, study_col = "STUDYf")
#'
#' @export
pt_data_study <- function(data, study_col = "STUDY", inner = study_col, ...) {
  pt_data_inventory(data  = data, outer = study_col, inner = inner, ...)
}

#' Create a data inventory table
#'
#' Output columns include counts for subjects, observations, BQL observations,
#' and missing observations and percentage of observations that are BQL.
#'
#' @inheritParams pt_cont_long
#'
#' @param outer the outer grouping variable; may be character or quosure
#' @param inner the inner grouping variable; may be character or quosure
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
#' and `id_col` (for ID). See the [data_inventory_chunk] help topic for
#' a description of these columns.
#'
#' @examples
#' data <- pmtables:::data("id")
#'
#' ans <- pt_data_inventory(data, outer = .cols("Renal function" = RFf))
#'
#'
#' @export
pt_data_inventory <- function(data, outer = ".total", inner = outer,
                              inner_summary = TRUE, drop_miss = FALSE,
                              stacked = FALSE, table = NULL,
                              all_name = "all", ...) {

  outer <- new_names(outer,table)
  inner <- new_names(inner,table)

  if(inner==outer | stacked) {
    inner_summary <- FALSE
  }

  total_name <- ifelse(stacked, "Group Total", "Grand Total")

  ans <- data_inventory_data(
    data,
    outer = outer,
    inner = inner,
    stacked = stacked,
    all_name = all_name,
    ...
  )

  if(exists(inner,ans)) {
    ans <- mutate(
      ans,
      !!sym(inner) := ifelse(!!sym(inner)==".total", total_name, !!sym(inner))
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

  if(inner!=outer) {
    ans <- mutate(
      ans,
      .total = NULL,
      !!sym(outer) := paste0(names(outer), ": ", !!sym(outer))
    )
    ans <- rename(ans,inner)
    ans <- rename(ans,outer)
    out <- gt(ans,groupname_col=names(outer))
  }
  if(inner==outer & outer != ".total") {
    ans <- rename(ans, outer)
    ans <- mutate(ans,.total = NULL)
    out <- gt(ans)
  }
  if(outer==".total" & inner==outer ) {
    ans <- mutate(ans,.total = NULL)
    out <- gt(ans)
  }

  out <- tab_sp_delim(out,delim = '.')

  out <- tab_source_note(
    out,
    "SUBJ: subjects; OBS: observations; MISS: missing;
     BQL: below quantitation limit"
  )

  out
}

data_inventory_stacked <- function(data,inner,stack_by,...) {
  pt_data_inventory(
    data,
    outer = outer,
    inner = inner,
    inner_summary = FALSE,
    stacked = TRUE,
    ...
  )
}
to_html <- function(x) {
  if(interactive()) return(x)
  as_raw_html(x)
}
