#' Scan data set columns for BQL / BLQ
#'
#' @param data a data frame
#'
#' @details
#' Will return the first match among `BQL` and `BLQ`.  If no match is found,
#' returns `NA_character_`.
#'
#' @export
find_bq_col <- function(data) {
  candidate <- intersect(c("BQL", "BLQ"), names(data))
  if(length(candidate)==0) return(NA_character_)
  candidate[1]
}

#' Create a data inventory summary for a data chunk
#'
#' @inheritParams pt_data_inventory
#' @param tot logical indicating if a summary row should be included
#' @param ... used to absorb other arguments; not used
data_inventory_chunk <- function(data, by, panel = by, stacked = FALSE,
                                 tot = FALSE, all_name = "all",
                                 dv_col = "DV",
                                 bq_col = "BQL",
                                 id_col = "ID",
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
    data <- mutate(data, .N = n_non_missing(!!sym(dv_col),!!sym(bq_col)))
    data <- ungroup(data)
  } else {
    data <- mutate(data, .N = n_non_missing(!!sym(dv_col),!!sym(bq_col)))
    data <- ungroup(data)
  }
  data <- group_by(data, !!sym(panel))
  data <- mutate(data,..n = n_non_missing(!!sym(dv_col),!!sym(bq_col)))
  data <- ungroup(data)
  data <- group_by(data, !!!syms(.groups))
  body <- summarise(
    data,
    SUBJ = n_unique(!!sym(id_col)),
    NOBS = n_obs(!!sym(dv_col),!!sym(bq_col)),
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
      data,
      by = ".total",
      panel = ".total",
      stacked = FALSE,
      all_name = all_name, ...
    )
    tot <- mutate(tot, .total = all_name)
    if(!stacked) {
      tot <- mutate(
        tot,
        POBS = "---",
        PBQL = "---"
      )
    }
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
#' @param data the data frame to summarize; the user should filter or subset
#' so that `data` contains exactly the records to be summarized; pmtables will
#' not add or remove rows prior to summarizing `data`
#' @param study_col the name of the column containing the study identifier;
#' may be character or quosure (see [dplyr::vars])
#' @param panel another categorical data set column name to stratify the
#' data summary
#' @param ... other arguments passed to [pt_data_inventory] and
#' [data_inventory_chunk]
#'
#' @examples
#'
#' ans <- pt_data_study(pmt_pk, study_col = "STUDYf")
#'
#' \dontrun{
#' st2report(stable(ans))
#' }
#'
#' @export
pt_data_study <- function(data, study_col = "STUDY", panel = study_col, ...) {
  pt_data_inventory(data  = data, by = study_col, panel = panel, ...)
}

#' Create a data inventory table
#'
#' This function summarizes your data in a specific way and returns an object
#' that can be converted into a `latex` table.
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
#' @param dv_col character name of `DV` column
#' @param bq_col character name of `BQL` column; see [find_bq_col()]
#' @param id_col character name of `ID` column
#' @param ... other arguments passed to [data_inventory_chunk()]
#'
#'
#' @details
#'
#' Output columns include counts for subjects (`SUBJ`), observations (`OBS)`,
#' BQL observations, missing observations (`MISS`) and percentage of
#' observations that are BQL. When panels are requested, then the percentages
#' for `OBS` and `BQL` are presented for the `Overall` data and for the panel
#' `Group`.
#'
#' Specifically, please note that:
#' - `MISS` is the number of data records that are missing (`NA`) and where
#'   the `BQL` (or `BLQ`) column is `0`
#' - `OBS` is the number of data records which are not missing (non-`NA`) and
#'   are not `BQL`
#' - `BQL` are records where the `BQL` (or `BLQ`) column is not equal to `0`
#'
#' The sum of `MISS` + `OBS` + `BQL` should equal the number of rows in the
#' data frame passed to `pt_data_inventory()`.
#'
#' When calculating percent `OBS` and percent `BQL`, we use `OBS + BQL` as the
#' denominator such that the percent `BQL` and percent `OBS` sum to `100`
#' within a group or panel. When the `panel` argument is set, these percentages
#' are calculated for the group (or `panel`)  as well as overall.
#'
#
#' The summary function is expecting certain columns to be named in a certain
#' way. This can be modified to suit your need by passing the following
#' arguments: `dv_col` (for observations), `bq_col` (for BQL observations),
#' and `id_col` (for ID). For example, if BQL indicator is in a column called
#' `BELOW` you would pass `bq_col = BELOW`.
#'
#' See the [data_inventory_chunk()] help topic for a description of these
#' columns.
#'
#' The notes for this table are generated by [pt_data_inventory_notes()].
#'
#' @examples
#'
#' ans <- pt_data_inventory(pmt_pk, by = c("Renal function" = "RFf"))
#'
#' ans <- pt_data_inventory(
#'    pmt_pk,
#'    by = "STUDYf",
#'    panel = "RFf"
#' )
#'
#' ans <- pt_data_inventory(
#'    pmt_obs,
#'    by = "STUDYf",
#'    panel = "SEQf",
#'    stacked = TRUE
#' )
#'
#' \dontrun{
#' st2report(stable(ans))
#' }
#'
#' @return
#' An object with class `pmtable`; see [class-pmtable].
#'
#' @export
pt_data_inventory <- function(data, by = ".total", panel = by,
                              inner_summary = TRUE, drop_miss = FALSE,
                              stacked = FALSE, table = NULL,
                              all_name = "all",
                              dv_col = "DV",
                              bq_col = find_bq_col(data),
                              id_col = "ID", ...) {

  has_panel <- !missing(panel)
  panel_data <- as.panel(panel)
  panel <- panel_data$col
  has_by <- !missing(by)

  by <- new_names(by,table)

  panel <- new_names(panel,table)

  drop_bql <- FALSE

  if(is.na(bq_col)) {
    data[["BQL"]] <- 0
    bq_col <- "BQL"
    drop_bql <- TRUE
  }

  if(panel==by | stacked) {
    inner_summary <- FALSE
  }

  total_name <- case_when(
    isTRUE(stacked)  ~ "\\hline {\\it Group Total}",
    TRUE ~ "\\hline \\hline {\\bf All data}"
  )

  ans <- data_inventory_data(
    data,
    by = by,
    panel = panel,
    stacked = stacked,
    all_name = all_name,
    dv_col = dv_col,
    bq_col = bq_col,
    id_col = id_col,
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

  if(isTRUE(drop_miss)) {
    ans <- mutate(ans, Number.MISS = NULL)
  }

  ans <- mutate(ans,.total = NULL)
  out <- ans

  notes <- pt_data_inventory_notes()

  if(isTRUE(drop_miss)) notes <- notes[!grepl("MISS", notes)]

  if(isTRUE(drop_bql)) {
    notes <- notes[!grepl("below", notes)]
    out <- select(out, !contains("BQL"))
  }

  .sumrows <- NULL

  .panel <- rowpanel(NULL)
  if(has_panel) {
    .panel <- panel_data
    .panel$prefix_skip <- "(Grand|Group) Total"
  }

  if(panel==by) panel <- NULL

  if(!stacked & isTRUE(has_by)) {
    .sumrows <- sumrow(out[,1]==total_name, bold = TRUE)
  }

  out <- list(
    data = out,
    panel = .panel,
    cols_rename = by,
    span_split = colsplit(sep = "."),
    align = cols_center(.outer = 'l'),
    notes = notes
  )

  out <- structure(out, class = c("pmtable", class(out)))

  return(out)
}

#' Return table notes for pt_data_inventory
#'
#' See [pt_data_inventory()].
#'
#' @param note_add additional notes to include
#'
#' @export
pt_data_inventory_notes <- function(note_add = NULL) {
  ans <- note_add
  ans <- c(
    ans,
    "SUBJ: subjects",
    "BQL: below quantitation limit",
    "MISS: missing observations (not BQL)",
    "OBS: observations"
  )
  ans
}
