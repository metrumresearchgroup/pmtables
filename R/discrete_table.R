cat_long_all <- function(data, group = ".total", all_name = "All data") {
  N <- nrow(data)
  data_long <- pivot_longer(
    data,
    cols = all_of(group),
    values_to = "by"
  )
  data_long <- group_by(data_long, by)
  data_summ <- summarise(
    data_long,
    level = all_name,
    N = .env[["N"]],
    #value = paste(n(), paste0("(",digit1(100*n()/N),")")),
    value = paste0("n = ", n()),
    .groups = "drop"
  )
  pivot_wider(data_summ, names_from = "by")
}

prep_cat_data <- function(data, cols) {
  for(col in cols) {
    if(anyNA(data[[col]])) {
      warning(glue('col `{col}`: missing values replaced with "NA"'))
      is_fctr <- is.factor(data[[col]])
      if(is_fctr) {
        lvls <- unique(c(levels(data[[col]]),"NA"))
        data[[col]] <- as.character(data[[col]])
      }
      w <- is.na(data[[col]])
      data[[col]][w] <- rep("NA", sum(w))
      if(is_fctr) data[[col]] <- factor(data[[col]], levels = lvls)
    }
    if(!is.factor(data[[col]])) {
      data[[col]] <- fct_inorder(data[[col]])
    }
  }
  data
}

#' Summarize categorical data
#'
#' @inheritParams pt_cont_wide
#' @param summarize_all logical indicating whether or not to include a summary
#' of the full data in the output
#' @param all_name label for full data summary
#' @param nby number of unique levels for the `by` variable
#' @param wide `logical`; if `TRUE`, data frame will be returned in wide format;
#' if `FALSE`, it will be returned in `long` format
#'
#' @examples
#'
#' cat_data(pmt_first, cols = c(SEX = "SEXf", RF = "RFf"), by = "STUDYf")
#'
#' @export
cat_data <- function(data, cols, by = ".total", panel = by,
                     summarize_all = TRUE, all_name = "All",
                     wide = FALSE, nby = NULL) {

  cols <- new_names(cols)

  data <- ungroup(data)

  data <- data_total_col(data, all_name)

  if(is.null(nby)) nby <- length(unique(data[[by]]))

  .groups <- unique(c(panel,by))

  data <- group_by(data, !!!syms(unname(.groups)))

  ans <- group_modify(data, ~ summarize_cat_chunk(.,cols))

  ans <- ungroup(ans)

  ans <- mutate(ans,name=names(cols)[.data[["name"]]])

  if(wide) {
    ans <- pivot_wider(
      ans,
      names_from = c("name", "level"),
      values_from = "summary",
      names_sep = '_._'
    )
    #ans <- complete(ans, !!!syms(unname(.groups)))
    #print(ans)
    #ans <- mutate(ans, N = replace_na(N, 0))
    #ans <- mutate(ans, across(everything(), replace_na, "0 (0.0)"))
  } else {
    ans[["N"]] <- NULL
    ans <- pivot_wider(
      ans,
      names_from = all_of(unname(by)),
      values_from = "summary",
      names_sep = '_._'
    )
  }
  ans
}

#' Discrete data summary in long format
#'
#' @inheritParams pt_cont_long
#' @param span variable name for column spanner
#' @param all_name_span table column name to use for data summaries across
#' levels of `span` if it is provided
#' @param summarize where to include a data summary across subgroups;
#' use `none` to drop the summary from the table
#' @param by use `span` argument instead
#'
#' @details
#' The data summary for all cells in the table is `count (percent)`.
#'
#' The notes in this table are generated with [pt_cat_long_notes()].
#'
#' @examples
#'
#' out <- pt_cat_long(pmt_first, cols = "SEXf,ASIANf", span = "FORMf")
#'
#' \dontrun{
#' st2report(stable(out))
#' }
#'
#' @return
#' An object with class `pmtable`; see [class-pmtable].
#'
#' @export
pt_cat_long <- function(data, cols, span  =  ".total",
                        all_name = " ",
                        all_name_span = "Summary",
                        summarize = c("both", "right", "top", "none"),
                        table = NULL, by = NULL) {

  summarize <- match.arg(summarize)
  summarize_all <- summarize != "none"

  has_span <- !missing(span)

  if(!missing(by) & missing(span)) {
    warning("the 'by' argument was used; maybe you wanted 'span' instead?")
  }

  if(span == ".total" & missing(all_name_span)) {
    all_name_span <- "Summary"
  }

  cols <- new_names(cols, table = table)

  data <- data_total_col(data, all_name_span)

  assert_that(length(span)==1)
  span <- new_names(span, table = table)

  spans <- levels(factor(data[[span]]))
  nspan <- length(spans)

  check_discrete(data = data, cols = cols, others = span)

  data <- prep_cat_data(data, cols)

  ans <- cat_data(
    data = data,
    cols = cols,
    by = span,
    nby = nspan
  )

  if(summarize_all) {
    if(has_span && summarize %in% c("right", "both")) {
      all <- cat_data(
        data,
        cols = cols,
        by = ".total",
        nby = nspan,
        all_name = all_name_span
      )
      all[["N"]] <- NULL
      ans <- left_join(ans, all, by = c("name", "level"))
    }
  }

  output_span <- NULL

  if(has_span) {
    output_span <- colgroup(names(span), unique(data[[span]]))
  }

  out <- list(
    data = ans,
    span = output_span,
    align = cols_center(.outer = 'l'),
    cols_rename = span,
    cols_blank = "level",
    panel = "name",
    notes = pt_cat_long_notes()
  )

  if(summarize %in% c("top", "both")) {
    bot <- cat_long_all(data, unname(span))
    if(summarize=="both") {
      bot[[all_name_span]] <- paste0("n = ", nrow(data))
    }
    bot[["N"]] <- NULL
    bot[["level"]] <- ""
    out$cols_extra <- bot
  }

  out <- structure(out, class = c("pmtable", class(out)))

  return(out)
}

#' Return table notes for pt_cat_long
#'
#' See [pt_cat_long()].
#'
#' @param include_n if `TRUE`, add a note for `n`
#' @param note_add additional notes to include
#'
#' @export
pt_cat_long_notes <- function(include_n = TRUE, note_add = NULL) {
  ans <- note_add
  ans <- c(ans, "Summary is count (percent)")
  if(isTRUE(include_n)) {
    ans <- c(ans, "n: number of records summarized")
  }
  ans
}

#' Discrete data summary in long format
#'
#' @inheritParams pt_cont_wide
#' @param by a grouping variable for the summary; may be given as character
#' vector or quosure
#' @param summarize where to put an all-data summary; choose `none` to omit the
#' summary from the table
#'
#' @details
#' The data summary for this table is `count (percent)`. The number of
#' data points for each row is also summarized as `n` on the left hand side
#' of the table (either on the far left or just to the right of the `by`
#' column).
#'
#' The notes in this table are generated with [pt_cat_wide_notes()].
#'
#' @examples
#' out1 <- pt_cat_wide(pmt_first, cols = "SEXf,ASIANf")
#' stable(out1)
#'
#' out2 <- pt_cat_wide(pmt_first, cols = "SEXf,ASIANf", by = "FORMf")
#' stable(out2)
#'
#' \dontrun{
#' st2report(stable(out1))
#' st2report(stable(out2))
#' }
#'
#' @return
#' An object with class `pmtable`; see [class-pmtable].
#'
#' @export
pt_cat_wide <- function(data, cols, by = ".total", panel = by,
                        table = NULL, all_name = "All data",
                        summarize = c("bottom", "none")) {

  summarize <- match.arg(summarize)
  summarize_all <- summarize != "none"

  has_by <- !missing(by)
  has_panel <- !missing(panel)
  panel_data <- as.panel(panel)
  panel <- panel_data$col

  cols <- new_names(cols, table)
  by <- new_names(by, table)
  panel <- new_names(panel,table)

  assert_that(length(by) == 1)

  data <- data_total_col(data, all_name)

  nby <- length(unique(data[[by]]))

  check_discrete(data = data, cols = cols, others = by)

  summarize_all <- summarize_all & (has_by | has_panel)

  data <- prep_cat_data(data, cols)

  ans <- cat_data(data, cols, by = by, panel = panel, wide = TRUE)
  ans <- mutate(ans, !!sym(by) := as.character(!!sym(by)))


  if(summarize_all) {

    all <- cat_data(data, cols, by = ".total", panel = ".total", wide = TRUE)

    all_name_fmt <- paste0("\\hline \\hline {\\bf ",all_name,"}")

    if(has_panel) {
      if(has_by) {
        all <- mutate(all, !!sym(panel) := ".panel.waiver.")

        all[[by]] <- all_name_fmt
      } else {
        all <- mutate(all, !!sym(panel) := all_name)
      }
    } else {
      if(has_by) {
        all <- mutate(all, !!sym(by) := all_name_fmt)
      }
    }
    ans <- bind_rows(ans, all)
  }

  ans <- ungroup(ans)

  ans[[".total"]] <- NULL

  if("N" %in% names(ans)) {
    ans <- rename(ans, n = .data[["N"]])
  }

  .panel <- rowpanel(NULL)
  if(has_panel) {
    .panel <- panel_data
    .panel$prefix_skip <- all_name
  }

  if(has_by) {
    names(ans)[names(ans)==by] <- names(by)[1]
  }

  out <- list(
    data = ans,
    span_split = colsplit(sep = '_._'),
    align = cols_center(.outer = 'l'),
    cols_rename = c(.panel$col,by),
    panel = .panel,
    notes = pt_cat_wide_notes()
  )

  out <- structure(out, class = c("pmtable", class(out)))

  return(out)
}


#' Return table notes for pt_cat_wide
#'
#' See [pt_cat_wide()].
#'
#' @param include_n include the note for `n` column
#' @param note_add additional notes to include
#'
#' @export
pt_cat_wide_notes <- function(include_n = TRUE, note_add = NULL) {
  ans <- note_add
  ans <- c(ans, "Summary is count (percent)")
  if(isTRUE(include_n)) {
    ans <- c(ans, "n: number of records summarized")
  }
  ans
}
