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
    value = paste(n(), paste0("(",digit1(100*n()/N),")")),
    .groups = "drop"
  )
  pivot_wider(data_summ, names_from = "by")
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

  if(wide) {
    ans <- pivot_wider(
      ans,
      names_from = c("name", "level"),
      values_from = "summary",
      names_sep = '.'
    )
  } else {
    ans[["N"]] <- NULL
    ans <- pivot_wider(
      ans,
      names_from = by,
      values_from = "summary",
      names_sep = "."
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
#' @export
pt_cat_long <- function(data, cols, span  =  ".total",
                        all_name = "Total",
                        all_name_span = "By category",
                        summarize = c("right", "none"),
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
    if(summarize %in% c("bottom", "both")) {
      bot <- cat_long_all(data, unname(span))
      if(summarize=="both") {
        bot[[all_name_span]] <- paste(nrow(data), "(100.0)")
      }
      bot[["N"]] <- NULL
      bot[["level"]] <- all_name
      bot[["name"]] <- ""
      ans <- bind_rows(bot, ans)
    }
    if(summarize %in% c("right", "both")) {
      to_bold <- which(names(ans)==all_name_span)
      names(ans)[to_bold] <- split_bold(names(ans)[to_bold])
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
    notes = "Summary is count (percent)"
  )

  out <- structure(out, class = c("pmtable", class(out)))

  if(summarize %in% c("bottom", "both")) {
    out$sumrows <- sumrow(ans[["level"]] == all_name, label = all_name, bold = TRUE, hline = TRUE, it = FALSE)
    out$hline_at <- which(ans[["level"]] == all_name) + 1
  }

  return(out)
}

#' Discrete data summary in long format
#'
#' @inheritParams pt_cont_wide
#' @param by a grouping variable for the summary; may be given as character
#' vector or quosure
#' @param summarize where to put an all-data summary; choose `none` to omit the
#' summary from the table
#' @param drop passed to [stable()]
#'
#' @export
pt_cat_wide <- function(data, cols, by = ".total", panel = by,
                        table = NULL, all_name = "All data",
                        summarize = c("bottom", "none"),
                        drop = character(0)) {

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

  .panel <- rowpanel(NULL)
  if(has_panel) {
    .panel <- panel_data
    .panel$prefix_skip <- all_name
  }

  if(has_by) {
    names(ans)[names(ans)==by] <- names(by)[1]
  }

  notes <- "Summary is count (percent)"
  if(!is.element("N", drop)) {
    notes <- c(notes, "N: subject count for the row")
  }

  out <- list(
    data = ans,
    span_split = colsplit(sep = '.'),
    align = cols_center(.outer = 'l'),
    cols_rename = c(.panel$col,by),
    panel = .panel,
    notes = notes,
    drop = drop
  )

  out <- structure(out, class = c("pmtable", class(out)))

  return(out)
}
