
#' Set spans to group columns
#'
#' @param title label for groups of columns
#' @param vars tidyselect specification of variables
#' @param level relative position for the grouping spanner; level 0 is the
#' column names; level 1 is one step away (up) from the column names, etc
#' @param sep character on which to split column names
#'
#' @return an object with class `colgroup`
#' @export
colgroup <- function(title = NULL, vars = c(), level = 1, sep = ".", split = FALSE) {

  if(isTRUE(split)) {
    return(colsplit(title = title, level = level, sep = sep, split = TRUE))
  }

  assert_that(is.character(title))

  ans <- list(
    title = title,
    level = level,
    vars = enquo(vars),
    split = split,
    sep = sep,
    gather = FALSE
  )
  structure(ans, class = "colgroup")
}

#' @rdname colgroup
#' @param split logical; if `TRUE` column groupings will be determined by
#' splitting columns names on a separator
#' @param sep character; the separator used for finding column groupings
#' @export
colsplit <- function(sep, level = 1, split = TRUE, title = NULL) {
  assert_that(is.null(title) || is.list(title))
  if(!is.null(title)) assert_that(is_named(title))
  structure(
    list(
      title = title,
      level = level,
      split = split,
      sep = sep,
      gather = FALSE
    ),
    class = "colsplit"
  )
}


#' @rdname colgroup
#' @param x an R object
#' @export
is.colgroup <- function(x) inherits(x,"colgroup")
#' @rdname colgroup
#' @export
is.colsplit <- function(x) inherits(x,"colsplit")

process_colgroup <- function(x,cols) {
  nc <- length(cols)
  names(cols) <- cols
  ans <-
    tibble(
      coln = eval_select(x$vars, data = cols),
      col = names(cols)[.data[["coln"]]],
      newcol = col,
      title = x$title
    )
  ans <- mutate(ans, level = x$level)
  ans
}

fill_nospan <- function(span,cols) {
  nc <- length(cols)
  row_fill <- setdiff(seq(nc), span$coln)
  tbl <- tibble(
    coln = row_fill,
    col = cols[.data[["coln"]]],
    newcol = col,
    title = ""
  )
  ans <- bind_rows(span,tbl)
  ans <- arrange(ans,.data[["coln"]])
  ans <- mutate(
    ans,
    flg = chunk_runs(.data[["title"]]),
    align = 'c',
    level=span$level[1]
  )
  ans
}

make_span_tex <- function(span) {
  c(form_span_tex(span),
    form_cline_tex(span))
}

combine_spans <- function(..., cols) {
  all_spans <- bind_rows(...)
  all_spans <- filter(all_spans, .data[["title"]] != "")
  all_spans <- arrange(all_spans,  .data[["level"]], .data[["coln"]], .data[["title"]])
  all_spans <- group_by(all_spans, .data[["level"]])
  all_spans <- distinct(all_spans, .data[["coln"]], .keep_all = TRUE)
  all_spans <- ungroup(all_spans)
  all <- split(all_spans, all_spans$level)
  all <- map(all, fill_nospan, cols = cols)
  all
}

find_span_split <- function(cols,xsp) {
  x <- str_split(cols, fixed(xsp$sep), n = 2)
  spans <- tibble(
    coln = seq_along(x),
    col = cols,
    newcol = map_chr(x,last),
    tag  = map_chr(x,1),
    tagf = fct_inorder(.data[["tag"]])
  )
  if(xsp$gather) spans <- arrange(spans, .data[["tagf"]])
  spans <- mutate(
    spans,
    tag = ifelse(col == .data[["newcol"]], "", .data[["tag"]]),
    align = 'c'
  )
  spans <- mutate(spans, flg = chunk_runs(.data[["tag"]]))
  spans[["tagf"]] <- NULL
  spandf <- spans
  recol <- spandf$coln
  spandf <- mutate(spandf, coln = seq_len(n()), level = xsp$level)
  spans <- filter(spans, .data[["col"]] != .data[["newcol"]])
  spans <- unname(split(spans, spans$flg))
  spans <- map(spans, ~ c(.x$tag[1],.x$col[1],.x$col[nrow(.x)]))
  resort <- !identical(spandf$coln,seq_len(nrow(spandf)))

  if(!is.null(xsp$title)) {
    assert_that(rlang::is_named(xsp$title))
    title <- spandf$tag
    if(!all(names(xsp$title) %in% title)) {
      diff <- setdiff(names(xsp$title),title)
      for(bad in diff) {
        warning("[pmtables] split tag not found in any columns: ", bad, call.=FALSE)
      }
    }
    to_rename <- intersect(names(xsp$title),title)
    indx_rename <- match(title,to_rename)
    indx_title <- which(!is.na(indx_rename))
    indx_rename <- na.omit(indx_rename)
    title[indx_title] <- xsp$title[indx_rename]
    spandf[["title"]] <- flatten_chr(title)
  } else {
    spandf[["title"]] <- spandf[["tag"]]
  }

  spandf[["tag"]] <- NULL

  list(
    data = spandf,
    spans = spans,
    resort = resort,
    any = length(spans) > 0,
    recol = recol
  )
}

form_span_tex <- function(spans) {
  chunks <- split(spans, spans$flg)
  out <- vector("list", length(chunks))
  span_tex <- map_chr(chunks, function(ch) {
    length <- nrow(ch)
    title <- ch$title[1]
    title <- bold_each(title)
    ans <- gluet("\\multicolumn{<length>}{<ch$align[1]>}{<title>}")
    ans
  })
  span_tex <- paste0(span_tex,collapse = " & ")
  paste0(span_tex, "\\\\")
}

form_cline_tex <- function(spans) {
  spans <- filter(spans, nchar(.data[["title"]]) > 0)
  spans <- split(spans, spans$flg)
  clin <- vector("list", length(spans))

  clin <- map_chr(spans, function(ch) {
    start <- min(ch$coln)
    end <- max(ch$coln)
    gluet("\\cmidrule(lr){<start>-<end>}")
  })
  unname(clin)
}

#' Create groups of columns with spanners
#'
#' @inheritParams stable
#' @param span_split not implemented at this time; ; see also [st_span_split()]
#' @param cols a character vector of column names
#' @param ... not used
#' @export
tab_spanners <- function(data, cols = NULL, span = NULL, span_split = NULL, ...) {

  assert_that(is.character(cols))

  do_span_split <- is.colsplit(span_split)

  spans_from_split <- NULL

  if(do_span_split) {
    spans <- find_span_split(cols,span_split)
    if(isTRUE(spans$any)) {
      data <- data[,spans$recol]
      cols <- spans$data$newcol
      spans_from_split <- spans$data
    }
  }

  if(is.null(span)) {
    span <- list()
  } else {
    if(is.colgroup(span)) span <- list(span)
    assert_that(is.list(span))
    span <- map(span, process_colgroup, cols = cols)
  }

  all_span_tex <- NULL

  if(length(span) > 0 || length(spans_from_split) > 0) {
    all_spans <- combine_spans(span, spans_from_split, cols = cols)

    all_span_tex <- map(rev(all_spans), make_span_tex)

    all_span_tex <- flatten_chr(unname(all_span_tex))
  }

  all_span_tex
}

