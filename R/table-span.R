
#' Set spans to group columns
#'
#' @param title label for groups of columns
#' @param vars tidyselect specification of variables
#' @param level relative position for the grouping spanner; level 0 is the
#' column names; level 1 is one step away (up) from the column names, etc
#' @param sep not implemented yet
#'
#' @return an object with class `colgroup`
#' @export
colgroup <- function(title, vars = c(), level = 1, sep = NULL) {
  ans <- list(title = title, level = level, vars = enquo(vars),sep = sep)
  structure(ans, class = "colgroup")
}

#' @rdname colgroup
#' @param x an R object
#' @export
is.colgroup <- function(x) inherits(x,"colgroup")

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
  #all_spans <- arrange(all_spans, .data[["level"]], .data[["coln"]])
  all_spans <- arrange(all_spans,  .data[["level"]], .data[["coln"]], .data[["title"]])
  all_spans <- group_by(all_spans, .data[["level"]])
  all_spans <- distinct(all_spans, .data[["coln"]], .keep_all = TRUE)
  all_spans <- ungroup(all_spans)
  all <- split(all_spans, all_spans$level)
  all <- map(all, fill_nospan, cols = cols)
  all
}


find_span_split <- function(cols,sep = ".",gather = FALSE,level = 1) {
  x <- str_split(cols, fixed(sep), n = 2)
  spans <- tibble(
    coln = seq_along(x),
    col = cols,
    newcol = map_chr(x,last),
    title  = map_chr(x,1),
    titlef = fct_inorder(.data[["title"]])
  )
  if(gather) spans <- arrange(spans, .data[["titlef"]])
  spans <- mutate(
    spans,
    title = ifelse(col == .data[["newcol"]], "", .data[["title"]]),
    align='c'
  )
  spans <- mutate(spans, flg = chunk_runs(.data[["title"]]))
  spans[["titlef"]] <- NULL
  spandf <- spans
  recol <- spandf$coln
  spandf <- mutate(spandf,coln = seq_len(n()),level=level)
  spans <- filter(spans, .data[["col"]] != .data[["newcol"]])
  spans <- unname(split(spans, spans$flg))
  spans <- map(spans, ~ c(.x$title[1],.x$col[1],.x$col[nrow(.x)]))
  resort <- !identical(spandf$coln,seq_len(nrow(spandf)))
  list(
    data = spandf,
    spans = spans,
    resort = resort,
    any = length(spans)>0,
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
