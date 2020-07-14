
#' Set spans to group columns
#'
#' @param title label for groups of columns
#' @param vars tidyselect specification of variables
#' @param level relative position for the grouping spanner; level 0 is the
#' column names; level 1 is one step away (up) from the column names, etc
#' @param set not implemented yet
#'
#' @return an object with class `colgroup`
#' @export
colgroup <- function(title, vars = c(), level = 1, sep = NULL) {
  ans <- list(title = title, level = level, vars = enquo(vars),sep = sep)
  structure(ans, class = "colgroup")
}

#' @rdname colgroup
#' @export
is.colgroup <- function(x) inherits(x,"colgroup")

process_colgroup <- function(x,cols) {
  nc <- length(cols)
  names(cols) <- cols
  ans <-
    tibble(
      coln = eval_select(x$vars, data = cols),
      col = names(cols)[coln],
      newcol = col,
      title = x$title
    )
  ans <- mutate(ans, level = x$level)
  ans
}

fill_nospan <- function(span,cols) {
  nc <- length(cols)
  row_fill <- setdiff(seq(nc), span$coln)
  tbl <- tibble(coln = row_fill, col = cols[coln], newcol=col, title = "")
  ans <- bind_rows(span,tbl) %>% arrange(coln)
  ans <- mutate(ans, flg = chunk_runs(title), align = 'c',level=span$level[1])
  ans
}

make_span_tex <- function(span) {
  c(form_span_tex(span),
    form_cline_tex(span))
}

combine_spans <- function(..., cols) {
  all_spans <- bind_rows(...)
  all_spans <- filter(all_spans, title != "")
  all_spans <- arrange(all_spans,level,coln)
  all_spans <- arrange(all_spans, level, coln, title)
  all_spans <- ungroup(distinct(group_by(all_spans,level), coln, .keep_all = TRUE))
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
    titlef = fct_inorder(title)
  )
  if(gather) spans <- arrange(spans, titlef)
  spans <- mutate(spans, title = ifelse(col == newcol, "", title), align='c')
  spans <- mutate(spans, flg = chunk_runs(title),titlef=NULL)
  spandf <- spans
  recol <- spandf$coln
  spandf <- mutate(spandf,coln = seq_len(n()),level=level)
  spans <- filter(spans, col != newcol)
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
  spans <- filter(spans, nchar(title) > 0)
  spans <- split(spans, spans$flg)
  clin <- vector("list", length(spans))

  clin <- map_chr(spans, function(ch) {
    start <- min(ch$coln)
    end <- max(ch$coln)
    gluet("\\cmidrule(lr){<start>-<end>}")
  })
  unname(clin)
}
