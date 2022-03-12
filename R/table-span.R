
#' Set spans to group columns
#'
#' @param title label for groups of columns
#' @param vars tidyselect specification of variables
#' @param level relative position for the grouping spanner; level 0 is the
#' column names; level 1 is one step away (up) from the column names, etc
#' @param sep character on which to split column names
#' @param split logical; if `TRUE` column groupings will be determined by
#' splitting columns names on a separator
#' @param sep character; the separator used for finding column groupings
#' @param title_side which side of the split should be taken as the `title`?
#' defaults to left (`.l`) but can also take the right (`.r`) side of the split
#' @param align justify the span title to the center, left or right
#' @return an object with class `colgroup`
#' @export
colgroup <- function(title = NULL, vars = c(), level = 1, sep = ".",
                     split = FALSE, title_side = c(".l", ".r"),
                     align = c("c", "l", "r")) {

  if(isTRUE(split)) {
    ans <- colsplit(
      title = title,
      level = level,
      align = align,
      sep = sep,
      split = TRUE,
      title_side = title_side
    )
    return(ans)
  }

  assert_that(is.character(title))

  ans <- list(
    title = title,
    level = level,
    align = match.arg(align),
    vars = enquo(vars),
    split = split,
    sep = sep,
    gather = FALSE
  )
  structure(ans, class = "colgroup")
}

#' @rdname colgroup
#' @export
as.span <- colgroup

#' @rdname colgroup
#' @export
colsplit <- function(sep, level = 1, split = TRUE, title = NULL,
                     title_side = c(".l", ".r"), align = c("c", "l", "r")) {
  assert_that(is.null(title) || is.list(title))
  if(is.list(title)) {
    assert_that(is_named(title))
  }
  title_side <- match.arg(title_side)
  tagn <- ifelse(title_side == ".l", 1, 2)
  structure(
    list(
      title = title,
      level = level,
      split = split,
      align = match.arg(align),
      sep = sep,
      gather = FALSE,
      tagn = tagn
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
      title = x$title,
      align = x$align
    )
  ans <- mutate(ans, level = x$level)
  ans
}

fill_nospan <- function(span, cols) {
  nc <- length(cols)
  row_fill <- setdiff(seq(nc), span$coln)
  non_span <- tibble(
    coln = row_fill,
    col = cols[.data[["coln"]]],
    newcol = col,
    title = "",
    align = 'c' # placeholder only
  )
  ans <- bind_rows(span, non_span)
  ans <- arrange(ans, .data[["coln"]])
  ans <- mutate(
    ans,
    flg = chunk_runs(.data[["title"]]),
    level = span$level[1]
  )
  ans
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

find_span_split <- function(cols, xsp) {
  x <- str_split(cols, fixed(xsp$sep), n = 2)

  sp <- list(
    newcol = map_chr(x, last),
    tag = map_chr(x, first)
  )
  if(xsp$tagn ==2) {
    names(sp) <- rev(names(sp))
  }
  spans <- tibble(
    coln = seq_along(x),
    col = cols,
    newcol = sp$newcol,
    tag  = sp$tag,
    tagf = fct_inorder(.data[["tag"]])
  )

  if(xsp$gather) spans <- arrange(spans, .data[["tagf"]])
  spans <- mutate(
    spans,
    tag = ifelse(col == .data[["newcol"]], "", .data[["tag"]]),
    align = ifelse(.data[["tag"]] == "", "c", xsp$align[1])
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

  # title is a list to rename
  if(is.list(xsp$title)) {
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

#' Create groups of columns with spanners
#'
#' @inheritParams stable
#' @inheritParams tab_cols
#' @param span_split a `colsplit` object ; ; see also [st_span_split()]
#' @param cols a character vector of column names
#' @param span_title_break a character sequence indicating where to split the
#' title across multiple lines
#' @param ... not used
#' @seealso [colgroup()], [colsplit()], [st_span()], [st_span_split()]
#' @export
tab_spanners <- function(data, cols = NULL, span = NULL, span_split = NULL,
                         span_title_break = "...", sizes = tab_size(), ...) {

  assert_that(is.character(cols))

  if(is.null(span)) {
    span <- list()
  } else {
    if(is.colgroup(span)) span <- list(span)
    assert_that(is.list(span))
    assert_that(
      all(map_lgl(span, is.colgroup)),
      msg = "All objects passed under `span` must have class `colgroup`."
    )
    span <- map(span, process_colgroup, cols = cols)
  }

  if(!is.null(span_split)) {
    assert_that(is.colsplit(span_split))
  }

  spans_from_split <- NULL
  all_span_tex <- NULL
  all_spans <- NULL

  if(is.colsplit(span_split)) {
    spans <- find_span_split(cols, span_split)
    if(isTRUE(spans$any)) {
      data <- data[,spans$recol]
      cols <- spans$data$newcol
      spans_from_split <- spans$data
    }
  }

  if(length(span) > 0 || length(spans_from_split) > 0) {

    all_spans <- combine_spans(span, spans_from_split, cols = cols)

    all_span_tex <- map(
      rev(all_spans),
      make_span_header_tex,
      title_break = span_title_break,
      header_space = sizes$span_title_row
    )

    all_span_tex <- flatten_chr(unname(all_span_tex))
  }

  return(list(tex = all_span_tex, cols = cols, span = all_spans))
}

make_span_header_tex <- function(x, ...) {
  sp <- split(x, x$flg)
  c(
    span_header_box(sp, ... ),
    form_cline_tex(sp)
  )
}

span_header_box <- function(x, title_break = "...", header_space = -0.5) {

  titles <-  map_chr(x, ~.x$title[1])

  box <- header_matrix(
    cols = titles,
    newline = title_break
  )

  nr <- nrow(box)

  ncols <- map_int(x, ~ nrow(.x))
  align <- map_chr(x, ~ .x$align[1])

  for(i in seq_along(ncols)) {
    length <- ncols[[i]]
    box[[i]] <- gluet("\\multicolumn{<length>}{<align[[i]]>}{<box[[i]]>}")
  }

  box <- apply(box, MARGIN = 1, FUN = form_tex_cols)

  if(nr > 1) {
    assert_that(is.numeric(header_space))
    pb <- paste0(" [", header_space, "em]")
    w <- seq(nr-1)
    box[w] <- paste0(box[w], pb)
  }

  box
}

form_cline_tex <- function(spans) {
  spans <- keep(spans, ~ nchar(.x[["title"]][[1]]) > 0)
  clin <- map_chr(spans, function(ch) {
    start <- min(ch$coln)
    end <- max(ch$coln)
    gluet("\\cmidrule(lr){<start>-<end>}")
  })
  unname(clin)
}
