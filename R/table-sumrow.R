#' Identify and style summary rows
#'
#' @param rows integer or logical vector of rows that are summary rows
#' @param col integer or character (name) column location that labels the
#' summary row
#' @param hline logical; if `TRUE`, a horizontal line will be placed above the summary
#' row
#' @param bold logical; if `TRUE`, then the cell(s) identified by `rows` and
#' `col` will be rendered in bold font
#' @param it logical; if `TRUE`, then the cell(s) identified by `rows` and
#' `col` will be rendered in italic font
#' @param blank integer column positions in the summary row(s) to be made blank
#' @param label character label to replace text in cell(s) marked by `row(s)`
#' and `col
#' @param depanel if `TRUE`, then these rows are not included in panel
#' determination
#'
#' @export
sumrow <- function(rows,
                   col = 1,
                   hline = TRUE,
                   bold = FALSE,
                   it = FALSE,
                   blank = NULL,
                   label = NULL,
                   depanel = TRUE
                   ) {
  if(is.logical(rows)) rows <- which(rows)
  assert_that(is.numeric(rows))
  rows <- rows[rows >= 1]
  if(length(rows) ==0) {
    stop("no rows were selected in sumrow call", call.=FALSE)
  }
  if(!is.null(label)) {
    label <- label[1]
  }
  ans <- list(
    rows = as.integer(rows),
    col = col[1],
    hline = as.logical(hline),
    bold = as.logical(bold),
    it = as.logical(it),
    blank = as.integer(blank),
    label = label,
    nrows = length(rows),
    depanel = isTRUE(depanel)
  )
  structure(ans, class = "sumrow")
}

sumrow_get_hline <- function(x) {
  ans <- NULL
  if(isTRUE(x$hline)) ans <- x$rows
  ans
}

sumrow_depanel_rows <- function(x) {
  if(x$depanel) return(x$rows)
  return(integer(0))
}

sumrow_add_style <- function(x,data) {
  if(is.null(data[[x$col]])) {
    stop("sumrow column '", x$col, "' not in 'data'",call.=FALSE)
  }
  for(r in x$rows) {
    data[[x$col]] <- as.character(data[[x$col]])
    if(!is_empty(x$blank)) {
      data[r,x$blank] <- blank_each(data[r,x$blank])
    }
    if(!is_empty(x$label)) {
      data[r,x$col] <- x$label
    }
    if(isTRUE(x$bold)) {
      data[r,x$col] <- bold_each(data[r,x$col])
    }
    if(isTRUE(x$it)) {
      data[r,x$col] <- it_each(data[r,x$col])
    }
  }
  data
}


