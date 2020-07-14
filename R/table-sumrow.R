
#' @export
sumrow <- function(rows,
                   col = 1,
                   hline = TRUE,
                   bold = FALSE,
                   blank = NULL,
                   label = NULL) {
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
    blank = as.integer(blank),
    label = label,
    nrows = length(rows)
  )
  structure(ans, class = "sumrow")
}

sumrow_get_hline <- function(x) {
  ans <- NULL
  if(isTRUE(x$hline)) ans <- x$rows
  ans
}

sumrow_add_style <- function(x,data) {
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
  }
  data
}


