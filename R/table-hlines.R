
#' Insert hlines into table
#'
#' @param data the table data frame
#' @param hline_at logical or integer vector specifying rows above which an
#' `\hline` will be placed; see also [st_hline()]
#' @param hline_from a character column name from which to separate the table
#' with `\hline`; non-duplicated values of `hline_from` will be used to create
#' the split; see also [st_hline()]
#' @param ... not used
#'
#' @export
tab_hlines <- function(data, hline_at = NULL, hline_from = NULL, ...) {
  add_hlines <- NULL
  if(!is.null(hline_at)) {
    if(is.logical(hline_at)) {
      hline_at <- which(hline_at)
    }
    add_hlines <- c(add_hlines,hline_at-1)
  }
  if(!is.null(hline_from)) {
    assert_that(is.character(hline_from))
    for(this_col in hline_from) {
      require_col(data,this_col)
      hline_row <- !duplicated(chunk_runs(data[[this_col]]))
      hline_row[1] <- FALSE
      add_hlines <- c(add_hlines, which(hline_row)-1)
    }
  }
  add_hlines
}

tab_add_hlines <- function(tab, hlines, sumrows = NULL) {
  if(is.null(hlines) || length(hlines)==0) {
    return(tab)
  }
  hlines <- sort(hlines)
  for(i in hlines) {
    tab[i] <- paste0(tab[i], " \\hline")
  }
  if(is.list(sumrows)) {
    hlinex <- map(sumrows, sumrow_get_hlinex2)
    above <- sort(unique(flatten_int(hlinex)-1))
    below <- above + 1
    tab[above] <- paste0(tab[above], " \\hline")
    tab[below] <- paste0(tab[below], " \\hline")
  }
  tab
}

find_hline_col <- function(x,re) {
  which(str_detect(x,re))
}

find_hline_df <- function(data, re,  cols = NULL) {
  if(is.null(cols)) cols <- names(data)
  cols <- cols[cols %in% names(data)]
  if(length(cols)==0) return(NULL)
  rows <- map(data[,cols], find_hline_col, re = re)
  rows <- flatten_int(rows)
  if(length(rows)==0) return(NULL)
  return(rows)
}
