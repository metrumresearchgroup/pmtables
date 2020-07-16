#' Create panel object
#'
#' @export
rowpanel <- function(col = NULL, prefix_name = FALSE, prefix = "") {
  null <- FALSE
  if(is.null(col)) {
    col <- NULL
    null <- TRUE
    col <- ""
  } else {
    col <- new_names(col)
  }
  ans <- list(col = col, prefix = prefix, prefix_name = isTRUE(prefix_name), null = null)
  structure(ans, class = "rowpanel")
}

#' @export
is.rowpanel <- function(x) inherits(x,"rowpanel")

#' @export
is.null.rowpanel <- function(x) x$null

panel_by <- function(data, col, prefix = NULL) {
  nc <- ncol(data)-1
  u <- unique(data[[col]])
  where <- match(u,data[[col]])
  lab <- data[[col]][where]
  lab <- paste(prefix,lab)
  lab <- bold_each(lab)
  insrt <- paste0("\\multicolumn{", nc,"}{l}{", lab,"}\\\\")
  insrt[2:length(insrt)] <- paste0("\\hline ", insrt[2:length(insrt)])
  list(where = where, to_insert = insrt)
}

