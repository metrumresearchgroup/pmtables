#' Create panel object
#'
#' @param col name of column to be used for creating panels
#' @param prefix to be added to each panel title
#' @param prefix_name `logical`; if `TRUE`, then the prefix will be derived by the name associated with `col` input
#' to the function
#' @param prefix_skip a regular expression for identifying panel titles where the prefix won't be applied
#'
#' @export
rowpanel <- function(col = NULL, prefix = "",  prefix_name = FALSE, prefix_skip = NULL) {
  null <- FALSE
  if(is.null(col)) {
    col <- NULL
    null <- TRUE
    col <- ""
  } else {
    col <- new_names(col)
  }
  ans <- list(
    col = col, prefix = prefix, prefix_name = isTRUE(prefix_name),
    prefix_skip = prefix_skip, null = null
  )
  structure(ans, class = "rowpanel")
}

#' @rdname rowpanel
#' @param x an object to test
#' @export
is.rowpanel <- function(x) inherits(x,"rowpanel")

panel_by <- function(data, x) { #col, prefix = NULL) {
  col <- x$col
  prefix <- x$prefix
  if(x$prefix_name) prefix <- names(x$col)[1]
  nc <- ncol(data)-1
  u <- as.character(unique(data[[col]]))
  u <- u[u!=""]
  u <- u[u!=".panel.waiver."]
  where <- match(u,data[[col]])
  lab <- data[[col]][where]
  prefix <- rep(prefix, length(lab))
  if(is.character(x$prefix_skip)) {
    skip <- grepl(x$prefix_skip,lab)
    if(any(skip)) {
      prefix[skip] <- rep("",sum(skip))
    }
  }
  lab <- paste(prefix,lab)
  lab <- bold_each(lab)
  insrt <- paste0("\\multicolumn{", nc,"}{l}{", lab,"}\\\\")
  insrt[2:length(insrt)] <- paste0("\\hline ", insrt[2:length(insrt)])
  list(where = where, to_insert = insrt)
}

