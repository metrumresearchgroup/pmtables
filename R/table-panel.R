#' Create panel object
#'
#' @param col name of column to be used for creating panels
#' @param prefix to be added to each panel title
#' @param prefix_name `logical`; if `TRUE`, then the prefix will be derived by
#' the name associated with `col` input to the function
#' @param prefix_skip a regular expression for identifying panel titles where
#' the prefix won't be applied
#' @param duplicates_ok if `FALSE`, an error is generated if more than one
#' panel will have the same header
#' @export
rowpanel <- function(col = NULL, prefix = "",  prefix_name = FALSE,
                     prefix_skip = NULL, duplicates_ok = FALSE) {
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
    prefix_skip = prefix_skip, null = null, dup_err = !isTRUE(duplicates_ok)
  )
  structure(ans, class = "rowpanel")
}

#' @rdname rowpanel
#' @param x an object to test
#' @export
is.rowpanel <- function(x) inherits(x,"rowpanel")

panel_by <- function(data, x) {
  col <- x$col
  prefix <- x$prefix
  if(x$prefix_name) prefix <- names(x$col)[1]
  nc <- ncol(data)-1
  u <- non_rep(data[[col]])
  ui <- which(u)
  uc <- data[[col]][ui]
  ui <- ui[uc!=""]
  ui <- ui[uc!=".panel.waiver."]
  where <- ui
  lab <- data[[col]][where]
  if(x$dup_err && any(duplicated(lab))) {
    stop(
      "panel labels are duplicated; ",
      "please sort the data frame by the panel column ",
      "or set dup_is_error to FALSE",
      call.=FALSE
    )
  }
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
  if(length(insrt) > 1) {
    insrt[seq(2,length(insrt))] <- paste0("\\hline ", insrt[seq(2,length(insrt))])
  }
  list(where = where, to_insert = insrt)
}

