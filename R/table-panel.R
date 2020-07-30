#' Create panel object
#'
#' Objects may also be coerced with [as.panel()]
#'
#' @param col name of column to be used for creating panels
#' @param prefix to be added to each panel title
#' @param prefix_name `logical`; if `TRUE`, then the prefix will be derived by
#' the name associated with `col` input to the function
#' @param prefix_skip a regular expression for identifying panel titles where
#' the prefix won't be applied
#' @param duplicates_ok if `FALSE`, an error is generated if more than one
#' panel will have the same header
#'
#' @seealso [as.panel()]
#'
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
  prefix <- ifelse(is.null(prefix), "", prefix)
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

#' Coerce object to rowpanel
#'
#' @param x object to coerce
#' @param ... arguments passed to [rowpanel()]
#'
#' @examples
#' as.panel("STUDY", prefix = "Study: ")
#'
#' @export
as.panel <- function(x, ...) UseMethod("as.panel")

#' @rdname as.panel
#' @export
as.panel.rowpanel <- function(x, ...) x

#' @rdname as.panel
#' @export
as.panel.default <- function(x, ...) {
  assert_that(length(x)==1)
  col <- new_names(x)
  ans <- rowpanel(unname(col), ...)
  if(col != names(col)) {
    ans$prefix <- names(col)
  }
  ans
}

panel_by <- function(data, x) {
  if(x$null) {
    return(list(insert_row = NULL, insert_data = NULL, insert = FALSE))
  }
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
  list(insert_row = where, insert_data = insrt, insert = TRUE)
}

# Calculates the panel and modifies the data set
# returns list that you have to grab data out of
tab_panel <- function(data, panel, sumrows) {
  if(panel$null) {
    ins <- panel_by(data,panel)
    ins$data <- data
    return(ins)
  }
  require_col(data,panel$col,context = "panel column input name")
  paneln <- match(panel$col,names(data))
  if(any(is.na(paneln))) {
    stop("panel column not found: ", squote(panel$col), call.=FALSE)
  }
  data[[paneln]] <- replace_na(data[[paneln]],"")
  # check summary rows
  if(!is.null(sumrows)) {
    dep <- map(sumrows, sumrow_depanel_rows)
    dep <- flatten_int(dep)
    dep <- sort(unique(dep))
    data[[paneln]][dep] <- rep(".panel.waiver.", length(dep))
  }
  ins <- panel_by(data, panel)
  data[[panel$col]] <- NULL
  ins$data <- data
  ins
}

# Execute the insertion of panel rows
tab_panel_insert <- function(tab, panel_insert) {
  if(!isTRUE(panel_insert$insert)) return(tab)
  insrt_vec(
    vec = tab,
    where = panel_insert$insert_row,
    nw = panel_insert$insert_data
  )
}




