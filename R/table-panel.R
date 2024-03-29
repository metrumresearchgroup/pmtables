#' Create panel object
#'
#' Objects may also be coerced with [as.panel()]
#'
#' @param col name of column to be used for creating panels
#' @param prefix to be added to each panel title
#' @param skip regular expression for panels to skip
#' @param prefix_name `logical`; if `TRUE`, then the prefix will be derived by
#' the name associated with `col` input to the function
#' @param prefix_skip a regular expression for identifying panel titles where
#' the prefix won't be applied
#' @param duplicates_ok if `FALSE`, an error is generated if more than one
#' panel will have the same header
#' @param bold render panel title in bold font face
#' @param it render panel title in italic font face
#' @param hline logical indicating whether or not to draw an `hline` above
#' the panel row; the first panel row never receives an `hline`
#' @param jut amount (in TeX `ex` units) by which the panel headers are
#' outdented from the first column in the main table and header. Consider
#' using a value of `2` for a clear offset.
#' @param nopagebreak if `TRUE` (the default) then the page will not break
#' immediately after a panel title _when creating a longtable_; this argument
#' does nothing if you are not generating a `longtable`. Set to `FALSE` to
#' prevent the `nopagebreak` command (implemented as `*`) in the longtable
#' output.
#'
#' @seealso [as.panel()]
#'
#' @export
rowpanel <- function(col = NULL, prefix = "", skip = ".panel.skip.",
                     prefix_name = FALSE,
                     prefix_skip = NULL, duplicates_ok = FALSE,
                     bold = TRUE, it = FALSE, hline = TRUE,
                     jut = 0, nopagebreak = TRUE) {
  null <- FALSE
  if(is.null(col)) {
    col <- NULL
    null <- TRUE
    col <- ""
  } else {
    col <- new_names(col)
  }
  assert_that(is.numeric(jut))
  assert_that(is.character(skip))
  ans <- list(
    col = col, prefix = prefix, prefix_name = isTRUE(prefix_name),
    prefix_skip = prefix_skip, null = null, dup_err = !isTRUE(duplicates_ok),
    bold = isTRUE(bold), it = isTRUE(it), skip = skip, hline = isTRUE(hline),
    jut = jut, nopagebreak = isTRUE(nopagebreak)
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
as.panel.character <- function(x, ...) {
  assert_that(length(x)==1)
  col <- new_names(x)
  ans <- rowpanel(unname(col), ...)
  if(col != names(col)) {
    ans$prefix <- names(col)
  }
  ans
}

#' @rdname as.panel
#' @export
as.panel.quosures <- function(x, ...) {
  as.panel.character(new_names(x),...)
}

#' @rdname as.panel
#' @export
as.panel.NULL <- function(x, ...) {
  rowpanel(NULL)
}

panel_by <- function(data, x) {
  if(x$null) {
    return(list(insert_row = NULL, insert_data = NULL, insert = FALSE))
  }
  col <- x$col
  prefix <- x$prefix
  if(x$prefix_name) prefix <- names(x$col)[1]
  nc <- ncol(data)-1
  xpanel <- replace_na(data[[col]],"")
  u <- non_rep(xpanel)
  ui <- which(u)
  uc <- xpanel[ui]
  drop <- uc == ""
  drop <- drop | uc == ".panel.waiver."
  if(!is.null(x$skip)) {
    drop <- drop | str_detect(uc,as_str_regex(x$skip))
  }
  ui <- ui[!drop]
  uc <- uc[!drop]
  where <- ui
  lab <- xpanel[where]
  if(x$dup_err && any(duplicated(lab))) {
    stop(
      "panel labels are duplicated; ",
      "please sort the data frame by the panel column ",
      "or set duplicates_ok to TRUE",
      call. = FALSE
    )
  }
  prefix <- rep(prefix, length(lab))
  if(!is.null(x$prefix_skip)) {
    skip <- str_detect(lab,as_str_regex(x$prefix_skip))
    if(any(skip)) {
      prefix[skip] <- rep("",sum(skip))
    }
  }
  lab <- trimws(paste(prefix,lab))
  lab <- tab_escape(lab)
  if(x$bold) {
    lab <- bold_each(lab)
  }
  if(x$it) {
    lab <- it_each(lab)
  }
  insrt <- paste0("\\multicolumn{", nc,"}{l}{", lab,"}\\\\")
  if(length(insrt) > 1 && x$hline) {
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
  require_col(data,panel$col, context = "panel column input name")
  assert_that(
    ncol(data) > 1,
    msg = "must have more than one column to use 'panel' option"
  )
  paneln <- match(panel$col, names(data))
  if(any(is.na(paneln))) {
    stop("panel column not found: ", squote(panel$col), call.=FALSE)
  }
  data[[paneln]] <- as.character(data[[paneln]])
  data[[paneln]] <- replace_na(data[[paneln]],"")
  # check summary rows
  if(!is.null(sumrows)) {
    dep <- map(sumrows, sumrow_depanel_rows)
    dep <- flatten_int(dep)
    dep <- sort(unique(dep))
    data[[paneln]][dep] <- rep(NA_character_, length(dep))
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
    nw = tab_panel_mark(panel_insert$insert_data)
  )
}

tab_panel_marker <- function() {
  .internal$marker.panel
}

tab_panel_mark <- function(x) {
  paste0(x, tab_panel_marker())
}

tab_panel_star <- function(x) {
  marker <- tab_panel_marker()
  where <- grepl(marker, x, fixed = TRUE)
  if(!any(where)) return(x)
  x[where] <- sub(marker, paste0("*", marker), x[where], fixed = TRUE)
  x
}
