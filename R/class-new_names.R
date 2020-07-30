new_names <- function(x,...) UseMethod("new_names")

#' @export
new_names.character <- function(x,table=NULL,...) {
  if(rlang::is_named(x)) return(x)
  ans <- cvec_cs(x)
  if(any(duplicated(ans))) {
    dup <- ans[duplicated(ans)]
    for(d in dup) {
      message(" duplicate value: ", d)
    }
    stop("duplicated values",call.=FALSE)
  }
  names(ans) <- ans
  new_names_update_table(ans,table)
}

new_names.list <- function(x,...) {
  assert_that(is_named(x))
  if(any(duplicated(names(x)))) {
    stop("duplicated names", call.=FALSE)
  }
  labels <- names(x)
  ans <- map_chr(x,~ as.character(.x[1]))
  names(ans) <- labels
  new_names.character(ans)
}

#' @export
new_names.quosures <- function(x,table=NULL,chr=FALSE,...) {
  x <- map(x,quo_get_expr)
  .names <- names(x)
  ans <- map_chr(x,as_string)
  if(any(duplicated(ans))) {
    dup <- ans[duplicated(ans)]
    for(d in dup) {
      message(" duplicate value: ", d)
    }
    stop("duplicated values",call.=FALSE)
  }
  .names[.names==""] <- ans[.names==""]
  names(ans) <- .names
  new_names_update_table(ans,table)
}

#' @export
new_names.rowpanel <- function(x,...) {
  out <- x$col
  names(out) <- x$col
  out
}

new_names_update_table <- function(x,table=NULL,...) {
  if(is.null(table)) return(x)
  to_up <- intersect(x,names(table))
  to_up <- setdiff(to_up,setdiff(x,names(x)))
  if(length(to_up)==0) return(x)
  new_name <- unlist(table[to_up],use.names=FALSE)
  names(x)[which(x %in% to_up)] <- new_name
  x
}

panel_to_prefix <- function(x) {
  x <- x[1]
  if(identical(x,names(x)) || is.null(names(x))) return("")
  return(names(x))
}
