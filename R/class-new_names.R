new_names <- function(x,...) UseMethod("new_names")

new_names.character <- function(x,table=NULL,...) {
  if(rlang::is_named(x)) return(x)
  ans <- cvec_cs(x)
  names(ans) <- ans
  new_names_update_table(ans,table)
}
new_names.quosures <- function(x,table=NULL,chr=FALSE,...) {
  x <- map(x,quo_get_expr)
  .names <- names(x)
  ans <- map_chr(x,as_string)
  .names[.names==""] <- ans[.names==""]
  names(ans) <- .names
  new_names_update_table(ans,table)
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
