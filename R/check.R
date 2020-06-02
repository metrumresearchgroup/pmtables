

columns_exist <- function(data,cols,msg) {
  if(!is.data.frame(data)) return(msg)
  for(col in cols) {
    a <- validate_that(
      exists(col,data),
      msg = glue("column '{col}' does not exist in 'data'")
    )
    if(!isTRUE(a)) msg <- c(msg,a)
  }
  return(msg)
}

check_exists <- function(data,cols) {
  msg <- c()
  msg <- columns_exist(data,cols,msg)
  if(length(msg)==0) return(invisible(TRUE))
  if(length(msg) > 0) {
    walk(msg,emessage)
  }
  stop("there were problems with input data",call.=FALSE)
}


check_continuous <- function(data,cols,others = NULL) {
  msg <- c()
  a <- validate_that(is.data.frame(data), msg = "'data' must be a data frame")
  if(!isTRUE(a)) msg <- c(msg,a)

  msg <- columns_exist(data,c(cols,others),msg)

  real_cols <- intersect(c(cols,others),names(data))
  for(col in real_cols) {
    a <- validate_that(
      is.numeric(data[[col]]),
      msg = glue("column '{col}' is not numeric")
    )
    if(!isTRUE(a)) msg <- c(msg,a)
  }

  if(length(msg)==0) return(invisible(TRUE))

  if(length(msg) > 0) {
    walk(msg,emessage)
  }
  stop("there were problems with input data",call.=FALSE)
}

check_discrete <- function(data,cols=NULL,others = NULL) {
  msg <- c()
  a <- validate_that(is.data.frame(data), msg = "'data' must be a data frame")
  if(!isTRUE(a)) msg <- c(msg,a)

  msg <- columns_exist(data,c(cols,others),msg)

  real_cols <- intersect(c(cols,others),names(data))
  for(col in real_cols) {
    result <-
      is.factor(data[[col]]) |
      is.character(data[[col]]) |
      is.integer(data[[col]])
    a <- validate_that(
      result,
      msg = glue("column '{col}' is not factor, character, or integer")
    )
    if(!isTRUE(a)) msg <- c(msg,a)
  }

  if(length(msg)==0) return(invisible(TRUE))

  if(length(msg) > 0) {
    walk(msg,emessage)
  }

  stop("there were problems with input data",call.=FALSE)
}
