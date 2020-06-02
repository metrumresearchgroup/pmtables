
#' Set global plot options
#'
#' @param id_col ID column name
#' @param dv_col DV column name
#' @param bq_col BQL column name
#' @param units named list of units
#' @param table named list of column name transformations
#'
#' @details
#' `pt_opts` is the options environment.
#'
#' Global options can
#' be set in the environment with `pt_opts$set(name = value)`.  There is also
#' a `.list` argument to `pt_opts$set` that allows you to pass in a named list of
#' options to set.
#'
#' Values can be extracted with `pt_opts$get("name")`.
#'
#' Because it is an environment, the `$` operator can also be used to get and
#' set values (see examples).
#'
#' Other methods in the environment include `pt_opts$as.list()`, `pt_opts$mget()`,
#' `pt_opts$reset()`.  `pt_opts$self` refers to the environment itself. A list of
#' default settings can be obtained with `pt$defaults`.  Methods for the
#' `pt_opts` object include: [print.pt_opts], [as.list.pt_opts],
#' [`$<-.pt_opts`].
#'
#'
#'
#' @examples
#'
#' pt_opts$set(id_col = "USUBJID")
#'
#' pt_opts$id_col <- "ID"
#'
#' pt_opts$bq_col
#'
#' pt_opts$reset()
#'
#' x <- pt_opts$as.list()
#'
#' \dontrun{
#'  defs <- pt_opts$defaults
#'  defs$dv_col <- "dv"
#'  pt_opts$set(.list = defs)
#' }
#'
#' @md
#' @include class-digits.R
#' @name pt_opts
pt_options <- function(
  id_col = "ID",
  dv_col = "DV",
  bq_col = "BQL",
  units = NULL,
  table = NULL,
  fun.cont.long = pmtables:::df_sum_2,
  fun.cont.wide = pmtables:::str_sum_2,
  digits = NULL
  ) {
  set <- function(..., .list = NULL) {
    if(is.list(.list)) {
      x <- .list
    } else {
      x <- list(...)
    }
    if(length(x)==0) invisible(NULL)
    for(k in names(x)) assign(k,x[[k]],envir=self)
    return(invisible(NULL))
  }
  get <- function(x) {
    self[[x]]
  }
  reset <- function() {
    for(k in names(defaults)) assign(k,defaults[[k]],envir=self)
    return(invisible(NULL))
  }
  mget <- function(x) base::mget(x,envir=self)
  as.list <- function() {
    ans <- base::as.list.environment(self)
    ans$defaults <- NULL
    ans$self <- NULL
    ans$set <- NULL
    ans$get <- NULL
    ans$reset <- NULL
    ans$mget <- NULL
    ans$as.list <- NULL
    ans
  }
  self <- environment()
  defaults <- as.list()
  structure(self, class = c("pt_opts", "environment"))
}

#' Assign a pmtables option
#'
#' @param x the `pt_opts` object
#' @param i the name of the option to set
#' @param value the value to set
#'
#' @examples
#'
#' pt_opts$bq_col <- "BLQ"
#'
#' @export
`$<-.pt_opts` <- function(x,i,value) {
  if(!(i %in% names(x[["defaults"]]))) {
    warning(glue("'{i}' is not a valid option to set"),call.=FALSE)
    return(invisible(x))
  }
  assign(i,value,envir=x)
  return(invisible(x))
}

#' Extract multiple pmtables options
#'
#' @param x the `pt_opts` object
#' @param i a character option name to extract
#' @param ... other option names to extract
#' @param exact not used
#'
#' @examples
#' pt["id_col", "dv_col"]
#'
#' @export
`[.pt_opts` <- function(x,i,...,exact=TRUE) {
  i <- as.character(i)
  dots <- as.character(list(...))
  x$mget(c(i,dots))
}

#' Coerce pt_opts object to a list
#'
#' @param x the `pt_opts` object
#' @param ... not used
#'
#' @examples
#' x <- as.list(pt_opts)
#'
#' @export
as.list.pt_opts <- function(x,...) {
  x$as.list()
}

#' Print the pt_opts object
#'
#' @param x the `pt_opts` object
#' @param ... not used
#' @examples
#' pt_opts
#'
#' @export
print.pt_opts <- function(x,...) {
  cat(str(x))
}

#' @rdname pt_opts
#' @export
pt_opts <- pt_options()
#' @rdname pt_opts
#' @export
opts <- pt_opts

