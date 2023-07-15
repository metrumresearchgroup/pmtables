#' Read yaml input into data frame
#'
#' @param path to the yaml source file.
#' @param quiet logical; if `TRUE`, suppress messages.
#' @param row_var character; column name where row names (from the yaml
#' source) will be stored; pass `NULL` to discard this column of row names.
#'
#' @section Prototyped tables:
#'
#' A prototyped table has one row identified as the prototype and
#' defines the table column names as well as the required number of
#' columns. This is similar behavior to what `dplyr::tribble()` does.
#' Specify a prototype column name under `SETUP__:`.
#' You must provide names for all columns in the prototype.  Other rows
#' will inherit those names and you must enter a number of columns in
#' other rows equal to the number found in the prototype. If a prototype row
#' is used, then other rows do not need to be entered as (named) lists, but
#' can be entered as arrays; they will be coerced to list and named according
#' to the prototype.
#'
#' @return
#' A data frame is created from yaml content and returned. By default,
#' the first column in the data frame contains the outer names of the yaml
#' source (see `row_var` argument and examples).
#'
#' @examples
#'
#' path <- system.file("yaml", "table.yml", package = "pmtables")
#'
#' yaml_as_df(path)
#'
#' yaml_as_df(path, row_var = "name")
#'
#' yaml_as_df(path, row_var = NULL)
#'
#' # Example prototyped table
#' \dontrun{
#' file <- system.file("yaml", "prototype.yaml", package = "pmtables")
#' cat(readLines(file), sep = "\n")
#' }
#'
#' @export
yaml_as_df <- function(path, quiet = FALSE, row_var = ".row") { #nocov start
  assert_that(requireNamespace("yaml"))
  x <- yaml::yaml.load_file(path)
  meta <- list()
  prototype <- NULL
  if(exists("SETUP__", x)) {
    meta <- x[["SETUP__"]]
    x[["SETUP__"]] <- NULL
    prototype <- meta[["prototype"]]
    if(is.character(prototype) && !isTRUE(quiet)) {
      message("using column ", prototype, " as the prototype")
    }
  }
  spec_names <- names(x)
  rename <- is.character(prototype) | is.numeric(prototype)
  if(rename) {
    x <- yamdf_validate_prototype(x, prototype)
    prototype_names <- names(x[[prototype]])
    x <- yamdf_rename(x)
  }
  assert_that(yaml_as_df_valid(x), msg = "invalid yaml input")
  ans <- map_dfr(x, as_tibble)
  if(rename) {
    names(ans) <- prototype_names
  }

  if(exists("cols", meta)) {
    cols <- meta[["cols"]]
    ans <- select(ans, all_of(cols), everything())
  }

  if(is.character(row_var)) {
    row_var0 <- row_var
    i <- 1
    while(row_var %in% names(ans)) {
      row_var <- paste0(row_var0, "_", i)
      i <- i + 1
    }
    ans[, row_var] <- spec_names
    ans <- select(ans, all_of(unique(c(row_var, names(ans)))))
  }

  ans
} #nocov end

yamdf_validate_prototype <- function(x, prototype) {
  assert_that(
    prototype %in% names(x),
    msg = "prototype value out of protype out of range"
  )
  assert_that(
    is.list(x[[prototype]]),
    msg = "prototype column must be a list"
  )
  n_prototype <- length(x[[prototype]])
  x <- map(x, as.list)
  n_col <- map_int(x, length)
  if(any(n_col != n_prototype)) {
    w <- which(n_col != n_prototype)
    message("columns not matching prototype:")
    message(paste0("- ", names(x)[w]))
    stop("inconsistent data dimension", call. = FALSE)
  }
  x
}

yamdf_rename <- function(x) {
  map(x, function(y) {
    names(y) <- paste0("V", seq_along(y))
    y
  })
}

yaml_as_df_valid <- function(x) {
  ok <- TRUE

  found_list <- yaml_as_df_valid_outer(x)
  if(!all(found_list)) {
    w <- names(x)[!found_list]
    for(col in w) {
      message("row data in yaml is not a list: ", col)
      ok <- FALSE
    }
  }

  valid_items <- map_lgl(x, ~ all(map_lgl(.x, yaml_as_df_valid_item)))
  if(!all(valid_items)) {
    w <- names(x)[!valid_items]
    for(col in w) {
      message("row data in yaml contains invalid cell: ", col)
      ok <- FALSE
    }
  }
  ok
}

yaml_as_df_valid_outer <- function(x) {
  map_lgl(x, is.list)
}

yaml_as_df_valid_item <- function(x) {
  all(rlang::is_atomic(x), length(x)==1)
}
