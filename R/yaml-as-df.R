#' Read yaml input into data frame
#'
#' @param path to the yaml source file
#' @param quiet if `TRUE`, suppress messages
#'
#' @examples
#'
#' path <- system.file("table.yml", package = "pmtables")
#'
#' yaml_as_df(path)
#'
#' @export
yaml_as_df <- function(path, quiet = FALSE) { #nocov start
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
  rename <- is.character(prototype)
  if(rename) {
    x <- yamdf_validate_prototype(x, prototype)
    prototype_names <- names(x[[prototype]])
    x <- yamdf_rename(x)
  }
  assert_that(yaml_as_df_valid(x))
  ans <- map_dfr(x, as_tibble)
  if(rename) {
    names(ans) <- prototype_names
  }
  if(exists("cols", meta)) {
    cols <- meta[["cols"]]
    ans <- select(ans, cols, tidyselect::everything())
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
  a <- yaml_as_df_valid_outer(x)
  b <- map_lgl(x, ~ all(map_lgl(.x, yaml_as_df_valid_item)))
  all(a,b)
}

yaml_as_df_valid_outer <- function(x) {
  all(map_lgl(x, is.list))
}

yaml_as_df_valid_item <- function(x) {
  all(is.atomic(x),length(x)==1)
}
