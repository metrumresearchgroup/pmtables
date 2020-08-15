#' Read yaml input into data frame
#'
#' @param path to the yaml source file
#'
#' @examples
#'
#' path <- system.file("table.yml", package = "pmtables")
#'
#' yaml_as_df(path)
#'
#' @export
yaml_as_df <- function(path) { #nocov start
  assert_that(requireNamespace("yaml"))
  x <- yaml::yaml.load_file(path)
  meta <- list()
  if(exists("SETUP__", x)) {
    meta <- x[["SETUP__"]]
    x[["SETUP__"]] <- NULL
  }
  ans <- map_dfr(x,as_tibble)
  if(exists("cols", meta)) {
    cols <- meta[["cols"]]
    ans <- select(ans,cols, tidyselect::everything())
  }
  ans
} #nocov end


