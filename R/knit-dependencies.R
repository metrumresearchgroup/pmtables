
#' Invoke latex dependencies
#'
#' Call this function when knitting a document that includes pmtables output.
#'
#' @param packages a character vector of latex package names; will get passed
#' to [rmarkdown::latex_dependency()] and then to [knitr::knit_meta_add()];
#' default is to install packages returned by [st_knit_deps()]
#' @param force if `TRUE`, then add latex dependencies regardless of
#' knit status; this is for internal testing use only
#'
#' @return
#' Returns invisible `NULL`.
#'
#' @details
#' The following packages will be invoked __when knitting a document__:
#'
#' - `float`
#' - `booktabs`
#' - `longtable`
#' - `threeparttable`
#' - `pdflscape`
#' - `array`
#' - `caption`
#'
#' @export
st_use_knit_deps <- function(packages = NULL, force = FALSE) {
  assert_that(requireNamespace("knitr", quietly=TRUE))
  if((knitr::is_latex_output() || isTRUE(force)) && !st_using_knit_deps()) {
    assert_that(requireNamespace("rmarkdown", quietly = TRUE))
    if(is.null(packages)) packages <- st_knit_deps()
    for(pkg in packages) {
      knitr::knit_meta_add(
        list(rmarkdown::latex_dependency(pkg))
      )
    }
    .internal$using_knit_deps <- TRUE
  }
  return(invisible(NULL))
}

#' @param addl additional packages to use
#' @rdname st_use_knit_deps
#' @export
st_knit_deps <- function(addl = NULL) {
  c(
    "float",
    "booktabs",
    "longtable",
    "threeparttable",
    "pdflscape",
    "array",
    "caption",
    addl
  )
}

#' @rdname st_use_knit_deps
#' @export
st_using_knit_deps <- function() isTRUE(.internal$using_knit_deps)

st_reset_knit_deps <- function() .internal$using_knit_deps <- FALSE
