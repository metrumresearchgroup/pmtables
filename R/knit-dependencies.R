
#' Invoke latex dependencies
#'
#' Call this function when knitting a document that includes pmtables output.
#'
#' @param packages a character vector of latex package names; will get passed
#' to [rmarkdown::latex_dependency()] and then to [knitr::knit_meta_add()].
#' The default is to install packates returned by [st_packages()].
#'
#' @return
#' Returns invisible `NULL`.
#'
#'
#' @export
st_usepackage <- function(packages = NULL) {
  assert_that(requireNamespace("knitr"))
  if(knitr::is_latex_output()) {
    assert_that(requireNamespace("rmarkdown"))
    if(is.null(packages)) packages <- st_packages()
    for(pkg in packages) {
      knitr::knit_meta_add(
        list(rmarkdown::latex_dependency(pkg))
      )
    }
    .internal$using_packages <- TRUE
  }
  return(invisible(NULL))
}

#' @rdname st_usepackage
#' @export
st_packages <- function() {
  c(
    "float",
    "booktabs",
    "longtable",
    "threeparttable",
    "pdflscape",
    "array",
    "caption"
  )
}

#' @rdname st_usepackage
#' @export
st_using_deps <- function() isTRUE(.internal$using_packages)
