#' @importFrom assertthat validate_that assert_that
#' @importFrom tidyselect all_of eval_select eval_rename
#' @importFrom dplyr mutate bind_rows select ungroup summarise left_join
#' @importFrom dplyr group_modify rename count vars group_by n first last
#' @importFrom dplyr case_when filter arrange group_vars distinct bind_cols
#' @importFrom dplyr groups
#' @importFrom purrr map_dfr walk partial map map_chr modify flatten_chr
#' @importFrom purrr flatten_int map_lgl modify_if map_int map2 keep flatten
#' @importFrom tidyr pivot_wider pivot_longer replace_na fill
#' @importFrom forcats fct_inorder
#' @importFrom rlang sym syms quo_get_expr as_string := .data .env is_empty
#' @importFrom rlang enquo enquos is_named
#' @importFrom glue glue
#' @importFrom tibble tibble as_tibble
#' @importFrom stats median rnorm sd na.omit
#' @importFrom utils capture.output packageVersion str
#' @importFrom stringr fixed str_split str_count str_detect str_replace
#'
#' @include summary-functions.R
#' @include utils.R
#' @include class-digits.R
#' @include class-new_names.R
#'
NULL

#' Load an example data set
#'
#' @export
ptdata <- function() { # nocov start
  env <- new.env()
  utils::data(list = "pmt.summ", package = "pmtables", envir = env)
  return(env$pmt.summ)
} # nocov end

#' @rdname ptdata
#' @export
stdata <- ptdata # nocov


#' analysis1 data set
#'
#'
"analysis1"

#' analysis1 data set - first record only
#'
#'
"pmt.first"

#' analysis1 data set - observations only
#'
#'
"pmt.obs"

#' analysis1 data set - PK observations only
#'
#'
"pmt.pk"

#' analysis1 data set - summarized
#'
#'
"pmt.summ"
