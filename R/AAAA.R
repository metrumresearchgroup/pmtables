#' @importFrom assertthat validate_that assert_that
#' @importFrom tidyselect all_of eval_select eval_rename contains
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

#' pmtables: Tables for Pharmacometrics.
#'
#' Summarize data sets and create publication-quality tables for
#' inclusion in 'tex' documents.
#'
#' @section Creating general tables:
#' - To create a table with a single function call, use [stable()] for single
#'   page tables and [stable_long()] for longtable
#' - Alternatively, use [st_new()] to create a s-table object and pipe to other
#'   st-functions; see the documentation links in [stable()] to discover these
#'   functions; finally, create an stable object with [st_make()]
#'
#' @section Table data and grooming:
#' - The functions [tab_prime()] and [triage_data()] are called to get the data
#'   frame ready to be in tabular format (see [make_tabular()]
#' - The function [tab_escape()] is used to escape `%` and `_` in the table
#'   (sanitization); use `options(pmtables.escape = ...)` to change the set of
#'   escape characters or set that option to `NULL` to prevent any sanitization
#'
#' @section Summarizing and creating tables:
#' - Use [pt_cont_wide()] to create continuous data summary in wide format
#' - Use [pt_cont_long()] to create continuous data summary in long format
#' - Use [pt_cat_wide()] to create discrete data summaries in wide format
#' - Use [pt_cat_long()] to create discrete data summaries in long format
#' - Use [pt_data_inventory()] to create a table summarizing observations and
#'   individuals in a data set
#' - All of the above functions return an object with class `pmtable`, which is
#'   a list; you can access the summarized data by looking at the `data` slot;
#'   otherwise, pass the `pmtable` object to [as_stable()] in order to create
#'   the table
#' - You can configure the digits in these summaries with [new_digits()]
#'
#' @section Helper functions for working with data frames:
#' - Make text bold with [tex_bold()] or italics with [tex_it()]
#' - Clear repeated values in a column with [clear_grouped_values()] and
#'   [tab_clear_reps()]
#' - Use [tab_edit()] to do find / replace on your data frame
#' - Use [df_grep_rows()] and [df_grepl_rows()] to find rows that match a given
#'   pattern
#' - Use [sig()] or [digit1()] to control the number of digits that are
#'   displayed; both these functions return character
#'
#' @section Functions to align columns:
#' - When you want latex to figure out column widths: [cols_left()],
#'   [cols_right()], [cols_center()], [cols_align()]
#' - When you want to fix the width of a specific column in the table:
#'   [col_ragged()], [col_fixed()]
#'
#' @section Functions to modify or configure a table:
#' - Alter column names: [tab_cols()]
#' - Partition a table into panels: [as.panel()] and [rowpanel()]
#' - Create groups of columns with column spanners: [colgroup()] and
#'   [colsplit()]
#' - Configure row spacing, column spacing, or font size: [tab_size()]
#' - Configure the appearance of table notes: [noteconf()]
#' - Identify rows that are "summary" rows: [sumrow()]; these rows can get
#'   automatically styled with a label (optionally in bold font) and a
#'   horizontal line above
#'
#' @section Preview tables:
#' - Use [st_preview()] to send s-table output to `texPreview`
#' - Use [st2doc()] to render a pdf file with one or more tables
#' - Use [pt_wrap()] to wrap s-table output in a `table` environment and
#'   optionally send the output to [stdout()]; this is helpful when rendering
#'   tables in Rmarkdown documents
#'
#' @section Save s-tables:
#' - Use [stable_save()] to write an s-table object to file
#'
#' @section Data sets:
#' - [analysis1] - a NMTRAN-style data set; the basis for most other
#'   example data sets
#' - [pmt_pk] - this is [analysis1], but with only PK observation records
#' - [pmt_obs] - this is [analysis1], but with only observations
#' - [pmt_first] - this is [analysis1] but with only the first record from
#'   each individual
#' - [pmt_summarized] - an example data set that has been summarized
#
#'
#' @md
#' @docType package
#' @name pmtables
NULL


#' Load an example data set
#'
#' @export
ptdata <- function() { # nocov start
  env <- new.env()
  utils::data(list = "pmt_summarized", package = "pmtables", envir = env)
  return(env$pmt_summarized)
} # nocov end

#' @rdname ptdata
#' @export
stdata <- ptdata # nocov


#' analysis1 data set
#'
#' This is the complete example analysis data set.
#'
#' @format A data frame with 4680 rows and 36 variables
#'
#' @seealso [pmt_first], [pmt_pk], [pmt_obs]
#'
"analysis1"

#' analysis1 data set - first record only
#'
#' This is the first record from each individual in the
#' [analysis1] data set.
#'
#' @format A data frame with 160 rows and 36 variables
#'
#' @seealso [analysis1], [pmt_pk], [pmt_obs]
#'
"pmt_first"

#' analysis1 data set - observations only
#'
#' These are observation records (`EVID=0`) from the
#' [analysis1] data set.
#'
#' @format A data frame with 3530 rows and 36 variables
#'
#' @seealso [analysis1], [pmt_first], [pmt_pk]
#'
"pmt_obs"

#' analysis1 data set - PK observations only
#'
#' These are PK observation records from the [analysis1]
#' data set.
#'
#' @format A data frame with 3210 rows and 36 variables
#'
#' @seealso [analysis1], [pmt_first], [pmt_obs]
#'
"pmt_pk"

#' analysis1 data set - summarized
#'
#' This is an example of a summary of the [analysis1]
#' data set.
#'
#' @format A data frame with 13 rows and 9 variables
#'
"pmt_summarized"
