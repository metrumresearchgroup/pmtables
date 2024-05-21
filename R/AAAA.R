#' @importFrom assertthat validate_that assert_that
#' @importFrom tidyselect all_of eval_select eval_rename contains everything
#' @importFrom dplyr mutate bind_rows select ungroup summarise left_join
#' @importFrom dplyr group_modify rename count vars group_by n first last
#' @importFrom dplyr case_when filter arrange group_vars distinct bind_cols
#' @importFrom dplyr groups slice mutate_at across
#' @importFrom purrr map_dfr walk partial map map_chr modify flatten_chr imap
#' @importFrom purrr flatten_int map_lgl modify_if map_int map2 keep flatten
#' @importFrom purrr list_flatten
#' @importFrom tidyr pivot_wider pivot_longer replace_na fill separate unite
#' @importFrom tidyr complete
#' @importFrom forcats fct_inorder
#' @importFrom rlang sym syms quo_get_expr as_string := .data .env is_empty
#' @importFrom rlang enquo enquos expr is_named is_atomic have_name
#' @importFrom rlang env_clone abort warn
#' @importFrom glue glue
#' @importFrom tibble tibble as_tibble is_tibble
#' @importFrom stats median rnorm sd na.omit setNames update
#' @importFrom utils capture.output packageVersion str
#' @importFrom stringr fixed str_split str_count str_detect str_replace
#' @importFrom stringr str_extract
#' @importFrom tools file_ext
#' @importFrom lifecycle deprecate_warn
#'
#' @include summary-functions.R
#' @include utils.R
#' @include class-digits.R
#' @include class-new_names.R
#'
NULL


# GLOBAL object
.internal <- new.env(parent = emptyenv())
.internal$marker.panel <- "%--pmtables-insert-panel"

.onLoad <- function(libname, pkgname) {
  st_reset_knit_deps()
}

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
#' @section Table data grooming and sanitization:
#' - The functions [tab_prime()] and [triage_data()] are called to get the data
#'   frame ready to be in tabular format (see [make_tabular()]
#' - The function [tab_escape()] is used to escape `%` and `_` in the table
#'   (sanitization); use `options(pmtables.escape = ...)` to change the set of
#'   escape characters or set that option to `NULL` to prevent any sanitization
#' - Any unit of data that has a character escaped with double backslash
#'   (`\\`) or that contains a math expression with two `$` will not be
#'   sanitized
#' - The function [tab_escape()] also performs the following substitutions:
#'   - change `~` to `$\\sim$`
#'   - change `>` to `$>$`
#'   - change `<` to `$<$`
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
#'   otherwise, pass the `pmtable` object to [stable()] or [as_stable()] in
#'   order to create the table.  See [class-pmtable] for details around this
#'   object
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
#' - Use [st2viewer()] to send s-table output to [texPreview::tex_preview()]
#' - Use [st2article()] or [st2report()] to render several tables in
#'   a stand-alone tex document rendered directly by `pdflatex` (no involvement
#'   of `Rmarkdown` or `pandoc`); this requires `pdflatex` to be installed and
#'   in your `PATH`.
#' - Use [st2doc()] to render a pdf file with one or more tables using pandoc;
#'   in general, use [st2article()] instead
#' - Pipe tables to [st_asis()] to render a table in line while knitting an
#'   Rmd document
#' - Use [st_wrap()]  to wrap s-table output in a `table`
#'   environment and optionally send the output to [stdout()]; this is helpful
#'   when rendering tables in Rmarkdown documents.  There is an [st_wrap()]
#'   method for longtables that won't add the table environment.
#' - Use [as_lscape()] to mark [stable()] or [stable_long()] output for display
#'   in landscape environment
#'
#' @section Save s-tables:
#' - Use [stable_save()] to write an `stable` or `stable_long` object to file
#'
#' - Note that there is a `dir` argument to that function that lets you
#'   route the table to a specific directory; `dir` defaults to the
#'   `pmtables.dir` option, so setting `options(pmtables.dir = "../deliv/table")`
#'   will route the table to that directory without requiring additional input
#'
#' @section Latex / markdown information:
#'
#' The following latex packages are required in your `Rmd` or `latex` document:
#'
#' - `threeparttable`
#' - `array`
#' - `booktabs`
#' - `pdflscape`
#' - `longtable` (only when long tables are in the document)
#' - `float` (mainly if you want to use `H` placement in your Rmd output)
#'
#' In `Rmd`, include these as `extra_dependencies`. Or try using
#' [st_use_knit_deps()] to include these packages via [knitr::knit_meta_add()];
#' this is __only__ when you are including a table in a knit `Rmd` document.
#'
#' You may also want to include this package:
#'
#' - `mathdesign` with option `utopia`
#'
#' The tables are generated with defaults that look sensible when the table is
#' rendered with single spacing.  If you are working in a 1.5 spaced
#' environment, the table may look roomy.  In that case, load the `setspace`
#' package and switch to `singlespacing` prior to sourcing the table.
#'
#' If you render tables in an `Rmarkdown` document with processing by `pandoc`,
#' `pandoc` may make mistakes when parsing the `latex` code.  To keep `pandoc`
#' from making parsing errors, use `latex` code fence.  This formatting can
#' be added to your table with the [st_latex()] command.  You can see what the
#' fence looks like by running `st_latex("abc")`. Asserting that the code is
#' `latex` can also be accomplished with [pt_wrap()] and using the
#' `context = "rmd"` option. See [st2article()] for instructions on
#' how to view a complete working `latex` example.
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

