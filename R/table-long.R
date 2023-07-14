
longtable_head <- function(multicol) {
  c("\\endhead", "\\hline", multicol, "\\endfoot", "\\hline", "\\endlastfoot")
}

ltcaption <- function(macro = "", text = "", short = "", label = "") {
  if(identical(c(macro, text, short), c("", "", ""))) {
    return(NULL)
  }
  assert_that(is.character(macro))
  assert_that(is.character(text))
  assert_that(is.character(short))
  assert_that(is.character(label))
  temp <- "\\caption<short>{<text><label>} \\\\"
  if(label != "") {
    label <- paste0(" \\label{",label,"}")
  }
  if(short != "") {
    short <- paste0("[", short,"]")
  }
  if(macro != ""){
    if(substr(macro, 1, 1)=="\\") {
      macro <- substr(macro, 2, nchar(macro))
    }
    if(str_detect(macro, "[^a-zA-Z]")){
      stop(macro, " appears to be invalid for use in latex", call.=FALSE)
    }
    text <- paste0("\\", macro)
  }
  ans <- as.character(gluet(temp))
  ans
}

longtable_notes <- function(notes) {
  if(is.null(notes)) return(NULL)
  c("\\begin{center}",notes,"\\end{center}")
}

#' Create longtable output from an R data frame
#'
#' Use this function to allow your table to span multiple pages, with a
#' "to be continued" statement at the bottom of each page. There are important
#' differences between this `longtable` environment and the `tabular`
#' environment that is used to generate tables from [stable()]. See the
#' `details` section for more information.
#'
#' @inheritParams tab_notes
#' @param data an object to render as a long table; this could be a `data.frame`,
#' a `pmtable` object or an `stobject`; when passing in a `data.frame`, the data
#' should be filtered or subset so that `data` contains exactly the rows (and
#' columns) to be processed; pmtables will not add or remove rows prior to
#' processing `data`
#' @param ... passed to [stable()]
#' @param inspect fixed to `TRUE` and passed to [stable()]
#' @param lt_cap_macro the name of a macro that will hold caption text; to not
#'   lead with `\\` - this will be added for you
#' @param lt_cap_text full caption text, appearing where the table is rendered
#' @param lt_cap_short short caption text, appearing in the list of tables
#' @param lt_cap_label table label for use in latex document
#' @param lt_continue longtable continuation message
#'
#' @details
#'
#' To create `longtable` output, `pmtables` first passes the data frame
#' through [stable()] and then modifies the output to create a table in
#' `longtable` environment. The `...` arguments to [stable_long()] are passed
#' to [stable()] and can be used to configure the table. One important difference
#' between `tablular` and `longtable` environments is that captions need to
#' get inserted **inside** the `longtable` environment; this is why you see
#' several additional arguments for [stable_long()].
#'
#' You may have to run `pdflatex` on your `longtable` more than once to get the
#' table to render properly; this is not unexpected behavior for `longtable`.
#'
#' If you have panels in your table, the default is to prevent page breaks
#' right after the panel title row using the `\\*` command in the `longtable`
#' package. This shouldn't need to be changed by the user, but if needed this
#' can be suppressed by adding `nopagebreak = FALSE` when calling [as.panel()]
#' or [rowpanel()].
#'
#' @return A character vector with the TeX code for the table with `class`
#' attribute set to `stable_long` and `stable`.
#'
#' @examples
#' stable_long(stdata())
#'
#' @export
stable_long <- function(data, ...) UseMethod("stable_long")

#' @rdname stable_long
#' @export
stable_long.data.frame <- function(data,
                                   note_config = noteconf(type="minipage"),
                                   inspect = FALSE,
                                   lt_cap_macro = "",
                                   lt_cap_text = "",
                                   lt_cap_short = "",
                                   lt_cap_label = "",
                                   lt_continue = "\\footnotesize{continued on next page}",
                                   ...) {

  assert_that(note_config$type=="minipage")

  x <- stable(data = data, note_config = note_config, inspect = TRUE, ...)

  if(is.character(cap_main(x))) {
    short <- cap_short(x)
    if(!is.null(short)) {
      short <- paste0("[", short, "]")
    }
    cap <- paste0("\\caption",short,"{",cap_main(x),"} \\\\")
  } else {
    cap <- ltcaption(lt_cap_macro, lt_cap_text, lt_cap_short, lt_cap_label)
  }

  x <- get_stable_data(x)

  start <- paste0("\\begin{longtable}{", x$align_tex, "}")
  end <- "\\end{longtable}"

  extra_row_height <- gluet("\\setlength{\\extrarowheight}{<x$sizes$lt_row_space>em}")
  row_space <- gluet("\\renewcommand{\\arraystretch}{<x$sizes$row_space>}")
  col_space <- gluet("\\setlength{\\tabcolsep}{<x$sizes$col_space>pt} ")

  n_col <- x$nc
  continued <- gluet("\\multicolumn{<n_col>}{r}{<lt_continue>}")
  head <- longtable_head(continued)

  lt_notes <- longtable_notes(x$mini_notes)

  # Put stars on panel rows for long tables
  if(!x$panel$null && x$panel$nopagebreak) {
    x$tab <- tab_panel_star(x$tab)
  }

  longtab <- c(
    x$sizes$font_size$start,
    row_space,
    col_space,
    extra_row_height,
    start,
    head,
    cap,
    x$head_rows,
    "\\endfirsthead",
    x$head_rows,
    "\\endhead",
    "\\hline",
    x$tab,
    "\\hline",
    "\\end{longtable}",
    lt_notes,
    x$sizes$font_size$end
  )
  out <- structure(
    longtab,
    class = c("stable_long", "stable"),
    caption = x$caption,
    stable_file = x$stable_file
  )

  if(isTRUE(inspect)) {
    stable_data <- x
    stable_data$tab  <- longtab
    stable_data$lt_notes <- lt_notes
    out <- structure(out, stable_data = stable_data)
  }

  out
}

#' @rdname stable_long
#' @export
stable_long.stobject <- function(data, ...) {
  as_stable(data, ..., long = TRUE)
}

#' @rdname stable_long
#' @export
stable_long.pmtable <- function(data, ...) {
  as_stable(data, ..., long = TRUE)
}
