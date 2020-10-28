Lorem <- "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do
eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim
veniam, quis nostrud exercitation ullamco laboris."

form_caption <- function(long = NULL, short = NULL, label = NULL) {
  if(is.null(long)) return(NULL)
  if(is.character(label)) {
    label <- paste0(" \\", label)
  }
  paste0("\\caption[",short,"]{",long,label,"}")
}

#' Render a table a pdf document
#'
#' @param text [stable()] output
#' @param preview if `TRUE`, the rendered pdf file is opened using
#' [fs::file_show()]
#' @param output_dir passed to [rmarkdown::render()]
#' @param output_file passed to [rmarkdown::render()]
#' @param landscape passed to [st_wrap()]
#'
#' @return the `text` is returned invisibly
#'
#' @details
#' In order to render the table in the pdf document, the following
#' packages must be installed, regardless of the type of table
#' you are trying to render:
#'
#' 1. `threeparttable`
#' 1. `booktabs`
#' 1. `array`
#' 1. `longtable`
#' 1. `mathpazo`
#' 1. `pdflscape`
#'
#' @examples
#'
#' \dontrun{
#'   library(dplyr)
#'   ptdata()  %>% stable() %>% st2doc()
#' }
#'
#' template <- system.file("rmd", "st2doc.Rmd", package = "pmtables")
#' # cat(readLines(template), sep = "\n")
#'
#' @export
st2doc <- function(text, preview = TRUE, output_dir = tempdir(), # nocov start
                   output_file = "st2doc.pdf", landscape = is_lscape(text)) {

  assert_that(requireNamespace("rmarkdown"))
  assert_that(requireNamespace("fs"))
  file <- system.file("rmd", "st2doc.Rmd", package = "pmtables")

  if(is.list(text)) {
    tab <- map(text, .f = function(this_table) {
      if(!any(grepl("begin{table}", this_table, fixed = TRUE))) {
        this_table <- st_wrap(this_table, con = NULL, landscape = landscape)
      }
      c(this_table, "\\clearpage")
    })
    tab <- flatten_chr(tab)
  } else {
    assert_that(is.character(text))
    if(!any(grepl("begin{table}", text, fixed = TRUE))) {
      tab <- st_wrap(text, con = NULL, landscape = landscape)
    } else {
      tab <- text
    }
  }

  ans <- rmarkdown::render(
    input = file,
    intermediates_dir = tempdir(),
    output_dir = output_dir,
    output_file = output_file,
    params = list(table_text = tab)
  )

  if(file.exists(ans) && isTRUE(preview)) {
    fs::file_show(ans)
  }

  return(invisible(text))
} # nocov end

#' Preview an stable object
#'
#' @param x an stable object
#' @param ... passed to [texPreview::tex_preview()]
#'
#' @examples
#'
#' \dontrun{
#' ptdata() %>% stable() %>% st_preview()
#' }
#' @export
st_preview <- function(x,...) { # nocov start
  if(inherits(x,what="pmtable")) x <- as_stable(x)
  assert_that(requireNamespace("texPreview"))
  if(length(x) > 1) {
    x <- paste0(x,collapse = "\n")
  }
  pk <- texPreview::build_usepackage(
    c("threeparttable", "array", "booktabs")
  )
  texPreview::tex_preview(
    x,
    usrPackages = pk,
    ...
  )
} # nocov end

#' Preview tables in a TeX article
#'
#' Use [st2article()] to see tables in a plain pdf TeX document. Use
#' [st2report()] to see tables in a more formal, report-like document.
#' [st2article()] might have a **slight** speed advantage for shorter
#' development cycle and simpler presentation. See `details`.
#'
#' @param ... stable objects
#' @param .list a list of stable objects
#' @param ntex number of times to build the pdf file
#' @param stem name for the article file, without extension
#' @param output_dir the output directory for the rendered pdf file
#' @param margin the page margins; this must be a character vector of length
#' 1 or 2 specifying the page margins; if length is 1, the data will be recycled
#' into the second position (e.g. equal margins left/right and top/bottom);
#' if length is 2, use the first position to set left & right margins and the
#' second position to set top & bottom margins; when specifying the margin
#' size, include both the number and the unit (e.g. `3cm` or `1in`; you must
#' enter the unit and the input must be character)
#' @param template an optional template for rendering the article; this normally
#' shouldn't be used
#' @param caption placeholder text to be included as a caption; this text will
#' be used for every table that is passed in; this isn't intended to be the
#' actual caption for the table, but just placeholder text as you preview the
#' appearance of the table
#' @param dry_run if `TRUE`, then the document and table code are returned
#' (visibly) and no attempt is made to try to pass the document through
#' `pdflatex`
#' @param stdout passed to [system2()]; by default, the `pdflatex` build output
#' is suppressed; if you are having difficulty generating a pdf document,
#' set `stdout = ""` and you'll see the output in the R console
#' @param show_pdf if `TRUE`, then the rendered pdf file will be opened using
#' [fs::file_show()]
#'
#' @details
#'
#' This is experimental. Pass in either `stable` or `stable_long` objects,
#' and [st2article()] or [st2report()] will write the output to temporary file
#' and render the collection of tables in a pdf document without using Rmarkdown
#' or pandoc. The rendering is accomplished through system call to `pdflatex`.
#'
#' A working tex distribution is required to run this function.It is important
#' to review the different latex dependencies in the document template.  It is
#' assumed that all of these dependencies are available. See `examples` below
#' for code to open the template for review.
#'
#' This function requires `pdflatex` to be installed and in your path to build
#' the document.  Run `system2("pdflatex", "-v")` to see if `pdflatex` properly
#' installed.
#'
#' This function requires specific packages to be available. Review the
#' `tex` template files in `/inst/article` for a complete specification of the
#' requirements. If a requirement is not available, the document will not build.
#'
#' To render a table in `landscape` environment, pass the `stable` object
#' through `as_lscape`.
#'
#' @return
#' If `dry_run` is `FALSE`, then a list (invisible) containing the document
#' template as `doc` and the table data as `tables`. If `dry_run` is `TRUE`,
#' then the `doc` and `tables` are returned visibly (see `dry_run` argument).
#'
#' @examples
#' template_file <- system.file("tex", "article.tex", package = "pmtables")
#' template_contents <- readLines(template_file)
#'
#' template_file <- system.file("tex", "report.tex", package = "pmtables")
#' template_contents <- readLines(template_file)
#'
#' \dontrun{
#'   tab <- stable(ptdata())
#'   pmtables::st2article(tab)
#' }
#'
#' @export
st2article <- function(..., .list = NULL, ntex = 1,  #nocov start
                       stem = "view-st2article",
                       output_dir = tempdir(), template = NULL,
                       margin = "3cm", caption = NULL,
                       dry_run = FALSE, stdout = FALSE, show_pdf = TRUE) {

  tables <- c(list(...),.list)
  assert_that(dir.exists(output_dir))
  assert_that(is.character(margin))
  if(length(margin)==1) {
    margin <- c(margin, margin)
  }
  assert_that(length(margin) ==2)
  tables_are_stable <- map_lgl(tables, inherits, what = "stable")
  assert_that(all(tables_are_stable))

  if(is.null(template)) {
    template <- system.file("tex", "article.tex", package = "pmtables")
  } else {
    assert_that(file.exists(template))
  }

  texfile <- paste0(stem,".tex")
  pdffile <- paste0(stem,".pdf")
  st2article_input <- paste0(stem,"-tables.tex")

  temp <- readLines(template)

  env <- list()
  env$list_of_tables <- c("\\listoftables", "\\clearpage")
  env$input_file <- st2article_input
  env$hmargin <- margin[1]
  env$vmargin <- margin[2]

  if(length(tables)==1 || is.null(caption)) {
    env$list_of_tables <- "% listoftables"
  }

  temp <- mgluet(temp, .envir = env)

  if(is.character(caption)) {
    wrap_with_caption <- function(text, i) {
      if(is.null(i)) i <- "<no name given>"
      short <- paste0("pmtables output preview - ", i)
      pt_wrap(
        text, context = "tex", caption = caption, short = short,
        con = NULL
      )
    }
    tables <- imap(tables, wrap_with_caption)
  } else {
    tables <- map(tables, pt_wrap, context = "tex", con = NULL)
  }

  tables <- map(tables, ~ c(.x, "\\clearpage"))
  tables <- flatten_chr(tables)

  output_dir <- normalizePath(output_dir)

  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(tempdir())

  writeLines(tables,st2article_input)
  writeLines(temp,texfile)
  ans <- list(doc = temp, tables = tables)

  if(dry_run) return(ans)

  if(file.exists(pdffile)) {
    unlink(pdffile)
  }

  if(file.exists(texfile)) {
    for(i in seq_len(ntex)) {
      result <- system2(
        "pdflatex",
        args=c("-halt-on-error ",texfile),stdout=stdout)
      if(!identical(result, 0L)) {
        warning("non-zero exit from pdflatex", call.=FALSE)
      }
    }
  } else {
    stop("could not locate the template tex file", call.=FALSE)
  }

  if(output_dir != tempdir()) {
    file.copy(pdffile, file.path(output_dir,pdffile), overwrite = TRUE)
  }

  if(isTRUE(show_pdf)) {
    pdfshow <- file.path(output_dir,pdffile)
    if(file.exists(pdfshow)) {
      fs::file_show(pdfshow)
    } else {
      stop("could not locate the output pdf file", call. = FALSE)
    }
  }

  return(invisible(ans))
} # nocov end


#' @rdname st2article
#' @export
st2report <- function(..., template = "report.tex", caption = Lorem) {
  if(missing(template)) {
    template <- system.file("tex", template, package = "pmtables")
  }
  st2article(
    ..., template = template, stem = "view-st2report", caption = caption
  )
}

#' Wrap stable output in table environment
#'
#' @param x tabular text
#' @param con where to write the output
#' @param table if `TRUE`, the code is wrapped in latex table environment
#' @param center if `TRUE`, center the table
#' @param landscape if `TRUE` render the table in landscape mode
#' @param caption the long table description
#' @param short the short table description
#' @param float the float specifier to if a `table` environment is used; this
#' defaults to `H` if [st_use_deps()] has been called and `!ht` otherwise
#' @param context if `rmd`, then the code is enclosed in a pandoc `latex` fenced
#' code block; if `tex`, then the fencing is omitted
#' @param ... not used
#'
#'
#' @export
st_wrap <- function(x,...) UseMethod("st_wrap")
#' @rdname st_wrap
#' @export
st_wrap.default <- function(x,  # nocov start
                            con = stdout(),
                            table = TRUE,
                            center = TRUE,
                            landscape = is_lscape(x),
                            caption = NULL,
                            short = NULL,
                            float = "H",
                            context = c("rmd", "tex"), ...) {
  context <- match.arg(context)

  ans <- c()

  if(isTRUE(table)) {
    ans <- c(ans, gluet("\\begin{table}[<float>]"))
    if(isTRUE(center)) ans <- c(ans, "\\centering")
    ans <- c(ans, form_caption(caption,short))
    ans <- c(ans, x)
    ans <- c(ans,"\\end{table}")
  } else {
    ans <- x
  }
  if(isTRUE(center) && !isTRUE(table)) {
    ans <- c("\\begin{center}", ans, "\\end{center}")
  }
  if(isTRUE(landscape)) {
    ans <- c("\\begin{landscape}", ans, "\\end{landscape}")
  }
  if(context=="rmd") {
    if(!st_using_deps()) st_use_deps()
    ans <- c("```{=latex}", ans, "```")
  }
  if(!is.null(con)) {
    writeLines(text = ans, con = con)
  }
  return(invisible(ans))
} # nocov end

#' @rdname st_wrap
#' @export
st_wrap.stable_long <- function(x, table = FALSE, ...) {
  st_wrap.default(x, ..., table = FALSE)
}

#' @rdname st_wrap
#' @export
pt_wrap <- st_wrap

#' @rdname st_wrap
#' @export
st_latex <- function(x, con = stdout(), center = TRUE, context = c("rmd", "tex")) {
  context <- match.arg(context)
  ans <- x
  if(isTRUE(center)) {
    ans <- c("\\begin{center}", ans, "\\end{center}")
  }
  if(context=="rmd") {
    ans <- c("```{=latex}", ans, "```")
  }
  if(!is.null(con)) {
    writeLines(text = ans, con = con)
  }
  return(invisible(ans))

}

#' Mark table text for display in landscape environment
#'
#' **Important**: this function will __not__ make your table render in
#' landscape mode in a stand alone TeX document. This function doesn't actually
#' make the table "landscape". It simply adds to the class attribute so that
#' functions down the line can look for this tag and wrap the table in
#' `landscape` environment. The `landscaping` is entirely the responsibility of
#' some other function, just not this one.
#'
#' @param x output from either [stable()] or [stable_long()]
#'
#' @examples
#'
#' out <- stable(stdata())
#' is_lscape(out)
#'
#' land <- as_lscape(out)
#' is_lscape(land)
#'
#' st_wrap(as_lscape(out))
#'
#' @return
#' For [as_lscape()], the input is returned, but with `stable_lscape`
#' added to the `class` attribute.  For [is_lscape()], a logical value
#' is returned, indicating whether or not the object inherits from
#' `stable_lscape`.
#'
#' @seealso [st_wrap()], [st2article()], [st2report()]
#'
#' @export
as_lscape <- function(x) {
  structure(x, class = unique(c(class(x), "stable_lscape")))
}

#' @rdname as_lscape
#' @export
is_lscape <- function(x) {
  inherits(x, "stable_lscape")
}

