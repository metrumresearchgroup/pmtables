
#' Render a table a pdf document
#'
#' @param text [stable()] output
#' @param preview if `TRUE`, the rendered pdf file is opened using
#' [fs::file_show()]
#' @param output_dir passed to [rmarkdown::render()]
#' @param output_file passed to [rmarkdown::render()]
#'
#' @return the `text` is returned invisibly
#'
#' @examples
#'
#' \dontrun{
#'   library(dplyr)
#'   ptdata()  %>% stable() %>% st2doc()
#' }
#'
#' @export
st2doc <- function(text, preview = TRUE, output_dir = tempdir(), # nocov start
                   output_file = "st2doc.pdf") {
  assert_that(requireNamespace("rmarkdown"))
  assert_that(requireNamespace("fs"))
  file <- system.file("rmd", "st2doc.Rmd", package = "pmtables")

  if(is.list(text)) {
    tab <- map(text, .f = function(this_table) {
      if(!any(grepl("begin{table}", this_table, fixed = TRUE))) {
        this_table <- st_wrap(this_table, con = NULL)
      }
      c(this_table, "\\clearpage")
    })
    tab <- flatten_chr(tab)
  } else {
    assert_that(is.character(text))
    if(!any(grepl("begin{table}", text, fixed = TRUE))) {
      tab <- st_wrap(text, con = NULL)
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
#' This is experimental. Pass in either `stable` or `stable_long` objects,
#' and [st2article()] will write the output to temporary file and render the
#' collection of tables in a pdf document without using Rmarkdown or pandoc.
#' The rendering is accomplished through system call to `pdflatex`. See
#' `details`.
#'
#' @param ... stable objects
#' @param .list a list of stable objects
#' @param ntex number of times to build the pdf file
#' @param stem name for the article file, without extension
#' @param output_dir the output directory for the rendered pdf file
#' @param margin the horizontal margin in `cm`; default is 3 cm; the vertical
#' margin is fixed to 3 cm
#' @param template an optional template for rendering the article; this normally
#' shouldn't be used
#'
#' @details
#' A working tex distribution is required to run this function.It is important
#' to review the different latex dependencies in the document template.  It is
#' assumed that all of these dependencies are available. See `examples` below
#' for code to open the template for review.
#'
#' This function requires `pdflatex` to be installed and in your path to build
#' the document.  Run `system2("pdflatex", "-v")` to see if `pdflatex` properly
#' installed.
#'
#' @examples
#' template_file <- system.file("article", "article.tex", package = "pmtables")
#' template_contents <- readLines(template_file)
#'
#' \dontrun{
#'   tab <- stable(ptdata())
#'   pmtables:::st2article(tab)
#' }
#'
st2article <- function(..., .list = NULL, ntex = 1, stem  = "article", #nocov start
                       output_dir = tempdir(), margin = NULL,
                       template = NULL) {
  tables <- c(list(...),.list)
  assert_that(dir.exists(output_dir))
  tables_are_stable <- map_lgl(tables, inherits, what = "stable")
  assert_that(all(tables_are_stable))
  if(is.null(template)) {
    template <- system.file("article", "article.tex", package = "pmtables")
  }
  assert_that(file.exists(template))
  texfile <- paste0(stem,".tex")
  pdffile <- paste0(stem,".pdf")
  st2article_input <- paste0(stem,"-tables.tex")

  temp <- readLines(template)

  w <- grep("\\input{tables}", temp, fixed = TRUE)
  if(length(w)==1) {
    temp[w] <- paste0("\\input{", st2article_input, "}")
  } else {
    stop(
      "problem processing the template; couldn't find input statement",
      call.=FALSE
    )
  }
  if(is.numeric(margin)) {
    w <- grep("[margin=3cm]{geometry}", temp, fixed = TRUE)
    if(length(w)==1) {
      geo <- paste0("\\usepackage[vmargin=3cm,hmargin=",margin,"cm]{geometry}")
      temp[w] <- geo
    }
  }
  wrap_with_caption <- function(text, i) {
    if(is.null(i)) i <- "<no name given>"
    cap <- paste0("st2article preview output - ", i)
    pt_wrap(text, context = "tex", caption = cap, con = NULL)
  }

  tables <- imap(tables, wrap_with_caption)
  tables <- map(tables, ~ c(.x, "\\clearpage"))
  tables <- flatten_chr(tables)

  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(tempdir())

  writeLines(tables,st2article_input)
  writeLines(temp,texfile)

  if(file.exists(pdffile)) {
    unlink(pdffile)
  }

  if(file.exists(texfile)) {
    for(i in seq_len(ntex)) {
      result <- system2("pdflatex", args=c("-halt-on-error ",texfile))
      if(!identical(result, 0L)) {
        warning("non-zero exit from pdflatex", call.=FALSE)
      }
    }
  } else {
    stop("could not locate the article tex file", call.=FALSE)
  }

  if(output_dir != tempdir()) {
    file.copy(pdffile, file.path(output_dir,pdffile), overwrite=TRUE)
  }

  pdfshow <- file.path(output_dir,pdffile)

  if(file.exists(pdfshow)) {
    fs::file_show(pdfshow)
  } else {
    stop("could not locate the output pdf file", call.=FALSE)
  }

  return(invisible(NULL))
} # nocov end
