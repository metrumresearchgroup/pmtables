
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
#' packages must be installed, regardless of the type of tabble
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
#' To render a table in `landscape` environment, pass the `stable` object
#' through `as_lscape`.
#'
#' @examples
#' template_file <- system.file("article", "article.tex", package = "pmtables")
#' template_contents <- readLines(template_file)
#'
#' \dontrun{
#'   tab <- stable(ptdata())
#'   pmtables::st2article(tab)
#' }
#'
#' @export
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


#' Wrap stable output in table environment
#'
#' @param x tabular text
#' @param con where to write the output
#' @param table if `TRUE`, the code is wrapped in latex table environment
#' @param center if `TRUE`, center the table
#' @param landscape if `TRUE` render the table in landscape mode
#' @param caption the table caption
#' @param context if `rmd`, then the code is enclosed in a pandoc `latex` fenced
#' code block; if `tex`, then the fencing is omitted
#' @param ... not used
#'
#'
#' @export
st_wrap <- function(x,...) UseMethod("st_wrap")
#' @rdname st_wrap
#' @export
st_wrap.default <- function(x, con = stdout(), table = TRUE, center = TRUE, # nocov start
                            landscape = is_lscape(x),
                            caption = NULL, context = c("rmd", "tex"), ...) {
  context <- match.arg(context)
  ans <- c()
  if(isTRUE(table)) {
    ans <- c(ans, "\\begin{table}[h]")
    if(isTRUE(center)) ans <- c(ans, "\\centering")
    if(is.character(caption)) {
      ans <- c(ans, paste0("\\caption{",caption,"}"))
    }
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
#' **Important**: this function doesn't actually make the table "landscape".
#' It simply adds to the class attribute so that functions down the line
#' can look for this tag and wrap the table in `landscape` environment. The
#' `landscaping` is entirely the responsibility of some other function, just
#' not this one.
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
#' @export
as_lscape <- function(x) {
  structure(x, class = unique(c(class(x), "stable_lscape")))
}

#' @rdname as_lscape
#' @export
is_lscape <- function(x) {
  inherits(x, "stable_lscape")
}

