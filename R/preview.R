
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
      this_table <- as.character(this_table)
      if(!any(grepl("begin{table}", this_table, fixed = TRUE))) {
        this_table <- pt_wrap(as.character(this_table))
      }
      c(this_table, "\\clearpage")
    })
    tab <- flatten_chr(tab)
  } else {
    tab <- as.character(text)
    if(!any(grepl("begin{table}", tab, fixed = TRUE))) {
      tab <- pt_wrap(as.character(tab))
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
#' This is experimental
#'
#' @param ... stable objects
#' @param .list a list of stable objects
#' @param npdf number of times to build the pdf file
#' @param stem name for the article, without extension
#'
st2article <- function(..., .list = NULL, npdf = 1, stem  = "article") { #nocov start
  tables <- c(list(...),.list)
  assert_that(all(map_lgl(tables, inherits, what = "stable")))
  template <- system.file("article", "article.tex", package="pmtables")
  texstem <- file.path(tempdir(), stem)
  texfile <- paste0(texstem, ".tex")
  pdffile <- basename(paste0(texstem, ".pdf"))
  tablefile <- paste0(stem, "-tables.tex")
  st2article_input <- file.path(tempdir(),tablefile)
  temp <- readLines(template)
  w <- grep("st2article_input", temp)
  for(i in w) {
    temp[i] <- glue::glue(temp[i], .open = "<<<", .close = ">>>")
  }
  wrap_with_caption <- function(text, i) {
    cap <- paste0("st2article preview item: ", i)
    pt_wrap(text, context = "tex", caption = cap, con = NULL)
  }
  tables <- imap(tables, wrap_with_caption)
  tables <- map(tables, ~ c(.x, "\\clearpage"))
  tables <- flatten_chr(tables)

  writeLines(tables,st2article_input)
  writeLines(temp, texfile)

  if(file.exists(texfile)) {
    for(i in seq_len(npdf)) {
      system2("pdflatex", args=c("-halt-on-error",texfile))
    }
  } else {
    stop("there was an error - could not locate the article tex file")
  }
  if(file.exists(pdffile)) {
    fs::file_show(pdffile)
  } else {
    stop("there was an error - could not locate the output pdf file")
  }
  return(invisible(NULL))
} # nocov end
