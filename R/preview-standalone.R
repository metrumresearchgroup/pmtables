#' Check for magick dependency
#' @noRd
require_magick <- function() {
  if(!requireNamespace("magick", quietly = TRUE)) {
    stop("this function requires the magick package to be installed.")
  }
}

#' Check for pdftools dependency
#' @noRd
require_pdftools <- function() {
  if(!requireNamespace("pdftools", quietly = TRUE)) {
    stop("this function requires the pdftools package to be installed.")
  }
}

#' Check for knitr dependency
#' @noRd
require_knitr <- function() {
  if(!requireNamespace("knitr", quietly = TRUE)) {
    stop("this function requires the knitr package to be installed.")
  }
}

#' If a build failed, copy an image into the output location that includes
#' text communicating that it failed.
#' @noRd
fail_png <- function(to_file) {
  from_file <- system.file("image", "fail.png", package = "pmtables")
  file.copy(from_file, to_file, overwrite = TRUE)
}

fail_pdf <- function(to_file) {
  from_file <- system.file("image", "fail.pdf", package = "pmtables")
  file.copy(from_file, to_file, overwrite = TRUE)
}

#' Some settings for fonts; default is a san serif font
#' @noRd
fonts <- list(
  helvetica  = "\\usepackage{helvet}\n\\renewcommand{\\familydefault}{\\sfdefault}",
  utopia = "\\usepackage[adobe-utopia]{mathdesign}",
  roboto = "\\usepackage[sfdefault]{roboto}"
)

#' Convert stable text to a standalone snippet
#'
#' @inheritParams st_aspdf
#' @param text character vector of table text.
#' @param command pass `pdflatex` when building a `pdf` file or `latex` when
#' building `png`.
#' @param ltversion numeric version number for the longtable package; newer
#' versions have an issue that will break this code for longtables; so we are
#' requiring an older version at this point.
#' @keywords internal
st_to_standalone <- function(text, stem, dir,
                             font = c("helvetica","roboto", "utopia"),
                             command = "latex",
                             textwidth = 6.5,
                             border = "0.2cm 1cm",
                             ntex = 1,
                             ltversion = getOption("pmtables.image.ltversion" , 4.13)) {

  out_ext <- ifelse(command=="latex", ".dvi", ".pdf")
  font <- match.arg(font)
  assert_that(is.character(border) && length(border)==1)
  assert_that(is.numeric(ntex) && length(ntex)==1)
  assert_that(is.numeric(ltversion))

  if(!dir.exists(dir)) dir.create(dir)
  cwd <- getwd()
  on.exit(setwd(cwd), add = TRUE)
  setwd(dir)

  # INSIDE THE BUILD DIRECTORY ----------
  texfile <- paste0(stem, ".tex")
  outfile <- paste0(stem, out_ext)
  if(file.exists(outfile)) file.remove(outfile)

  # The standalone template
  temp_file <- system.file(
    "tex",
    "standalone-preview.tex",
    package = "pmtables"
  )

  temp_text <- readLines(temp_file)

  if(is.numeric(textwidth) && textwidth > 0) {
    textw_tex <- gluet(
      "\\setlength{\\textwidth}{<textwidth>in}"
    )
    rule <- paste0("\\rule{", textwidth, "in}{0pt}")
    vwidth <- paste0("=", textwidth, "in")
  } else {
    textw_tex <- "% textwidth not set"
    rule <- "% no rule"
    vwidth <- ""
  }

  env <- list(
    border = border,
    texfile = texfile,
    font = fonts[[font]],
    textw_tex = as.character(textw_tex),
    rule = rule,
    vwidth = vwidth,
    ltversion = paste0("[=v", ltversion, "]%")
  )

  temp_text <- mgluet(temp_text, .envir = env)
  build_file <- basename(temp_file)
  writeLines(temp_text, con = build_file)
  writeLines(text, con = texfile)

  args <- c(
    "-halt-on-error",
    paste0("-jobname=", stem),
    build_file
  )

  for(i in seq(ntex)) {
    x <- system2(
      command = command,
      args = args,
      stdout = TRUE,
      stderr = TRUE
    )
  }

  ans <- file.path(dir, outfile)
  if(!file.exists(outfile)) {
    warning("the standalone preview build didn't work.")
    class(ans) <- "latex-failed"
  }

  # SETWD BACK TO ORIGINAL WORKING DIRECTORY
  return(ans)
}

#' Render stable object to pdf file
#'
#' Create a "standalone" `pdf` snippet from an stable object using the
#' `pdflatex` utility. The resultant `pdf` file is saved on disk and the
#' relative path to the file is returned. `st2pdf()` is an alias to
#' `st_as_pdf()`.
#'
#' @details
#' The `pdf` file is built using `pdflatex` so this utility must be installed.
#'
#' The `textwidth` argument is set to 6.5 inches by default to mimic a 8.5 x 11
#' page with 1 inch margins on the left and right. Setting `textwidth` sets the
#' length of the `\textwidth` latex macro to that value and also inserts an
#' invisible rule across the page with that width as well. This means for
#' skinny tables, there will be whitespace on the left and right, but the font
#' in the resultant images will be similar regardless of the width of the
#' table. To skip setting the latex `\textwidth` macro, pass `NULL`.
#'
#' The `border` argument can be one, two or four space-separated elements, each
#' formatted as `"<number><unit>"` (e.g. "0.2cm"); pass one element to set the
#' same border on all sides; two elements to set the border on left/right
#' (first) and top/bottom (second); pass four elements to have separate borders
#' for the left, bottom, right and top (see the documentation for the
#' `standalone` latex package).
#'
#' @inheritParams st2article
#' @param x an stable object; this can be the result of calling [stable()] or
#' [stable_long()].
#' @param stem used to build intermediate and output file names.
#' @param dir directory for building the pdf file.
#' @param font the font to use; alternative values include `roboto` and
#' `utopia`; passed to [st_to_standalone()].
#' @param textwidth the page width (in inches) when building with `pdflatex`;
#' passed to [st_to_standalone()]; see details.
#' @param border passed as an option to `standalone` latex output type; see
#' details.
#'
#' @examples
#'
#' # check that pdflatex is installed
#' \dontrun{
#' Sys.which("pdflatex")
#' }
#'
#' \dontrun{
#' tab <- stable(stdata())
#' st_aspdf(tab)
#' }
#'
#' # the template for building the image
#' temp <- system.file("tex", "standalone-preview.tex", package = "pmtables")
#' cat(temp, sep = "\n")
#'
#' @return
#' A string containing the path to the rendered `pdf` file.
#'
#' @seealso
#' [st_aspng()], [st_as_image()], [st_image_show()]
#'
#' @export
st_aspdf <- function(x,
                     stem = "pmt-standalone-preview",
                     dir = tempdir(),
                     font = "helvetica",
                     textwidth = getOption("pmtables.textwidth", 6.5),
                     border = getOption("pmtables.image.border", "0.2cm 0.7cm"),
                     ntex = 1) {
  assert_that(inherits(x, "stable"))
  ans <- st_to_standalone(
    x,
    stem,
    dir,
    command = "pdflatex",
    font = font,
    textwidth = textwidth,
    border = border,
    ntex = ntex
  )
  if(inherits(ans, "latex-failed")) {
    fail_pdf(unclass(ans))
  }
  ans
}

#' @rdname st_aspdf
#' @export
st2pdf <- st_aspdf

#' Render stable object in png format
#'
#' Create a standalone png snippet using `latex` and `dvipng` from an stable
#' object; both functions are required to be installed for this to work.
#' The resultant `png` file is saved on disk and the relative path to the file
#' is returned. `st2png()` is an alias to `st_as_png()`.
#'
#' @inheritParams st_aspdf
#' @inheritParams st2article
#' @param dpi dots per inch for the resulting `png` file; used by `dvipng` when
#' converting `dvi` file to the final `png` result.
#'
#' @examples
#' \dontrun{
#' Sys.which("latex")
#' Sys.which("dvipng")
#'
#' tab <- stable(stdata())
#' st_aspng(tab)
#' }
#'
#'
#' @return
#' A string containing the path to the rendered `png` file.
#'
#' @seealso
#' [st_aspdf()], [st_as_image()], [st_image_show()]
#'
#' @export
st_aspng <- function(x,
                     stem = "pmt-standalone-preview",
                     dir = tempdir(),
                     font = "helvetica",
                     textwidth = getOption("pmtables.text.width", 6.5),
                     border = getOption("pmtables.image.border", "0.2cm 0.7cm"),
                     ntex = 1, dpi = 200) {
  assert_that(inherits(x, "stable"))
  outfile <- st_to_standalone(
    x,
    stem,
    dir,
    command = "latex",
    font = font,
    textwidth = textwidth,
    border = border,
    ntex = ntex
  )
  png_file <- sub("dvi$", "png", outfile, perl = TRUE)
  if(inherits(outfile, "latex-failed")) {
    fail_png(png_file)
  } else {
    ans <- dvi_to_png(outfile, png_file, dpi = dpi)
    if(inherits(outfile, "dvipng-failed")) {
      fail_png(png_file)
    }
  }
  png_file
}

#' @rdname st_aspng
#' @export
st2png <- st_aspng


#' @keywords internal
dvi_to_png <- function(dvifile, pngfile, dpi = 200) {

  if(file.exists(pngfile)) file.remove(pngfile)
  args <- c(
    paste0("-D ", dpi),
    paste0("-o ", pngfile),
    "-q*",
    dvifile
  )
  ans <- system2(
    "dvipng",
    args,
    stdout = TRUE,
    stderr = TRUE
  )
  if(!file.exists(pngfile)) {
    pngfile <- structure(pngfile, class = "dvipng-failed")
  }
  return(pngfile)
}

#' Display table from pdf image
#'
#' Use this function to turn an stable object in to a high-quality pdf file,
#' The result can be included in an Rmarkdown file rendered to `.html` or
#' other uses.
#'
#' @details
#' This function depends on the magick and pdftools packages being installed.
#'
#' 1. First, `x` is rendered with [st_aspdf()]
#' 1. Next, the pdf file is read using [magick::image_read_pdf()]
#' 2. Finally, the image is possibly resized via [st_image_show()]
#'
#' @inheritParams st_aspdf
#' @param width the relative width of the image; passed to [st_image_show()];
#' this must be greater than 0 and less than or equal to 1.
#' @param ... arguments passed to [st_aspdf()]; please take the time to review
#' the details in that help topic.
#'
#' @return
#' A possibly resized `magick` image object (see [magick::image_read_pdf()]).
#'
#' @export
st_as_image <- function(x, ...) UseMethod("st_as_image")

#' @rdname st_as_image
#' @export
st_as_image.stable <- function(x,
                               width = getOption("pmtables.image.width", 0.95),
                               border = getOption("pmtables.image.border", "0.2cm 0.7cm"),
                               ...) {
  assert_that(inherits(x, "stable"))
  pdf_file <- st_aspdf(x, border = border, ...)
  st_image_show(pdf_file, width = width)
}

#' @rdname st_as_image
#' @export
st_as_image.pmtable <- function(x, ...) {
  tab <- stable(x)
  st_as_image(tab, ...)
}

#' @rdname st_as_image
#' @export
st_as_image.stobject <- function(x, ...) {
  tab <- stable(x)
  st_as_image(tab, ...)
}

#' Show table output that has been saved to pdf or png format
#'
#' @details
#' This function requires the magick and pdftools packages to be installed.
#'
#' If you are not knitting an rmarkdown document, then the image is read
#' using [magick::image_read_pdf()] (pdf files) or [magick::image_read()]
#' (png files) and resized relative to the width of the current graphics
#' device (see [grDevices::dev.size()]).
#'
#' If you are knitting with pdf output, then the image is read in
#' using [magick::image_read_pdf()] and passed through
#' [knitr::include_graphics()] without resizing. If you are knitting with html
#' output, then the image is read in using [magick::image_read()]
#' and resized using [magick::image_resize()] to a certain fraction of the
#' available space (see `width` argument).
#'
#' @param path path to a png or pdf file saved on disk.
#' @param width for resizing the magick object; must be a value greater than 0
#' and less than or equal to 1; when not knitting, the width is taken as a
#' fraction of the current graphics device width; when knitting and the image
#' is in png format, the width is taken to be a fraction of the available space;
#' this parameter is ignored when knitting pdf output.
#' @param knitting if `TRUE`, the context is assumed to be document knit (see
#' default) and the magick object is returned after resizing; if `FALSE`, then
#' interactive context is assumed and the magick object is returned after
#' resizing according to the current device width.
#'
#' @return
#' Depends the context; see details.
#'
#' @seealso
#' [st_aspng()], [st_aspdf()], [st_as_image()]
#'
#' @export
st_image_show <- function(path,
                          width = getOption("pmtables.image.width", 0.95),
                          knitting = getOption("knitr.in.progress")) {
  require_magick()
  require_pdftools()

  assert_that(is.numeric(width))
  assert_that(length(width)==1)
  assert_that(width > 0 && width <= 2)
  knitting <- isTRUE(knitting)

  format_pdf <- FALSE
  if(requireNamespace("knitr")) {
    format <- knitr::opts_knit$get("rmarkdown.pandoc.to")
    format_pdf <- identical(format, "latex")
  } else {
    knitting <- FALSE
  }
  if(knitting && format_pdf) {
    return(knitr::include_graphics(path))
  }
  if(file_ext(path)=="pdf") {
    img <- magick::image_read_pdf(path)
  } else {
    img <- magick::image_read(path)
  }
  if(knitting) {
    ans <- magick::image_resize(
      img,
      magick::geometry_size_percent(width*100)
    )
    return(ans)
  }
  ans <-  magick::image_resize(
    img,
    magick::geometry_size_pixels(width*grDevices::dev.size("px"))
  )
  return(ans)
}
