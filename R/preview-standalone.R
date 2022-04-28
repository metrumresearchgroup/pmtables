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

#' Some settings for fonts; default is a san serif font
#' @noRd
fonts <- list(
  utopia = list(options = "adobe-utopia", package = "mathdesign"),
  roboto = list(options = "sfdefault", package = "roboto")
)

#' If a build failed, copy an image into the output location that includes
#' text communicating that it failed.
#' @noRd
fail_png <- function(to_file) {
  from_file <- system.file("image", "fail.png", package = "pmtables")
  file.copy(from_file, to_file, overwrite=TRUE)
}

fail_pdf <- function(to_file) {
  from_file <- system.file("image", "fail.pdf", package = "pmtables")
  file.copy(from_file, to_file)
}

#' Convert stable text to a standalone snippet
#' @noRd
st_to_standalone <- function(text, stem, dir, font = c("roboto", "utopia"),
                             command = "latex") {

  out_ext <- ifelse(command=="latex", ".dvi", ".pdf")
  font <- match.arg(font)

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

  env <- list(
    border_cm = 0.2,
    texfile = texfile,
    font = fonts[[font]]
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

  x <- system2(
    command = command,
    args = args,
    stdout = TRUE,
    stderr = TRUE
  )
  ans <- file.path(dir, outfile)
  if(!file.exists(outfile)) {
    warning("the standalone preview build didn't work")
    class(ans) <- "latex-failed"
  }

  # SETWD BACK TO ORIGINAL WORKING DIRECTORY
  return(ans)
}

#' Render stable object in pdf format
#'
#' @param x an stable object.
#' @param stem for intermediate and output file names.
#' @param dir directory for building the image.
#' @param font the font to use; pass `roboto` for sans serif and `utopia`
#' for serif; only these two options are available at this time.
#'
#' @return
#' A string containing the path to the rendered pdf file.
#'
#' @seealso
#' [st_aspng()], [st_as_image()], [st_image_show()]
#'
#' @export
st_aspdf <- function(x, stem = "pmt-standalone-preview", dir = tempdir(),
                     font = "roboto") {
  assert_that(inherits(x, "stable"))
  if(inherits(x, "stable_long")) {
    stop("longtables cannot be rendered with this function.")
  }
  ans <- st_to_standalone(x, stem, dir, command = "pdflatex", font = font)
  if(inherits(ans, "latex-failed")) {
    fail_pdf(unclass(ans))
  }
  ans
}

#' Render stable object in png format
#'
#' This function creates a standalone png snippet using `latex` and
#' `dvipng` from an stable object; both functions are required to be installed
#' for this to work. The resultant `png` file is saved on disk and the relative
#' path to the file is returned.
#'
#' @inheritParams st_aspdf
#' @param dpi dots per inch for the resulting png file.
#'
#' @return
#' A string containing the path to the rendered png file.
#'
#' @seealso
#' [st_aspdf()], [st_as_image()], [st_image_show()]
#'
#' @export
st_aspng <- function(x, stem = "pmt-standalone-preview", dir = tempdir(),
                     dpi = 200, font = "roboto") {
  assert_that(inherits(x, "stable"))
  if(inherits(x, "stable_long")) {
    stop("longtables cannot be rendered with this function.")
  }
  outfile <- st_to_standalone(x, stem, dir, command = "latex", font = font)
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

#' @keywords internal
dvi_to_png <- function(dvifile, pngfile, dpi = 200) {

  if(file.exists(pngfile)) file.remove(pngfile)

  args <- c(
    paste0("-D ", dpi),
    paste0("-o ", pngfile),
    "-q*",
    #"-bg Transparent",
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
#' @param knitting if `TRUE`, the context is assummed to be document knit (see
#' default) and the magick object is returned after resizing; if `FALSE`, then
#' interactive context is assummed and the magick object is returned after
#' resizing according to the current device width.
#'
#' @seealso
#' [st_aspng()], [st_aspdf()], [st_as_image()]
#'
#' @export
st_image_show <- function(path, width = 0.95,
                          knitting = getOption('knitr.in.progress') ) {
  require_magick()
  require_pdftools()

  assert_that(is.numeric(width))
  assert_that(length(width)==1)
  assert_that(width > 0 && width <= 1)
  knitting <- isTRUE(knitting)

  format_pdf <- FALSE
  if(requireNamespace("knitr")) {
    format <- knitr::opts_knit$get('rmarkdown.pandoc.to')
    format_pdf <- identical(format, "latex")
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

#' Display table from pdf image
#'
#' Use this function to run an stable object in to a high-quality pdf file
#' and then include it into a html document.
#'
#' @details
#' This function depends on the magick and pdftools packages being installed.
#'
#' 1. First, `x` is rendered with [st_aspdf()]
#' 1. Next, the pdf file is read using [magick::image_read_pdf()]
#' 2. Finally, the image is possibly resized via [st_image_show()]
#'
#' @inheritParams st_aspng
#' @param width the relative width of the image; passed to [st_image_show()];
#' this must be greater than 0 and less than or equal to 1.
#' @param ... arguments passed to [st_aspdf()].
#'
#' @return
#' A possibly resized `magick` image object (see [magick::image_read_pdf()]).
#'
#' @export
st_as_image <- function(x, width = getOption("pmtables.image.width", 0.8),
                        ...) {

  assert_that(inherits(x, "stable"))
  if(inherits(x, "stable_long")) {
    stop("longtables cannot be rendered with this function.")
  }
  pdf_file <- st_aspdf(x, ...)
  st_image_show(pdf_file, width = width)
}