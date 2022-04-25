require_magick <- function() {
  if(!requireNamespace("magick", quietly = TRUE)) {
    stop("this function requires the magick package to be installed.")
  }
}

require_pdftools <- function() {
  if(!requireNamespace("pdftools", quietly = TRUE)) {
    stop("this function requires the pdftools package to be installed.")
  }
}

fonts <- list(
  utopia = list(options = "adobe-utopia", package = "mathdesign"),
  roboto = list(options = "sfdefault", package = "roboto")
)

fail_png <- function(to_file) {
  from_file <- system.file("image", "fail.png", package = "pmtables")
  file.copy(from_file, to_file, overwrite=TRUE)
}

fail_pdf <- function(to_file) {
  from_file <- system.file("image", "fail.pdf", package = "pmtables")
  file.copy(from_file, to_file)
}

st_to_standalone <- function(text, stem, dir, font = c("roboto", "utopia"),
                             command = "latex") {

  out_ext <- ifelse(command=="latex", ".dvi", ".pdf")
  font <- match.arg(font)

  if(!dir.exists(dir)) dir.create(dir)

  cwd <- getwd()
  on.exit(setwd(cwd), add = TRUE)
  setwd(dir)

  texfile <- paste0(stem, ".tex")
  outfile <- paste0(stem, out_ext)

  temp_file <- system.file(
    "tex",
    "standalone-preview.tex",
    package = "pmtables"
  )
  temp_stem <- basename(temp_file)
  temp_text <- readLines(temp_file)
  env <- list(
    border_cm = 0.2,
    texfile = texfile,
    font = fonts[[font]]
  )
  temp_text <- mgluet(temp_text, .envir = env)
  build_file <- temp_stem
  writeLines(temp_text, con = build_file)
  writeLines(text, con = texfile)
  args <- c(
    "-halt-on-error",
    paste0("-jobname=", stem),
    build_file
  )

  if(file.exists(outfile)) file.remove(outfile)

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
#' A string containing the full path to the rendered pdf file.
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
#' @inheritParams st_aspdf
#' @param dpi dots per inch for the resulting png file.
#'
#' @return
#' A string containing the full path to the rendered png file.
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
    dvi_to_png(outfile, png_file, dpi = dpi)
  }
  png_file
}

#' @keywords internal
dvi_to_png <- function(dvifile, pngfile, dpi = 200) {
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
  return(pngfile)
}

#' Read table output that has been saved to pdf or png format
#'
#' This function requires the magick and pdftools packages to be installed.
#'
#' @param path path to a png or pdf file saved on disk.
#' @param width for resizing the magick object; depends on context.
#' @param knitting if `TRUE`, the context is assummed to be document knit (see
#' default) and the magick object is returned after resizing; if `FALSE`, then
#' interactive context is assummed and the magick object is returned after
#' resizing according to the current device width.
#' @export
st_image_show <- function(path, width = 0.95,
                          knitting = getOption('knitr.in.progress') ) {
  require_magick()
  require_pdftools()
  knitting <- isTRUE(knitting)
  if(tools::file_ext(path)=="pdf") {
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
#' 2. Finally, the image is resized if `width` is passed.
#'
#' @inheritParams st_aspng
#' @param width the width as percent; passed to
#' [magick::geometry_size_percent()].
#' @param ... arguments passed to [st_aspdf()].
#'
#' @return
#' A possibly resized image object (see [magick::image_read_pdf()]).
#'
#' @export
st_inline_pdf <- function(x,
                          width = getOption("pmtables.st_inline.width", 0.8),
                          ...) {
  require_magick()
  require_pdftools()
  assert_that(inherits(x, "stable"))
  assert_that(is.numeric(width))
  assert_that(length(width)==1)
  assert_that(width > 0 && width <= 1)
  if(inherits(x, "stable_long")) {
    stop("longtables cannot be rendered with this function.")
  }
  pdf_file <- st_aspdf(x, ...)
  st_image_show(pdf_file, width = width)
}
