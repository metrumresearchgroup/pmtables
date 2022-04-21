
require_magick <- function() {
  if(!requireNamespace("magick")) {
    stop("this function requires the magick package to be installed.")
  }
}

build_magick_preview <- function(text, stem, dir) {

  texfile <- paste0(stem, ".tex")
  file <- file.path(dir, stem)
  texfile <- paste0(file, ".tex")
  pdffile <- paste0(file, ".pdf")

  temp_file <- system.file(
    "tex",
    "magick-preview.tex",
    package = "pmtables"
  )

  temp_stem <- basename(temp_file)
  temp_text <- readLines(temp_file)
  env <- list(border_cm = 0.2, texfile = texfile)
  temp_text <- mgluet(temp_text, .envir = env)
  build_file <- file.path(dir, temp_stem)

  writeLines(temp_text, con = build_file)
  writeLines(text, con = texfile)

  args <- c(
    "-halt-on-error",
    glue("-output-directory={dir}"),
    glue("-jobname={basename(file)}"),
    glue("{build_file}")
  )

  x <- system2(
    command = "pdflatex",
    args = args,
    stdout = TRUE,
    stderr = TRUE
  )

  if(!file.exists(pdffile)) {
    warning("the magick preview build didn't work")
    return("failed")
  }

  return(pdffile)
}


#' Generate a preview
#'
#'
#' @export
stmagick <- function(x, width = -1, dir = tempdir(),
                     stem = "pmt-magick-preview", dev_scale = 0.9) {
  require_magick()
  ans <- build_magick_preview(x, stem, dir)
  if(identical(ans, "failed")) {
    return()
  }
  img <- magick::image_read_pdf(ans)
  if(width < 0) {
    width <- dev_scale*grDevices::dev.size("px")
  }
  img <- magick::image_resize(
    img,
    magick::geometry_size_pixels(width = width[1])
  )
  print(img, info = FALSE)

  return(invisible(ans))
}



