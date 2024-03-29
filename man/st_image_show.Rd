% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preview-standalone.R
\name{st_image_show}
\alias{st_image_show}
\title{Show table output that has been saved to pdf or png format}
\usage{
st_image_show(
  path,
  width = getOption("pmtables.image.width", 0.95),
  knitting = getOption("knitr.in.progress")
)
}
\arguments{
\item{path}{path to a png or pdf file saved on disk.}

\item{width}{for resizing the magick object; must be a value greater than 0
and less than or equal to 1; when not knitting, the width is taken as a
fraction of the current graphics device width; when knitting and the image
is in png format, the width is taken to be a fraction of the available space;
this parameter is ignored when knitting pdf output.}

\item{knitting}{if \code{TRUE}, the context is assumed to be document knit (see
default) and the magick object is returned after resizing; if \code{FALSE}, then
interactive context is assumed and the magick object is returned after
resizing according to the current device width.}
}
\value{
Depends the context; see details.
}
\description{
Show table output that has been saved to pdf or png format
}
\details{
This function requires the magick and pdftools packages to be installed.

If you are not knitting an rmarkdown document, then the image is read
using \code{\link[magick:editing]{magick::image_read_pdf()}} (pdf files) or \code{\link[magick:editing]{magick::image_read()}}
(png files) and resized relative to the width of the current graphics
device (see \code{\link[grDevices:dev.size]{grDevices::dev.size()}}).

If you are knitting with pdf output, then the image is read in
using \code{\link[magick:editing]{magick::image_read_pdf()}} and passed through
\code{\link[knitr:include_graphics]{knitr::include_graphics()}} without resizing. If you are knitting with html
output, then the image is read in using \code{\link[magick:editing]{magick::image_read()}}
and resized using \code{\link[magick:transform]{magick::image_resize()}} to a certain fraction of the
available space (see \code{width} argument).
}
\seealso{
\code{\link[=st_aspng]{st_aspng()}}, \code{\link[=st_aspdf]{st_aspdf()}}, \code{\link[=st_as_image]{st_as_image()}}
}
