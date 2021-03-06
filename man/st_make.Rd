% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table-object.R
\name{st_make}
\alias{st_make}
\title{Convert st object to table output}
\usage{
st_make(x, ..., .preview = FALSE, .cat = FALSE, long = FALSE)
}
\arguments{
\item{x}{an stobject}

\item{...}{other arguments passed to \code{\link[=stable]{stable()}}}

\item{.preview}{if \code{TRUE}, pass result to \code{\link[=st_preview]{st_preview()}}}

\item{.cat}{if \code{TRUE}, pass result to \code{\link[=st_wrap]{st_wrap()}}; use this option to write
table text to standard output while creating Rmarkdown documents with pdf
output}

\item{long}{render as longtable}
}
\value{
The latex code for the table.
}
\description{
Convert st object to table output
}
\examples{

library(dplyr)

ob <- st_new(ptdata())

st_make(ob)

st_make(ob, .cat = TRUE)

\dontrun{
st_make(ob, .preview = TRUE)
}

}
