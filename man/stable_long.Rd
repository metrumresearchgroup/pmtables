% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table-long.R
\name{stable_long}
\alias{stable_long}
\alias{stable_long.data.frame}
\alias{stable_long.stobject}
\alias{stable_long.pmtable}
\title{Create longtable output from an R data frame}
\usage{
stable_long(data, ...)

\method{stable_long}{data.frame}(
  data,
  note_config = noteconf(type = "minipage"),
  inspect = FALSE,
  lt_cap_macro = "",
  lt_cap_text = "",
  lt_cap_short = "",
  lt_cap_label = "",
  lt_continue = "\\\\footnotesize{continued on next page}",
  ...
)

\method{stable_long}{stobject}(data, ...)

\method{stable_long}{pmtable}(data, ...)
}
\arguments{
\item{data}{an object to render as a long table; this could be a \code{data.frame},
a \code{pmtable} object or an \code{stobject}; when passing in a \code{data.frame}, the data
should be filtered or subset so that \code{data} contains exactly the rows (and
columns) to be processed; pmtables will not add or remove rows prior to
processing \code{data}}

\item{...}{passed to \code{\link[=stable]{stable()}}}

\item{note_config}{a \code{\link[=noteconf]{noteconf()}} object used to configure how table notes
are displayed; ; see also \code{\link[=st_noteconf]{st_noteconf()}}}

\item{inspect}{fixed to \code{TRUE} and passed to \code{\link[=stable]{stable()}}}

\item{lt_cap_macro}{the name of a macro that will hold caption text; to not
lead with \verb{\\\\} - this will be added for you}

\item{lt_cap_text}{full caption text, appearing where the table is rendered}

\item{lt_cap_short}{short caption text, appearing in the list of tables}

\item{lt_cap_label}{table label for use in latex document}

\item{lt_continue}{longtable continuation message}
}
\description{
Create longtable output from an R data frame
}
