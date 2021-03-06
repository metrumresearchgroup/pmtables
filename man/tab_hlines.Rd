% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table-hlines.R
\name{tab_hlines}
\alias{tab_hlines}
\title{Insert hlines into table}
\usage{
tab_hlines(data, hline_at = NULL, hline_from = NULL, ...)
}
\arguments{
\item{data}{the table data frame}

\item{hline_at}{logical or integer vector specifying rows above which an
\verb{\\hline} will be placed; see also \code{\link[=st_hline]{st_hline()}}}

\item{hline_from}{a character column name from which to separate the table
with \verb{\\hline}; non-duplicated values of \code{hline_from} will be used to create
the split; see also \code{\link[=st_hline]{st_hline()}}}

\item{...}{not used}
}
\description{
Insert hlines into table
}
