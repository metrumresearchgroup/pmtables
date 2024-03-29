% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table-cols.R
\name{tab_cols}
\alias{tab_cols}
\title{Form table column names}
\usage{
tab_cols(
  cols,
  cols_replace = NULL,
  cols_rename = NULL,
  cols_blank = NULL,
  cols_split = NULL,
  cols_bold = FALSE,
  cols_break = "...",
  cols_extra = NULL,
  cols_omit = NULL,
  ...
)
}
\arguments{
\item{cols}{the starting set of table column names}

\item{cols_replace}{a character vector with the same length as the number of
output table columns; use this to completely replace the names (as opposed
to one by on editing with \code{col_rename})}

\item{cols_rename}{a \code{name = value} character vector to translate column names
to table names; ; see also \code{\link[=st_rename]{st_rename()}}}

\item{cols_blank}{a character vector of column names that will not be printed
in the table header; see also \code{\link[=st_blank]{st_blank()}}}

\item{cols_split}{a string that is used to split column labels into tag
(on the left) or name (on the right); if supplied, then \code{col_split} will be
used to remove the tag; for example, a column named \code{x.WT} would be renamed
\code{WT} if \code{cols_split} was set to \code{.}}

\item{cols_bold}{if \code{TRUE}, table column names are rendered with bold font}

\item{cols_break}{character sequence to break column names into new lines}

\item{cols_extra}{a data frame with extra column header information; the data
frame should have the same columns, in the same order as the table data
frame (see \code{data} argument to \code{\link[=stable]{stable()}}); the data frame can have any number
of rows; all of the rows in \code{cols_extra} will be placed between the column
label and the unit (if \code{units} is supplied)}

\item{cols_omit}{if \code{TRUE}, then column names are suppressed in the table
output along with units; column spanners are retained}

\item{...}{not used}
}
\description{
Form table column names
}
