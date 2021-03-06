% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/continuous_table.R
\name{pt_cont_wide}
\alias{pt_cont_wide}
\title{Continuous data summary in wide format}
\usage{
pt_cont_wide(
  data,
  cols,
  by = ".total",
  panel = by,
  table = NULL,
  units = NULL,
  digits = new_digits(),
  all_name = "All data",
  fun = cont_wide_fun,
  na_fill = "--",
  id_col = "ID"
)
}
\arguments{
\item{data}{the data frame to summarize; the user should filter or subset
so that \code{data} contains exactly the records to be summarized; pmtables will
not add or remove rows prior to summarizing \code{data}}

\item{cols}{the columns to summarize; may be character vector or quosure}

\item{by}{a grouping variable; may be character vector or quosure}

\item{panel}{data set column name to stratify the summary}

\item{table}{a named list to use for renaming columns (see details and
examples)}

\item{units}{a named list to use for unit lookup (see details and examples)}

\item{digits}{a \code{digits} object (see \code{\link[=new_digits]{new_digits()}})}

\item{all_name}{a name to use for the complete data summary}

\item{fun}{the data summary function (see details)}

\item{na_fill}{value to fill with when all values in the summary are missing}

\item{id_col}{the ID column name}
}
\value{
An object with class \code{pmtable}; see \link{class-pmtable}.
}
\description{
This function summarizes your data in a specific way and returns an object
that can be converted into a \code{latex} table.
}
\details{
The default summary function is \code{\link[=cont_wide_fun]{cont_wide_fun()}}. Please review that
documentation for details on the default summary for this table.

The notes for this table are generated by \code{\link[=pt_cont_wide_notes]{pt_cont_wide_notes()}}.
}
\section{Custom summary function}{

The summary function (\code{fun}) should take \code{value} as the first argument and
return a data frame or tibble with one row and one column named \code{summary}.
The function can also accept an \code{id} argument which is a vector of \code{IDs}
that is the same length as \code{value}. Be sure to include \code{...} to the function
signature as other arguments will be passed along. Make sure your function
completely formats the output ... it will appear in the table as you return
from this function. See \code{\link[=cont_wide_fun]{cont_wide_fun()}} for details on the default
implementation.
}

\examples{
pmtables:::cont_wide_fun(rnorm(100))

out <- stable(pt_cont_wide(pmt_first, cols = "AGE,WT,SCR"))
out

\dontrun{
st2report(out)
}

}
