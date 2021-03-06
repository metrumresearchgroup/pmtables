% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{sig}
\alias{sig}
\alias{digit1}
\title{Format digits}
\usage{
sig(x, digits = 3, maxex = NULL)

digit1(x)
}
\arguments{
\item{x}{numeric, value to manipulate}

\item{digits}{numeric, number of significant digits Default: 3}

\item{maxex}{numeric, maximum number of significant
digits before moving to scientific notation, Default: NULL}
}
\value{
character vector of formatted values
}
\description{
Use \code{\link[=sig]{sig()}} to set the number of significant digits; use \code{\link[=digit1]{digit1()}} to limit
to one digit.  See examples.
}
\examples{
sig(1.123455)
sig(0.123455)
sig(1.123455,digits = 5)
sig(1123,maxex = 3)
sig(1123,maxex = 4)

digit1(1.234)
digit1(1321.123)

}
