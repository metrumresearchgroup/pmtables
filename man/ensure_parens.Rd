% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{ensure_parens}
\alias{ensure_parens}
\title{Add parens if not found}
\usage{
ensure_parens(x)
}
\arguments{
\item{x}{a list or vector}
}
\value{
\code{x} is returned, possibly modified with parens added.
}
\description{
Opening and closing parens will be added if an opening paren is not the
first non-whitespace character.
}
\examples{
ensure_parens(letters[1:4])

ensure_parens(as.list(letters[1:4]))

ensure_parens(c("(a)", "b", "(c)"))

}
