% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pandocfilters.R
\name{as.inline}
\alias{as.inline}
\title{Inline Objects}
\usage{
as.inline(x)
}
\arguments{
\item{x}{an object of type \code{"NULL"}, \code{"character"} or \code{"inline"}.}
}
\value{
an object of class \code{"inline"}.
}
\description{
Objects of the classes \code{"NULL"} and \code{"character"} 
   can be coerced to \code{"inline"}.
}
\examples{
as.inline("some text")
as.inline(NULL)
}
