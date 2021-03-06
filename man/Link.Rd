% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pandocfilters.R
\name{Link}
\alias{Link}
\title{Hyperlink}
\usage{
Link(target, text, title = "", attr = Attr())
}
\arguments{
\item{target}{a character string giving the target (hyper reference)}

\item{text}{a inline object or a list of inline objects giving the visible part}

\item{title}{an optional character string giving the title}

\item{attr}{an optional object of type \code{"Attr"}}
}
\description{
Constructs an inline object of type \code{"Link"}.
}
\details{
Further Usage examples can be found in the README.
}
\examples{
Link("https://cran.r-project.org/", "Text_Shown", "some title")
}
