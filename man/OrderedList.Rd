% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pandocfilters.R
\name{OrderedList}
\alias{OrderedList}
\title{Ordered List}
\usage{
OrderedList(lattr, llblocks)
}
\arguments{
\item{lattr}{a list of attributes}

\item{llblocks}{a list of lists of blocks}
}
\description{
Constructs a block object of type \code{"OrderedList"}.
}
\examples{
ordered_1 <- Plain("A")
ordered_2 <- list(Plain(Str("B")))
ordered_3 <- list(Plain(list(Str("C"))))
OrderedList(ListAttributes(), ordered_1)
OrderedList(ListAttributes(), list(ordered_1, ordered_2, ordered_3))
}
