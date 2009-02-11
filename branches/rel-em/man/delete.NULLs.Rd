\name{delete.NULLs}
\alias{delete.NULLs}

\title{Remove NULL Values}
\description{
  Removes all \code{NULL} elements from a list or vector.
}
\usage{
delete.NULLs(x)
}

\arguments{
  \item{x}{A vector or list}
}
\details{
  Removes all \code{NULL} elements.
}
\value{
  The returned object has the same type as \code{x}.
}

\references{Taken from https://stat.ethz.ch/pipermail/r-help/2006-August/111896.html}

\author{}
\note{This function is internally used for simple lists and vectors. 
  The behaviour for nested lists and vectors embedded in lists is not 
  thorougly tested.
}

\keyword{misc}
