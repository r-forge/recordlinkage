\name{errorMeasures}
\Rdversion{1.1}
\alias{errorMeasures}

\title{
  Calculate Error Measures
}
\description{
  Computes various error measures for the classification of a data set.
}
\usage{
errorMeasures(result)
}

\arguments{
  \item{result}{
    A \code{\link{RecLinkResult}} object.
  }
}

\details{
  A wrapper function for S4 method \code{\link{getErrorMeasures}} with
  signature \code{\link[=RecLinkResult-class]{"RecLinkResult"}}, included
  for back-compatibility.
}
\value{
  A list with components \code{alpha}, \code{beta}, \code{accuracy}, 
  \code{precision}, \code{sensitivity} and \code{specificity}, each a number in
  the range \eqn{[0,1]}.
}

\author{
  Murat Sariyar, Andreas Borg
}

\keyword{classif}

