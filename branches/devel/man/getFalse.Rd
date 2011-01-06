\name{getFalse}
\alias{getFalse}
\alias{getFalsePos}
\alias{getFalseNeg}

\title{
  Inspect misclassification
}
\description{
  Shortcut functions to extract misclassified pairs from a
  \code{"\linkS4class{RLResult}"} object.
}
\usage{
  getFalsePos(object, single.rows = FALSE)
  getFalseNeg(object, single.rows = FALSE)
  getFalse(object, single.rows = FALSE)
}

\arguments{
  \item{object}{Object of class \code{"\linkS4class{RLResult}"}}
  \item{single.rows}{Logical. Wether to display pairs on a single
    line instead of two consecutive lines.}
}

\details{
  \code{getFalsePos} returns false positives, i.e. non-matches classified as
  links.

  \code{getFalseNeg} returns false negatives, i.e. matches classified as
  non-links.
  
  \code{getFalse} returns false positives and false negatives appended in one
    table.
    
  For the output format and the effect of \code{single.rows}, see
  \code{\link{getPairs}}.

}
\value{
  A data frame.
}

\author{
  Andreas Borg, Murat Sariyar
}
\seealso{
  \code{\link{getPairs}}.
}

\keyword{classif}
