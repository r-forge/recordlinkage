\name{levenshteinDist}
\Rdversion{1.1}
\alias{levenshteinDist}
\title{
  Compute Levenshtein distance
}
\description{
  Computes the Levenshtein edit distance for two character
  vectors.
}
\usage{
levenshteinDist(str1, str2)
}
\arguments{
  \item{str1, str2}{The character vectors to compare.
  }
}
\details{
  The function is vectorized and supports recycling. If both \code{str1} and
  \code{str2} are arrays, their dimensions must agree. The elements of 
  the shorter vector of \code{str1} and \code{str2} are recycled as necessary.
}

\value{
  An integer vector with the computed edit distance.
  }

\author{
  Andreas Borg (interface to GNU implementation)
}

\seealso{
  \code{\link{levenshteinSim}} for a string metric based on edit distance,
  \code{\link{jarowinkler}} for the Jaro-Winkler string metric.
}
\examples{
# compare two strings:
levenshteinDist("Andreas","Anreas")
# compare one string with several others:
levenshteinDist("Andreas",c("Anreas","Andeas"))
# compare two vectors of strings:
levenshteinDist(c("Andreas","Borg"),c("Andreas","Bork"))
}
\keyword{misc }
