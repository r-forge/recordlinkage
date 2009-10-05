\name{levenshteinSim}
\Rdversion{1.1}
\alias{levenshteinSim}
\title{
  Compute Levenshtein string metric
}
\description{
  Computes a string metric based on the Levenshtein edit distance
  for two character vectors.
}
\usage{
levenshteinSim(str1, str2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{str1, str2}{The character vectors to compare.
  }
}

\details{
  The similarity of two strings is computed by \cr
  \code{1-(\link{levenshteinDist}(str1,str2)/pmax(nchar(str1),nchar(str2))))},
  which returns a number from 0 (maximal disagreement) and 1 (equality).
  The function is vectorized and supports recycling. If both \code{str1} and
  \code{str2} are arrays, their dimensions must agree. The elements of 
  the shorter vector of \code{str1} and \code{str2} are recycled as necessary.}


\value{A numeric vector with similarity values in the interval 
  \eqn{[0,1]}{[0,1]}.
}

\author{Andreas Borg}


\seealso{
  \code{\link{levenshteinDist}} for Levenshtein edit distance,
  \code{\link{jarowinkler}} for the Jaro-Winkler string metric.
}
\examples{
# compare two strings:
jarowinkler("Andreas","Anreas")
# compare one string with several others:
jarowinkler("Andreas",c("Anreas","Andeas"))
# compare two vectors of strings:
jarowinkler(c("Andreas","Borg"),c("Andreas","Bork"))
}

\keyword{misc}
