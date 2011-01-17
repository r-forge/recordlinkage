\name{show}

\alias{show}
\alias{show,RLBigData-method}

\title{
  Show a RLBigData object
}
\description{
  Shows summarized information on a \code{"\linkS4class{RLBigData}"} object.
}

\usage{
  \S4method{show}{RLBigData}(object)
}
\arguments{
  \item{object}{The object for which to show a summary.}
}
\details{
  Object will be shown automatically by a call to \code{show}.
}
\value{
  \code{show} returns an invisible \code{NULL}.
}

\author{
  Andreas Borg, Murat Sariyar
}

\seealso{
  \code{\link[methods]{show}}
}
\examples{
  data(RLdata500)
  rpairs <- RLBigDataDedup(RLdata500)
  rpairs
}
\keyword{methods}
