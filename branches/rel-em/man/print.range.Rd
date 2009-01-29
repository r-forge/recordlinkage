\name{print.range}
\alias{print.range}

\title{Print a Range of Record Pairs}
\description{
  Prints weighted record pairs within a certain weights range for
  threshold assignment.
}
\usage{
print.range(object, threshold_upper = Inf, threshold_lower = 0)
}
\arguments{
  \item{object}{A \code{\link{RecLinkResult}} object containing weight information}
  \item{threshold_upper}{Upper weight bound}
  \item{threshold_lower}{Lower weight bound}
}
\details{
  Input is a set of record pairs which were assigned weights with 
  \code{\link{emWeights}}. They are sorted by weight and those for which
  the weigth \code{object$Wdata} holds the condition 
  \code{object$Wdata < threshold_upper && object$Wdata >= threshold_lower}
  are returned.
}
\value{
  A data frame with two rows for each record pair, each row holding one
  record. The first column of every odd row lists the assigned weight,
  all following columns correspond to the data fields.
}
\author{Andreas Borg}

\seealso{code{\link{print.results}}}
\keyword{classif}
