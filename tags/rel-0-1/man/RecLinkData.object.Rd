\name{RecLinkData.object}
\alias{RecLinkData}
\title{
  Record Linkage Data Object 
}
\description{
  An object representing information about compared records for Record
  Linkage, as returned by functions \code{\link{compare.dedup}} and
  \code{\link{compare.linkage}}. 
}

\value{
  \item{data}{
    Data frame of original records.
  }
  \item{train,valid}{
    Data frames of data pairs, split into training and validation set (see note 
    below). Each row represents the comparison of two records, 
    identified by columns \code{id1} and \code{id2}. The following columns contain for each
    considered attribute a real number in the range [0..1] representing the degree of
    similarity. These columns are named according to the respective columns in
    \code{data}.
  }
  \item{frequencies}{Numeric vector with average frequency of values for each column 
    included in \code{pairs} (reciprocal of number of distinct values).
  }
  \item{type}{Character string identifying whether a linkage
    ("linkage") or a deduplication ("dedup") project is represented.}

  Furthermore, the components \code{M}, \code{U}, \code{W} and \code{Wdata}
  are present if weights have been calculated. See
  \code{\link{emWeights}} for details.
  
}

\note{
  The possibility to split into training and validation data has no application
  at the moment, but might be used in later versions of the package. Currently,
  only the \code{valid} component is used.
}

\seealso{
\code{\link{compare.dedup}}, \code{\link{compare.linkage}}
}

\author{Andreas Borg}
\keyword{classif}
